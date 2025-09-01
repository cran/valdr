#' Get ForceFrame Tests
#'
#' Fetches ForceFrame test records modified since a given start date.
#' Supports automatic pagination and updates the persistent `start_date` after each call.
#' Returns a data frame of test records, with null values replaced by blank strings ("").
#'
#' @param start_date Optional ISO 8601 UTC date string (e.g., "2025-06-25T00:00:00Z"). Input as a string.
#' @param profile_id Optional Profile ID to filter results. Input as a string.
#' @return A data frame containing ForceFrame test results matching the optional filters. If no tests are found, returns an empty data frame. Returned invisibly.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
get_forceframe_tests <- function(start_date = NULL, profile_id = NULL) {
    config <- get_config(quiet = TRUE)
    access_token <- authenticate()

    # Resolve the start_date from argument or persistent config
    if (is.null(start_date)) {
        start_date <- get_start_date()
        if (is.null(start_date)) {
            stop("Start date not set. Please call `set_start_date(\"<ISO 8601 UTC>\")` first.")
        }
    } else {
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", start_date)) {
            stop("`start_date` must be in ISO 8601 format: e.g., '2025-06-25T00:00:00Z'")
        }
    }

    # Validate profile_id if supplied
    if (!is.null(profile_id)) {
        profile_id <- .validate_single_guid(profile_id, arg_name = "`profile_id`")
    }

    modified_from_utc <- start_date
    all_tests <- list()

    # Begin pagination loop
    repeat {
        url <- paste0(config$endpoints$forceframe, "/tests/v2")

        query_params <- list(
            tenantId = config$tenant_id,
            modifiedFromUtc = modified_from_utc
        )
        if (!is.null(profile_id)) {
            query_params$profileId <- profile_id
        }

        # Send request with headers
        response <- tryCatch(
            httr::GET(
                url = url,
                query = query_params,
                .add_vald_headers(access_token)
            ),
            error = function(e) {
                stop("Failed to connect to the ForceFrame API: ", e$message, call. = FALSE)
            }
        )

        # Consistent response handling
        .handle_api_response(response)

        # 204 = no content (pagination complete)
        if (response$status_code == 204) {
            message("No more tests to retrieve. Stopping pagination.")
            break
        }

        body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
        test_data <- tryCatch(
            jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
            error = function(e) {
                stop("Failed to parse JSON from API: ", e$message, call. = FALSE)
            }
        )

        # If tests are returned, append to list and update modified timestamp
        if (!is.null(test_data$tests) && length(test_data$tests) > 0) {
            all_tests <- append(all_tests, test_data$tests)
            modified_from_utc <- test_data$tests[[length(test_data$tests)]]$modifiedDateUtc
            message("Continuing pagination from ", modified_from_utc)
        } else {
            message("No tests returned, stopping pagination.")
            break
        }

        # Respect rate limits
        Sys.sleep(0.2)
    }

    if (length(all_tests) == 0) {
        return(data.frame())
    }

    # Build dataframe using helper function
    forceframe_df <- .build_forceframe_df(all_tests)

    # Update persistent start_date
    # latest_mod_time as ISO8601 with milliseconds e.g. 2025-07-01T03:37:15.712Z, this needs to be cleaned
    latest_mod_time <- max(forceframe_df$modifiedDateUtc, na.rm = TRUE)

    # Remove fractional seconds for compatibility with start_date requirements
    latest_mod_time_clean <- sub("\\.\\d+Z$", "Z", latest_mod_time)
    set_start_date(latest_mod_time_clean)

    return(forceframe_df)
}
