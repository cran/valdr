#' Get NordBord Tests
#'
#' Retrieves all NordBord test data modified since a given start date.
#' Automatically paginates through results using `modifiedFromUtc`, and safely
#' detects infinite loops using the last seen test ID.
#'
#' @param start_date Optional ISO 8601 UTC date string (e.g., "2025-06-25T00:00:00Z"). Input as a string.
#' @param profile_id Optional Profile ID to filter results. Input as a string.
#'
#' @return A data frame containing NordBord test results matching the optional filters. If no tests are found, returns an empty data frame. Returned invisibly.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
get_nordbord_tests <- function(start_date = NULL, profile_id = NULL) {
    config <- get_config(quiet = TRUE)
    token <- authenticate()

    # Resolve or validate start date
    if (is.null(start_date)) {
        start_date <- get_start_date()
        if (is.null(start_date)) {
            stop("No start date set. Use `set_start_date(\"<ISO 8601 UTC>\")` first.")
        }
    } else if (!grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", start_date)) {
        stop("`start_date` must be in ISO 8601 format: e.g., '2025-08-01T00:00:00Z'")
    }

    # Validate profile_id if supplied
    if (!is.null(profile_id)) {
        profile_id <- .validate_single_guid(profile_id, arg_name = "`profile_id`")
    }

    modified_from_utc <- start_date
    all_tests <- list()
    last_test_id <- NULL

    repeat {
        url <- paste0(config$endpoints$nordbord, "/tests/v2")

        # Build query params
        query <- list(
            tenantId = config$tenant_id,
            modifiedFromUtc = modified_from_utc
        )
        if (!is.null(profile_id)) {
            query$profileId <- profile_id
        }

        # Make the request
        resp <- tryCatch(
            httr::GET(url, query = query, .add_vald_headers(token)),
            error = function(e) stop("Failed to call NordBord API: ", e$message, call. = FALSE)
        )
        .handle_api_response(resp)

        if (resp$status_code == 204) {
            message("No more data. Stopping pagination.")
            break
        }

        body <- httr::content(resp, as = "text", encoding = "UTF-8")
        parsed <- tryCatch(
            jsonlite::fromJSON(body, simplifyVector = FALSE),
            error = function(e) stop("Failed to parse NordBord response: ", e$message, call. = FALSE)
        )

        tests <- parsed$tests
        if (length(tests) == 0) {
            message("No tests returned. Stopping pagination.")
            break
        }

        # API logic handling - check if we're on the last test
        new_last_id <- tests[[length(tests)]]$testId
        if (!is.null(last_test_id) && new_last_id == last_test_id) {
            break
        }

        all_tests <- append(all_tests, tests)
        modified_from_utc <- tests[[length(tests)]]$modifiedDateUtc
        last_test_id <- new_last_id
        message("Continuing pagination from ", modified_from_utc)

        # Respect rate limits
        Sys.sleep(0.2)
    }

    if (length(all_tests) == 0) {
        return(data.frame())
    }

    # Build dataframe using helper function
    nordbord_df <- .build_nordbord_df(all_tests)

    # Update persistent start_date
    # latest_mod_time as ISO8601 with milliseconds e.g. 2025-07-01T03:37:15.712Z, this needs to be cleaned
    latest_mod_time <- max(nordbord_df$modifiedDateUtc, na.rm = TRUE)

    # Remove fractional seconds for compatibility with start_date requirements
    latest_mod_time_clean <- sub("\\.\\d+Z$", "Z", latest_mod_time)
    set_start_date(latest_mod_time_clean)

    return(nordbord_df)
}
