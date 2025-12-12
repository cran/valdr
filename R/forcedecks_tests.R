#' Get ForceDecks tests
#'
#' Retrieves ForceDecks test data with optional filtering by start date and profile ID.
#'
#' @param start_date Optional ISO 8601 UTC date string (e.g., "2025-06-25T00:00:00Z").
#' @param profile_id Optional Profile ID to filter results.
#' @param include_attributes Logical; if FALSE (default), returns only the tests
#'   data frame. If TRUE, returns a named list with components \code{tests}
#'   (the tests data frame) and \code{attributes} (a long-format attributes
#'   mapping table).
#'
#' @return If \code{include_attributes = FALSE}, a data frame containing
#'   ForceDecks test results matching the optional filters. If no tests are
#'   found, returns an empty data frame.
#'   If \code{include_attributes = TRUE}, a list with elements \code{tests}
#'   and \code{attributes}. Returned invisibly.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
get_forcedecks_tests <- function(start_date = NULL,
                                 profile_id = NULL,
                                 include_attributes = FALSE) {
    config <- get_config(quiet = TRUE)
    access_token <- authenticate()

    # Resolve start_date from argument or stored config
    if (is.null(start_date)) {
        start_date <- get_start_date()
        if (is.null(start_date)) {
            stop("Start date not set. Please call `set_start_date(\"<ISO 8601 UTC>\")` first.")
        }
    } else {
        # Validate format if supplied directly
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", start_date)) {
            stop("`start_date` must be in ISO 8601 format: e.g., '2025-06-25T00:00:00Z'")
        }
    }

    modified_from_utc <- start_date
    all_tests <- list()

    repeat {
        url <- paste0(config$endpoints$forcedecks, "/tests")

        # Build query parameters
        query_params <- list(
            tenantId = config$tenant_id,
            modifiedFromUtc = modified_from_utc
        )
        if (!is.null(profile_id)) {
            query_params$profileId <- profile_id
        }

        # Perform GET request with httr
        response <- tryCatch(
            httr::GET(
                url   = url,
                query = query_params,
                .add_vald_headers(access_token)
            ),
            error = function(e) {
                stop("Failed to connect to the ForceDecks API: ", e$message, call. = FALSE)
            }
        )
        # Consistent response handling
        .handle_api_response(response)

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

        if (!is.null(test_data$tests) && length(test_data$tests) > 0L) {
            all_tests <- append(all_tests, test_data$tests)
            modified_from_utc <- test_data$tests[[length(test_data$tests)]]$modifiedDateUtc
            message("Continuing pagination from ", modified_from_utc)
        } else {
            message("No tests returned, stopping pagination.")
            break
        }

        Sys.sleep(0.2) # Pause to respect rate limits
    }

    if (length(all_tests) == 0L) {
        return(invisible(data.frame()))
    }

    tests_df <- data.frame(
        testId               = .safe_extract(all_tests, "testId"),
        tenantId             = .safe_extract(all_tests, "tenantId"),
        profileId            = .safe_extract(all_tests, "profileId"),
        recordingId          = .safe_extract(all_tests, "recordingId"),
        modifiedDateUtc      = .safe_extract(all_tests, "modifiedDateUtc"),
        recordedDateUtc      = .safe_extract(all_tests, "recordedDateUtc"),
        recordedDateOffset   = .safe_extract(all_tests, "recordedDateOffset"),
        recordedDateTimezone = .safe_extract(all_tests, "recordedDateTimezone"),
        analysedDateUtc      = .safe_extract(all_tests, "analysedDateUtc"),
        analysedDateOffset   = .safe_extract(all_tests, "analysedDateOffset"),
        analysedDateTimezone = .safe_extract(all_tests, "analysedDateTimezone"),
        testType             = .safe_extract(all_tests, "testType"),
        weight               = .safe_extract(all_tests, "weight"),
        stringsAsFactors     = FALSE
    )

    # Save new start_date to config based on last modified
    latest_mod_time <- max(tests_df$modifiedDateUtc, na.rm = TRUE)
    set_start_date(latest_mod_time)

    if (isTRUE(include_attributes)) {
        attributes_df <- .build_test_attributes_long(all_tests)
        result <- list(
            tests      = tests_df,
            attributes = attributes_df
        )
        return(invisible(result))
    }

    invisible(tests_df)
}
