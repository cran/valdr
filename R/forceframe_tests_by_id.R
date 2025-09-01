#' Get a single ForceFrame test by ID
#'
#' Calls the ForceFrame API endpoint `/tests/{testId}` to retrieve one test record.
#'
#' @param ... The GUID of the ForceFrame test. Input as a string.
#' @return A data frame containing the test details, or an empty data frame if not found.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
get_forceframe_tests_by_id <- function(...) {
    # Argument validation
    test_id <- .validate_single_guid(...)

    # Build API request
    config <- get_config(quiet = TRUE)
    access_token <- authenticate()

    url <- paste0(
        config$endpoints$forceframe,
        "/tests/",
        utils::URLencode(test_id, reserved = TRUE)
    )
    query_params <- list(tenantId = config$tenant_id)

    response <- tryCatch(
        httr::GET(url, query = query_params, .add_vald_headers(access_token)),
        error = function(e) {
            stop("Failed to connect to the ForceFrame API: ", e$message, call. = FALSE)
        }
    )

    # Consistent response handling
    .handle_api_response(response)

    if (response$status_code == 204) {
        message("No test found for the given ID.")
        return(data.frame())
    }

    body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(
        jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
        error = function(e) {
            stop("Failed to parse JSON from API: ", e$message, call. = FALSE)
        }
    )

    if (length(parsed) == 0) {
        return(data.frame())
    }

    # Build dataframe using helper function
    .build_forceframe_df(list(parsed))
}