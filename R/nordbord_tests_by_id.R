#' Get a single NordBord test by ID
#'
#' Calls the NordBord API endpoint `/tests/{testId}` to retrieve one test record.
#'
#' @param ... The GUID of the NordBord test. Input as a string.
#' @return A data frame containing the test details, or an empty data frame if not found.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
get_nordbord_tests_by_id <- function(...) {
    # Argument validation
    test_id <- .validate_single_guid(...)

    config <- get_config(quiet = TRUE)
    token <- authenticate()

    url <- paste0(config$endpoints$nordbord, "/tests/", utils::URLencode(test_id, reserved = TRUE))
    query_params <- list(tenantId = config$tenant_id)

    # Make API request
    resp <- tryCatch(
        httr::GET(url, query = query_params, .add_vald_headers(token)),
        error = function(e) stop("Failed to call NordBord API: ", e$message, call. = FALSE)
    )

    # Consistent response handling
    .handle_api_response(resp)

    if (resp$status_code == 204) {
        message("No test found for the given ID.")
        return(data.frame())
    }

    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(
        jsonlite::fromJSON(body, simplifyVector = FALSE),
        error = function(e) stop("Failed to parse NordBord response: ", e$message, call. = FALSE)
    )

    if (length(parsed) == 0) {
        return(data.frame())
    }

    # Build dataframe using helper function
    .build_nordbord_df(list(parsed))
}
