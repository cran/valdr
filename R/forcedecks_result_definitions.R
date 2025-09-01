#' Get ForceDecks result definitions
#'
#' Retrieves all available result definitions from the ForceDecks API.
#'
#' @return A data frame where each row corresponds to a ForceDecks result definition retrieved from the API. Returned invisibly.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
get_forcedecks_result_definitions <- function() {
  config <- get_config(quiet = TRUE)
  access_token <- authenticate()

  url <- paste0(config$endpoints$forcedecks, "/resultdefinitions")

  # Perform GET request with httr
  response <- tryCatch(
    httr::GET(
      url = url,
      .add_vald_headers(access_token)
    ),
    error = function(e) {
      stop("Failed to connect to the ForceDecks API: ", e$message, call. = FALSE)
    }
  )

  # Centralised error + empty response handling, get body text back
  body_txt <- .handle_api_response(response)

  # Parse JSON safely
  parsed <- tryCatch(
    jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse JSON from API: ", e$message, call. = FALSE)
    }
  )

  # Validate presence of resultDefinitions
  if (is.null(parsed$resultDefinitions) || length(parsed$resultDefinitions) == 0) {
    stop("No result definitions found in the API response.", call. = FALSE)
  }

  # Build data.frame using null-safe extraction
  results_df <- data.frame(
    resultId             = .safe_extract(parsed$resultDefinitions, "resultId"),
    resultIdString       = .safe_extract(parsed$resultDefinitions, "resultIdString"),
    resultName           = .safe_extract(parsed$resultDefinitions, "resultName"),
    resultDescription    = .safe_extract(parsed$resultDefinitions, "resultDescription"),
    resultGroup          = .safe_extract(parsed$resultDefinitions, "resultGroup"),
    supportsAsymmetry    = .safe_extract(parsed$resultDefinitions, "supportsAsymmetry"),
    isRepeatResult       = .safe_extract(parsed$resultDefinitions, "isRepeatResult"),
    resultUnit           = .safe_extract(parsed$resultDefinitions, "resultUnit"),
    resultUnitName       = .safe_extract(parsed$resultDefinitions, "resultUnitName"),
    resultUnitScaleFactor= .safe_extract(parsed$resultDefinitions, "resultUnitScaleFactor"),
    numberOfDecimalPlaces = .safe_extract(parsed$resultDefinitions, "numberOfDecimalPlaces"),
    trendDirection       = .safe_extract(parsed$resultDefinitions, "trendDirection"),
    stringsAsFactors     = FALSE
  )

  return(results_df)
}