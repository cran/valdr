#' Get ForceDecks result definitions
#'
#' Retrieves all available result definitions from the ForceDecks API.
#'
#' @return A data frame where each row corresponds to a ForceDecks result definition retrieved from the API. Returned invisibly.
#' @export
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

  # Build data.frame from results
  df <- do.call(rbind, lapply(parsed$resultDefinitions, as.data.frame))
  as.data.frame(df, stringsAsFactors = FALSE)
}
