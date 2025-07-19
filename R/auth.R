#' Get or refresh VALD access token
#'
#' Retrieves a cached access token from disk if valid, otherwise fetches a new one.
#'
#' @param config Configuration object. If NULL, uses internal config set by \code{set_credentials()}.
#' @param verbose Whether to print a message when refreshing the token.
#' @return A character vector of length 1 (a single string) representing the bearer token used to authenticate requests. Returned invisibly.
#' @export
get_access_token <- function(config = NULL, verbose = TRUE) {
  if (is.null(config)) config <- get_config(quiet = TRUE)

  service_name <- "valdr_token"
  username <- config$tenant_id

  # Try to retrieve token from keyring
  cached_token <- tryCatch(
    keyring::key_get(service = service_name, username = username),
    error = function(e) NULL
  )

  if (!is.null(cached_token) && is_token_valid(cached_token)) {
    if (verbose) message("Reusing cached access token from keyring.")
    return(cached_token)
  }

  # Fetch new token via OAuth2
  response <- tryCatch(
    httr::POST(
      url = config$token_url,
      body = list(
        grant_type = "client_credentials",
        client_id = config$client_id,
        client_secret = config$client_secret
      ),
      encode = "form"
    ),
    error = function(e) {
      stop("Failed to connect to auth server: ", e$message, call. = FALSE)
    }
  )

  # Consistent response handling
  .handle_api_response(response)

  body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
  resp <- tryCatch(
    jsonlite::fromJSON(body_txt, simplifyVector = TRUE),
    error = function(e) {
      stop("Unexpected response format from auth endpoint: ", e$message, call. = FALSE)
    }
  )

  if (is.null(resp$access_token) || resp$access_token == "") {
    stop("Failed to retrieve access token from response.", call. = FALSE)
  }

  # Store new token securely in keyring
  tryCatch(
    keyring::key_set_with_value(service = service_name, username = username, password = resp$access_token),
    error = function(e) {
      warning("Failed to write token to keyring: ", e$message)
    }
  )

  if (verbose) message("New access token retrieved and securely cached in keyring.")

  invisible(resp$access_token)
}


#' Authenticate and retrieve a valid access token
#'
#' Ensures a valid access token is available based on stored credentials.
#' Also validates required tenant ID and token presence for subsequent function calls.
#'
#' @return A character vector of length 1 (a single string) representing a valid access token. Returned invisibly and used internally for authentication.
#' @export
authenticate <- function() {
  config <- get_config(quiet = TRUE)

  if (is.null(config$tenant_id) || config$tenant_id == "") {
    stop("Tenant ID is invalid or missing.")
  }

  token <- get_access_token(config)

  if (is.null(token) || token == "") {
    stop("Access token is invalid. Check your credentials.")
  }

  invisible(token)
}

#' Check if JWT access token is expired
#'
#' @param token A JWT access token
#' @return A logical scalar (TRUE or FALSE) indicating whether the provided JWT access token is still valid based on its expiry timestamp.
#' @keywords internal
is_token_valid <- function(token) {
  if (is.null(token) || token == "") {
    return(FALSE)
  }

  parts <- strsplit(token, "\\.")[[1]]
  if (length(parts) != 3) {
    return(FALSE)
  }

  payload_raw <- rawToChar(base64enc::base64decode(parts[2]))
  payload <- tryCatch(jsonlite::fromJSON(payload_raw), error = function(e) NULL)

  if (is.null(payload$exp)) {
    return(FALSE)
  }

  current_time <- as.numeric(Sys.time())
  return(current_time < payload$exp - 60) # 60s buffer
}
