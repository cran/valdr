# Package-level environment to store global VALD config
.vald_api_env <- new.env(parent = emptyenv())

# Helper for the config file path
.vald_config_path <- function() {
  file.path(Sys.getenv("HOME"), ".vald_config.json")
}

#' Set and Save VALD API Credentials
#'
#' Stores VALD API credentials securely using the system keyring and
#' saves non-sensitive configuration to a config file in the user's home directory for reuse across sessions.
#'
#' Sensitive values are never written to disk and are retrieved securely when needed.
#'
#' @param client_id Your VALD API Client ID (stored securely in keyring)
#' @param client_secret Your VALD API Client Secret (stored securely in keyring)
#' @param tenant_id Your VALD Tenant ID
#' @param region The VALD data region code (e.g., "aue", "use", "euw")
#'
#' @return A logical scalar (TRUE or FALSE), returned invisibly, indicating whether the credentials and configuration were saved successfully. Called primarily for side effects.
#' @export
set_credentials <- function(client_id, client_secret, tenant_id, region) {
  service_name <- "valdr_credentials"

  # Save secrets to key registry using keyring
  keyring::key_set_with_value(service = service_name, username = "client_id", password = client_id)
  keyring::key_set_with_value(service = service_name, username = "client_secret", password = client_secret)

  # Save non-sensitive config to disk
  endpoints <- list(
    profile    = paste0("https://prd-", region, "-api-externalprofile.valdperformance.com"),
    forcedecks = paste0("https://prd-", region, "-api-extforcedecks.valdperformance.com")
  )

  config <- list(
    tenant_id = tenant_id,
    region    = region,
    token_url = "https://security.valdperformance.com/connect/token",
    endpoints = endpoints
  )

  jsonlite::write_json(config, .vald_config_path(), auto_unbox = TRUE)

  # Load full config into memory
  load_credentials()

  message("VALD API configuration saved: sensitive values stored in keyring, non-sensitive data saved to disk.")
  invisible(TRUE)
}

#' Load Stored VALD API Credentials and Configuration
#'
#' Loads the saved VALD API configuration from the config file in the user's home directory
#' and retrieves sensitive credentials securely from the system keyring.
#'
#' This function is automatically called on package load if a saved configuration file exists.
#'
#' @return A logical scalar (TRUE or FALSE), returned invisibly, indicating whether the saved credentials and configuration were loaded successfully. Called primarily for side effects.
#' @export
load_credentials <- function() {
  service_name <- "valdr_credentials"
  path <- .vald_config_path()

  if (!file.exists(path)) {
    stop("No saved config file found. Please run `set_credentials()` first.")
  }

  # Load non-sensitive config from disk
  config <- jsonlite::read_json(path, simplifyVector = TRUE)

  # Retrieve sensitive secrets from keyring
  config$client_id <- tryCatch(
    keyring::key_get(service = service_name, username = "client_id"),
    error = function(e) stop("Missing client_id in keyring. Please rerun `set_credentials()`.", call. = FALSE)
  )
  config$client_secret <- tryCatch(
    keyring::key_get(service = service_name, username = "client_secret"),
    error = function(e) stop("Missing client_secret in keyring. Please rerun `set_credentials()`.", call. = FALSE)
  )

  .vald_api_env$config <- config
  invisible(TRUE)
}

#' Retrieve stored VALD configuration
#'
#' Returns the configuration list previously set by \code{set_credentials()}.
#' If credentials have not been set, this function will raise an error.
#'
#' @param safe If TRUE (default), sensitive values are redacted in printed output (only when not quiet).
#' @param quiet If TRUE, no printed output is shown (default is FALSE).
#'
#' @return A named list containing the stored VALD configuration values for the current user. Sensitive values are redacted. Returned invisibly.
#' @export
get_config <- function(safe = TRUE, quiet = FALSE) {
  if (is.null(.vald_api_env$config)) {
    stop("VALD API credentials not set. Run `set_credentials()` or `load_credentials()` first.")
  }

  config <- .vald_api_env$config

  if (!quiet && safe) {
    display_config <- config
    display_config$client_id <- "<hidden>"
    display_config$client_secret <- "<hidden>"
    display_config$tenant_id <- substr(config$tenant_id, 1, 8)
    print(display_config)
  }

  invisible(config)
}

#' Set and persist the start date
#'
#' Saves the provided ISO 8601 start date to the config file.
#'
#' @param start_date Date in ISO 8601 UTC format, e.g., "2025-06-25T00:00:00Z"
#' @return A logical scalar (TRUE), returned invisibly, indicating that the start date was saved and persisted successfully.
#' @export
set_start_date <- function(start_date) {
  # Strip fractional seconds (e.g., .324Z → Z)
  start_date <- sub("\\.\\d+Z$", "Z", start_date)

  if (!grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", start_date)) {
    stop("Start date must be in ISO 8601 format: 'YYYY-MM-DDTHH:MM:SSZ'")
  }

  # Load current config from disk
  path <- .vald_config_path()
  if (!file.exists(path)) {
    stop("No existing config file found. Please run `set_credentials()` first.")
  }

  config <- jsonlite::read_json(path, simplifyVector = TRUE)
  config$start_date <- start_date

  # Persist updated config
  jsonlite::write_json(config, path, auto_unbox = TRUE)

  # Update in-memory config (if loaded)
  .vald_api_env$config <- config
  invisible(TRUE)
}

#' Retrieve the start date from config
#'
#' Returns the saved start date string from the config file.
#'
#' @return A character scalar containing the start date string in ISO 8601 format previously saved to configuration.
#' @export
get_start_date <- function() {
  config <- get_config(quiet = TRUE)
  if (is.null(config$start_date)) {
    stop("No start_date has been set. Use `set_start_date()` to define one.")
  }
  config$start_date
}

#' Add headers to API requests for logging purposes
#'
#' Internal helper to standardise headers on all API calls.
#' @keywords internal
#' @noRd
.add_vald_headers <- function(token) {
  httr::add_headers(
    Authorization = paste("Bearer", token),
    `Client-Name` = "valdr",
    `Client-Version` = as.character(utils::packageVersion("valdr"))
  )
}

#' Handle API response errors consistently across package functions
#' @keywords internal
#' @noRd
.handle_api_response <- function(response) {
  status <- httr::status_code(response)

  # Safely read the response body
  body_txt <- tryCatch(
    httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) "<Failed to read response body>"
  )

  # 2xx: Success (including 204 No Content)
  if (status >= 200 && status < 300) {
    # Only check for empty body if it's NOT a 204
    if (status != 204 && !nzchar(body_txt)) {
      stop("Empty response body from the API. If you believe you should be seeing data, please check your query parameters and validate your request.", call. = FALSE)
    }
    return(invisible(body_txt)) # Always return the body (even if empty) invisibly
  }

  # Try to extract error message
  err_msg <- tryCatch(
    {
      err <- jsonlite::fromJSON(body_txt, simplifyVector = TRUE)
      err$message %||% err$error_description %||% err$error %||% substr(body_txt, 1, 200)
    },
    error = function(e) substr(body_txt, 1, 200)
  )

  # Friendly messages
  msg <- switch(as.character(status),
    "400" = "Bad Request (400): The request was invalid. Please check your start_date value and API credentials using get_config().",
    "401" = "Unauthorized (401): Authentication failed. Please check your API credentials using get_config() and then set_credentials().",
    "403" = "Forbidden (403): You do not have permission to access this resource. Please check your API credentials using get_config().",
    "404" = "Not Found (404): The requested resource was not found. Please check your start_date value and API credentials using get_config().",
    "429" = "Too Many Requests (429): Rate limit exceeded. Please try again later.",
    NULL
  )

  if (!is.null(msg)) {
    stop(msg, call. = FALSE)
  } else if (status >= 500 && status < 600) {
    stop(paste0("Server Error (", status, "): A server-side error occurred. Please try again later."), call. = FALSE)
  } else {
    stop(paste0("Unexpected Error (", status, "): ", err_msg), call. = FALSE)
  }
}
