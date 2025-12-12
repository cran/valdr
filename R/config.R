# Package-level environment to store global VALD config
.vald_api_env <- new.env(parent = emptyenv())

# Helper for the config file path
.vald_config_path <- function() {
  file.path(Sys.getenv("HOME"), ".vald_config.json")
}

#' Internal helper function to retry reading a key from the system keyring.
#' This handles potential intermittent delays after writing to the keyring.
#'
#' @param service Character. Keyring service name (e.g., "valdr_credentials").
#' @param username Character. Keyring username (e.g., "client_id").
#' @param retries Integer. Number of retry attempts before giving up (default 5).
#' @param delay Numeric. Delay in seconds between retry attempts (default 0.5).
#'
#' @return Character scalar. The retrieved credential value.
#' Will throw an error if unable to retrieve a valid, non-blank value after all retries.
.retry_key_get <- function(service, username, retries = 5, delay = 0.5) {
  for (i in seq_len(retries)) {
    value <- tryCatch(
      {
        val <- keyring::key_get(service = service, username = username)
        if (nzchar(trimws(val))) {
          if (i > 1) {
            message(sprintf("Retrieved '%s' from keyring on attempt %d.", username, i))
          }
          return(val)
        } else {
          message(sprintf("Retry %d/%d: '%s' value is blank. Retrying in %.1f seconds...", i, retries, username, delay))
          NULL
        }
      },
      error = function(e) {
        # Suppress error messages during retry
        NULL
      }
    )
    if (!is.null(value)) {
      return(value)
    }
    Sys.sleep(delay)
  }

  stop(sprintf(
    "Unable to retrieve a valid (non-blank) '%s' from keyring after %d attempts. Please call `valdr:::load_credentials()` manually.",
    username, retries
  ), call. = FALSE)
}

#' Build default VALD API endpoints for a region
#'
#' Internal helper used to derive endpoint URLs from a region code.
#' @keywords internal
.vald_default_endpoints <- function(region) {
  if (is.null(region) || !nzchar(region)) {
    stop("Region is missing; cannot construct default endpoints.", call. = FALSE)
  }

  list(
    profile    = paste0("https://prd-", region, "-api-externalprofile.valdperformance.com"),
    forcedecks = paste0("https://prd-", region, "-api-extforcedecks.valdperformance.com"),
    nordbord   = paste0("https://prd-", region, "-api-externalnordbord.valdperformance.com"),
    forceframe = paste0("https://prd-", region, "-api-externalforceframe.valdperformance.com"),
    tenant     = paste0("https://prd-", region, "-api-externaltenants.valdperformance.com")
  )
}

#' Set and Save VALD API Credentials
#'
#' Securely stores VALD API credentials in the system keyring and
#' saves non-sensitive configuration to a JSON config file in the user's home directory
#' for reuse across sessions.
#'
#' Sensitive values (client ID and secret) are never written to disk and
#' are retrieved securely from the keyring when needed.
#'
#' @param client_id Your VALD API Client ID (stored securely in keyring)
#' @param client_secret Your VALD API Client Secret (stored securely in keyring)
#' @param tenant_id Your VALD Tenant ID
#' @param region The VALD data region code (e.g., "aue", "use", "euw")
#'
#' @return Invisibly returns TRUE if credentials and configuration were saved successfully.
#' @export
set_credentials <- function(client_id, client_secret, tenant_id, region) {
  service_name <- "valdr_credentials"

  # Store sensitive credentials securely in the keyring
  keyring::key_set_with_value(service = service_name, username = "client_id", password = client_id)
  keyring::key_set_with_value(service = service_name, username = "client_secret", password = client_secret)

  # Verify credentials were saved and are retrievable
  .retry_key_get(service_name, "client_id", retries = 5, delay = 0.5)
  .retry_key_get(service_name, "client_secret", retries = 5, delay = 0.5)

  # Construct endpoint URLs based on region (single source of truth)
  endpoints <- .vald_default_endpoints(region)

  # Create config list excluding sensitive info
  config <- list(
    tenant_id = tenant_id,
    region    = region,
    token_url = "https://security.valdperformance.com/connect/token",
    endpoints = endpoints
  )

  # Persist non-sensitive config to disk as JSON
  jsonlite::write_json(config, .vald_config_path(), auto_unbox = TRUE)

  # Load credentials into package environment to refresh state
  load_credentials()

  message("VALD API configuration saved: sensitive values stored in keyring, non-sensitive data saved to disk.")
  invisible(TRUE)
}

#' Load Stored VALD API Credentials and Configuration (with retry logic)
#'
#' Loads the saved VALD API configuration from the user's config file
#' and retrieves sensitive credentials securely from the system keyring.
#'
#' Also ensures that API endpoints are kept up to date for the current
#' package version (e.g. when new endpoints are added),
#' and persists any repaired endpoint configuration back to disk
#' (non-sensitive fields only).
#'
#' @return Invisibly returns TRUE if credentials and configuration were loaded successfully, FALSE otherwise.
#' @param verbose Logical; if TRUE, prints messages on load status (default FALSE).
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
load_credentials <- function(verbose = TRUE) {
  service_name <- "valdr_credentials"
  path <- .vald_config_path()

  if (!file.exists(path)) {
    if (verbose) message("VALD config file not found. Please run `set_credentials()`.")
    return(FALSE)
  }

  config <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) {
      if (verbose) message("Failed to read config file: ", e$message)
      NULL
    }
  )
  if (is.null(config)) {
    return(FALSE)
  }

  # Backwards compatibility for endpoint updates
  # Build defaults from region, then overlay any stored endpoints
  defaults <- .vald_default_endpoints(config$region)

  if (is.null(config$endpoints)) {
    config$endpoints <- list()
  }

  config$endpoints <- utils::modifyList(defaults, config$endpoints)

  # Persist the repaired, non-sensitive config back to disk so future sessions
  safe_config <- config
  safe_config$client_id <- NULL
  safe_config$client_secret <- NULL

  # In case older versions somehow wrote sensitive fields, make sure they're removed
  jsonlite::write_json(safe_config, path, auto_unbox = TRUE)

  # Retrieve sensitive credentials securely from keyring with retry and error handling
  config$client_id <- tryCatch(
    .retry_key_get(service_name, "client_id"),
    error = function(e) {
      if (verbose) message("Unable to retrieve client_id from keyring. Run `set_credentials()` again.")
      return(NULL)
    }
  )
  config$client_secret <- tryCatch(
    .retry_key_get(service_name, "client_secret"),
    error = function(e) {
      if (verbose) message("Unable to retrieve client_secret from keyring. Run `set_credentials()` again.")
      return(NULL)
    }
  )

  if (is.null(config$client_id) || is.null(config$client_secret)) {
    return(FALSE)
  }

  .vald_api_env$config <- config
  invisible(TRUE)
}

#' Retrieve stored VALD configuration
#'
#' Returns the configuration list previously set by \code{set_credentials()}.
#' If credentials have not been set, this function will raise an error.
#'
#' @param safe Logical; if TRUE (default), sensitive values are redacted in printed output (only when not quiet).
#' @param quiet Logical; if TRUE, suppresses printed output (default FALSE).
#'
#' @return A named list containing the stored VALD configuration values for the current user.
#' Sensitive values are redacted when printed with safe = TRUE.
#' Returned invisibly.
#' @export
get_config <- function(safe = TRUE, quiet = FALSE) {
  if (is.null(.vald_api_env$config)) {
    
    # Try to lazy load credentials if not already loaded in this session
    load_ok <- load_credentials(verbose = !quiet)
    if (!isTRUE(load_ok) || is.null(.vald_api_env$config)) {
      stop(
        "VALD API credentials not set or could not be loaded. ",
        "Please run `set_credentials()` first.",
        call. = FALSE
      )
    }
  }

  config <- .vald_api_env$config

  # Retrieve sensitive credentials securely from keyring with retry and error handling
  config$client_id <- tryCatch(
    .retry_key_get("valdr_credentials", "client_id"),
    error = function(e) {
      stop("Unable to retrieve client_id from keyring: ", e$message, call. = FALSE)
    }
  )
  config$client_secret <- tryCatch(
    .retry_key_get("valdr_credentials", "client_secret"),
    error = function(e) {
      stop("Unable to retrieve client_secret from keyring: ", e$message, call. = FALSE)
    }
  )

  # Print config with sensitive data redacted
  if (!quiet && safe) {
    display_config <- config
    display_config$client_id <- "<hidden>"
    display_config$client_secret <- "<hidden>"
    display_config$tenant_id <- if (!is.null(config$tenant_id)) {
      substr(config$tenant_id, 1, 8)
    } else {
      "<missing>"
    }
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
  # Strip fractional seconds (e.g., .324Z - Z)
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
    "400" = "Bad Request (400): The request was invalid. Please verify your arguments, query parameters, and API credentials using `get_config()`, and compare against the function documentation.",
    "401" = "Unauthorized (401): Authentication failed. Please check your API credentials using `get_config()` and then `set_credentials()`.",
    "403" = "Forbidden (403): You do not have permission to access this resource. Please check your API credentials using `get_config()`.",
    "404" = "Not Found (404): The requested resource was not found. Please check your start_date value and API credentials using `get_config()`.",
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
