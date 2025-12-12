#' @importFrom dplyr %>%
NULL

#' Package load hook for VALD API credentials
#'
#' `.onLoad()` initialises the internal package environment and attempts to load
#' previously saved VALD API credentials and configuration.
#'
#' If a configuration file exists, this function tries to load the stored credentials.
#' It defers messaging until `.onAttach()` to comply with CRAN policies.
#'
#' @param libname The name of the package library (automatically provided)
#' @param pkgname The name of the package (automatically provided)
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  if (!exists(".vald_api_env", envir = ns, inherits = FALSE)) {
    assign(".vald_api_env", new.env(parent = emptyenv()), envir = ns)
  }

  config_path <- .vald_config_path()
  if (file.exists(config_path)) {
    success <- load_credentials()
    assign(".__valdr_load_status__", success, envir = ns)
  } else {
    assign(".__valdr_load_status__", NA, envir = ns)
  }
}

#' Package attach hook for VALD API credentials
#'
#' `.onAttach()` displays credential load status messages to the user.
#'
#' @param libname The name of the package library (automatically provided)
#' @param pkgname The name of the package (automatically provided)
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  loaded <- tryCatch(get(".__valdr_load_status__", envir = ns), error = function(e) NA)

  if (isTRUE(loaded)) {
    packageStartupMessage("VALD credentials auto-loaded successfully.")
  } else if (identical(loaded, FALSE)) {
    packageStartupMessage(
      "Failed to automatically load VALD credentials. Please run set_credentials() to configure."
    )
  }
}
