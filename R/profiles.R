#' Get VALD profiles
#'
#' Queries the External Profiles API and returns a data frame of VALD profiles.
#'
#' @return A data frame containing VALD profiles information for the stored `tenant_id`.
#' If no profiles are found, the function raises an error.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
get_profiles <- function() {
  config <- get_config(quiet = TRUE)
  access_token <- authenticate()

  # Build URL
  url <- paste0(config$endpoints$profile, "/profiles")

  # Perform GET request with httr
  response <- tryCatch(
    httr::GET(
      url = url,
      .add_vald_headers(access_token),
      query = list(tenantId = config$tenant_id)
    ),
    error = function(e) {
      stop("Failed to connect to the Profiles API: ", e$message, call. = FALSE)
    }
  )

  # Consistent response handling
  .handle_api_response(response)

  # Parse JSON body safely
  body_txt <- httr::content(response, as = "text", encoding = "UTF-8")

  profiles_json <- tryCatch(
    jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse Profiles API response: ", e$message, call. = FALSE)
    }
  )

  # Validate and extract profiles
  if (!is.null(profiles_json$profiles) && length(profiles_json$profiles) > 0) {
    profiles <- profiles_json$profiles

    profiles_df <- data.frame(
      profileId = .safe_extract(profiles, "profileId"),
      syncId = .safe_extract(profiles, "syncId"),
      givenName = .safe_extract(profiles, "givenName"),
      familyName = .safe_extract(profiles, "familyName"),
      dateOfBirth = .safe_extract(profiles, "dateOfBirth"),
      externalId = .safe_extract(profiles, "externalId"),
      stringsAsFactors = FALSE
    )

    if (any(profiles_df$dateOfBirth == "")) {
      warning("You have profiles in your organisation which have NULL date of birth values. This is no longer supported by VALD. Please ensure you populate these values for all profiles.")
    }

    return(profiles_df)
  } else {
    stop("No 'profiles' data found in API response.", call. = FALSE)
  }
}
