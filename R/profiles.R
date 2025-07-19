#' Get VALD profiles
#'
#' Queries the External Profiles API and returns a data frame of VALD profiles.
#'
#' @return A data frame containing VALD profiles information for the stored `tenant_id`.
#' If no profiles are found, the function raises an error.
#' @export
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
      profileId = sapply(profiles, function(p) p$profileId),
      syncId = sapply(profiles, function(p) if (is.null(p$syncId)) NA else p$syncId),
      givenName = sapply(profiles, function(p) p$givenName),
      familyName = sapply(profiles, function(p) p$familyName),
      dateOfBirth = sapply(profiles, function(p) p$dateOfBirth),
      externalId = sapply(profiles, function(p) if (is.null(p$externalId)) NA else p$externalId),
      stringsAsFactors = FALSE
    )

    return(profiles_df)
  } else {
    stop("No 'profiles' data found in API response.", call. = FALSE)
  }
}
