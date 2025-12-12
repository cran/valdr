get_profiles <- function() {
  config <- get_config(quiet = TRUE)
  access_token <- authenticate()

  # Step 1 - Get base profiles
  url <- paste0(config$endpoints$profile, "/profiles")

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

  if (is.null(profiles_json$profiles) || length(profiles_json$profiles) == 0L) {
    stop("No 'profiles' data found in API response.", call. = FALSE)
  }

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
    warning(
      "You have profiles in your organisation which have NULL date of birth values. ",
      "This is no longer supported by VALD. Please ensure you populate these values for all profiles."
    )
  }

  profiles_df
}


#' Get profile group memberships
#'
#' For a vector of profileIds, query the Profiles API for each profile
#' and extract its groupIds.
#'
#' @param profile_ids Character vector of profile IDs.
#' @return A data frame with columns profileId and groupId (long format).
#' @keywords internal
.get_profile_group_ids <- function(profile_ids) {
  # No profiles provided = return an empty data frame
  if (length(profile_ids) == 0L) {
    return(data.frame(
      profileId = character(0),
      groupId   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  config <- get_config(quiet = TRUE)
  access_token <- authenticate()
  base_url <- paste0(config$endpoints$profile, "/profiles/")

  # User friendly message
  message("Fetching matching Categories and Groups for your Profiles. Please wait... ")

  # Throttling to delay between detail calls (seconds)
  throttle_delay <- getOption("valdr.profile_detail_delay", 0.25)
  records <- list()

  for (pid in profile_ids) {
    url <- paste0(base_url, pid)

    resp <- tryCatch(
      httr::GET(
        url = url,
        .add_vald_headers(access_token),
        query = list(tenantId = config$tenant_id)
      ),
      error = function(e) {
        stop("Failed to connect to the Profiles API for profileId ", pid, ": ",
          e$message,
          call. = FALSE
        )
      }
    )

    .handle_api_response(resp)

    body_txt <- httr::content(resp, as = "text", encoding = "UTF-8")

    prof_json <- tryCatch(
      jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
      error = function(e) {
        stop("Failed to parse profile details for profileId ", pid, ": ",
          e$message,
          call. = FALSE
        )
      }
    )

    group_ids <- prof_json$groupIds

    if (is.null(group_ids) || length(group_ids) == 0L) {
      # Legacy profiles without groups are included with a blank groupId
      records <- c(
        records,
        list(list(
          profileId = pid,
          groupId   = ""
        ))
      )

      # Throttle even when there are no groups to stay under rate limits
      if (!is.null(throttle_delay) && throttle_delay > 0) {
        Sys.sleep(throttle_delay)
      }
      next
    }

    # One record per (profileId, groupId)
    recs_for_profile <- lapply(group_ids, function(gid) {
      list(
        profileId = pid,
        groupId   = gid
      )
    })

    records <- c(records, recs_for_profile)

    # Apply throttling after each profile detail request
    if (!is.null(throttle_delay) && throttle_delay > 0) {
      Sys.sleep(throttle_delay)
    }
  }

  # No records at all (very unlikely if profile_ids was non-empty, but a safety guard anyways)
  if (length(records) == 0L) {
    return(data.frame(
      profileId = character(0),
      groupId   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Use .safe_extract helper for consistent handling of NULL/empty
  data.frame(
    profileId = .safe_extract(records, "profileId"),
    groupId = .safe_extract(records, "groupId"),
    stringsAsFactors = FALSE
  )
}

#' Get groups for a tenant
#'
#' Queries the External Tenants API /groups endpoint.
#'
#' @return A data frame with group meta data.
#' @keywords internal
.get_groups <- function() {
  config <- get_config(quiet = TRUE)
  access_token <- authenticate()

  url <- paste0(config$endpoints$tenant, "/groups")

  resp <- tryCatch(
    httr::GET(
      url = url,
      .add_vald_headers(access_token),
      query = list(tenantId = config$tenant_id)
    ),
    error = function(e) {
      stop("Failed to connect to the Tenant API: ", e$message, call. = FALSE)
    }
  )

  .handle_api_response(resp)

  body_txt <- httr::content(resp, as = "text", encoding = "UTF-8")

  groups_json <- tryCatch(
    jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse Groups API response: ", e$message, call. = FALSE)
    }
  )

  if (is.null(groups_json$groups) || length(groups_json$groups) == 0L) {
    warning(
      "No 'groups' data found in Tenant API response. ",
      "Profiles without groups will have blank group fields."
    )
    return(data.frame(
      groupId = character(0),
      groupName = character(0),
      categoryId = character(0),
      groupSyncId = character(0),
      stringsAsFactors = FALSE
    ))
  }

  grps <- groups_json$groups

  data.frame(
    groupId = .safe_extract(grps, "id"),
    groupName = .safe_extract(grps, "name"),
    categoryId = .safe_extract(grps, "categoryId"),
    groupSyncId = .safe_extract(grps, "syncId"),
    stringsAsFactors = FALSE
  )
}

#' Get categories for a tenant
#'
#' Queries the External Tenants API /categories endpoint.
#'
#' @return A data frame with category meta data.
#' @keywords internal
.get_categories <- function() {
  config <- get_config(quiet = TRUE)
  access_token <- authenticate()

  url <- paste0(config$endpoints$tenant, "/categories")

  resp <- tryCatch(
    httr::GET(
      url = url,
      .add_vald_headers(access_token),
      query = list(tenantId = config$tenant_id)
    ),
    error = function(e) {
      stop("Failed to connect to the Categories API: ", e$message, call. = FALSE)
    }
  )

  .handle_api_response(resp)

  body_txt <- httr::content(resp, as = "text", encoding = "UTF-8")

  cats_json <- tryCatch(
    jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse Categories API response: ", e$message, call. = FALSE)
    }
  )

  if (is.null(cats_json$categories) || length(cats_json$categories) == 0L) {
    warning(
      "No 'categories' data found in Tenant API response. ",
      "Profiles without categories will have blank category fields."
    )
    return(data.frame(
      categoryId = character(0),
      categorySyncId = character(0),
      categoryName = character(0),
      stringsAsFactors = FALSE
    ))
  }

  cats <- cats_json$categories

  data.frame(
    categoryId = .safe_extract(cats, "id"),
    categorySyncId = .safe_extract(cats, "syncId"),
    categoryName = .safe_extract(cats, "name"),
    stringsAsFactors = FALSE
  )
}

#' Get profile group and category mappings
#'
#' Returns a long-format data frame of profile–category–group relationships.
#' Each row represents a single combination of:
#'   - profileId
#'   - categoryId / categoryName (may be blank for legacy profiles)
#'   - groupId / groupName       (may be blank for legacy profiles)
#'
#' Legacy profiles without groups or categories are still included, with blank group/category fields.
#'
#' Internal function – use the exported wrapper(s) instead.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{profileId}
#'     \item \code{categoryId}
#'     \item \code{categoryName}
#'     \item \code{groupId}
#'     \item \code{groupName}
#'   }
#' @keywords internal
get_profiles_groups_categories <- function() {
  # Base profiles (one row per profile)
  profiles_df <- get_profiles()

  # No profiles at all
  if (nrow(profiles_df) == 0L) {
    return(data.frame(
      profileId = character(0),
      categoryId = character(0),
      categoryName = character(0),
      groupId = character(0),
      groupName = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Profile ---> groupId mappings (handles legacy profiles with blank groupId)
  profile_groups <- .get_profile_group_ids(profiles_df$profileId)

  # If somehow nothing is returned, fall back to one row per profile with blanks
  if (nrow(profile_groups) == 0L) {
    return(data.frame(
      profileId = profiles_df$profileId,
      categoryId = "",
      categoryName = "",
      groupId = "",
      groupName = "",
      stringsAsFactors = FALSE
    ))
  }

  # Tenant groups + categories
  groups_df <- .get_groups() # groupId, groupName, categoryId, groupSyncId
  categories_df <- .get_categories() # categoryId, categorySyncId, categoryName

  # Join groups with categories on categoryId
  if (nrow(groups_df) > 0L && nrow(categories_df) > 0L) {
    groups_full <- merge(
      groups_df,
      categories_df,
      by    = "categoryId",
      all.x = TRUE
    )
  } else if (nrow(groups_df) > 0L) {
    # Groups but no categories returned = keep groups, blank category fields
    groups_full <- groups_df
    if (!"categoryName" %in% names(groups_full)) {
      groups_full$categoryName <- ""
    }
  } else {
    # No groups at all for this tenant
    groups_full <- data.frame(
      groupId = character(0),
      groupName = character(0),
      categoryId = character(0),
      categoryName = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Join profile/group mappings with group/category metadata
  pg_groups <- merge(
    profile_groups,
    groups_full,
    by    = "groupId",
    all.x = TRUE
  )

  # Ensure required columns exist
  needed_cols <- c("profileId", "categoryId", "categoryName", "groupId", "groupName")
  for (cc in needed_cols) {
    if (!cc %in% names(pg_groups)) {
      pg_groups[[cc]] <- ""
    }
  }

  # Coerce NAs and blank strings in these columns to "N/A" for consistency
  for (cc in needed_cols) {
    vals <- pg_groups[[cc]]
    vals[is.na(vals) | vals == ""] <- "N/A"
    pg_groups[[cc]] <- vals
  }

  # Return only the requested columns in the requested order
  result <- pg_groups[, needed_cols]

  # Sort so rows with real categoryName values appear first, "N/A" at the bottom
  result <- result[
    order(
      result$categoryName == "N/A", # FALSE (real name) first, TRUE ("N/A") later
      result$categoryName,
      result$groupName,
      result$profileId
    ), ,
    drop = FALSE
  ]

  result
}
