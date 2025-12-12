#' Run full initial data fetch from the VALD ForceDecks API and External Profiles API
#'
#' This function is intended for first-time use or a full refresh of all datasets.
#' It retrieves profiles, result definitions, tests (from a specified start date), and trials.
#'
#' @param start_date In ISO 8601 UTC format (e.g., "2025-06-25T00:00:00Z") indicating
#'   the start of the test retrieval window.
#' @param include_attributes Logical; if TRUE, the returned list will include an
#'   additional data frame `test_attributes` containing a long-format mapping of
#'   test attributes. Defaults to FALSE.
#'
#' @return A named list with data frames: `profiles`, `result_definitions`,
#'   `tests`, and `trials`. If `include_attributes = TRUE`, the list also
#'   contains `test_attributes`.
#' @export
#' @examples
#' \dontrun{
#' # Fetch all data (profiles, results, tests, trials)
#' data <- get_forcedecks_data()
#' View(data$profiles)
#'
#' # Fetch all data including test attributes mapping
#' data_with_attrs <- get_forcedecks_data(include_attributes = TRUE)
#' View(data_with_attrs$test_attributes)
#' }
get_forcedecks_data <- function(start_date = NULL,
                                include_attributes = FALSE) {
  profiles <- get_profiles()
  results <- get_forcedecks_result_definitions()

  if (isTRUE(include_attributes)) {
    tests_res <- get_forcedecks_tests(
      start_date = start_date,
      include_attributes = TRUE
    )
    tests <- tests_res$tests
    test_attributes <- tests_res$attributes
  } else {
    tests <- get_forcedecks_tests(
      start_date = start_date,
      include_attributes = FALSE
    )
    test_attributes <- NULL
  }

  trials <- get_forcedecks_trials(tests)

  out <- list(
    profiles           = profiles,
    result_definitions = results,
    tests              = tests,
    trials             = trials
  )

  if (isTRUE(include_attributes)) {
    out$test_attributes <- test_attributes
  }

  out
}

#' Run a standard session to get new ForceDecks tests and trials only
#'
#' Use this when profiles and result definitions have already been downloaded previously.
#'
#' @param start_date In ISO 8601 UTC format (e.g., "2025-06-25T00:00:00Z") indicating
#'   the start of the test retrieval window.
#' @param include_attributes Logical; if TRUE, the returned list will include an
#'   additional data frame `test_attributes` containing a long-format mapping of
#'   test attributes. Defaults to FALSE.
#'
#' @return A named list with data frames: `tests` and `trials`. If
#'   `include_attributes = TRUE`, the list also contains `test_attributes`.
#' @export
#' @examples
#' \dontrun{
#' # Fetch tests and trials only
#' session <- get_forcedecks_tests_trials()
#' View(session$tests)
#' View(session$trials)
#'
#' # Fetch tests, trials, and test attributes mapping
#' session2 <- get_forcedecks_tests_trials(include_attributes = TRUE)
#' View(session2$test_attributes)
#' }
get_forcedecks_tests_trials <- function(start_date = NULL,
                                        include_attributes = FALSE) {
  if (isTRUE(include_attributes)) {
    tests_res <- get_forcedecks_tests(
      start_date = start_date,
      include_attributes = TRUE
    )
    tests <- tests_res$tests
    test_attributes <- tests_res$attributes
  } else {
    tests <- get_forcedecks_tests(
      start_date = start_date,
      include_attributes = FALSE
    )
    test_attributes <- NULL
  }

  trials <- get_forcedecks_trials(tests)

  out <- list(
    tests  = tests,
    trials = trials
  )

  if (isTRUE(include_attributes)) {
    out$test_attributes <- test_attributes
  }

  out
}

#' Get only ForceDecks test data
#'
#' Wrapper around \code{get_forcedecks_tests()} if the user only wants
#' test-level data without trials.
#'
#' @param start_date In ISO 8601 UTC format (e.g., "2025-06-25T00:00:00Z")
#'   indicating the start of the test retrieval window.
#' @param include_attributes Logical; if TRUE, returns a named list with
#'   \code{tests} (the test data frame) and \code{attributes} (the long-format
#'   attributes mapping table). If FALSE (default), returns only the tests
#'   data frame.
#'
#' @return Either a data frame containing ForceDecks test-level data, or a list
#'   with components \code{tests} and \code{attributes} if
#'   \code{include_attributes = TRUE}.
#' @export
#' @examples
#' \dontrun{
#' # Fetch only tests
#' tests <- get_forcedecks_tests_only()
#'
#' # Fetch tests plus attributes mapping
#' res <- get_forcedecks_tests_only(include_attributes = TRUE)
#' tests <- res$tests
#' attrs <- res$attributes
#' }
get_forcedecks_tests_only <- function(start_date = NULL,
                                      include_attributes = FALSE) {
  get_forcedecks_tests(
    start_date = start_date,
    profile_id = NULL,
    include_attributes = include_attributes
  )
}

#' Get trials for an existing test data frame
#'
#' Useful for re-running or extending analysis without re-downloading tests.
#'
#' @param tests_df A data frame of tests (from \code{get_forcedecks_tests()} or a previous session)
#' @return A data frame containing trial-level results corresponding to the input tests.
#' @export
#' @examples
#' \dontrun{
#' # Fetch trials based on existing tests
#' tests <- get_forcedecks_tests_only()
#' trials <- get_forcedecks_trials_only(tests)
#' View(trials)
#' }
get_forcedecks_trials_only <- function(tests_df) {
  get_forcedecks_trials(tests_df)
}

#' Get only VALD profiles
#'
#' Wrapper around \code{get_profiles()} to retrieve VALD profiles.
#' Useful if profile data needs to be refreshed independently.
#'
#' @return A data frame containing VALD profiles information for the stored `tenant_id`.
#' @export
#' @examples
#' \dontrun{
#' # Fetch profiles
#' profiles <- get_profiles_only()
#' View(profiles)
#' }
get_profiles_only <- function() {
  get_profiles()
}

#' Get only ForceDecks result definitions
#'
#' Wrapper around \code{get_forcedecks_result_definitions()} to refresh result definitions.
#' Typically called infrequently unless definitions are known to have changed.
#'
#' @return A data frame where each row corresponds to a ForceDecks result definition retrieved from the API. Returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Fetch result definitions
#' results <- get_forcedecks_result_definitions_only()
#' View(results)
#' }
get_forcedecks_result_definitions_only <- function() {
  get_forcedecks_result_definitions()
}

#' Get ForceFrame test data
#'
#' Wrapper around \code{get_forceframe_tests()} that also calls \code{get_profiles()}
#' to ensure profile data is available before retrieving ForceFrame test data.
#' Intended for use when you want to fetch Profiles information as well as ForceFrame tests from the API.
#'
#' @param start_date (Optional) A UTC ISO 8601 datetime string (e.g. "2025-06-25T00:00:00Z")
#' used to filter results by modification time. Input as a string.
#' @param profile_id (Optional) A specific profile ID to filter results for a single athlete. Input as a string.
#'
#' @return A data frame containing Profiles information and a data frame containing ForceFrame test data.
#' @export
#' @examples
#' \dontrun{
#' # Fetch all recent ForceFrame tests along with Profiles information
#' tests <- get_forceframe_data()
#' View(tests)
#'
#' # Fetch ForceFrame tests for a specific profile
#' get_forceframe_data(profile_id = "abc123")
#' }
get_forceframe_data <- function(start_date = NULL, profile_id = NULL) {
  profiles <- get_profiles()
  tests <- get_forceframe_tests(start_date = start_date, profile_id = profile_id)

  list(
    profiles = profiles,
    tests = tests
  )
}

#' Get NordBord test data
#'
#' Wrapper around \code{get_nordbord_tests()} that also calls \code{get_profiles()}
#' to ensure profile data is available before retrieving NordBord test data.
#' Intended for use when you want to fetch Profiles information as well as NordBord tests from the API.
#'
#' @param start_date (Optional) A UTC ISO 8601 datetime string (e.g. "2025-06-25T00:00:00Z")
#' used to filter results by modification time. Input as a string.
#' @param profile_id (Optional) A specific profile ID to filter results for a single athlete. Input as a string.
#'
#' @return A data frame containing Profiles information and a data frame containing NordBord test data.
#' @export
#' @examples
#' \dontrun{
#' # Fetch all recent NordBord tests along with Profiles information
#' tests <- get_nordbord_data()
#' View(tests)
#'
#' # Fetch NordBord tests for a specific profile
#' get_nordbord_data(profile_id = "abcd1234-ab12-cd34-ef56-abcdef123456")
#' }
get_nordbord_data <- function(start_date = NULL, profile_id = NULL) {
  profiles <- get_profiles()
  tests <- get_nordbord_tests(start_date = start_date, profile_id = profile_id)

  list(
    profiles = profiles,
    tests = tests
  )
}

#' Get only ForceFrame test data
#'
#' Wrapper around \code{get_forceframe_tests()} to retrieve ForceFrame test data.
#' Intended for use when you want to fetch ForceFrame tests.
#'
#' @param start_date (Optional) A UTC ISO 8601 datetime string (e.g. "2025-06-25T00:00:00Z") used to filter results by modification time. Input as a string.
#' @param profile_id (Optional) A specific profile ID to filter results for a single athlete. Input as a string.
#'
#' @return A data frame where each row corresponds to a ForceFrame test retrieved from the API. Returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Fetch all recent ForceFrame tests
#' tests <- get_forceframe_tests_only()
#' View(tests)
#'
#' # Fetch ForceFrame tests for a specific profile
#' get_forceframe_tests_only(profile_id = "abcd1234-ab12-cd34-ef56-abcdef123456")
#' }
get_forceframe_tests_only <- function(start_date = NULL, profile_id = NULL) {
  get_forceframe_tests(start_date = start_date, profile_id = profile_id)
}


#' Get only NordBord test data
#'
#' Wrapper around \code{get_nordbord_tests()} to retrieve NordBord test data.
#' Intended for use when you want to fetch NordBord tests.
#'
#' @param start_date (Optional) A UTC ISO 8601 datetime string (e.g. "2025-06-25T00:00:00Z") used to filter results by modification time. Input as a string.
#' @param profile_id (Optional) A specific profile ID to filter results for a single athlete. Input as a string.
#'
#' @return A data frame where each row corresponds to a NordBord test retrieved from the API. Returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Fetch all recent NordBord tests
#' tests <- get_nordbord_tests_only()
#' View(tests)
#'
#' # Fetch NordBord tests for a specific profile
#' get_nordbord_tests_only(profile_id = "abcd1234-ab12-cd34-ef56-abcdef123456")
#' }
get_nordbord_tests_only <- function(start_date = NULL, profile_id = NULL) {
  get_nordbord_tests(start_date = start_date, profile_id = profile_id)
}

#' Get a single ForceFrame test by ID
#'
#' Wrapper around \code{get_forceframe_tests_by_id()} to retrieve a specific ForceFrame test.
#' Intended for use when you want to fetch a single ForceFrame test record.
#'
#' @param test_id (Required) The unique test ID for the ForceFrame test you want to retrieve.
#'
#' @return A data frame with one row corresponding to the requested ForceFrame test. Returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Fetch a ForceFrame test by ID
#' test <- get_forceframe_test_by_id("abcd1234-ab12-cd34-ef56-abcdef123456")
#' View(test)
#' }
get_forceframe_test_by_id <- function(test_id) {
  get_forceframe_tests_by_id(test_id)
}

#' Get a single NordBord test by ID
#'
#' Wrapper around \code{get_nordbord_tests_by_id()} to retrieve a specific NordBord test.
#' Intended for use when you want to fetch a single NordBord test record.
#'
#' @param test_id (Required) The unique test ID for the NordBord test you want to retrieve.
#'
#' @return A data frame with one row corresponding to the requested NordBord test. Returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Fetch a NordBord test by ID
#' test <- get_nordbord_test_by_id("abcd1234-ab12-cd34-ef56-abcdef123456")
#' View(test)
#' }
get_nordbord_test_by_id <- function(test_id) {
  get_nordbord_tests_by_id(test_id)
}

#' Export ForceDecks data for dashboards
#'
#' Queries the ForceDecks API, joins trials/tests/profiles/result definitions,
#' and writes a CSV file to the user's Downloads/VALD_Exports folder (by default).
#'
#' This export is ForceDecks-specific and uses the stored start date from
#' \code{get_start_date()}, which is automatically updated by the ForceDecks
#' data retrieval functions.
#'
#' @param export_dir Optional directory to write the CSV to. If \code{NULL},
#'   uses \code{~/Downloads/VALD_Exports}.
#'
#' @return Invisibly, the full path to the created CSV file.
#' @export
export_forcedecks_csv <- function(export_dir = NULL) {
  message("Executing dataset export process...")

  # Resolve start_date from stored config
  start_date <- get_start_date()

  if (is.null(export_dir)) {
    export_dir <- .vald_default_export_dir()
  }

  # Ensure export directory exists
  if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # 1 - Pull ForceDecks data (trials, tests, profiles, result definitions)
  fd <- get_forcedecks_data(
    start_date = start_date
  )

  # 2 - Join trials, tests, profiles, result_definitions using dplyr
  merged <- fd$trials %>%
    dplyr::left_join(fd$tests, by = "testId") %>%
    dplyr::left_join(fd$profiles, by = c("athleteId" = "profileId")) %>%
    dplyr::left_join(fd$result_definitions, by = "resultId") %>%
    dplyr::select(
      dplyr::all_of(c(
        "athleteId",
        "givenName",
        "familyName",
        "recordedUTC",
        "testType",
        "trialId",
        "weight",
        "resultName",
        "value",
        "resultUnitName"
      ))
    )

  # 3 - Build file name + write CSV
  fname <- paste0(
    "forceDecks_",
    format(Sys.time(), "%Y-%m-%d_%H%M%S"),
    ".csv"
  )

  full_path <- file.path(export_dir, fname)

  readr::write_csv(merged, full_path, na = "")

  message("ForceDecks export written to: ", full_path)

  invisible(full_path)
}

#' Get profile–group–category mappings
#'
#' Wrapper around \code{get_profiles_groups_categories()} to retrieve
#' the long-format mapping of profiles to their groups and categories.
#'
#' Intended for use when you want a relational view of profile metadata
#' rather than the one-row-per-profile structure returned by \code{get_profiles()}.
#'
#' @return A data frame with one row per profile–group–category combination,
#'   containing the columns:
#'   \itemize{
#'     \item \code{profileId}
#'     \item \code{categoryId}
#'     \item \code{categoryName}
#'     \item \code{groupId}
#'     \item \code{groupName}
#'   }
#'   Returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Fetch profile–group–category mappings for the current tenant
#' mappings <- get_profiles_groups_categories_mapping()
#' head(mappings)
#' }
get_profiles_groups_categories_mapping <- function() {
  get_profiles_groups_categories()
}
