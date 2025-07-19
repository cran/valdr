#' Run full initial data fetch from the VALD ForceDecks API and External Profiles API
#'
#' This function is intended for first-time use or a full refresh of all datasets.
#' It retrieves profiles, result definitions, tests (from a specified start date), and trials.
#'
#' @param start_date In ISO 8601 UTC format (e.g., "2025-06-25T00:00:00Z") indicating the start of the test retrieval window.
#' @return A named list with data frames: `profiles`, `result_definitions`, `tests`, and `trials`.
#' Each element contains the respective dataset fetched from the ForceDecks API.
#' @export
#' @examples
#' \dontrun{
#' # Fetch all data (profiles, results, tests, trials)
#' data <- get_forcedecks_data()
#' View(data$profiles)
#' }
get_forcedecks_data <- function(start_date = NULL) {
  profiles <- get_profiles()
  results <- get_forcedecks_result_definitions()
  tests <- get_forcedecks_tests(start_date)
  trials <- get_forcedecks_trials(tests)

  list(
    profiles           = profiles,
    result_definitions = results,
    tests              = tests,
    trials             = trials
  )
}

#' Run a standard session to get new ForceDecks tests and trials only
#'
#' Use this when profiles and result definitions have already been downloaded previously.
#'
#' @param start_date In ISO 8601 UTC format (e.g., "2025-06-25T00:00:00Z") indicating the start of the test retrieval window.
#' @return A named list with two data frames: `tests` and `trials`.
#' These contain newly retrieved ForceDecks tests and their associated trial results.
#' @export
#' @examples
#' \dontrun{
#' # Fetch tests and trials only
#' session <- get_forcedecks_tests_trials()
#' View(session$tests)
#' View(session$trials)
#' }
get_forcedecks_tests_trials <- function(start_date = NULL) {
  tests <- get_forcedecks_tests(start_date)
  trials <- get_forcedecks_trials(tests)

  list(
    tests  = tests,
    trials = trials
  )
}

#' Get only ForceDecks test data
#'
#' Wrapper around \code{get_forcedecks_tests()} if the user only wants test-level data without trials.
#'
#' @param start_date In ISO 8601 UTC format (e.g., "2025-06-25T00:00:00Z") indicating the start of the test retrieval window.
#' @return A data frame containing ForceDecks test-level data.
#' @export
#' @examples
#' \dontrun{
#' # Fetch only tests
#' tests <- get_forcedecks_tests_only()
#' View(tests)
#' }
get_forcedecks_tests_only <- function(start_date = NULL) {
  get_forcedecks_tests(start_date)
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
