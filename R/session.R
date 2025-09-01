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