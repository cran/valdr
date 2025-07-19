#' Get trial results for a set of ForceDecks tests
#'
#' Retrieves and flattens trial-level result data for each test provided.
#'
#' @param tests_df A data frame of tests (as returned by \code{get_forcedecks_tests()})
#'
#' @return A data frame containing flattened trial-level results for the supplied tests.
#' If no trial results are found, returns an empty data frame.
#' @export
get_forcedecks_trials <- function(tests_df) {
  config <- get_config(quiet = TRUE)
  access_token <- authenticate()

  all_trial_results <- list()

  for (i in seq_len(nrow(tests_df))) {
    test_id <- tests_df$testId[i]
    message("Fetching trials for Test ID: ", test_id)

    request_url <- paste0(
      config$endpoints$forcedecks,
      "/v2019q3/teams/", config$tenant_id,
      "/tests/", test_id, "/trials"
    )

    # Perform GET request with httr
    response <- tryCatch(
      httr::GET(
        url = request_url,
        .add_vald_headers(access_token)
      ),
      error = function(e) {
        stop("Failed to connect to the ForceDecks API for Test ID ", test_id, ": ", e$message, call. = FALSE)
      }
    )

    # Consistent response handling
    .handle_api_response(response)

    # Pause to respect rate limits
    Sys.sleep(0.2)

    body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
    trial_data <- tryCatch(
      jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
      error = function(e) {
        stop("Failed to parse trial data for Test ID ", test_id, ": ", e$message, call. = FALSE)
      }
    )

    if (length(trial_data) == 0) next

    # Flatten and extract trial data
    for (trial in trial_data) {
      if (length(trial$results) == 0) next

      for (result in trial$results) {
        definition <- result$definition

        row <- data.frame(
          testId = test_id,
          trialId = trial$id,
          athleteId = trial$athleteId,
          hubAthleteId = trial$hubAthleteId,
          recordedUTC = trial$recordedUTC,
          recordedOffset = trial$recordedOffset,
          recordedTimezone = trial$recordedTimezone,
          startTime = trial$startTime,
          endTime = trial$endTime,
          trialLimb = trial$limb,
          trialLastModifiedUTC = trial$lastModifiedUTC,
          resultId = result$resultId,
          value = format(result$value, scientific = FALSE),
          time = result$time,
          no_repeats = result[["repeat"]],
          resultLimb = result$limb,
          definition_id = definition$id,
          definition_result = definition$result,
          definition_description = definition$description,
          definition_name = definition$name,
          definition_unit = definition$unit,
          definition_repeatable = definition$repeatable,
          definition_asymmetry = definition$asymmetry,
          stringsAsFactors = FALSE
        )

        all_trial_results[[length(all_trial_results) + 1]] <- row
      }
    }
  }

  # Combine into one data frame
  if (length(all_trial_results) == 0) {
    return(data.frame())
  }

  do.call(rbind, all_trial_results)
}
