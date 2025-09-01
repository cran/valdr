#' Get trial results for a set of ForceDecks tests
#'
#' Retrieves and flattens trial-level result data for each test provided.
#'
#' @param tests_df A data frame of tests (as returned by \code{get_forcedecks_tests()})
#'
#' @return A data frame containing flattened trial-level results for the supplied tests.
#' If no trial results are found, returns an empty data frame.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
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
          testId               = .safe_value(test_id),
          trialId              = .safe_value(trial$id),
          athleteId            = .safe_value(trial$athleteId),
          hubAthleteId         = .safe_value(trial$hubAthleteId),
          recordedUTC          = .safe_value(trial$recordedUTC),
          recordedOffset       = .safe_value(trial$recordedOffset),
          recordedTimezone     = .safe_value(trial$recordedTimezone),
          startTime            = .safe_value(trial$startTime),
          endTime              = .safe_value(trial$endTime),
          trialLimb            = .safe_value(trial$limb),
          trialLastModifiedUTC = .safe_value(trial$lastModifiedUTC),
          resultId             = .safe_value(result$resultId),
          value                = ifelse(is.null(result$value), "", format(result$value, scientific = FALSE)),
          time                 = .safe_value(result$time),
          no_repeats           = .safe_value(result[["repeat"]]),
          resultLimb           = .safe_value(result$limb),
          definition_id        = .safe_value(definition$id),
          definition_result    = .safe_value(definition$result),
          definition_description = .safe_value(definition$description),
          definition_name      = .safe_value(definition$name),
          definition_unit      = .safe_value(definition$unit),
          definition_repeatable = .safe_value(definition$repeatable),
          definition_asymmetry = .safe_value(definition$asymmetry),
          stringsAsFactors     = FALSE
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
