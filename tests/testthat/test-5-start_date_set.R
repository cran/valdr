# Test scenarios where the start date is set
clear_all_credentials()
reset_credentials()
start_date <- "2025-06-25T00:00:00Z"

# SET START DATE ----
test_that("STEP 0: Setting a bad date FAILS", {
  expect_error(set_start_date("THIS_IS_A_BAD_DATE", "Start date must be in ISO 8601 format: 'YYYY-MM-DDTHH:MM:SSZ'"))
})

test_that("STEP 1: Set Start Date", {
  result <- set_start_date(start_date)
  expect_true(!is.null(result))
})

# RUN FUNCTIONS WITHOUT OPTIONAL START DATE ARG ----
test_that("STEP 2: get_forcedecks_data() PASSES", {
  result <- get_forcedecks_data()
  expect_true(!is.null(result))
})

test_that("STEP 3: get_forcedecks_tests_trials() PASSES", {
  result <- get_forcedecks_tests_trials()
  expect_true(!is.null(result))
})

test_that("STEP 4: get_forcedecks_tests_trials PASSES", {
  result <- get_forcedecks_tests_trials()
  expect_true(!is.null(result))
})

test_that("STEP 5: get_forcedecks_trials_only PASSES", {
  result <- get_forcedecks_trials_only(get_forcedecks_tests_only())
  expect_true(!is.null(result))
})

# RUN FUNCTIONS WITH OPTIONAL START DATE ARG ----
test_that("STEP 6: get_forcedecks_data(start_date) PASSES", {
  result <- get_forcedecks_data(start_date)
  expect_true(!is.null(result))
})

test_that("STEP 7: get_forcedecks_tests_trials(start_date) PASSES", {
  result <- get_forcedecks_tests_trials(start_date)
  expect_true(!is.null(result))
})

test_that("STEP 8: get_forcedecks_tests_only(start_date) PASSES", {
  result <- get_forcedecks_tests_only(start_date)
  expect_true(!is.null(result))
})

test_that("STEP 9: get_forcedecks_trials_only() with start date PASSES", {
  result <- get_forcedecks_trials_only(get_forcedecks_tests_only(start_date))
  expect_true(!is.null(result))
})

