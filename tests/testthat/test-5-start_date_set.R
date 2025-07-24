# Test scenarios where the start date is set
# This is where non-empty data checks are made
# SET START DATE ----
test_that("STEP 1: Setting a bad date FAILS", {
  reset_credentials()
  expect_error(set_start_date("THIS_IS_A_BAD_DATE", "Start date must be in ISO 8601 format: 'YYYY-MM-DDTHH:MM:SSZ'"))
})

test_that("STEP 2: Set Start Date", {
  reset_credentials()
  result <- set_start_date(Sys.getenv("start_date"))
  expect_true(!is.null(result))
})

# RUN FUNCTIONS WITHOUT OPTIONAL START DATE ARG ----
test_that("STEP 3: get_forcedecks_data() PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_data()
  expect_true(length(result$profiles) > 0)
  expect_true(length(result$result_definitions) > 0)
  expect_true(length(result$tests) > 0)
  expect_true(length(result$trials) > 0)
})

Sys.sleep(5) 

test_that("STEP 4: get_forcedecks_tests_trials() PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_tests_trials()
  expect_true(length(result$tests) > 0)
  expect_true(length(result$trials) > 0)
})

test_that("STEP 5: get_forcedecks_tests_only() PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_tests_only()
  expect_true(dim(result)[1] > 0)
})

Sys.sleep(5) 

test_that("STEP 6: get_forcedecks_trials_only PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_trials_only(get_forcedecks_tests_only())
  expect_true(dim(result)[1] > 0)
})

Sys.sleep(5) 

# RUN FUNCTIONS WITH OPTIONAL START DATE ARG ----
test_that("STEP 7: get_forcedecks_data(start_date) PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_data(Sys.getenv("start_date"))
  expect_true(length(result$profiles) > 0)
  expect_true(length(result$result_definitions) > 0)
  expect_true(length(result$tests) > 0)
  expect_true(length(result$trials) > 0)
})

Sys.sleep(5) 

test_that("STEP 8: get_forcedecks_tests_trials(start_date) PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_tests_trials(Sys.getenv("start_date"))
  expect_true(length(result$tests) > 0)
  expect_true(length(result$trials) > 0)
})

Sys.sleep(5) 

test_that("STEP 9: get_forcedecks_tests_only(start_date) PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_tests_only(Sys.getenv("start_date"))
  expect_true(dim(result)[1] > 0)
})

Sys.sleep(5) 

test_that("STEP 10: get_forcedecks_trials_only() with start date PASSES", {
  reset_credentials()
  set_start_date(Sys.getenv("start_date"))
  result <- get_forcedecks_trials_only(get_forcedecks_tests_only(Sys.getenv("start_date")))
  expect_true(dim(result)[1] > 0)
})

