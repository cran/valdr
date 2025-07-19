# Test scenarios of running functions after setting credentials but without setting start date
clear_all_credentials()
reset_credentials()

test_that("STEP 1: get_forcedecks_data() with no start date PASSES", {
  expect_error(get_forcedecks_data(), regexp = "^No start_date has been set")
})

test_that("STEP 2: get_forcedecks_tests_trials() with no start date PASSES", {
  expect_error(get_forcedecks_tests_trials(), regexp = "^No start_date has been set")
})

test_that("STEP 3: get_forcedecks_tests_only() with no start date PASSES", {
  expect_error(get_forcedecks_tests_only(), regexp = "^No start_date has been set")
})

test_that("STEP 4: get_forcedecks_trials_only() with no start date PASSES", {

  expect_error(get_forcedecks_trials_only(get_forcedecks_tests_only()), regexp = "^No start_date has been set")
})
