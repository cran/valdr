# Test scenarios where functions are run before setting credentials
test_that("STEP 1: get_forcedecks_data without creds FAILS", {
  clear_all_credentials()
  expect_error(get_forcedecks_data(), regexp = "^VALD API credentials not set.", fixed=FALSE)
})

test_that("STEP 2: get_forcedecks_tests_trials without creds FAILS", {
  clear_all_credentials()
  expect_error(get_forcedecks_tests_trials(), regexp = "^VALD API credentials not set.", fixed=FALSE)
})

test_that("STEP 3: get_forcedecks_tests_only without creds FAILS", {
  clear_all_credentials()
  expect_error(get_forcedecks_tests_only(), regexp = "^VALD API credentials not set.", fixed=FALSE)
})

test_that("STEP 4: get_forcedecks_trials_only without creds FAILS", {
  clear_all_credentials()
  expect_error(get_forcedecks_trials_only(), regexp = "^VALD API credentials not set.", fixed=FALSE)
})

test_that("STEP 5: get_profiles_only without creds FAILS", {
  clear_all_credentials()
  expect_error(get_profiles_only(), regexp = "^VALD API credentials not set.", fixed=FALSE)
})

test_that("STEP 6: get_forcedecks_result_definitions without creds FAILS", {
  clear_all_credentials
  expect_error(get_forcedecks_result_definitions(), regexp = "^VALD API credentials not set.", fixed=FALSE)
})
