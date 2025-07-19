# Test that a start date written to a vald config file exists in a new session

test_that("STEP 1: Set Dummy start_date", {
  start_date <- "2025-06-25T00:00:00Z"
  
  result <- set_start_date(start_date)
  expect_true(!is.null(result))
})