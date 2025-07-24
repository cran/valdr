# Write a vald config file to see if it can be read in a new session

test_that("Write a dummy start date to config", {
  clear_config_file()
  reset_credentials()
  result<-set_start_date(dummy_start_date)
  expect_true(!is.null(result))
})