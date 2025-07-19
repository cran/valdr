# Test that a start date written to a vald config file exists in a new session
# Start date written previously in test-7-start_date_persistence_write.R

test_that("Persisted data match PASSES", {
  clear_config_file()
  reset_credentials()
  
  start_date <- "2025-06-25T00:00:00Z"
  
  config_path <- file.path(Sys.getenv("HOME"), ".vald_config.json")
  data <- read_json(config_path)
  
  result <- data$start_date == start_date
  expect_true(!is.null(result))
})