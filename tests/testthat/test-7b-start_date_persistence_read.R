# Read start date from a VALD config file created in a previous session
test_that("Read previously written dummy start date in config", {
  config_path <- file.path(Sys.getenv("HOME"), ".vald_config.json")
  data <- read_json(config_path)

  expect_true(data$start_date == dummy_start_date)
})