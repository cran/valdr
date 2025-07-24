# Test scenario of setting credentials
test_that("STEP 1: API credentials can be set", {
  # Uses credentials in local .Renviron file
  # Does not check if credentials are correct
  clear_all_credentials()

  result <- set_credentials(
      client_id     = Sys.getenv("client_id"),
      client_secret = Sys.getenv("client_secret"),
      tenant_id     = Sys.getenv("tenant_id"),
      region        = Sys.getenv("region")
    )

    expect_true(!is.null(result))
})

test_that("STEP 2: Token can be retrieved from keyring", {
  clear_all_credentials()
  reset_credentials()
  
  result <- length(key_list(service="valdr_credentials"))
  
  expect_equal(result, 2)
})