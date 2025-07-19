# Test scenario of setting credentials
test_that("API credentials can be set", {
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

