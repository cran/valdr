# Test scenario of using bad credentials to make a call
# Expect a 400 response

test_that("API credentials can be set", {
  # Uses credentials in local .Renviron file
  # Does not check if credentials are correct
  set_credentials(
    client_id     = "A",
    client_secret = "B",
    tenant_id     = "C",
    region        = "D"
  )

  # Make a call with the bad credentials
  result <-
    expect_error(get_forcedecks_data(),regexp = "^Bad Request \\(400\\)")
  
})
