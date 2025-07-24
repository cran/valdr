# Test scenario that correct API endpoints are hit when changing regions in credentials
# Assumes testing with credentials which only has permissions to `aue` 
test_that("STEP 1: AUE region returns data", {
  # Clear creds
  clear_all_credentials()
  reset_credentials()

  set_start_date(Sys.getenv("start_date"))
  
  result <- get_forcedecks_data()
  expect_true(!is.null(result))
})

test_that("STEP 2: USE region FAILS", {
  # Clear creds
  clear_all_credentials()
  reset_credentials()
  
  # Set with USE
  set_credentials(
    client_id     = Sys.getenv("client_id"),
    client_secret = Sys.getenv("client_secret"),
    tenant_id     = Sys.getenv("tenant_id"),
    region        = "use"
  )
  
  set_start_date(Sys.getenv("start_date"))
  
  expect_error(get_forcedecks_data(), "Forbidden \\(403\\): You do not have permission to access this resource.")
})

test_that("STEP 3: EUW region FAILS", {
  # Clear creds
  clear_all_credentials()
  reset_credentials()
  
  # Set with EUW
  set_credentials(
    client_id     = Sys.getenv("client_id"),
    client_secret = Sys.getenv("client_secret"),
    tenant_id     = Sys.getenv("tenant_id"),
    region        = "euw"
  )
  
  set_start_date(Sys.getenv("start_date"))
  
  expect_error(get_forcedecks_data(), "Forbidden \\(403\\): You do not have permission to access this resource.")
})