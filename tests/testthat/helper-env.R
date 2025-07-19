# If running locally use .Renviron
#readRenviron(file.path(rprojroot::find_rstudio_root_file(), ".Renviron"))

# Load libraries
library(jsonlite)

# DELETE EXISTING CREDS ----
# Simulates a fresh install
# Tests involving a reinstall should expect pre-existing config and tokens

# 
clear_token <- function() {
  # Deletes valdr token
  try(key_delete("valdr_token", username = Sys.getenv("tenant_id")), silent=TRUE)
}

# Delete config and token files if they already exist
clear_config_file <- function() {
  delete_if_exists <- function(path) {
    if (file.exists(path)) {
      file.remove(path)
    }
  }

  file_path_config <- paste(Sys.getenv("HOME"), "\\.vald_config.json", sep = "")
  delete_if_exists(file_path_config)
}

clear_all_credentials <- function(){
  clear_token()
  clear_config_file()
}

# Clears credentials in the environment if they exist
clear_env_creds <- function() {
  Sys.unsetenv("client_id")
  Sys.unsetenv("client_secret")
  Sys.unsetenv("tenant_id")
  Sys.unsetenv("region")
}

# Reload credentials into keyring
reset_credentials <- function() {
  
  set_credentials(
    client_id     = Sys.getenv("client_id"),
    client_secret = Sys.getenv("client_secret"),
    tenant_id     = Sys.getenv("tenant_id"),
    region        = Sys.getenv("region")
  )
}

