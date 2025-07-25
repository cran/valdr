% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{get_config}
\alias{get_config}
\title{Retrieve stored VALD configuration}
\usage{
get_config(safe = TRUE, quiet = FALSE)
}
\arguments{
\item{safe}{Logical; if TRUE (default), sensitive values are redacted in printed output (only when not quiet).}

\item{quiet}{Logical; if TRUE, suppresses printed output (default FALSE).}
}
\value{
A named list containing the stored VALD configuration values for the current user.
Sensitive values are redacted when printed with safe = TRUE.
Returned invisibly.
}
\description{
Returns the configuration list previously set by \code{set_credentials()}.
If credentials have not been set, this function will raise an error.
}
