% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-getData.R
\name{isOldROnWindows}
\alias{isOldROnWindows}
\title{Is old windows}
\usage{
isOldROnWindows()
}
\value{
(logical) TRUE if system is Windows and R version is < 4.2.0
}
\description{
Checks if package is used with an older R version which possibly leads to encryption errors on Windows.
Gives a warning in that case.
}
