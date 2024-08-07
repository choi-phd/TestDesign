% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eligibility_functions.R
\name{clipEligibilityRates}
\alias{clipEligibilityRates}
\title{(Internal) Clip eligibility rates into 0-1 bounds}
\usage{
clipEligibilityRates(exposure_record, simulation_constants)
}
\arguments{
\item{exposure_record}{a named list containing exposure records.}

\item{simulation_constants}{a named list containing simulation constants.}
}
\value{
\code{\link{clipEligibilityRates}} returns an updated exposure record.
}
\description{
\code{\link{clipEligibilityRates}} is an internal function for
processing eligibility rate updates.
}
\keyword{internal}
