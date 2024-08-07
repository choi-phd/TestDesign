% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eligibility_functions.R
\name{flagAdministeredAsEligible}
\alias{flagAdministeredAsEligible}
\title{(Internal) Update eligibility flags to mark administered items as eligible}
\usage{
flagAdministeredAsEligible(
  eligibility_flag_in_current_theta_segment,
  x,
  position,
  simulation_constants
)
}
\arguments{
\item{eligibility_flag_in_current_theta_segment}{a list containing eligibility flags for the current theta segment.}

\item{x}{an \code{\linkS4class{output_Shadow}} object, containing data for a single examinee.}

\item{position}{the item position, ranging from 1 to test length.}

\item{simulation_constants}{a named list containing simulation constants.}
}
\value{
\code{\link{flagAdministeredAsEligible}} returns an updated eligibility flag list.
}
\description{
\code{\link{flagAdministeredAsEligible}} is an internal function for updating eligibility flags.
Specifically, the function marks items/sets that are already administered to the current examinee as eligible.
This is necessary to ensure already administered items/sets are included in the shadowtest.
}
\keyword{internal}
