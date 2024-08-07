% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposure_control_functions.R
\name{initializeSegmentRecord}
\alias{initializeSegmentRecord}
\title{(Internal) Initialize segment record}
\usage{
initializeSegmentRecord(simulation_constants)
}
\arguments{
\item{simulation_constants}{a named list containing simulation constants.}
}
\value{
\code{\link{initializeSegmentRecord}} returns a list containing segment record.
\itemize{
  \item{\code{freq_true} is a length-\code{n_segment} vector containing the number of times examinees belonged in each theta segment, based on true thetas.}
  \item{\code{freq_est} is a length-\code{n_segment} vector containing the number of times examinees belonged in each theta segment, based on final theta estimates.}
  \item{\code{count_true}}
  \item{\code{count_est}}
}
}
\description{
\code{\link{initializeSegmentRecord}} is an internal function
for creating a new segment record.
}
\keyword{internal}
