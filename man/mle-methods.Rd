% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theta_functions.R
\docType{methods}
\name{mle}
\alias{mle}
\alias{mle,item_pool-method}
\alias{MLE}
\alias{MLE,test-method}
\alias{MLE,test_cluster-method}
\title{Compute maximum likelihood estimates of theta}
\usage{
mle(
  object,
  select = NULL,
  resp,
  start_theta = NULL,
  max_iter = 100,
  crit = 0.001,
  truncate = FALSE,
  theta_range = c(-4, 4),
  max_change = 1,
  use_step_size = FALSE,
  step_size = 0.5,
  do_Fisher = TRUE
)

\S4method{mle}{item_pool}(
  object,
  select = NULL,
  resp,
  start_theta = NULL,
  max_iter = 50,
  crit = 0.005,
  truncate = FALSE,
  theta_range = c(-4, 4),
  max_change = 1,
  use_step_size = FALSE,
  step_size = 0.5,
  do_Fisher = TRUE
)

MLE(
  object,
  select = NULL,
  start_theta = NULL,
  max_iter = 100,
  crit = 0.001,
  theta_range = c(-4, 4),
  truncate = FALSE,
  max_change = 1,
  do_Fisher = TRUE
)

\S4method{MLE}{test}(
  object,
  select = NULL,
  start_theta = NULL,
  max_iter = 100,
  crit = 0.001,
  theta_range = c(-4, 4),
  truncate = FALSE,
  max_change = 1,
  do_Fisher = TRUE
)

\S4method{MLE}{test_cluster}(object, select = NULL, start_theta = NULL, max_iter = 100, crit = 0.001)
}
\arguments{
\item{object}{an \code{\linkS4class{item_pool}} object.}

\item{select}{(optional) if item indices are supplied, only the specified items are used.}

\item{resp}{item response on all (or selected) items in the \code{object} argument. Can be a vector, a matrix, or a data frame. \code{length(resp)} or \code{ncol(resp)} must be equal to the number of all (or selected) items.}

\item{start_theta}{(optional) initial theta values. If not supplied, EAP estimates using uniform priors are used as initial values. Uniform priors are computed using the \code{theta_range} argument below, with increments of \code{.1}.}

\item{max_iter}{maximum number of iterations. (default = \code{100})}

\item{crit}{convergence criterion to use. (default = \code{0.001})}

\item{truncate}{set \code{TRUE} to impose a bound using \code{theta_range} on the estimate. (default = \code{FALSE})}

\item{theta_range}{a range of theta values to bound the estimate. Only effective when \code{truncate} is \code{TRUE}. (default = \code{c(-4, 4)})}

\item{max_change}{upper bound to impose on the absolute change in theta between iterations. Absolute changes exceeding this value will be capped to \code{max_change}. (default = \code{1.0})}

\item{use_step_size}{set \code{TRUE} to use \code{step_size}. (default = \code{FALSE})}

\item{step_size}{upper bound to impose on the absolute change in initial theta and estimated theta. Absolute changes exceeding this value will be capped to \code{step_size}. (default = \code{0.5})}

\item{do_Fisher}{set \code{TRUE} to use Fisher scoring instead of Newton-Raphson method. (default = \code{TRUE})}
}
\value{
\code{\link{mle}} returns a list containing estimated values.

\itemize{
  \item{\code{th}} theta value.
  \item{\code{se}} standard error.
  \item{\code{conv}} \code{TRUE} if estimation converged.
  \item{\code{trunc}} \code{TRUE} if truncation was applied on \code{th}.
}
}
\description{
\code{\link{mle}} is a function for computing maximum likelihood estimates of theta.
}
\examples{
mle(itempool_fatigue, resp = resp_fatigue_data[10, ])
mle(itempool_fatigue, select = 1:20, resp = resp_fatigue_data[10, 1:20])
}
