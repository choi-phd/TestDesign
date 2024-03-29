% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R, R/cpp_theta_documents.r
\name{theta_FB}
\alias{theta_FB}
\alias{theta_FB_single}
\title{(C++) Calculate a theta estimate using FB (Full Bayes) method}
\usage{
theta_FB(
  nx,
  theta_init,
  theta_prop,
  items_list,
  item_init,
  resp,
  ncat,
  model,
  prior,
  prior_parm
)

theta_FB_single(
  nx,
  theta_init,
  theta_prop,
  item_mcmc,
  item_init,
  resp,
  ncat,
  model,
  prior,
  prior_parm
)
}
\arguments{
\item{nx}{the number of MCMC draws.}

\item{theta_init}{the initial estimate to use.}

\item{theta_prop}{the SD of the proposal distribution.}

\item{item_init}{item parameter estimates. Must be a vector for \code{theta_FB_single()}, and a matrix for \code{theta_FB()}.}

\item{resp}{a vector containing responses on each item.}

\item{ncat}{a vector containing the number of response categories of each item.}

\item{model}{a vector indicating item models of each item, using \itemize{
  \item{\code{1}}: 1PL model
  \item{\code{2}}: 2PL model
  \item{\code{3}}: 3PL model
  \item{\code{4}}: PC model
  \item{\code{5}}: GPC model
  \item{\code{6}}: GR model
}}

\item{prior}{an integer indicating the type of prior distribution, using \itemize{
  \item{\code{1}}: normal distribution
  \item{\code{2}}: uniform distribution
}}

\item{prior_parm}{a vector containing parameters for the prior distribution.}

\item{item_mcmc, items_list}{sampled item parameters. Must be a matrix for \code{theta_FB_single()}, and a list of matrices for \code{theta_FB()}.}
}
\description{
\code{theta_FB_single()} and \code{theta_FB()} are functions for calculating a theta estimate using FB (Full Bayes) method.
}
\details{
\code{theta_FB_single()} is designed for one item, and \code{theta_FB()} is designed for multiple items.

Currently supports unidimensional models.
}
