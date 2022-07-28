#' @include item_class.R
NULL

#' Calculate Fisher information (multiple items)
#'
#' \code{calc_info} and \code{calc_info_matrix} are functions to calculate Fisher information.
#' These functions are designed for multiple items.
#'
#' \code{calc_info} accepts a single theta value, and \code{calc_info_matrix} accepts multiple theta values.
#'
#' Currently supports unidimensional models.
#'
#' @param x the theta value. This must be a column vector in matrix form for \code{array_info_*} functions.
#' @param item_parm a matrix containing item parameters. Each row represents each item.
#' @param ncat a vector containing the number of response categories of each item.
#' @param model a vector indicating item models of each item, using \itemize{
#'   \item{\code{1}}: 1PL model
#'   \item{\code{2}}: 2PL model
#'   \item{\code{3}}: 3PL model
#'   \item{\code{4}}: PC model
#'   \item{\code{5}}: GPC model
#'   \item{\code{6}}: GR model
#' }
#'
#' @examples
#' # item parameters
#' item_parm <- matrix(c(
#'   1, NA,   NA,
#'   1,  2,   NA,
#'   1,  2, 0.25,
#'   0,  1,   NA,
#'   2,  0,    1,
#'   2,  0,    2),
#'   nrow = 6,
#'   byrow = TRUE
#' )
#'
#' ncat  <- c(2, 2, 2, 3, 3, 3)
#' model <- c(1, 2, 3, 4, 5, 6)
#'
#' # single theta example
#' x <- 0.5
#' calc_info(x, item_parm, ncat, model)
#' # same as
#' info_1pl(x, 1)
#' info_2pl(x, 1, 2)
#' info_3pl(x, 1, 2, 0.25)
#' info_pc(x, c(0, 1))
#' info_gpc(x, 2, c(0, 1))
#' info_gr(x, 2, c(0, 2))
#'
#' # multiple thetas example
#' x <- matrix(seq(0.1, 0.5, 0.1)) # column vector in matrix form
#' calc_info_matrix(x, item_parm, ncat, model)
#' # same as
#' array_info_1pl(x, 1)
#' array_info_2pl(x, 1, 2)
#' array_info_3pl(x, 1, 2, 0.25)
#' array_info_pc(x, c(0, 1))
#' array_info_gpc(x, 2, c(0, 1))
#' array_info_gr(x, 2, c(0, 2))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name calc_info
NULL

#' Calculate likelihoods
#'
#' \code{calc_likelihood} and \code{calc_likelihood_function} are functions to calculate likelihoods.
#'
#' \code{calc_log_likelihood} and \code{calc_log_likelihood_function} are functions to calculate log likelihoods.
#'
#' These functions are designed for multiple items.
#'
#' \code{calc_*} functions accept a single theta value, and \code{calc_*_function} functions accept multiple theta values.
#'
#' Currently supports unidimensional models.
#'
#' @param x,theta_grid the theta value. This must be a column vector in matrix form for \code{calc_*_function} functions.
#' @param item_parm a matrix containing item parameters. Each row represents each item.
#' @param resp a vector containing responses on each item.
#' @param ncat a vector containing the number of response categories of each item.
#' @param model a vector indicating item models of each item, using \itemize{
#'   \item{\code{1}}: 1PL model
#'   \item{\code{2}}: 2PL model
#'   \item{\code{3}}: 3PL model
#'   \item{\code{4}}: PC model
#'   \item{\code{5}}: GPC model
#'   \item{\code{6}}: GR model
#' }
#' @param prior an integer indicating the type of prior distribution, using \itemize{
#'   \item{\code{1}}: normal distribution
#'   \item{\code{2}}: uniform distribution
#' }
#' @param prior_parm a vector containing parameters for the prior distribution.
#'
#' @examples
#' # item parameters
#' item_parm <- matrix(c(
#'   1, NA,   NA,
#'   1,  2,   NA,
#'   1,  2, 0.25,
#'   0,  1,   NA,
#'   2,  0,    1,
#'   2,  0,    2),
#'   nrow = 6,
#'   byrow = TRUE
#' )
#'
#' ncat  <- c(2, 2, 2, 3, 3, 3)
#' model <- c(1, 2, 3, 4, 5, 6)
#' resp  <- c(0, 1, 0, 1, 0, 1)
#'
#' x <- 3
#' l  <- calc_likelihood(x, item_parm, resp, ncat, model)
#' ll <- calc_log_likelihood(x, item_parm, resp, ncat, model, 2, NA)
#' log(l) == ll
#'
#' theta_grid <- matrix(seq(-3, 3, .1))
#' l  <- calc_likelihood_function(theta_grid, item_parm, resp, ncat, model)
#' ll <- calc_log_likelihood_function(theta_grid, item_parm, resp, ncat, model, 2, NA)
#' all(log(l) == ll)
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name calc_likelihood
NULL
