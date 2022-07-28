#' @include item_class.R
NULL

#' (C++) Calculate a theta estimate using EB (Empirical Bayes) method
#'
#' \code{theta_EB_single()} and \code{theta_EB()} are functions to calculate a theta estimate using EB (Empirical Bayes) method.
#'
#' \code{theta_EB_single()} is designed for one item, and \code{theta_EB()} is designed for multiple items.
#'
#' Currently supports unidimensional models.
#'
#' @param nx the number of MCMC draws.
#' @param theta_init the initial estimate to use.
#' @param theta_prop the SD of the proposal distribution.
#' @param item_parm a matrix containing item parameters. Each row should represent an item.
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
#' nx <- 100
#' theta_init <- 0
#' theta_prop <- 1.0
#' set.seed(1)
#' theta_EB_single(nx, theta_init, theta_prop, item_parm[1, ], resp[1], ncat[1], model[1], 1, c(0, 1))
#' theta_EB(nx, theta_init, theta_prop, item_parm, resp, ncat, model, 1, c(0, 1))
#'
#' @name theta_EB
NULL

#' (C++) Calculate a theta estimate using FB (Full Bayes) method
#'
#' \code{theta_FB_single()} and \code{theta_FB()} are functions to calculate a theta estimate using FB (Full Bayes) method.
#'
#' \code{theta_FB_single()} is designed for one item, and \code{theta_FB()} is designed for multiple items.
#'
#' Currently supports unidimensional models.
#'
#' @param nx the number of MCMC draws.
#' @param theta_init the initial estimate to use.
#' @param theta_prop the SD of the proposal distribution.
#' @param item_mcmc,items_list sampled item parameters. Must be a matrix for \code{theta_FB_single()}, and a list of matrices for \code{theta_FB()}.
#' @param item_init item parameter estimates. Must be a vector for \code{theta_FB_single()}, and a matrix for \code{theta_FB()}.
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
#' @name theta_FB
NULL

#' (C++) Classify theta values into segments using cutpoints
#'
#' \code{find_segment()} is a function to classify theta values into segments based on supplied cutpoints.
#'
#' @param x the theta value. This can be a vector.
#' @param segment segment cutpoints. Values of \code{-Inf, Inf} are not implied and must be explicitly supplied if intended.
#'
#' @examples
#' cuts <- c(-Inf, -2, 0, 2, Inf)
#'
#' find_segment(-3, cuts)
#' find_segment(-1, cuts)
#' find_segment(1, cuts)
#' find_segment(3, cuts)
#' find_segment(seq(-3, 3, 2), cuts)
#'
#' @name find_segment
NULL
