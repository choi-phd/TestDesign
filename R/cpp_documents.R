#' @include item_class.R
NULL

#' Calculate item response probability
#'
#' \code{p_*} and \code{array_p_*} are functions to calculate item response probability.
#'
#' \code{p_*} functions accept a single theta value, and \code{array_p_*} functions accept multiple theta values.
#'
#' Currently supports unidimensional models.
#'
#' \itemize{
#'   \item{\code{p_1pl}, \code{array_p_1pl}}: 1PL models
#'   \item{\code{p_2pl}, \code{array_p_2pl}}: 2PL models
#'   \item{\code{p_3pl}, \code{array_p_3pl}}: 3PL models
#'   \item{\code{p_pc}, \code{array_p_pc}}: PC (partial credit) models
#'   \item{\code{p_gpc}, \code{array_p_gpc}}: GPC (generalized partial credit) models
#'   \item{\code{p_gr}, \code{array_p_gr}}: GR (graded response) models
#' }
#'
#' @param x the theta value. This must be a column vector in matrix form for \code{array_p_*} functions.
#' @param a the *a*-parameter.
#' @param b the *b*-parameter.
#' @param c the *c*-parameter.
#'
#' @examples
#' x <- 0.5
#'
#' p_1pl(x, 1)
#' p_2pl(x, 1, 2)
#' p_3pl(x, 1, 2, 0.25)
#' p_pc(x, c(0, 1))
#' p_gpc(x, 2, c(0, 1))
#' p_gr(x, 2, c(0, 2))
#'
#' x <- matrix(seq(0.1, 0.5, 0.1)) # column vector in matrix form
#'
#' array_p_1pl(x, 1)
#' array_p_2pl(x, 1, 2)
#' array_p_3pl(x, 1, 2, 0.25)
#' array_p_pc(x, c(0, 1))
#' array_p_gpc(x, 2, c(0, 1))
#' array_p_gr(x, 2, c(0, 2))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name p_item
NULL

#' Calculate Fisher information (single item)
#'
#' \code{info_*} and \code{array_info_*} are functions to calculate Fisher information.
#'
#' \code{info_*} functions accept a single theta value, and \code{array_info_*} functions accept multiple theta values.
#'
#' Currently supports unidimensional models.
#'
#' \itemize{
#'   \item{\code{info_1pl}, \code{array_info_1pl}}: 1PL models
#'   \item{\code{info_2pl}, \code{array_info_2pl}}: 2PL models
#'   \item{\code{info_3pl}, \code{array_info_3pl}}: 3PL models
#'   \item{\code{info_pc}, \code{array_info_pc}}: PC (partial credit) models
#'   \item{\code{info_gpc}, \code{array_info_gpc}}: GPC (generalized partial credit) models
#'   \item{\code{info_gr}, \code{array_info_gr}}: GR (graded response) models
#' }
#'
#' @param x the theta value. This must be a column vector in matrix form for \code{array_info_*} functions.
#' @param a the *a*-parameter.
#' @param b the *b*-parameter.
#' @param c the *c*-parameter.
#'
#' @examples
#' x <- 0.5
#'
#' info_1pl(x, 1)
#' info_2pl(x, 1, 2)
#' info_3pl(x, 1, 2, 0.25)
#' info_pc(x, c(0, 1))
#' info_gpc(x, 2, c(0, 1))
#' info_gr(x, 2, c(0, 2))
#'
#' x <- matrix(seq(0.1, 0.5, 0.1)) # column vector in matrix form
#'
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
#' @name info_item
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

#' Calculate theta estimates using EB (Empirical Bayes) method
#'
#' \code{theta_EB_single} and \code{theta_EB} are functions to calculate theta estimates using EB (Empirical Bayes) method.
#'
#' \code{theta_EB_single} is designed for one item, and \code{theta_EB} is designed for multiple items.
#'
#' Currently supports unidimensional models.
#'
#' @param nx the number of MCMC draws.
#' @param theta_init initial estimate of theta.
#' @param theta_prop SD of the proposal distribution.
#' @param item_parm a matrix containing item parameters. Each row represents each item.
#' @param resp a vector (or a value if for one item) containing responses on each item.
#' @param ncat a vector (or a value if for one item) containing the number of response categories of each item.
#' @param model a vector (or a value if for one item) indicating item models of each item, using \itemize{
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
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name theta_EB
NULL

#' Classify theta into segments
#'
#' \code{find_segment} is a function to classify theta values into segments based on supplied cutpoints.
#'
#' @param x the theta value. This can be a vector.
#' @param segment segment cutpoints.
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
