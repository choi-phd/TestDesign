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
