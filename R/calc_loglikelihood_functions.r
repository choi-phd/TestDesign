#' @include calc_fisher_functions.r
NULL

#' Calculate log-likelihood
#'
#' \code{\link{calcLogLikelihood}} is a function to calculate log-likelihood values.
#'
#' @param object an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#' @param resp the response data to use.
#'
#' @return \code{\link{calcLogLikelihood}} returns values of log-likelihoods.
#'
#' @examples
#' j_pool   <- calcLogLikelihood(itempool_science, seq(-3, 3, 1), 0)
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @docType methods
#' @rdname calcLogLikelihood-methods
#' @export
setGeneric(
  name = "calcLogLikelihood",
  def = function(object, theta, resp) {
    standardGeneric("calcLogLikelihood")
  }
)

#' @rdname calcLogLikelihood-methods
#' @aliases calcLogLikelihood,item_pool,numeric,numeric-method
setMethod(
  f = "calcLogLikelihood",
  signature = c("item_pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    resp  <- matrix(resp, 1, )
    return(calcLogLikelihood(object, theta, resp))
  }
)

#' @rdname calcLogLikelihood-methods
#' @aliases calcLogLikelihood,item_pool,numeric,matrix-method
setMethod(
  f = "calcLogLikelihood",
  signature = c("item_pool", "numeric", "matrix"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    return(calcLogLikelihood(object, theta, resp))
  }
)

#' @rdname calcLogLikelihood-methods
#' @aliases calcLogLikelihood,item_pool,matrix,numeric-method
setMethod(
  f = "calcLogLikelihood",
  signature = c("item_pool", "matrix", "numeric"),
  definition = function(object, theta, resp) {
    resp <- matrix(resp, 1, )
    return(calcLogLikelihood(object, theta, resp))
  }
)

#' @rdname calcLogLikelihood-methods
#' @aliases calcLogLikelihood,item_pool,matrix,matrix-method
setMethod(
  f = "calcLogLikelihood",
  signature = c("item_pool", "matrix", "matrix"),
  definition = function(object, theta, resp) {
    if (nrow(theta) == 0) {
      stop("'theta' must have at least one value")
    }
    if (any(is.na(theta))) {
      stop("'theta' must have no missing values")
    }
    LL <- calc_log_likelihood_function(
      theta, object@ipar, resp, object@NCAT,
      sanitizeModel(object@model),
      prior = 0, prior_parm = NA)
    return(LL)
  }
)
