#' @include calc_loglikelihood_functions.r
NULL

#' Calculate first derivative of log-likelihood
#'
#' \code{\link{calcJacobian}} is a function to calculate the first derivative of the log-likelihood function.
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#' @param resp the response data to use.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcJacobian}} returns a length \emph{nq} vector containing the first derivative of the log-likelihood function, of observing the response at each theta.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcJacobian}} returns a (\emph{nq}, \emph{ni}) matrix containing the first derivative of the log-likelihood function, of observing the response at each theta.}
#' }
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{nq} denotes the number of theta values.}
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' @examples
#' item_1    <- new("item_1PL", difficulty = 0.5)
#' item_2    <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3    <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4    <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5    <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6    <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' j_item_1 <- calcJacobian(item_1, seq(-3, 3, 1), 0)
#' j_item_2 <- calcJacobian(item_2, seq(-3, 3, 1), 0)
#' j_item_3 <- calcJacobian(item_3, seq(-3, 3, 1), 0)
#' j_item_4 <- calcJacobian(item_4, seq(-3, 3, 1), 0)
#' j_item_5 <- calcJacobian(item_5, seq(-3, 3, 1), 0)
#' j_item_6 <- calcJacobian(item_6, seq(-3, 3, 1), 0)
#' j_pool   <- calcJacobian(
#'   itempool_science, seq(-3, 3, 1),
#'   rep(0, itempool_science@ni)
#' )
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @docType methods
#' @rdname calcJacobian-methods
#' @export
setGeneric(
  name = "calcJacobian",
  def = function(object, theta, resp) {
    standardGeneric("calcJacobian")
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_1PL,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_1PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(resp - calcEscore(object, theta))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_2PL,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_2PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(object@slope * (resp - calcEscore(object, theta)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_3PL,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_3PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    expected_score <- calcEscore(object, theta)
    return(object@slope * (resp - expected_score) * (expected_score - object@guessing) / (expected_score * (1.0 - object@guessing)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_PC,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_PC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(resp - calcEscore(object, theta))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_GPC,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_GPC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(object@slope * (resp - calcEscore(object, theta)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_GR,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_GR", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    resp <- resp + 1
    prob <- calcProb(object, theta)
    ps <- matrix(NA, length(theta), object@ncat + 1)
    ps[, 1] <- 1
    ps[, object@ncat + 1] <- 0
    for (k in 2:(object@ncat)) {
      ps[, k] <- ps[, k - 1] - prob[, k - 1]
    }
    return(object@slope * ((ps[, resp] * (1 - ps[, resp]) - ps[, resp + 1] * (1 - ps[, resp + 1])) / prob[, resp]))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_pool,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (length(resp) != object@ni) {
      stop("length(resp) does not match item_pool@ni")
    }
    if (length(theta) > 0 && all(!is.na(theta))) {
      mat_Jacobian <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        mat_Jacobian[, i] <- calcJacobian(object@parms[[i]], theta, resp[i])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Jacobian)
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_pool_cluster,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_pool_cluster", "numeric", "list"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      mat_Jacobian <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        mat_Jacobian[[i]] <- calcJacobian(object@pools[[i]], theta, resp[[i]])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Jacobian)
  }
)
