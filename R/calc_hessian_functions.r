#' @include calc_jacobian_functions.r
NULL

#' Calculate second derivative of log-likelihood
#'
#' \code{\link{calcHessian}} is a function to calculate the second derivative of the log-likelihood function.
#'
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{nq} denotes the number of theta values.}
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#' @param resp the response data to use. This must be a single value for an \code{\link{item}}, or a length \emph{ni} vector for an \code{\linkS4class{item_pool}}.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcHessian}} returns a length \emph{nq} vector containing the second derivative of the log-likelihood function, of observing the response at each theta.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcHessian}} returns a (\emph{nq}, \emph{ni}) matrix containing the second derivative of the log-likelihood function, of observing the response at each theta.}
#' }
#'
#' @examples
#'
#' item_1    <- new("item_1PL", difficulty = 0.5)
#' item_2    <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3    <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4    <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5    <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6    <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' h_item_1 <- calcHessian(item_1, seq(-3, 3, 1), 0)
#' h_item_2 <- calcHessian(item_2, seq(-3, 3, 1), 0)
#' h_item_3 <- calcHessian(item_3, seq(-3, 3, 1), 0)
#' h_item_4 <- calcHessian(item_4, seq(-3, 3, 1), 0)
#' h_item_5 <- calcHessian(item_5, seq(-3, 3, 1), 0)
#' h_item_6 <- calcHessian(item_6, seq(-3, 3, 1), 0)
#' h_pool   <- calcHessian(
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
#' @rdname calcHessian-methods
#' @export
setGeneric(
  name = "calcHessian",
  def = function(object, theta, resp) {
    standardGeneric("calcHessian")
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_1PL,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_1PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    return(calcHessian(object, theta, resp))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_2PL,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_2PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    return(calcHessian(object, theta, resp))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_3PL,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_3PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    return(calcHessian(object, theta, resp))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_PC,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_PC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    return(calcHessian(object, theta, resp))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_GPC,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_GPC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    return(calcHessian(object, theta, resp))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_GR,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_GR", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    theta <- matrix(theta, , 1)
    return(calcHessian(object, theta, resp))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_1PL,matrix-method
setMethod(
  f = "calcHessian",
  signature = c("item_1PL", "matrix", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(array_h_1pl(theta, object@difficulty, resp)[, 1])
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_2PL,matrix-method
setMethod(
  f = "calcHessian",
  signature = c("item_2PL", "matrix", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(array_h_2pl(theta, object@slope, object@difficulty, resp)[, 1])
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_3PL,matrix-method
setMethod(
  f = "calcHessian",
  signature = c("item_3PL", "matrix", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(array_h_3pl(theta, object@slope, object@difficulty, object@guessing, resp)[, 1])
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_PC,matrix-method
setMethod(
  f = "calcHessian",
  signature = c("item_PC", "matrix", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(array_h_pc(theta, object@threshold, resp)[, 1])
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_GPC,matrix-method
setMethod(
  f = "calcHessian",
  signature = c("item_GPC", "matrix", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(array_h_gpc(theta, object@slope, object@threshold, resp)[, 1])
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_GR,matrix-method
setMethod(
  f = "calcHessian",
  signature = c("item_GR", "matrix", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(array_h_gr(theta, object@slope, object@category, resp)[, 1])
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_pool,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      calcProb(object, theta)
      mat_Hessian <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        mat_Hessian[, i] <- calcHessian(object@parms[[i]], theta, resp[i])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Hessian)
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_pool_cluster,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_pool_cluster", "numeric", "list"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      mat_Hessian <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        mat_Hessian[[i]] <- calcHessian(object@pools[[i]], theta, resp[[i]])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Hessian)
  }
)
