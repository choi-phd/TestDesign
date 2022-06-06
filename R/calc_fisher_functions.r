#' @include calc_location_functions.r
NULL

#' Calculate Fisher information
#'
#' \code{\link{calcFisher}} is a function to calculate Fisher information.
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcFisher}} returns a (\emph{nq}, \emph{1}) matrix of information values.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcProb}} returns a (\emph{nq}, \emph{ni}) matrix of information values.}
#' }
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{nq} denotes the number of theta values.}
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' A vector of Fisher information values over theta (nq values) for a single item or a matrix of dimension (nq, ni) for an "item_pool".
#'
#' @examples
#' item_1      <- new("item_1PL", difficulty = 0.5)
#' item_2      <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3      <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4      <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5      <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6      <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' info_item_1 <- calcFisher(item_1, seq(-3, 3, 1))
#' info_item_2 <- calcFisher(item_2, seq(-3, 3, 1))
#' info_item_3 <- calcFisher(item_3, seq(-3, 3, 1))
#' info_item_4 <- calcFisher(item_4, seq(-3, 3, 1))
#' info_item_5 <- calcFisher(item_5, seq(-3, 3, 1))
#' info_item_6 <- calcFisher(item_6, seq(-3, 3, 1))
#' info_pool   <- calcFisher(itempool_science, seq(-3, 3, 1))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @export
#' @docType methods
#' @rdname calcFisher-methods
setGeneric(
  name = "calcFisher",
  def = function(object, theta) {
    standardGeneric("calcFisher")
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_1PL,numeric-method
setMethod(
  f = "calcFisher",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcFisher(object, theta))
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_2PL,numeric-method
setMethod(
  f = "calcFisher",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcFisher(object, theta))
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_3PL,numeric-method
setMethod(
  f = "calcFisher",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcFisher(object, theta))
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_PC,numeric-method
setMethod(
  f = "calcFisher",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcFisher(object, theta))
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_GPC,numeric-method
setMethod(
  f = "calcFisher",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcFisher(object, theta))
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_GR,numeric-method
setMethod(
  f = "calcFisher",
  signature = c("item_GR", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcFisher(object, theta))
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_pool,numeric-method
setMethod(
  f = "calcFisher",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcFisher(object, theta))
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_1PL,matrix-method
setMethod(
  f = "calcFisher",
  signature = c("item_1PL", "matrix"),
  definition = function(object, theta) {
    info_Fisher <- array_info_1pl(theta, object@difficulty)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_2PL,matrix-method
setMethod(
  f = "calcFisher",
  signature = c("item_2PL", "matrix"),
  definition = function(object, theta) {
    info_Fisher <- array_info_2pl(theta, object@slope, object@difficulty)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_3PL,matrix-method
setMethod(
  f = "calcFisher",
  signature = c("item_3PL", "matrix"),
  definition = function(object, theta) {
    info_Fisher <- array_info_3pl(theta, object@slope, object@difficulty, object@guessing)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_PC,matrix-method
setMethod(
  f = "calcFisher",
  signature = c("item_PC", "matrix"),
  definition = function(object, theta) {
    info_Fisher <- array_info_pc(theta, object@threshold)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_GPC,matrix-method
setMethod(
  f = "calcFisher",
  signature = c("item_GPC", "matrix"),
  definition = function(object, theta) {
    info_Fisher <- array_info_gpc(theta, object@slope, object@threshold)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_GR,matrix-method
setMethod(
  f = "calcFisher",
  signature = c("item_GR", "matrix"),
  definition = function(object, theta) {
    info_Fisher <- array_info_gr(theta, object@slope, object@category)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_pool,matrix-method
setMethod(
  f = "calcFisher",
  signature = c("item_pool", "matrix"),
  definition = function(object, theta) {
    if (nrow(theta) > 0 && all(!is.na(theta))) {
      info_Fisher <- matrix(NA, nrow(theta), object@ni)
      for (i in 1:object@ni) {
        info_Fisher[, i] <- calcFisher(object@parms[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_pool_cluster,numeric-method
#' @export
setMethod(
  f = "calcFisher",
  signature = c("item_pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      info_Fisher <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        info_Fisher[[i]] <- calcFisher(object@pools[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(info_Fisher)
  }
)
