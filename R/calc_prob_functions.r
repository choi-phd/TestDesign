#' @include item_class.R
NULL

#' Calculate item response probabilities
#'
#' \code{\link{calcProb}} is a function to calculate item response probabilities.
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcProb}} returns a (\emph{nq}, \emph{ncat}) matrix of probability values.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcProb}} returns a length \emph{ni} list, each containing a matrix of probability values.}
#' }
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{nq} denotes the number of theta values.}
#'     \item{\emph{ncat} denotes the number of response categories.}
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' @examples
#' item_1      <- new("item_1PL", difficulty = 0.5)
#' item_2      <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3      <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4      <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5      <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6      <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' prob_item_1 <- calcProb(item_1, seq(-3, 3, 1))
#' prob_item_2 <- calcProb(item_2, seq(-3, 3, 1))
#' prob_item_3 <- calcProb(item_3, seq(-3, 3, 1))
#' prob_item_4 <- calcProb(item_4, seq(-3, 3, 1))
#' prob_item_5 <- calcProb(item_5, seq(-3, 3, 1))
#' prob_item_6 <- calcProb(item_6, seq(-3, 3, 1))
#' prob_pool   <- calcProb(itempool_science, seq(-3, 3, 1))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @docType methods
#' @name calcProb-methods
#' @aliases calcProb
#' @export
setGeneric(
  name = "calcProb",
  def = function(object, theta) {
    standardGeneric("calcProb")
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_1PL,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcProb(object, theta))
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_2PL,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcProb(object, theta))
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_3PL,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcProb(object, theta))
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_PC,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcProb(object, theta))
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_GPC,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcProb(object, theta))
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_GR,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_GR", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcProb(object, theta))
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_pool,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcProb(object, theta))
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_1PL,matrix-method
setMethod(
  f = "calcProb",
  signature = c("item_1PL", "matrix"),
  definition = function(object, theta) {
    prob <- matrix(NA, nrow(theta), 2)
    prob[, 2] <- array_p_1pl(theta, object@difficulty)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_2PL,matrix-method
setMethod(
  f = "calcProb",
  signature = c("item_2PL", "matrix"),
  definition = function(object, theta) {
    prob <- matrix(NA, nrow(theta), 2)
    prob[, 2] <- array_p_2pl(theta, object@slope, object@difficulty)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_3PL,matrix-method
setMethod(
  f = "calcProb",
  signature = c("item_3PL", "matrix"),
  definition = function(object, theta) {
    prob <- matrix(NA, nrow(theta), 2)
    prob[, 2] <- array_p_3pl(theta, object@slope, object@difficulty, object@guessing)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_PC,matrix-method
setMethod(
  f = "calcProb",
  signature = c("item_PC", "matrix"),
  definition = function(object, theta) {
    prob <- array_p_pc(theta, object@threshold)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_GPC,matrix-method
setMethod(
  f = "calcProb",
  signature = c("item_GPC", "matrix"),
  definition = function(object, theta) {
    prob <- array_p_gpc(theta, object@slope, object@threshold)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_GR,matrix-method
setMethod(
  f = "calcProb",
  signature = c("item_GR", "matrix"),
  definition = function(object, theta) {
    prob <- array_p_gr(theta, object@slope, object@category)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_pool,matrix-method
setMethod(
  f = "calcProb",
  signature = c("item_pool", "matrix"),
  definition = function(object, theta) {
    if (nrow(theta) > 0 && all(!is.na(theta))) {
      prob <- lapply(object@parms, calcProb, theta)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_pool_cluster,numeric-method
setMethod(
  f = "calcProb",
  signature = c("item_pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      prob <- lapply(object@pools, calcProb, theta)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(prob)
  }
)
