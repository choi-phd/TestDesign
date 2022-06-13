#' @include calc_prob_functions.r
NULL

#' Calculate expected scores
#'
#' \code{\link{calcEscore}} is a function for calculating expected scores.
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcEscore}} a vector containing expected score of the item at the theta values.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcEscore}} returns a vector containing the pool-level expected score at the theta values.}
#' }
#'
#' @examples
#' item_1     <- new("item_1PL", difficulty = 0.5)
#' item_2     <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3     <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4     <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5     <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6     <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' ICC_item_1 <- calcEscore(item_1, seq(-3, 3, 1))
#' ICC_item_2 <- calcEscore(item_2, seq(-3, 3, 1))
#' ICC_item_3 <- calcEscore(item_3, seq(-3, 3, 1))
#' ICC_item_4 <- calcEscore(item_4, seq(-3, 3, 1))
#' ICC_item_5 <- calcEscore(item_5, seq(-3, 3, 1))
#' ICC_item_6 <- calcEscore(item_6, seq(-3, 3, 1))
#' TCC_pool   <- calcEscore(itempool_science, seq(-3, 3, 1))
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
#' @rdname calcEscore-methods
setGeneric(
  name = "calcEscore",
  def = function(object, theta) {
    standardGeneric("calcEscore")
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_1PL,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcEscore(object, theta))
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_2PL,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcEscore(object, theta))
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_3PL,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcEscore(object, theta))
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_PC,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcEscore(object, theta))
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_GPC,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcEscore(object, theta))
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_GR,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_GR", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcEscore(object, theta))
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_pool,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    theta <- matrix(theta, , 1)
    return(calcEscore(object, theta))
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_1PL,matrix-method
setMethod(
  f = "calcEscore",
  signature = c("item_1PL", "matrix"),
  definition = function(object, theta) {
    return(array_e_1pl(theta, object@difficulty)[, 1])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_2PL,matrix-method
setMethod(
  f = "calcEscore",
  signature = c("item_2PL", "matrix"),
  definition = function(object, theta) {
    return(array_e_2pl(theta, object@slope, object@difficulty)[, 1])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_3PL,matrix-method
setMethod(
  f = "calcEscore",
  signature = c("item_3PL", "matrix"),
  definition = function(object, theta) {
    return(array_e_3pl(theta, object@slope, object@difficulty, object@guessing)[, 1])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_PC,matrix-method
setMethod(
  f = "calcEscore",
  signature = c("item_PC", "matrix"),
  definition = function(object, theta) {
    return(array_e_pc(theta, object@threshold)[, 1])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_GPC,matrix-method
setMethod(
  f = "calcEscore",
  signature = c("item_GPC", "matrix"),
  definition = function(object, theta) {
    return(array_e_gpc(theta, object@slope, object@threshold)[, 1])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_GR,matrix-method
setMethod(
  f = "calcEscore",
  signature = c("item_GR", "matrix"),
  definition = function(object, theta) {
    return(array_e_gr(theta, object@slope, object@category)[, 1])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_pool,matrix-method
setMethod(
  f = "calcEscore",
  signature = c("item_pool", "matrix"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      expected_score <- as.vector(Reduce("+", lapply(object@parms, calcEscore, theta)))
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(expected_score)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_pool_cluster,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("item_pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      expected_score <- lapply(object@pools, calcEscore, theta)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(expected_score)
  }
)
