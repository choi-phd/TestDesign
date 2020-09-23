#' @include item_class.R
NULL

#' Generate a test object
#'
#' \code{\link{makeTest}} is a function for creating a \code{\linkS4class{test}} object.
#' This is used in \code{\link{Shadow}} to determine all necessary data prior to the main simulation, so that they are not affected by random number generation.
#'
#' @param object an \code{\linkS4class{item_pool}} object.
#' @param theta a grid of theta values.
#' @param info_type the type of information.
#' @param true_theta (optional) true theta values to simulate response data.
#'
#' @docType methods
#' @rdname makeTest-methods
#'
#' @examples
#' test <- makeTest(itempool_science, seq(-3, 3, 1))
#' @export
setGeneric(
  name = "makeTest",
  def = function(object, theta = seq(-4, 4, .1), info_type = "FISHER", true_theta = NULL) {
    standardGeneric("makeTest")
  }
)

#' @docType methods
#' @rdname makeTest-methods
#' @export
setMethod(
  f = "makeTest",
  signature = "item_pool",
  definition = function(object, theta = seq(-4, 4, .1), info_type = "FISHER", true_theta = NULL) {

    o <- new("test")

    o@pool  <- object
    o@theta <- theta
    o@prob  <- calcProb(o@pool, o@theta)
    if (toupper(info_type) == "FISHER") {
      o@info <- calcFisher(o@pool, o@theta)
    } else {
      stop(sprintf("makeTest: unexpected info_type '%s' (accepts 'FISHER')", info_type))
    }
    if (!is.null(true_theta)) {
      o@data <- simResp(o@pool, true_theta)
    } else {
      o@data <- NULL
    }

    return(o)
  }
)

#' @title Basic operators for test objects
#'
#' @description
#'
#' Create a subset of a \code{\linkS4class{test}} object.
#'
#' @param x a \code{\linkS4class{test}} object.
#' @param i item indices to use in subsetting.
#' @param j,drop,... not used, exists for compatibility.
#'
#' @name test_operators
NULL

#' @rdname test_operators
#' @export
subsetTest <- function(x, i = NULL) {
  if (!inherits(x, "test")) {
    stop("'x' must be a 'test' object.")
  }
  if (is.null(i)) {
    return(x)
  } else if (all(i %in% 1:x@pool@ni) && anyDuplicated(i) == 0) {
    n_select            <- length(i)
    o            <- new("test")
    o@pool       <- subsetItemPool(x@pool, i)
    o@theta      <- x@theta
    o@prob       <- x@prob[i]
    o@info       <- x@info[, i, drop = FALSE]
    o@true_theta <- x@true_theta
    o@data       <- x@data[, i, drop = FALSE]
    return(o)
  } else {
    stop("'i' contains invalid values.")
  }
}

#' @aliases [,test,numeric,ANY,ANY-method
#' @docType methods
#' @rdname test_operators
setMethod(
  f = "[",
  signature = "test",
  definition = function(x, i, j, ...) {
    return(subsetTest(x, i))
  }
)
