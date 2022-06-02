#' @include item_class.R
NULL

#' Create a simulation data cache object
#'
#' \code{\link{makeSimulationDataCache}} is a function for creating a \code{\linkS4class{simulation_data_cache}} object.
#' This is used in \code{\link{Shadow}} to make all necessary data (e.g., item information, response data) prior to the main simulation.
#'
#' @param item_pool an \code{\linkS4class{item_pool}} object.
#' @param info_type the type of information.
#' @param theta_grid a grid of theta values.
#' @param seed (optional) seed to use for generating response data if needed.
#' @param true_theta (optional) true theta values of all simulees.
#' @param response_data (optional) response data on all items for all simulees.
#'
#' @docType methods
#' @rdname makeSimulationDataCache-methods
setGeneric(
  name = "makeSimulationDataCache",
  def = function(item_pool, info_type = "FISHER", theta_grid = seq(-4, 4, .1), seed = NULL, true_theta = NULL, response_data = NULL) {
    standardGeneric("makeSimulationDataCache")
  }
)

#' @docType methods
#' @rdname makeSimulationDataCache-methods
#' @export
setMethod(
  f = "makeSimulationDataCache",
  signature = "item_pool",
  definition = function(item_pool, info_type = "FISHER", theta_grid = seq(-4, 4, .1), seed = NULL, true_theta = NULL, response_data = NULL) {

    o <- new("simulation_data_cache")

    if (!toupper(info_type) %in% c("FISHER")) {
      stop(sprintf("makeSimulationDataCache: unexpected info_type '%s' (accepts 'FISHER')", info_type))
    }

    o@item_pool  <- item_pool
    o@theta_grid <- theta_grid
    o@prob_grid <- calcProb(item_pool, theta_grid)

    if (toupper(info_type) == "FISHER") {
      o@info_grid <- calcFisher(item_pool, theta_grid)
    }

    o@max_info <- max(o@info_grid)

    if (is.null(true_theta) & is.null(response_data)) {
      stop("makeSimulationDataCache: true_theta and response_data are both NULL (either true_theta or response_data must be supplied)")
    }

    if (!is.null(true_theta) & is.null(response_data)) {
      # if seed is empty, generate response data in advance
      if (is.null(seed)) {
        o@response_data <- simResp(item_pool, true_theta)
        return(o)
      }
      # if seed is available, do not generate response data here; do it on the fly
      if (!is.null(seed)) {
        return(o)
      }
    }

    if (!is.null(response_data)) {
      # allocate response data after filtering
      for (i in 1:item_pool@ni) {
        # replace out-of-bound responses with NA
        # TODO: may need to exclude these items from being selected
        # TODO: may need to raise a warning
        invalid_response <- !(response_data[, i] %in% 0:(item_pool@NCAT[i] - 1))
        response_data[invalid_response, i] <- NA
      }
      o@response_data <- response_data
      return(o)
    }

    return(o)
  }
)

#' Create a test object
#'
#' \code{\link{makeTest}} is a function for creating a \code{\linkS4class{test}} object.
#' This is used to make all necessary data (e.g., item information, response data) prior to the main simulation.
#' This function is only kept for backwards compatibility.
#' The functionality of this function is superseded by \code{\link{makeSimulationDataCache}}.
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
    stop("subsetTest: 'x' must be a 'test' object.")
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
    stop("subsetTest: 'i' contains invalid values")
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

#' Create a test cluster object
#'
#' \code{\link{makeTestCluster}} is a function for creating a \code{\linkS4class{test_cluster}} object.
#' This is used to make all necessary data (e.g., item information, response data) prior to the main simulation.
#' This function is only kept for backwards compatibility.
#'
#' @param object an \code{\linkS4class{item_pool_cluster}} object.
#' @param theta a grid of theta values.
#' @param true_theta an optional vector of true theta values to simulate response data.
#'
#' @docType methods
#' @rdname makeTestCluster-methods
#'
#' @export
setGeneric(
  name = "makeTestCluster",
  def = function(object, theta, true_theta) {
    standardGeneric("makeTestCluster")
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods
setMethod(
  f = "makeTestCluster",
  signature = c("item_pool_cluster", "numeric", "numeric"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta)
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods
setMethod(
  f = "makeTestCluster",
  signature = c("item_pool_cluster", "numeric", "list"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta[[p]])
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)
