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
        invalid_response <- !(response_data[, i] %in% 0:(item_pool@NCAT[i] - 1))
        response_data[invalid_response, i] <- NA
      }
      o@response_data <- response_data
      return(o)
    }

    return(o)
  }
)
