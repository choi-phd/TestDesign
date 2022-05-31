#' @include item_class.R
NULL

#' Class 'simulation_data_cache': data cache for Shadow()
#'
#' \code{\linkS4class{simulation_data_cache}} is an S4 class to represent data cache for Shadow().
#'
#' @slot item_pool the \code{\linkS4class{item_pool}} object.
#' @slot theta_grid the theta grid to use as quadrature points.
#' @slot prob_grid the list containing item response probabilities at theta quadratures.
#' @slot info_grid the matrix containing item information values at theta quadratures.
#' @slot max_info the maximum value of \code{info_grid}.
#' @slot true_theta (optional) the true theta values.
#' @slot response_data (optional) the matrix containing item responses.
#'
#' @export
setClass("simulation_data_cache",
  slots = c(
    item_pool     = "item_pool",
    theta_grid    = "numeric",
    prob_grid     = "list",
    info_grid     = "matrix",
    max_info      = "numeric",
    true_theta    = "numeric_or_null",
    response_data = "matrix_or_null"
  ),
  prototype = list(
    item_pool     = new("item_pool"),
    theta_grid    = numeric(0),
    prob_grid     = list(0),
    info_grid     = matrix(0),
    max_info      = numeric(0),
    true_theta    = numeric(0),
    response_data = matrix(NA, 0, 0)
  ),
  validity = function(object) {
    x <- NULL
    if (length(object@prob_grid) != object@item_pool@ni) {
      x <- c(x, "simulation_data_cache: length(@prob_grid) must be equal to @item_pool@ni")
    }
    if (ncol(object@info_grid) != object@item_pool@ni) {
      x <- c(x, "simulation_data_cache: ncol(@info_grid) must match @item_pool@ni")
    }
    if (nrow(object@info_grid) != length(object@theta_grid)) {
      x <- c(x, "simulation_data_cache: nrow(@info_grid) must match length(@theta_grid)")
    }
    if (length(x) == 0) {
      return(TRUE)
    }
    return(x)
  }
)

#' Class 'test': data cache for Shadow()
#'
#' \code{\linkS4class{test}} is an S4 class to represent data cache for Shadow().
#' This class is only kept for backwards compatibility.
#' The functionality of this class is superseded by \code{\linkS4class{simulation_data_cache}}.
#'
#' @slot pool the \code{\linkS4class{item_pool}} object.
#' @slot theta the theta grid to use as quadrature points.
#' @slot prob the list containing item response probabilities.
#' @slot info the matrix containing item information values.
#' @slot true_theta (optional) the true theta values.
#' @slot data (optional) the matrix containing item responses.
#'
#' @export
setClass("test",
  slots = c(
    pool       = "item_pool",
    theta      = "numeric",
    prob       = "list",
    info       = "matrix",
    true_theta = "numeric_or_null",
    data       = "matrix_or_null"
  ),
  prototype = list(
    pool       = new("item_pool"),
    theta      = numeric(0),
    prob       = list(0),
    info       = matrix(0),
    true_theta = numeric(0),
    data       = matrix(NA, 0, 0)
  ),
  validity = function(object) {
    x <- NULL
    if (length(object@prob) != object@pool@ni) {
      x <- c(x, "test: length(@prob) must be equal to @pool@ni")
    }
    if (ncol(object@info) != object@pool@ni) {
      x <- c(x, "test: ncol(@info) must match @pool@ni")
    }
    if (nrow(object@info) != length(object@theta)) {
      x <- c(x, "test: nrow(@info) must match length(@theta)")
    }
    if (length(x) == 0) {
      return(TRUE)
    }
    return(x)
  }
)

#' Class 'test_cluster': data cache for Shadow()
#'
#' \code{\linkS4class{test_cluster}} is an S4 class to represent data cache for Shadow().
#' This class is only kept for backwards compatibility.
#'
#' @slot nt the number of \code{\linkS4class{test}} objects in this cluster.
#' @slot tests the list containing \code{\linkS4class{test}} objects.
#' @slot names test ID strings for each \code{\linkS4class{test}} object.
#'
#' @export
setClass("test_cluster",
  slots = c(
    nt      = "numeric",
    tests   = "list",
    names   = "character"
  ),
  prototype = list(
    nt      = numeric(0),
    tests   = list(0),
    names   = character(0)
  ),
  validity = function(object) {
    x <- NULL
    if (length(object@tests) != object@nt) {
      x <- c(x, "test_cluster: @nt must be equal to length(@tests)")
    }
    if (length(object@names) != object@nt) {
      x <- c(x, "test_cluster: @nt must be equal to length(@names)")
    }
    if (length(x) == 0) {
      return(TRUE)
    }
    return(x)
  }
)
