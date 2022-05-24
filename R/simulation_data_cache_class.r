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
