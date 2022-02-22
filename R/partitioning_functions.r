#' @include shadow_functions.R
NULL

#' @noRd
dbind <- function(...) {

  x <- list(...)

  n_rows <- lapply(x, function(xx) dim(xx)[1])
  n_cols <- lapply(x, function(xx) dim(xx)[2])
  n_rows_total <- sum(unlist(n_rows))
  n_cols_total <- sum(unlist(n_cols))

  o <- matrix(0, n_rows_total, n_cols_total)

  leftpad_row <- 0
  leftpad_col <- 0

  for (i in 1:length(x)) {
    o[
      leftpad_row + (1:n_rows[[i]]),
      leftpad_col + (1:n_cols[[i]])
    ] <- x[[i]]
    leftpad_row <- leftpad_row + n_rows[[i]]
    leftpad_col <- leftpad_col + n_cols[[i]]
  }

  return(o)

}

#' Split an item pool into partitions
#'
#' \code{\link{Split}} is a function to split a pool into multiple parallel tests or pools.
#' When constructing parallel tests, each test is constructed to satisfy all constraints.
#' When constructing parallel pools, each pool is constructed so that it contains a test that satisfies all constraints.
#'
#' @template config_Static-param
#' @template constraints-param
#' @param n_partition the number of partitions to create.
#' @param partition_type \code{test} to create tests, or \code{pool} to create pools.
#' @template force_solver_param
#'
#' @return \code{\link{partition}} returns a \code{list} object containing item indices of created tests/pools.
#'
#' @examples
#'
#' @docType methods
#' @rdname Split-methods
#' @export
setGeneric(
  name = "Split",
  def = function(config, constraints, n_partition, partition_type, force_solver = FALSE) {
    standardGeneric("Split")
  }
)

#' @docType methods
#' @rdname Split-methods
#' @export
setMethod(
  f = "Split",
  signature = c("config_Static"),
  definition = function(config, constraints, n_partition, partition_type, force_solver = FALSE) {

    if (!validObject(config)) {
      stop("'config' object is not valid.")
    }

    if (!force_solver) {
      o <- validateSolver(config, constraints)
      if (!o) {
        return(invisible())
      }
    }

  }
)
