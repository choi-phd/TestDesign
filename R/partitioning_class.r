#' @include shadow_functions.R
NULL

#' Class 'output_Split': partitioning solution
#'
#' \code{\linkS4class{output_Split}} is an S4 class to represent the partitioning solution of an item pool.
#'
#' @slot output a list containing item/set indices of each partition.
#' @slot feasible for partitioning into sub-pools, \code{TRUE} indicates the complete assignment problem was feasible.
#' @slot solve_time elapsed time in running the solver.
#' @slot set_based whether the item pool is set-based.
#' @slot config the \code{\linkS4class{config_Static}} used in the assembly.
#' @slot constraints the \code{\linkS4class{constraints}} used in the assembly.
#' @slot partition_size_range the partition size range for splitting into sub-pools.
#' @slot partition_type the partition type. Can be a \code{test} or a \code{pool}.
#'
#' @export
setClass("output_Split",
  slots = c(
    output               = "list",
    feasible             = "logical",
    solve_time           = "numeric",
    set_based            = "logical",
    config               = "config_Static",
    constraints          = "constraints",
    partition_size_range = "numeric_or_null",
    partition_type       = "character"
  ),
  prototype = list(
    output               = list(0),
    feasible             = logical(0),
    solve_time           = numeric(0),
    set_based            = logical(0),
    config               = new("config_Static"),
    constraints          = new("constraints"),
    partition_size_range = numeric(0),
    partition_type       = character(0)
  ),
  validity = function(object) {
    return(TRUE)
  }
)
