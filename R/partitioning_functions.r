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

#' @noRd
getDecisionVariablesOfPoolForMultipool <- function(pool_idx, nv_per_bin) {
  return(
    ((pool_idx - 1) * nv_per_bin) + 1:nv_per_bin
  )
}

#' @noRd
getDecisionVariablesOfItemForMultipool <- function(item_idx, nv_per_bin, n_bins) {
  nv_total <- nv_per_bin * n_bins
  return(
    seq(item_idx, nv_total, nv_per_bin)
  )
}

#' @noRd
splitSolutionToBins <- function(solution, n_bins, ni_per_bin) {
  pool_size <- length(solution) / n_bins
  o <- split(solution, ceiling(seq_along(solution) / pool_size))
  o <- lapply(o, function(x) x[1:ni_per_bin])
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
      o <- validateSolver(config, constraints, purpose = "SPLIT")
      if (!o) {
        return(invisible())
      }
    }

    itempool          <- constraints@pool
    ni                <- itempool@ni
    n_bins            <- n_partition
    nv_total          <- ni * n_bins
    nv_total_with_dev <- nv_total + 1

    target_thetas <- config@item_selection$target_location
    n_targets <- length(target_thetas)

    # Step 1. Make base bins

    # bin assignment constraint: an item must be assigned to no more than one bin
    # n of constraints = ni
    mat_ba <- matrix(0, ni, nv_total_with_dev)
    for (i in 1:ni) {
      mat_ba[
        i,
        getDecisionVariablesOfItemForMultipool(i, ni, n_bins)
      ] <- 1
    }
    dir_ba <- rep("<=", ni)
    rhs_ba <- rep(1   , ni)

    # bin size constraint:
    mat_bs <- matrix(0, n_bins, nv_total_with_dev)
    for (b in 1:n_bins) {
      mat_bs[
        b,
        getDecisionVariablesOfPoolForMultipool(b, ni)
      ] <- 1
    }
    dir_bs <- rep("=="                   , n_bins)
    rhs_bs <- rep(constraints@test_length, n_bins)

    # content constraints
    mat_list <- vector("list", n_bins)
    dir_list <- vector("list", n_bins)
    rhs_list <- vector("list", n_bins)

    for (b in 1:n_bins) {
      mat_list[[b]] <- constraints@mat
      dir_list[[b]] <- constraints@dir
      rhs_list[[b]] <- constraints@rhs
    }

    mat_c <- do.call(dbind, mat_list)
    dir_c <- unlist(dir_list)
    rhs_c <- unlist(rhs_list)
    mat_c <- cbind(mat_c, 0) # add deviance variable

    # formulate information difference as constraints
    n_targets <- length(target_thetas)
    pairs_table <- t(combn(n_bins, 2))
    n_pairs <- dim(pairs_table)[1]
    pairs_list <- list()

    for (k in 1:n_targets) {
      item_info <- calcFisher(itempool, target_thetas[k])[1, ]
      for (p in 1:n_pairs) {
        positive_side <- pairs_table[p, 1]
        negative_side <- pairs_table[p, 2]
        idx <- (k - 1) * n_pairs + p
        pairs_list[[idx]] <- matrix(0, 2, nv_total_with_dev)
        pairs_list[[idx]][1,
          getDecisionVariablesOfPoolForMultipool(positive_side, ni)
        ] <- item_info
        pairs_list[[idx]][1,
          getDecisionVariablesOfPoolForMultipool(negative_side, ni)
        ] <- -item_info
        pairs_list[[idx]][1, nv_total_with_dev] <- -1
        pairs_list[[idx]][2,
          getDecisionVariablesOfPoolForMultipool(positive_side, ni)
        ] <- item_info
        pairs_list[[idx]][2,
          getDecisionVariablesOfPoolForMultipool(negative_side, ni)
        ] <- -item_info
        pairs_list[[idx]][2, nv_total_with_dev] <- 1
      }
    }

    mat_i <- do.call(rbind, pairs_list)
    dir_i <- rep(c("<=", ">="), n_pairs * n_targets)
    rhs_i <- rep(c(0, 0), n_pairs * n_targets)

    # enforce lowerbound on deviation
    mat_l <- matrix(0, 1, nv_total_with_dev)
    mat_l[1, nv_total_with_dev] <- 1
    dir_l <- ">="
    rhs_l <- config@MIP$obj_tol

    # aggregate all constraints
    mat <- rbind(mat_ba, mat_bs, mat_c, mat_i, mat_l)
    dir <-     c(dir_ba, dir_bs, dir_c, dir_i, dir_l)
    rhs <-     c(rhs_ba, rhs_bs, rhs_c, rhs_i, rhs_l)

    # solve
    obj <- rep(0, nv_total_with_dev)
    obj[nv_total_with_dev] <- 1
    types <- rep("B", nv_total_with_dev)
    types[nv_total_with_dev] <- "C"

    o <- runMIP(
      solver = config@MIP$solver,
      obj = obj,
      mat = mat,
      dir = dir,
      rhs = rhs,
      maximize = FALSE,
      types = types,
      verbosity     = config@MIP$verbosity,
      time_limit    = config@MIP$time_limit,
      gap_limit_abs = config@MIP$gap_limit_abs,
      gap_limit     = config@MIP$gap_limit
    )

    solution_per_bin <- splitSolutionToBins(o$solution, n_bins, ni)
    oo <- lapply(
      solution_per_bin,
      function(x) {
        which(x == 1)
      }
    )

    if (partition_type == "test") {
      return(oo)
    }

    # Step 2. Grow each bin --------------------------------------------------------

    # bin assignment constraint: an item must be assigned to exactly one bin
    mat_ba <- mat_ba
    dir_ba <- rep("==", ni)
    rhs_ba <- rhs_ba

    # bin size constraint:
    bin_size <- ni / n_bins
    if (bin_size %% 1 != 0) {
      stop(sprintf("unexpected resulting partition size '%s': this must result in an integer", bin_size))
    }
    mat_bs <- mat_bs
    dir_bs <- dir_bs
    rhs_bs <- rep(bin_size, n_bins)

    # bin items constraint from Step 1:
    idx_items_stepone <- which(o$solution == 1)
    n_items_stepone   <- length(idx_items_stepone)
    mat_bi <- matrix(0, n_items_stepone, nv_total_with_dev)
    for (i in 1:n_items_stepone) {
      mat_bi[i, idx_items_stepone[i]] <- 1
    }
    dir_bi <- rep("==", n_items_stepone)
    rhs_bi <- rep(1   , n_items_stepone)

    # combine all constraints
    mat <- rbind(mat_ba, mat_bs, mat_bi, mat_i, mat_l)
    dir <-     c(dir_ba, dir_bs, dir_bi, dir_i, dir_l)
    rhs <-     c(rhs_ba, rhs_bs, rhs_bi, rhs_i, rhs_l)

    # solve
    o <- runMIP(
      solver = config@MIP$solver,
      obj = obj,
      mat = mat,
      dir = dir,
      rhs = rhs,
      maximize = FALSE,
      types = types,
      verbosity     = config@MIP$verbosity,
      time_limit    = config@MIP$time_limit,
      gap_limit_abs = config@MIP$gap_limit_abs,
      gap_limit     = config@MIP$gap_limit
    )

    solution_per_bin <- splitSolutionToBins(o$solution, n_bins, ni)
    oo <- lapply(
      solution_per_bin,
      function(x) {
        which(x == 1)
      }
    )

    if (partition_type == "pool") {
      return(oo)
    }

  }
)
