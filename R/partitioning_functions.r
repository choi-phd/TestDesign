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
getDecisionVariablesOfPoolForMultipool <- function(pool_idx, ni_per_bin, nv_per_bin) {
  return(
    ((pool_idx - 1) * nv_per_bin) + 1:ni_per_bin
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
splitSolutionToBins <- function(solution, n_bins, ni_per_bin, nv_per_bin) {
  o <- list()
  for (b in 1:n_bins) {
    i <- (b - 1) * nv_per_bin + 1:ni_per_bin
    s <- (b - 1) * nv_per_bin + (ni_per_bin + 1):nv_per_bin
    o[[b]] <- list()
    o[[b]]$i <- which(solution[i] == 1)
    o[[b]]$s <- which(solution[s] == 1)
  }
  names(o) <- 1:n_bins
  return(o)
}

#' @noRd
getSetStructureConstraints <- function(constraints) {

  ni <- constraints@ni
  ns <- constraints@ns
  nv <- constraints@nv

  mat_ss <- matrix(0, ns, nv)

  for (s in 1:ns) {
    ni_this_s <- length(constraints@item_index_by_stimulus[[s]])
    mat_ss[
      s,
      constraints@item_index_by_stimulus[[s]]
    ] <- 1
    mat_ss[
      s,
      ni + s
    ] <- -ni_this_s
  }

  return(mat_ss)

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
#' @param partition_size_range (optional) two integer values for the desired range for the size of a partition. Has no effect when \code{partition_type} is \code{test}.
#' For discrete item pools, the default partition size is (pool size / number of partitions).
#' For set-based item pools, the default partition size is (pool size / number of partitions) +/- smallest set size.
#' @template force_solver_param
#'
#' @return \code{\link{partition}} returns an \code{\linkS4class{output_Split}} object containing item/set indices of created tests/pools.
#'
#' @examples
#' \dontrun{
#' config <- createStaticTestConfig(MIP = list(solver = "LPSYMPHONY"))
#' constraints <- constraints_science[1:10]
#'
#' solution <- Split(config, constraints, n_partition = 4, partition_type = "test"))
#' plot(solution)
#' solution <- Split(config, constraints, n_partition = 4, partition_type = "pool"))
#' plot(solution)
#' }
#' @docType methods
#' @rdname Split-methods
#' @export
setGeneric(
  name = "Split",
  def = function(config, constraints, n_partition, partition_type, partition_size_range = NULL, force_solver = FALSE) {
    standardGeneric("Split")
  }
)

#' @docType methods
#' @rdname Split-methods
#' @export
setMethod(
  f = "Split",
  signature = c("config_Static"),
  definition = function(config, constraints, n_partition, partition_type, partition_size_range = NULL, force_solver = FALSE) {

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
    ni                <- constraints@ni
    nv                <- constraints@nv
    n_bins            <- n_partition
    nv_total          <- nv * n_bins
    nv_total_with_dev <- nv_total + 1

    target_thetas  <- config@item_selection$target_location
    target_weights <- config@item_selection$target_weight
    n_targets <- length(target_thetas)

    obj_info <- calcFisher(itempool, target_thetas)
    obj_info <- obj_info * target_weights
    obj_info <- apply(obj_info, 2, sum)

    feasible <- FALSE
    solve_time <- 0

    # Step 1. Make base bins

    # bin partial assignment constraint: an item must be assigned to no more than one bin
    # n of constraints = ni
    mat_bpa <- matrix(0, ni, nv_total_with_dev)
    for (i in 1:ni) {
      mat_bpa[
        i,
        getDecisionVariablesOfItemForMultipool(i, nv, n_bins)
      ] <- 1
    }
    dir_bpa <- rep("<=", ni)
    rhs_bpa <- rep(1   , ni)

    # bin size constraint:
    mat_bs <- matrix(0, n_bins, nv_total_with_dev)
    for (b in 1:n_bins) {
      mat_bs[
        b,
        getDecisionVariablesOfPoolForMultipool(b, ni, nv)
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
      item_info <- item_info * target_weights[k]
      for (p in 1:n_pairs) {
        positive_side <- pairs_table[p, 1]
        negative_side <- pairs_table[p, 2]
        idx <- (k - 1) * n_pairs + p
        pairs_list[[idx]] <- matrix(0, 2, nv_total_with_dev)
        pairs_list[[idx]][1,
          getDecisionVariablesOfPoolForMultipool(positive_side, ni, nv)
        ] <- item_info
        pairs_list[[idx]][1,
          getDecisionVariablesOfPoolForMultipool(negative_side, ni, nv)
        ] <- -item_info
        pairs_list[[idx]][1, nv_total_with_dev] <- -1
        pairs_list[[idx]][2,
          getDecisionVariablesOfPoolForMultipool(positive_side, ni, nv)
        ] <- item_info
        pairs_list[[idx]][2,
          getDecisionVariablesOfPoolForMultipool(negative_side, ni, nv)
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
    mat <- rbind(mat_bpa, mat_bs, mat_c, mat_i, mat_l)
    dir <-     c(dir_bpa, dir_bs, dir_c, dir_i, dir_l)
    rhs <-     c(rhs_bpa, rhs_bs, rhs_c, rhs_i, rhs_l)

    # main optimization
    obj <- rep(0, nv)
    obj[1:ni] <- obj_info
    obj <- rep(obj, n_partition)
    delta_scale <- sum(target_weights) * n_bins
    obj[nv_total_with_dev] <- -delta_scale
    types <- rep("B", nv_total_with_dev)
    types[nv_total_with_dev] <- "C"

    # solve

    solve_time_stepone <- proc.time()

    o1 <- runMIP(
      solver = config@MIP$solver,
      obj = obj,
      mat = mat,
      dir = dir,
      rhs = rhs,
      maximize = TRUE,
      types = types,
      verbosity     = config@MIP$verbosity,
      time_limit    = config@MIP$time_limit,
      gap_limit_abs = config@MIP$gap_limit_abs,
      gap_limit     = config@MIP$gap_limit
    )

    if (!isOptimal(o1$status, config@MIP$solver)) {
      msg <- getSolverStatusMessage(o1$status, config@MIP$solver)
      stop(msg)
    }

    solve_time_stepone <- (proc.time() - solve_time_stepone)[["elapsed"]]
    solve_time[1] <- solve_time_stepone
    feasible[1] <- TRUE
    solution_per_bin <- splitSolutionToBins(o1$solution, n_bins, ni, nv)

    if (partition_type == "test") {
      o <- new("output_Split")
      o@output               <- solution_per_bin
      o@feasible             <- feasible
      o@solve_time           <- solve_time
      o@set_based            <- constraints@set_based
      o@config               <- config
      o@constraints          <- constraints
      o@partition_size_range <- partition_size_range
      o@partition_type       <- partition_type
      return(o)
    }

    # Step 2. Grow each bin --------------------------------------------------------

    # bin full assignment constraint: an item must be assigned to exactly one bin
    mat_bfa <- mat_bpa
    dir_bfa <- rep("==", ni)
    rhs_bfa <- rhs_bpa

    # bin size constraint:
    if (!constraints@set_based & is.null(partition_size_range)) {
      bin_size <- ni / n_bins
      if (bin_size %% 1 != 0) {
        stop(sprintf("unexpected resulting partition size '%s': this must result in an integer", bin_size))
      }
      bin_size_lb <- bin_size
      bin_size_ub <- bin_size
    }
    if (!constraints@set_based & !is.null(partition_size_range)) {
      bin_size_lb <- partition_size_range[1]
      bin_size_ub <- partition_size_range[2]
    }
    if (constraints@set_based & is.null(partition_size_range)) {
      n_i_per_s <- do.call(c, lapply(constraints@item_index_by_stimulus, length))
      smallest_s <- min(n_i_per_s)
      bin_size    <- ni / n_bins
      bin_size_lb <- bin_size - (smallest_s * 1)
      bin_size_ub <- bin_size + (smallest_s * 1)
    }
    if (constraints@set_based & !is.null(partition_size_range)) {
      bin_size_lb <- partition_size_range[1]
      bin_size_ub <- partition_size_range[2]
    }

    mat_bs <- matrix(0, n_bins * 2, nv_total_with_dev)
    for (b in 1:n_bins) {
      mat_bs[
        (b - 1) * 2 + (1:2),
        getDecisionVariablesOfPoolForMultipool(b, ni, nv)
      ] <- 1
    }
    dir_bs <- rep(c(">=", "<="), n_bins)
    rhs_bs <- rep(c(bin_size_lb, bin_size_ub), n_bins)

    # existing assignment constraint from Step 1:
    idx_assignment_stepone <- which(o1$solution == 1)
    n_assignment_stepone   <- length(idx_assignment_stepone)
    mat_be <- matrix(0, n_assignment_stepone, nv_total_with_dev)
    for (i in 1:n_assignment_stepone) {
      mat_be[i, idx_assignment_stepone[i]] <- 1
    }
    dir_be <- rep("==", n_assignment_stepone)
    rhs_be <- rep(1   , n_assignment_stepone)

    # set-based constraints (to keep set-based structure in growed portions)
    if (!constraints@set_based) {
      mat_ss <- matrix(0, 0, nv_total_with_dev)
      dir_ss <- character(0)
      rhs_ss <- numeric(0)
    }
    if (constraints@set_based) {
      mat_ss <- getSetStructureConstraints(constraints)
      dir_ss <- rep("==", constraints@ns)
      rhs_ss <- rep(0   , constraints@ns)
      mat_list <- vector("list", n_bins)
      dir_list <- vector("list", n_bins)
      rhs_list <- vector("list", n_bins)
      for (b in 1:n_bins) {
        mat_list[[b]] <- mat_ss
        dir_list[[b]] <- dir_ss
        rhs_list[[b]] <- rhs_ss
      }
      mat_ss <- do.call(dbind, mat_list)
      dir_ss <- unlist(dir_list)
      rhs_ss <- unlist(rhs_list)
      mat_ss <- cbind(mat_ss, 0) # add deviance variable
    }

    # combine all constraints
    mat <- rbind(mat_bfa, mat_bs, mat_be, mat_ss, mat_i, mat_l)
    dir <-     c(dir_bfa, dir_bs, dir_be, dir_ss, dir_i, dir_l)
    rhs <-     c(rhs_bfa, rhs_bs, rhs_be, rhs_ss, rhs_i, rhs_l)

    # main optimization
    obj <- rep(0, nv_total_with_dev)
    obj[nv_total_with_dev] <- 1
    types <- rep("B", nv_total_with_dev)
    types[nv_total_with_dev] <- "C"

    # solve

    solve_time_steptwo <- proc.time()

    o2 <- runMIP(
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

    if (isOptimal(o2$status, config@MIP$solver)) {
      feasible[2] <- TRUE
    }
    if (!isOptimal(o2$status, config@MIP$solver)) {
      feasible[2] <- FALSE
      # retry using partial assignment
      mat <- rbind(mat_bpa, mat_bs, mat_be, mat_ss, mat_i, mat_l)
      dir <-     c(dir_bpa, dir_bs, dir_be, dir_ss, dir_i, dir_l)
      rhs <-     c(rhs_bpa, rhs_bs, rhs_be, rhs_ss, rhs_i, rhs_l)
      o2 <- runMIP(
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
      if (!isOptimal(o2$status, config@MIP$solver)) {
        msg <- getSolverStatusMessage(o2$status, config@MIP$solver)
        stop(msg)
      }
    }

    solve_time_steptwo <- (proc.time() - solve_time_steptwo)[["elapsed"]]
    solve_time[2] <- solve_time_steptwo
    solution_per_bin <- splitSolutionToBins(o2$solution, n_bins, ni, nv)

    if (partition_type == "pool") {
      o <- new("output_Split")
      o@output               <- solution_per_bin
      o@feasible             <- feasible
      o@solve_time           <- solve_time
      o@set_based            <- constraints@set_based
      o@config               <- config
      o@constraints          <- constraints
      o@partition_size_range <- partition_size_range
      o@partition_type       <- partition_type
      return(o)
    }

  }
)
