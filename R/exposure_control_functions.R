#' @include shadow_functions.R
NULL

#' (Internal) Perform exposure control
#'
#' \code{\link{doExposureControl}} is an internal function for performing exposure control.
#' This is a wrapper function for calling other low-level functions for respective exposure control methods.
#'
#' @template parameter_exposure_record
#' @template parameter_segment_record
#' @param o an \code{\linkS4class{output_Shadow}} object, containing data for a single examinee.
#' @param j the numeric index of the examinee.
#' @param current_theta a named list containing data on current theta estimate.
#' @template parameter_eligibility_flag
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{doExposureControl}} returns an updated list for exposure control records.
#'
#' @keywords internal
doExposureControl <- function(
  exposure_record, segment_record,
  o, j,
  current_theta,
  eligibility_flag,
  simulation_constants
) {

  if (!simulation_constants$use_exposure_control) {
    return(exposure_record)
  }

  segment_of                 <- getSegmentOf(o, simulation_constants)
  segment_record             <- updateSegmentRecord(segment_record, segment_of, j)
  eligibility_flag_in_final_theta_segment <- getEligibilityFlagInSegment(eligibility_flag, segment_of$final_theta_est, simulation_constants)

  if (simulation_constants$exposure_control_method %in% c("ELIGIBILITY")) {

    assembly_was_feasible <- assemblyInFinalThetaSegmentWasFeasibleAtLeastOnceInInterimThetaSegments(
      o, segment_of$final_theta_est
    )
    segments_to_apply <- getSegmentsToApply(simulation_constants$n_segment, segment_of$final_theta_est)
    exposure_record   <- applyFading(exposure_record, segments_to_apply, simulation_constants)
    segment_prob      <- 1
    exposure_record   <- incrementN(exposure_record, segments_to_apply, segment_prob, simulation_constants)
    exposure_record   <- incrementPhi(exposure_record, segments_to_apply, segment_prob, assembly_was_feasible)
    exposure_record   <- incrementAlpha(exposure_record, segments_to_apply, segment_prob, o, simulation_constants)
    exposure_record   <- incrementRho(exposure_record, segments_to_apply, segment_prob, eligibility_flag, assembly_was_feasible, simulation_constants)
    exposure_record   <- adjustAlphaToReduceSpike(exposure_record, segment_prob, segment_of$visited, eligibility_flag_in_final_theta_segment, o, simulation_constants)
    exposure_record   <- updateEligibilityRates(exposure_record, simulation_constants)
    exposure_record   <- clipEligibilityRates(exposure_record, simulation_constants)
    return(exposure_record)

  }

  if (simulation_constants$exposure_control_method %in% c("BIGM")) {

    assembly_was_feasible <- TRUE
    segments_to_apply <- getSegmentsToApply(simulation_constants$n_segment, segment_of$final_theta_est)
    exposure_record   <- applyFading(exposure_record, segments_to_apply, simulation_constants)
    segment_prob      <- 1
    exposure_record   <- incrementN(exposure_record, segments_to_apply, segment_prob, simulation_constants)
  # exposure_record   <- incrementPhi(exposure_record, segments_to_apply, segment_prob, TRUE) # is not called for the purpose of code optimization; see comments in incrementPhi()
    exposure_record   <- incrementAlpha(exposure_record, segments_to_apply, segment_prob, o, simulation_constants)
    exposure_record   <- incrementRho(exposure_record, segments_to_apply, segment_prob, eligibility_flag, assembly_was_feasible, simulation_constants)
    exposure_record   <- adjustAlphaToReduceSpike(exposure_record, segment_prob, segment_of$visited, eligibility_flag_in_final_theta_segment, o, simulation_constants)
    exposure_record   <- updateEligibilityRates(exposure_record, simulation_constants)
    exposure_record   <- clipEligibilityRates(exposure_record, simulation_constants)
    return(exposure_record)

  }

  if (simulation_constants$exposure_control_method %in% c("BIGM-BAYESIAN")) {

    assembly_was_feasible <- TRUE
    segments_to_apply <- getSegmentsToApply(simulation_constants$n_segment, 1:simulation_constants$n_segment)
    exposure_record   <- applyFading(exposure_record, segments_to_apply, simulation_constants)
    segment_prob      <- getSegmentProb(current_theta$posterior_sample, simulation_constants)
    exposure_record   <- incrementN(exposure_record, segments_to_apply, segment_prob, simulation_constants)
  # exposure_record   <- incrementPhi(exposure_record, segments_to_apply, segment_prob, TRUE) # is not called for the purpose of code optimization; see comments in incrementPhi()
    exposure_record   <- incrementAlpha(exposure_record, segments_to_apply, segment_prob, o, simulation_constants)
    exposure_record   <- incrementRho(exposure_record, segments_to_apply, segment_prob, eligibility_flag, assembly_was_feasible, simulation_constants)
    exposure_record   <- adjustAlphaToReduceSpike(exposure_record, segment_prob[segment_of$final_theta_est], segment_of$visited, eligibility_flag_in_final_theta_segment, o, simulation_constants)
    exposure_record   <- updateEligibilityRates(exposure_record, simulation_constants)
    exposure_record   <- clipEligibilityRates(exposure_record, simulation_constants)
    return(exposure_record)

  }

}

#' (Internal) Update diagnostic exposure record
#'
#' \code{\link{updateDiagnosticExposureRecord}} is an internal function for updating diagnostic exposure record.
#' This function does not do actual exposure control;
#' this function is used for updating diagnostic exposure record after exposure control is performed.
#'
#' @template parameter_diagnostic_exposure_record
#' @param j the numeric index of the examinee.
#' @template parameter_exposure_record
#' @template parameter_config_Shadow
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{updateDiagnosticExposureRecord}} returns an updated diagnostic exposure record.
#'
#' @keywords internal
updateDiagnosticExposureRecord <- function(
  diagnostic_exposure_record,
  j,
  exposure_record,
  config,
  simulation_constants
) {

  if (!config@exposure_control$diagnostic_stats) {
    return(diagnostic_exposure_record)
  }

  n_segment     <- simulation_constants$n_segment
  fading_factor <- simulation_constants$fading_factor

  ni <- simulation_constants$ni
  for (g in 1:n_segment) {
    diagnostic_exposure_record$a_g_i[[g]][j, 1:ni] <- exposure_record$a_ijk[g, ]
    diagnostic_exposure_record$e_g_i[[g]][j, 1:ni] <- exposure_record$r_ijk[g, ]
    if (fading_factor != 1) {
      diagnostic_exposure_record$a_g_i_nofade[[g]][j, 1:ni] <- exposure_record$a_ijk_nofade[g, ]
      diagnostic_exposure_record$e_g_i_nofade[[g]][j, 1:ni] <- exposure_record$r_ijk_nofade[g, ]
    }
  }

  if (!simulation_constants$group_by_stimulus) {
    return(diagnostic_exposure_record)
  }

  ns <- simulation_constants$ns
  for (g in 1:n_segment) {
    diagnostic_exposure_record$a_g_s[[g]][j, 1:ns] <- exposure_record$a_sjk[g, ]
    diagnostic_exposure_record$e_g_s[[g]][j, 1:ns] <- exposure_record$r_sjk[g, ]
    if (fading_factor != 1) {
      diagnostic_exposure_record$a_g_s_nofade[[g]][j, 1:ns] <- exposure_record$a_sjk_nofade[g, ]
      diagnostic_exposure_record$e_g_s_nofade[[g]][j, 1:ns] <- exposure_record$r_sjk_nofade[g, ]
    }
  }

  return(diagnostic_exposure_record)

}

#' (Internal) Initialize segment record
#'
#' \code{\link{initializeSegmentRecord}} is an internal function
#' for creating a new segment record.
#'
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{initializeSegmentRecord}} returns a list containing segment record.
#' \itemize{
#'   \item{\code{freq_true} is a length-\code{n_segment} vector containing the number of times examinees belonged in each theta segment, based on true thetas.}
#'   \item{\code{freq_est} is a length-\code{n_segment} vector containing the number of times examinees belonged in each theta segment, based on final theta estimates.}
#'   \item{\code{count_true}}
#'   \item{\code{count_est}}
#' }
#'
#' @keywords internal
initializeSegmentRecord <- function(simulation_constants) {

  o <- list()

  if (!simulation_constants$use_exposure_control) {
    return(o)
  }

  o$freq_true  <- numeric(simulation_constants$n_segment)
  o$freq_est   <- numeric(simulation_constants$n_segment)
  o$count_true <- numeric(simulation_constants$nj)
  o$count_est  <- numeric(simulation_constants$nj)

  return(o)

}

#' (Internal) Initialize exposure record
#'
#' \code{\link{initializeExposureRecord}} is an internal function
#' for creating a new exposure record.
#'
#' @param exposure_control the \code{exposure_control} slot from a \code{\linkS4class{config_Shadow}} object.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{initializeExposureRecord}} returns a list containing exposure record.
#'
#' @keywords internal
initializeExposureRecord <- function(
  exposure_control, simulation_constants
) {

  # the j subscript used here is intentional and should not be removed.
  # the notation scheme is from van der Linden, W. J., & Veldkamp, B. P. (2007).
  # for example, a_ijk here is interpreted as:
  # - the number of examinees until j-th examinee who was in theta segment k and was administered item i.
  # - see how this interpretation does not imply the variable is a three-dimensional array.

  exposure_record <- list()

  ni <- simulation_constants$ni
  ns <- simulation_constants$ns
  n_segment <- simulation_constants$n_segment

  exposure_record$n_jk  <- numeric(n_segment)
  if (toupper(exposure_control$method) == "ELIGIBILITY") {
    exposure_record$f_jk  <- numeric(n_segment)
  }

  if (simulation_constants$fading_factor != 1) {
    exposure_record$n_jk_nofade  <- exposure_record$n_jk
  }

  exposure_record$a_ijk <- matrix(0, n_segment, ni)
  exposure_record$r_ijk <- matrix(0, n_segment, ni)
  exposure_record$p_e_i  <- matrix(1, n_segment, ni)
  if (simulation_constants$fading_factor != 1) {
    exposure_record$a_ijk_nofade <- exposure_record$a_ijk
    exposure_record$r_ijk_nofade <- exposure_record$r_ijk
  }

  if (!simulation_constants$group_by_stimulus) {
    return(exposure_record)
  }

  exposure_record$a_sjk <- matrix(0, n_segment, ns)
  exposure_record$r_sjk <- matrix(0, n_segment, ns)
  exposure_record$p_e_s  <- matrix(1, n_segment, ns)
  if (simulation_constants$fading_factor != 1) {
    exposure_record$a_sjk_nofade <- exposure_record$a_sjk
    exposure_record$r_sjk_nofade <- exposure_record$r_sjk
  }

  return(exposure_record)

}

#' (Internal) Initialize diagnostic exposure record
#'
#' \code{\link{initializeDiagnosticExposureRecord}} is an internal function
#' for creating a new diagnostic exposure record.
#'
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{initializeDiagnosticExposureRecord}} returns a list containing diagnostic exposure record.
#'
#' @keywords internal
initializeDiagnosticExposureRecord <- function(simulation_constants) {

  o <- list()
  ni <- simulation_constants$ni
  ns <- simulation_constants$ns
  nj <- simulation_constants$nj
  n_segment     <- simulation_constants$n_segment
  fading_factor <- simulation_constants$fading_factor

  if (!simulation_constants$group_by_stimulus) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (simulation_constants$group_by_stimulus) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  if (fading_factor != 1 & !simulation_constants$group_by_stimulus) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (fading_factor != 1 & simulation_constants$group_by_stimulus) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  return(o)

}

#' (Internal) Calculate theta segment of a given examinee
#'
#' \code{\link{getSegmentOf}} is an internal function for
#' calculating which theta segment an examinee belongs to.
#'
#' @param x an \code{\linkS4class{output_Shadow}} object.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{getSegmentOf}} returns a list containing:
#' \itemize{
#'   \item{\code{final_theta_est} the theta segment based on final theta estimate.}
#'   \item{\code{true_theta} the theta segment based on true theta.}
#'   \item{\code{visited} segments the examinee visited.}
#' }
#'
#' @keywords internal
getSegmentOf <- function(x, simulation_constants) {

  o <- list()

  # find_segment() needs to be updated for multidimensional segments
  o$final_theta_est   <- find_segment(x@final_theta_est, simulation_constants$segment_cut)

  tmp                 <- sort(unique(x@theta_segment_index))
  o$visited           <- tmp[tmp != o$final_theta_est]

  if (is.null(x@true_theta)) {
    return(o)
  }

  # find_segment() needs to be updated for multidimensional segments
  o$true_theta        <- find_segment(x@true_theta, simulation_constants$segment_cut)
  return(o)

}

#' (Internal) Update segment record
#'
#' \code{\link{updateSegmentRecord}} is an internal function for
#' updating segment record.
#'
#' @template parameter_segment_record
#' @param segment_of a named list containing theta segments of this examinee.
#' @param j examinee index.
#'
#' @returns \code{\link{updateSegmentRecord}} returns an updated segment record.
#'
#' @keywords internal
updateSegmentRecord <- function(segment_record, segment_of, j) {

  segment_record$freq_est[segment_of$final_theta_est] <-
  segment_record$freq_est[segment_of$final_theta_est] + 1
  segment_record$count_est[j] <-
  segment_record$freq_est[segment_of$final_theta_est]

  if (is.null(segment_of$true_theta)) {
    return(segment_record)
  }

  segment_record$freq_true[segment_of$true_theta] <-
  segment_record$freq_true[segment_of$true_theta] + 1
  segment_record$count_true[j] <-
  segment_record$freq_true[segment_of$true_theta]

  return(segment_record)

}

#' (Internal) Make a vector for segment-dimensioned matrix update
#'
#' \code{\link{getSegmentsToApply}} is an internal function for
#' creating a vector for the purpose of updating various matrices that use theta segments as a dimension.
#'
#' @param n_segment the total number of segments.
#' @param segments a vector of target segments.
#'
#' @returns \code{\link{getSegmentsToApply}} returns a vector.
#'
#' @keywords internal
getSegmentsToApply <- function(n_segment, segments) {
  o <- rep(FALSE, n_segment)
  o[segments] <- TRUE
  return(o)
}

#' (Internal) Initialize item usage matrix
#'
#' \code{\link{initializeUsageMatrix}} is an internal function for
#' creating a new item usage matrix. An item usage matrix is a
#' (\emph{nj}, \emph{nv}) matrix with \code{TRUE} or \code{FALSE} values.
#' The rows represent examinees, and the columns represent decision variables.
#'
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{initializeUsageMatrix}} returns a new item usage matrix.
#'
#' @keywords internal
initializeUsageMatrix <- function(simulation_constants) {

  if (!simulation_constants$group_by_stimulus) {
    o <- matrix(FALSE, nrow = simulation_constants$nj, ncol = simulation_constants$ni)
  }
  if (simulation_constants$group_by_stimulus) {
    o <- matrix(FALSE, nrow = simulation_constants$nj, ncol = simulation_constants$nv)
  }

  colnames(o) <- simulation_constants$id

  return(o)
}

#' (Internal) Update item usage matrix
#'
#' \code{\link{updateUsageMatrix}} is an internal function for
#' updating an item usage matrix. An item usage matrix is a
#' (\emph{nj}, \emph{nv}) matrix with \code{TRUE} or \code{FALSE} values.
#' The rows represent examinees, and the columns represent decision variables.
#'
#' @param usage_matrix a (\emph{nj}, \emph{nv}) matrix containing item usage.
#' @param j the examinee index.
#' @param x an \code{\linkS4class{output_Shadow}} object containing the examinee's simulation data.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{updateUsageMatrix}} returns an updated item usage matrix.
#'
#' @keywords internal
updateUsageMatrix <- function(usage_matrix, j, x, simulation_constants) {

  usage_matrix[j, x@administered_item_index] <- TRUE

  if (!simulation_constants$group_by_stimulus) {
    return(usage_matrix)
  }

  usage_matrix[j, simulation_constants$ni + x@administered_stimulus_index] <- TRUE

  return(usage_matrix)

}

#' (Internal) Aggregate item usage matrix into exposure rate table
#'
#' \code{\link{aggregateUsageMatrix}} is an internal function for
#' aggregating item usage matrix into an exposure rate table.
#'
#' @param usage_matrix a (\emph{nj}, \emph{nv}) matrix containing item usage.
#' @template parameter_simulation_constants
#' @param constraints a \code{\linkS4class{constraints}} object.
#'
#' @returns \code{\link{aggregateUsageMatrix}} returns a \code{\link{data.frame}} containing exposure rates.
#'
#' @keywords internal
aggregateUsageMatrix <- function(
  usage_matrix, simulation_constants, constraints
) {

  usage_matrix <- usage_matrix > 0

  if (!simulation_constants$group_by_stimulus) {
    o <- matrix(NA, simulation_constants$ni, 2)
    colnames(o) <- c("Item", "Item ER")
    o[, 1] <- 1:simulation_constants$ni
    o[, 2] <- apply(usage_matrix, 2, sum) / simulation_constants$nj
    return(o)
  }
  if (simulation_constants$group_by_stimulus) {
    o <- matrix(NA, simulation_constants$ni, 4)
    colnames(o) <- c("Item", "Stimulus", "Item ER", "Stimulus ER")
    x <- apply(usage_matrix, 2, sum) / simulation_constants$nj
    o[, 1] <- 1:simulation_constants$ni
    o[, 2] <- constraints@stimulus_index_by_item
    o[, 3] <- x[1:simulation_constants$ni]
    o[, 4] <- x[(simulation_constants$ni + 1):simulation_constants$nv][constraints@stimulus_index_by_item]
    return(o)
  }

}
