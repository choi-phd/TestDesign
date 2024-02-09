#' @include shadow_functions.R
NULL

#' @noRd
doExposureControl <- function(
  exposure_record, segment_record,
  o, j,
  current_theta,
  eligibility_flag,
  constants
) {

  if (!constants$use_exposure_control) {
    return(exposure_record)
  }

  segment_of                 <- getSegmentOf(o, constants)
  segment_record             <- updateSegmentRecord(segment_record, segment_of, j)
  eligibility_flag_in_final_theta_segment <- getEligibilityFlagInSegment(eligibility_flag, segment_of$final_theta_est, constants)

  if (constants$exposure_control_method %in% c("ELIGIBILITY")) {

    segments_to_apply <- getSegmentsToApply(constants$n_segment, segment_of$final_theta_est)
    exposure_record   <- applyFading(exposure_record, segments_to_apply, constants)
    segment_prob      <- 1
    segment_feasible  <- unique(o@theta_segment_index[o@shadow_test_feasible == TRUE])
    theta_is_feasible <- segment_of$final_theta_est %in% segment_feasible
    exposure_record   <- incrementN(exposure_record, segments_to_apply, segment_prob, constants)
    exposure_record   <- incrementPhi(exposure_record, segments_to_apply, segment_prob, theta_is_feasible)
    exposure_record   <- incrementAlpha(exposure_record, segments_to_apply, segment_prob, o, constants)
    exposure_record   <- incrementRho(exposure_record, segments_to_apply, segment_prob, eligibility_flag, theta_is_feasible, constants)
    exposure_record   <- adjustAlphaToReduceSpike(exposure_record, segment_prob, segment_of$visited, eligibility_flag_in_final_theta_segment, o, constants)
    exposure_record   <- updateEligibilityRates(exposure_record, constants)
    exposure_record   <- clipEligibilityRates(exposure_record, constants)
    return(exposure_record)

  }

  if (constants$exposure_control_method %in% c("BIGM")) {

    segments_to_apply <- getSegmentsToApply(constants$n_segment, segment_of$final_theta_est)
    exposure_record   <- applyFading(exposure_record, segments_to_apply, constants)
    segment_prob      <- 1
    exposure_record   <- incrementN(exposure_record, segments_to_apply, segment_prob, constants)
  # exposure_record   <- incrementPhi(exposure_record, segments_to_apply, segment_prob, TRUE) # is not called for the purpose of code optimization; see comments in incrementPhi()
    exposure_record   <- incrementAlpha(exposure_record, segments_to_apply, segment_prob, o, constants)
    exposure_record   <- incrementRho(exposure_record, segments_to_apply, segment_prob, eligibility_flag, TRUE, constants)
    exposure_record   <- adjustAlphaToReduceSpike(exposure_record, segment_prob, segment_of$visited, eligibility_flag_in_final_theta_segment, o, constants)
    exposure_record   <- updateEligibilityRates(exposure_record, constants)
    exposure_record   <- clipEligibilityRates(exposure_record, constants)
    return(exposure_record)

  }

  if (constants$exposure_control_method %in% c("BIGM-BAYESIAN")) {

    segments_to_apply <- getSegmentsToApply(constants$n_segment, 1:constants$n_segment)
    exposure_record   <- applyFading(exposure_record, segments_to_apply, constants)
    segment_prob      <- getSegmentProb(current_theta$posterior_sample, constants)
    exposure_record   <- incrementN(exposure_record, segments_to_apply, segment_prob, constants)
  # exposure_record   <- incrementPhi(exposure_record, segments_to_apply, segment_prob, TRUE) # is not called for the purpose of code optimization; see comments in incrementPhi()
    exposure_record   <- incrementAlpha(exposure_record, segments_to_apply, segment_prob, o, constants)
    exposure_record   <- incrementRho(exposure_record, segments_to_apply, segment_prob, eligibility_flag, TRUE, constants)
    exposure_record   <- adjustAlphaToReduceSpike(exposure_record, segment_prob[segment_of$final_theta_est], segment_of$visited, eligibility_flag_in_final_theta_segment, o, constants)
    exposure_record   <- updateEligibilityRates(exposure_record, constants)
    exposure_record   <- clipEligibilityRates(exposure_record, constants)
    return(exposure_record)

  }

}

#' @noRd
doExposureControlDetailed <- function(
  exposure_record_detailed,
  j,
  exposure_record,
  config,
  constants
) {

  if (!config@exposure_control$diagnostic_stats) {
    return(exposure_record_detailed)
  }

  exposure_record_detailed <- updateExposureRecordSegmentwise(
    exposure_record_detailed, j, exposure_record, constants
  )

}

#' @noRd
initializeSegmentRecord <- function(constants) {

  o <- list()

  if (!constants$use_exposure_control) {
    return(o)
  }

  o$freq_true  <- numeric(constants$n_segment)
  o$freq_est   <- numeric(constants$n_segment)
  o$count_true <- numeric(constants$nj)
  o$count_est  <- numeric(constants$nj)

  return(o)

}

#' @noRd
initializeExposureRecord <- function(exposure_control, constants) {

  # the j subscript used here is intentional and should not be removed.
  # the notation scheme is from van der Linden, W. J., & Veldkamp, B. P. (2007).
  # for example, a_ijk here is interpreted as:
  # - the number of examinees until j-th examinee who was in theta segment k and was administered item i.
  # - see how this interpretation does not imply the variable is a three-dimensional array.

  o <- list()

  ni <- constants$ni
  ns <- constants$ns
  n_segment <- constants$n_segment

  o$n_jk  <- numeric(n_segment)
  if (toupper(exposure_control$method) == "ELIGIBILITY") {
    o$f_jk  <- numeric(n_segment)
  }

  if (constants$fading_factor != 1) {
    o$n_jk_nofade  <- o$n_jk
  }

  o$a_ijk <- matrix(0, n_segment, ni)
  o$r_ijk <- matrix(0, n_segment, ni)
  o$p_e_i  <- matrix(1, n_segment, ni)
  if (constants$fading_factor != 1) {
    o$a_ijk_nofade <- o$a_ijk
    o$r_ijk_nofade <- o$r_ijk
  }

  if (!constants$group_by_stimulus) {
    return(o)
  }

  o$a_sjk <- matrix(0, n_segment, ns)
  o$r_sjk <- matrix(0, n_segment, ns)
  o$p_e_s  <- matrix(1, n_segment, ns)
  if (constants$fading_factor != 1) {
    o$a_sjk_nofade <- o$a_sjk
    o$r_sjk_nofade <- o$r_sjk
  }

  return(o)

}

#' @noRd
getInitialEligibilityStats <- function(o, initial_stats, constants) {

  o$n_jk  <- initial_stats$n_jk
  o$f_jk  <- initial_stats$f_jk
  o$a_ijk <- initial_stats$a_ijk
  o$r_ijk <- initial_stats$r_ijk

  if (!constants$group_by_stimulus) {
    return(o)
  }

  o$a_sjk <- initial_stats$a_sjk
  o$r_sjk <- initial_stats$r_sjk

  return(o)

}

#' @noRd
initializeExposureRecordSegmentwise <- function(constants) {

  o <- list()
  ni <- constants$ni
  ns <- constants$ns
  nj <- constants$nj
  n_segment     <- constants$n_segment
  fading_factor <- constants$fading_factor

  if (!constants$group_by_stimulus) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (constants$group_by_stimulus) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  if (fading_factor != 1 & !constants$group_by_stimulus) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (fading_factor != 1 & constants$group_by_stimulus) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  return(o)

}

#' @noRd
getSegmentOf <- function(x, constants) {

  o <- list()

  # find_segment() needs to be updated for multidimensional segments
  o$final_theta_est   <- find_segment(x@final_theta_est, constants$segment_cut)

  tmp                 <- sort(unique(x@theta_segment_index))
  o$visited           <- tmp[tmp != o$final_theta_est]

  if (is.null(x@true_theta)) {
    return(o)
  }

  # find_segment() needs to be updated for multidimensional segments
  o$true_theta        <- find_segment(x@true_theta, constants$segment_cut)
  return(o)

}

#' @noRd
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

#' @noRd
getSegmentsToApply <- function(n_segment, segments) {
  o <- rep(FALSE, n_segment)
  o[segments] <- TRUE
  return(o)
}

#' @noRd
updateExposureRecordSegmentwise <- function(o, j, x, constants) {

  n_segment     <- constants$n_segment
  fading_factor <- constants$fading_factor

  ni <- constants$ni
  for (g in 1:n_segment) {
    o$a_g_i[[g]][j, 1:ni] <- x$a_ijk[g, ]
    o$e_g_i[[g]][j, 1:ni] <- x$r_ijk[g, ]
    if (fading_factor != 1) {
      o$a_g_i_nofade[[g]][j, 1:ni] <- x$a_ijk_nofade[g, ]
      o$e_g_i_nofade[[g]][j, 1:ni] <- x$r_ijk_nofade[g, ]
    }
  }

  if (!constants$group_by_stimulus) {
    return(o)
  }

  ns <- constants$ns
  for (g in 1:n_segment) {
    o$a_g_s[[g]][j, 1:ns] <- x$a_sjk[g, ]
    o$e_g_s[[g]][j, 1:ns] <- x$r_sjk[g, ]
    if (fading_factor != 1) {
      o$a_g_s_nofade[[g]][j, 1:ns] <- x$a_sjk_nofade[g, ]
      o$e_g_s_nofade[[g]][j, 1:ns] <- x$r_sjk_nofade[g, ]
    }
  }

  return(o)

}

#' @noRd
initializeUsageMatrix <- function(constants) {

  if (!constants$group_by_stimulus) {
    o <- matrix(FALSE, nrow = constants$nj, ncol = constants$ni)
    return(o)
  }
  if (constants$group_by_stimulus) {
    o <- matrix(FALSE, nrow = constants$nj, ncol = constants$nv)
    return(o)
  }

}

#' @noRd
updateUsageMatrix <- function(usage_matrix, j, x, constants) {

  usage_matrix[j, x@administered_item_index] <- TRUE

  if (!constants$group_by_stimulus) {
    return(usage_matrix)
  }

  usage_matrix[j, constants$ni + x@administered_stimulus_index] <- TRUE

  return(usage_matrix)

}

#' @noRd
aggregateUsageMatrix <- function(usage_matrix, constants, constraints) {

  if (!constants$group_by_stimulus) {
    o <- matrix(NA, constants$ni, 2)
    colnames(o) <- c("Item", "Item ER")
    o[, 1] <- 1:constants$ni
    o[, 2] <- apply(usage_matrix, 2, sum) / constants$nj
    return(o)
  }
  if (constants$group_by_stimulus) {
    o <- matrix(NA, constants$ni, 4)
    colnames(o) <- c("Item", "Stimulus", "Item ER", "Stimulus ER")
    x <- apply(usage_matrix, 2, sum) / constants$nj
    o[, 1] <- 1:constants$ni
    o[, 2] <- constraints@stimulus_index_by_item
    o[, 3] <- x[1:constants$ni]
    o[, 4] <- x[(constants$ni + 1):constants$nv][constraints@stimulus_index_by_item]
    return(o)
  }

}
