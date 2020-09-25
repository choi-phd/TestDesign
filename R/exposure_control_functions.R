#' @include shadow_functions.R
NULL

#' @noRd
getExposureConstants <- function(exposure_control) {

  o <- list()

  if (toupper(exposure_control$method) %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
    o$use_eligibility_control <- TRUE
  } else {
    o$use_eligibility_control <- FALSE
  }

  o$max_exposure_rate   <- exposure_control$max_exposure_rate
  o$fading_factor       <- exposure_control$fading_factor
  o$acceleration_factor <- exposure_control$acceleration_factor
  o$n_segment           <- exposure_control$n_segment
  o$segment_cut         <- exposure_control$segment_cut
  o$cut_lower           <- o$segment_cut[(1:o$n_segment)]
  o$cut_upper           <- o$segment_cut[(1:o$n_segment) + 1]

  if (!length(o$max_exposure_rate) %in% c(1, o$n_segment)) {
    stop("length(max_exposure_rate) must be 1 or n_segment")
  }

  return(o)

}

#' @noRd
initializeSegmentRecord <- function(exposure_constants, constants) {

  o <- list()

  if (!exposure_constants$use_eligibility_control) {
    return(o)
  }

  o$freq_true  <- numeric(exposure_constants$n_segment)
  o$freq_est   <- numeric(exposure_constants$n_segment)
  o$count_true <- numeric(constants$nj)
  o$count_est  <- numeric(constants$nj)

  return(o)

}

#' @noRd
initializeExposureRecord <- function(exposure_control, exposure_constants, constants) {

  o <- list()

  ni <- constants$ni
  ns <- constants$ns
  n_segment <- exposure_constants$n_segment

  o$n_jk  <- numeric(n_segment)
  if (toupper(exposure_control$method) == "ELIGIBILITY") {
    o$p_jk  <- numeric(n_segment)
  }

  if (exposure_constants$fading_factor != 1) {
    o$n_jk_nofade  <- o$n_jk
  }

  o$a_ijk <- matrix(0, n_segment, ni)
  o$r_ijk <- matrix(0, n_segment, ni)
  o$pe_i  <- matrix(1, n_segment, ni)
  if (exposure_constants$fading_factor != 1) {
    o$a_ijk_nofade <- o$a_ijk
    o$r_ijk_nofade <- o$r_ijk
  }

  if (!constants$set_based) {
    return(o)
  }

  o$a_sjk <- matrix(0, n_segment, ns)
  o$r_sjk <- matrix(0, n_segment, ns)
  o$pe_s  <- matrix(1, n_segment, ns)
  if (exposure_constants$fading_factor != 1) {
    o$a_sjk_nofade <- o$a_sjk
    o$r_sjk_nofade <- o$r_sjk
  }

  return(o)

}

#' @noRd
getInitialEligibilityStats <- function(o, initial_stats, constants) {

  o$n_jk  <- initial_stats$n_jk
  o$p_jk  <- initial_stats$p_jk
  o$a_ijk <- initial_stats$a_ijk
  o$r_ijk <- initial_stats$r_ijk

  if (!constants$set_based) {
    return(o)
  }

  o$a_sjk <- initial_stats$a_sjk
  o$r_sjk <- initial_stats$r_sjk

  return(o)

}

#' @noRd
initializeExposureRecordSegmentwise <- function(exposure_constants, constants) {

  o <- list()
  ni <- constants$ni
  ns <- constants$ns
  nj <- constants$nj
  n_segment     <- exposure_constants$n_segment
  fading_factor <- exposure_constants$fading_factor

  if (!constants$set_based) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (constants$set_based) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  if (fading_factor != 1 & !constants$set_based) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (fading_factor != 1 & constants$set_based) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  return(o)

}

#' @noRd
getSegmentOf <- function(x, exposure_constants) {

  o <- list()

  o$final_theta_est   <- find_segment(x@final_theta_est, exposure_constants$segment_cut)

  tmp                 <- sort(unique(x@theta_segment_index))
  o$visited           <- tmp[tmp != o$final_theta_est]

  if (is.null(x@true_theta)) {
    return(o)
  }

  o$true_theta        <- find_segment(x@true_theta, exposure_constants$segment_cut)
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
updateExposureRecordSegmentwise <- function(o, j, x, exposure_constants, constants) {

  n_segment     <- exposure_constants$n_segment
  fading_factor <- exposure_constants$fading_factor

  ni <- constants$ni
  for (g in 1:n_segment) {
    o$a_g_i[[g]][j, 1:ni] <- x$a_ijk[g, ]
    o$e_g_i[[g]][j, 1:ni] <- x$r_ijk[g, ]
    if (fading_factor != 1) {
      o$a_g_i_nofade[[g]][j, 1:ni] <- x$a_ijk_nofade[g, ]
      o$e_g_i_nofade[[g]][j, 1:ni] <- x$r_ijk_nofade[g, ]
    }
  }

  if (!constants$set_based) {
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
