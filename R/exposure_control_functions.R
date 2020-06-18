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
  o$p_jk  <- numeric(n_segment)
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
initializeExposureRecordSegmentwise <- function(exposure_constants, constants) {

  o <- list()
  ni <- constants$ni
  ns <- constants$ns
  nj <- constants$nj
  n_segment     <- exposure_constants$n_segment
  fading_factor <- exposure_constants$fading_factor

  if (!constants$set_based) {
    o$a_g_i <- matrix(0, nrow = nj, ncol = n_segment * ni)
    o$e_g_i <- matrix(0, nrow = nj, ncol = n_segment * ni)
  }
  if (constants$set_based) {
    o$a_g_i <- matrix(0, nrow = nj, ncol = n_segment * ni)
    o$e_g_i <- matrix(0, nrow = nj, ncol = n_segment * ni)
    o$a_g_s <- matrix(0, nrow = nj, ncol = n_segment * ns)
    o$e_g_s <- matrix(0, nrow = nj, ncol = n_segment * ns)
  }

  if (fading_factor != 1 & !constants$set_based) {
    o$a_g_i_nofade <- matrix(0, nrow = nj, ncol = n_segment * ni)
    o$e_g_i_nofade <- matrix(0, nrow = nj, ncol = n_segment * ni)
  }
  if (fading_factor != 1 & constants$set_based) {
    o$a_g_i_nofade <- matrix(0, nrow = nj, ncol = n_segment * ni)
    o$e_g_i_nofade <- matrix(0, nrow = nj, ncol = n_segment * ni)
    o$a_g_s_nofade <- matrix(0, nrow = nj, ncol = n_segment * ns)
    o$e_g_s_nofade <- matrix(0, nrow = nj, ncol = n_segment * ns)
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
