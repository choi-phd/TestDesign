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
