#' @include shadow_functions.R
NULL

#' @noRd
initializeTheta <- function(config, constants, posterior_record) {
  nj <- constants$nj
  if (!is.null(config@item_selection$initial_theta)) {
    theta <- rep(config@item_selection$initial_theta, nj)
  } else {
    theta <- as.vector(posterior_record$posterior %*% matrix(constants$theta_q, ncol = 1))
  }
  return(theta)
}

#' @noRd
estimateInitialTheta <- function(config_theta, initial_theta, prior_par, nj, j, posterior_record, MCMC) {

  o <- list()
  theta_method <- toupper(config_theta$method)
  if (theta_method %in% c("EAP", "MLE")) {
    o$theta <- initial_theta[j]
  }
  if (theta_method %in% c("EB", "FB")) {
    o$prior_par <- parsePriorPar(
      prior_par, nj, j, config_theta$prior_par
    )
    o$posterior_sample <- getPosteriorSample(
      posterior_record$n_sample,
      o$prior_par[1], o$prior_par[2],
      MCMC
    )
    o$theta <- mean(o$posterior_sample)
    o$se    <- sd(o$posterior_sample) * MCMC$jump_factor
  }
  return(o)

}

#' @noRd
getThetaSegment <- function(current_theta, position, exposure_control, exposure_constants) {

  exposure_control_method <- toupper(exposure_control$method)
  n_segment   <- exposure_constants$n_segment
  segment_cut <- exposure_constants$segment_cut

  if (exposure_control_method %in% c("ELIGIBILITY", "BIGM")) {
    if (isFirstSegmentValid(exposure_control$first_segment, n_segment, position)) {
      segment <- exposure_control$first_segment[position]
      return(segment)
    } else {
      segment <- find_segment(current_theta$theta, segment_cut)
      return(segment)
    }
  }

  if (exposure_control_method %in% c("BIGM-BAYESIAN")) {
    segment_prob <- getSegmentProb(current_theta$posterior_sample, exposure_constants)
    segment      <- which.max(segment_prob)
    return(segment)
  }

}

#' @noRd
isFirstSegmentValid <- function(first_segment, n_segment, position) {
  if (
    !is.null(first_segment) &&
    all(first_segment %in% 1:n_segment) &&
    length(first_segment) >= position) {
    return(TRUE)
  }
  return(FALSE)
}

#' @noRd
estimateThetaEAP <- function(posterior, theta_grid) {
  o <- list()
  o$theta <- sum(posterior * theta_grid) / sum(posterior)
  o$se    <- sqrt(sum(posterior * (theta_grid - o$theta)^2) / sum(posterior))
  return(o)
}

#' @noRd
applyShrinkageCorrection <- function(EAP, config_theta) {

  if (toupper(config_theta$prior_dist) == "NORMAL" && config_theta$shrinkage_correction) {
    prior_sd <- config_theta$prior_sd
    o        <- list()
    o$theta  <- EAP$theta * (1 + (EAP$se ** 2))
    o$se     <- EAP$se
    if (o$se < prior_sd) {
      o$se <- 1 / sqrt((o$se ** -2) - (prior_sd ** -2))
    }
    return(o)
  }

  return(EAP)

}
