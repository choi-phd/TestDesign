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
getInitialThetaPrior <- function(config_theta, prior_par, nj, j, posterior_constants) {

  o <- list()

  o$prior_par <- parsePriorPar(
    prior_par, nj, j, config_theta$prior_par
  )

  o$posterior_sample <- getPriorSample(
    o$prior_par[1], o$prior_par[2],
    posterior_constants
  )

  o$posterior_sample <- applyThin(o$posterior_sample, posterior_constants)
  o$theta            <- mean(o$posterior_sample)
  o$se               <- sd(o$posterior_sample) * posterior_constants$jump_factor

  return(o)

}

#' @noRd
estimateInitialTheta <- function(config_theta, initial_theta, prior_par, nj, j, posterior_constants) {

  o <- list()
  theta_method <- toupper(config_theta$method)
  if (theta_method %in% c("EAP", "MLE")) {
    o$theta <- initial_theta[j]
  }
  if (theta_method %in% c("EB", "FB")) {
    o <- getInitialThetaPrior(config_theta, prior_par, nj, j, posterior_constants)
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
    segment_sample       <- find_segment(current_theta$posterior_sample, segment_cut)
    segment_distribution <- table(segment_sample) / length(segment_sample)
    segment_classified   <- as.numeric(names(segment_distribution))
    segment_prob         <- numeric(n_segment)
    segment_prob[segment_classified] <- segment_distribution
    segment <- which.max(segment_prob)
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
