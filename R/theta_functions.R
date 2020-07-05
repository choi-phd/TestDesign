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
