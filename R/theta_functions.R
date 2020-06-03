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
estimateInitialTheta <- function(config, initial_theta, prior_par, nj, j, posterior_record) {

  o <- list()
  theta_method <- toupper(config@interim_theta$method)
  if (theta_method %in% c("EAP", "MLE")) {
    o$theta <- initial_theta[j]
  }
  if (theta_method %in% c("EB", "FB")) {
    o$prior_par <- parsePriorPar(
      prior_par, nj, j, config@interim_theta$prior_par
    )
    o$posterior_sample <- getPosteriorSample(
      posterior_record$n_sample,
      o$prior_par[1], o$prior_par[2],
      config@MCMC
    )
    o$theta <- mean(o$posterior_sample)
    o$se    <- sd(o$posterior_sample) * config@MCMC$jump_factor
  }
  return(o)

}
