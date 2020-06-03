#' @include shadow_functions.R
NULL

#' @noRd
initializePosterior <- function(prior, prior_par, config, constants, pool) {

  theta_grid <- constants$theta_q
  nj         <- constants$nj
  nq         <- constants$nq

  o <- list()

  o$likelihood <- rep(1, nq)
  o$posterior  <- NULL

  if (is.null(prior) && is.null(prior_par)) {
    o$posterior <- generateDistributionFromPriorPar(
      toupper(config@interim_theta$prior_dist),
      config@interim_theta$prior_par,
      theta_grid, nj
    )
  }
  if (is.null(prior) && !is.null(prior_par)) {
    o$posterior <- generateDistributionFromPriorPar(
      "NORMAL",
      prior_par,
      theta_grid, nj
    )
  }
  if (is.vector(prior) && length(prior) == nq) {
    o$posterior <- matrix(prior, nj, nq, byrow = TRUE)
  }
  if (is.matrix(prior) && all(dim(prior) == c(nj, nq))) {
    o$posterior <- prior
  }
  if (is.null(o$posterior)) {
    stop("unrecognized 'prior': must be a vector of length nq, or a nj * nq matrix")
  }

  interim_method <- toupper(config@interim_theta$method)
  final_method   <- toupper(config@final_theta$method)
  if (any(c(interim_method, final_method) %in% c("EB", "FB"))) {
    o$n_sample  <- config@MCMC$burn_in + config@MCMC$post_burn_in
  }
  if (any(c(interim_method, final_method) %in% c("FB"))) {
    o$ipar_list <- iparPosteriorSample(pool, o$n_sample)
  }

  return(o)

}

#' @noRd
generateDistributionFromPriorPar <- function(dist_type, prior_par, theta_grid, nj) {

  nq <- length(theta_grid)
  m  <- NULL

  if (dist_type == "NORMAL" && is.vector(prior_par) && length(prior_par) == 2) {
    x <- dnorm(theta_grid, mean = prior_par[1], sd = prior_par[2])
    m <- matrix(x, nj, nq, byrow = TRUE)
  }
  if (dist_type == "NORMAL" && is.matrix(prior_par) && all(dim(prior_par) == c(nj, 2))) {
    m <- matrix(NA, nj, nq, byrow = TRUE)
    for (j in 1:nj) {
      m[j, ] <- dnorm(theta_grid, mean = prior_par[j, 1], sd = prior_par[j, 2])
    }
  }
  if (dist_type == "UNIFORM") {
    x <- 1
    m <- matrix(x, nj, nq, byrow = TRUE)
  }
  if (is.null(m)) {
    stop("unrecognized 'prior_par': must be a vector c(mean, sd), or a nj * 2 matrix")
  }

  return(m)

}