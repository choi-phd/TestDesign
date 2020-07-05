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
    if (toupper(config@interim_theta$prior_dist) == "NORMAL") {
      o$posterior <- matrix(dnorm(theta_grid, mean = config@interim_theta$prior_par[1], sd = config@interim_theta$prior_par[2]), nj, nq, byrow = TRUE)
    }
    if (toupper(config@interim_theta$prior_dist) == "UNIFORM") {
      o$posterior <- matrix(1, nj, nq)
    }
    if (is.null(o$posterior)) {
      stop("invalid configuration option for interim_theta$prior_dist")
    }
  }
  if (is.null(prior) && !is.null(prior_par)) {
    if (is.vector(prior_par) && length(prior_par) == 2) {
      o$posterior <- matrix(dnorm(theta_grid, mean = prior_par[1], sd = prior_par[2]), nj, nq, byrow = TRUE)
    }
    if (is.matrix(prior_par) && all(dim(prior_par) == c(nj, 2))) {
      o$posterior <- matrix(NA, nj, nq)
      for (j in 1:nj) {
        o$posterior[j, ] <- dnorm(theta_grid, mean = prior_par[j, 1], sd = prior_par[j, 2])
      }
    }
    if (is.null(o$posterior)) {
      stop("prior_par must be a vector of length 2, c(mean, sd), or a matrix of dim c(nj x 2)")
    }
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
