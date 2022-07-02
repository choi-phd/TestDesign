#' @include shadow_functions.R
NULL

#' @noRd
parsePriorParameters <- function(o, constants, prior_density_override, prior_par_override) {

  # override config with prior arguments supplied to Shadow()
  if (!is.null(prior_density_override) & !is.null(prior_par_override)) {
    stop("unexpected 'prior' and 'prior_par': only one must be supplied to Shadow()")
  }
  if (!is.null(prior_density_override)) {
    o$prior_dist <- "RAW"
    o$prior_par  <- prior_density_override
  }
  if (!is.null(prior_par_override)) {
    o$prior_dist <- "NORMAL"
    o$prior_par  <- prior_par_override
  }

  o$prior_dist <- toupper(o$prior_dist)

  # if prior_par is a vector, expand to an examinee-wise list
  if (is.vector(o$prior_par)) {
    o$prior_par <- lapply(1:constants$nj, function(x) o$prior_par)
  }

  # if prior_par is a matrix, expand to an examinee-wise list
  if (is.matrix(o$prior_par)) {
    o$prior_par <- apply(o$prior_par, 1, function(x) {x}, simplify = FALSE)
  }

  # if prior_par is a list, validate
  if (is.list(o$prior_par)) {
    if (length(o$prior_par) != constants$nj) {
      stop("unexpected 'prior_par': could not be expanded to a length-nj list (must be a length-2 vector, an nj * 2 matrix, or a length-nj list)")
    }
    n_prior_par <- unlist(lapply(o$prior_par, length))
    if (o$prior_dist == "NORMAL" & any(n_prior_par != 2)) {
      stop("unexpected 'prior_par': for 'NORMAL' distribution, each list element must be a length-2 vector")
    }
    if (o$prior_dist == "UNIFORM" & any(n_prior_par != 2)) {
      stop("unexpected 'prior_par': for 'UNIFORM' distribution, each list element must be a length-2 vector")
    }
    if (o$prior_dist == "RAW" & any(n_prior_par != constants$nq)) {
      stop("unexpected 'prior_par': for 'RAW' distribution, each list element must be a length-nq vector")
    }
  }

  return(o)

}

#' @noRd
initializePosterior <- function(config, constants, item_pool, posterior_constants) {

  o <- list()

  o$likelihood <- rep(1, constants$nq)
  o$posterior  <- NULL

  o$posterior <- generateDensityFromPriorPar(
    config@interim_theta,
    constants$theta_q,
    constants$nj
  )

  interim_method <- toupper(config@interim_theta$method)
  final_method   <- toupper(config@final_theta$method)
  if (any(c(interim_method, final_method) %in% c("FB"))) {
    o$ipar_list <- iparPosteriorSample(item_pool, posterior_constants$n_sample)
  }

  return(o)

}

#' @noRd
getPosteriorConstants <- function(config) {

  o <- list()

  interim_method <- toupper(config@interim_theta$method)
  final_method   <- toupper(config@final_theta$method)

  if (any(c(interim_method, final_method) %in% c("EB", "FB"))) {
    o$n_sample    <- config@MCMC$burn_in + config@MCMC$post_burn_in
    o$burn_in     <- config@MCMC$burn_in
    o$thin        <- config@MCMC$thin
    o$jump_factor <- config@MCMC$jump_factor
  }

  return(o)

}

#' @noRd
generateDensityFromPriorPar <- function(config_theta, theta_q, nj) {

  nq <- nrow(theta_q)
  prior_density <- NULL

  if (config_theta$prior_dist == "NORMAL") {
    prior_density <- matrix(NA, nj, nq, byrow = TRUE)
    for (j in 1:nj) {
      prior_density[j, ] <- dnorm(
        theta_q,
        mean = config_theta$prior_par[[j]][1],
        sd   = config_theta$prior_par[[j]][2]
      )
    }
    return(prior_density)
  }
  if (config_theta$prior_dist == "UNIFORM") {
    prior_density <- matrix(1, nj, nq, byrow = TRUE)
    for (j in 1:nj) {
      prior_density[j, ] <- dunif(
        theta_q,
        min = config_theta$prior_par[[j]][1],
        max = config_theta$prior_par[[j]][2]
      )
    }
    return(prior_density)
  }
  if (config_theta$prior_dist == "RAW") {
    prior_density <- matrix(NA, nj, nq, byrow = TRUE)
    for (j in 1:nj) {
      prior_density[j, 1:nq] <- config_theta$prior_par[[j]][1:nq]
    }
    return(prior_density)
  }

}

#' @noRd
parsePriorPar <- function(prior_par, nj, j, config_prior_par) {

  if (is.vector(prior_par) && length(prior_par) == 2) {
    return(prior_par)
  }
  if (is.matrix(prior_par) && all(dim(prior_par) == c(nj, 2))) {
    return(prior_par[j, ])
  }

  return(config_prior_par)

}

#' @noRd
applyThin <- function(posterior_sample, posterior_constants) {

  posterior_sample <- posterior_sample[seq(
    from = posterior_constants$burn_in + 1,
    to   = posterior_constants$n_sample,
    by   = posterior_constants$thin
  )]

  return(posterior_sample)

}

#' @noRd
getPriorSample <- function(arg_mean, arg_sd, posterior_constants) {

  prior_sample <- rnorm(
    posterior_constants$n_sample,
    mean = arg_mean,
    sd   = arg_sd
  )

  return(prior_sample)

}

#' @noRd
getSegmentProb <- function(posterior_sample, constants) {

  # find_segment() needs to be updated for multidimensional segments
  sample_segment                   <- find_segment(posterior_sample, constants$segment_cut)
  segment_distribution             <- table(sample_segment) / length(sample_segment)
  segment_classified               <- as.numeric(names(segment_distribution))
  segment_prob                     <- numeric(constants$n_segment)
  segment_prob[segment_classified] <- segment_distribution

  return(segment_prob)

}

#' @noRd
updatePosterior <- function(posterior_record, j, prob_resp) {

  posterior_record$posterior[j, ] <-
  posterior_record$posterior[j, ] * prob_resp
  posterior_record$likelihood     <-
  posterior_record$likelihood     * prob_resp

  return(posterior_record)

}
