#' @include shadow_functions.R
NULL

#' @noRd
parsePriorParameters <- function(config_theta, constants, prior_density_override, prior_par_override) {

  # override config with prior arguments supplied to Shadow()
  if (!is.null(prior_density_override) & !is.null(prior_par_override)) {
    stop("unexpected 'prior' and 'prior_par': only one must be supplied to Shadow()")
  }
  if (!is.null(prior_density_override)) {
    config_theta$prior_dist <- "RAW"
    config_theta$prior_par  <- prior_density_override
  }
  if (!is.null(prior_par_override)) {
    config_theta$prior_dist <- "NORMAL"
    config_theta$prior_par  <- prior_par_override
  }

  config_theta$prior_dist <- toupper(config_theta$prior_dist)

  # if prior_par is a vector, expand to an examinee-wise list
  if (
    is.vector(config_theta$prior_par) &
    !is.list(config_theta$prior_par) # because a list is also a vector
  ) {
    config_theta$prior_par <- lapply(1:constants$nj, function(x) config_theta$prior_par)
  }

  # if prior_par is a matrix, expand to an examinee-wise list
  if (is.matrix(config_theta$prior_par)) {
    config_theta$prior_par <- apply(config_theta$prior_par, 1, function(x) {x}, simplify = FALSE)
  }

  # if prior_par is a list, validate
  if (is.list(config_theta$prior_par)) {
    if (length(config_theta$prior_par) != constants$nj) {
      stop("unexpected 'prior_par': could not be expanded to a length-nj list (must be a length-2 vector, an nj * 2 matrix, or a length-nj list)")
    }
    n_prior_par <- unlist(lapply(config_theta$prior_par, length))
    if (config_theta$prior_dist == "NORMAL" & any(n_prior_par != 2)) {
      stop("unexpected 'prior_par': for 'NORMAL' distribution, each list element must be a length-2 vector")
    }
    if (config_theta$prior_dist == "UNIFORM" & any(n_prior_par != 2)) {
      stop("unexpected 'prior_par': for 'UNIFORM' distribution, each list element must be a length-2 vector")
    }
    if (config_theta$prior_dist == "RAW" & any(n_prior_par != constants$nq)) {
      stop("unexpected 'prior_par': for 'RAW' distribution, each list element must be a length-nq vector")
    }
  }

  return(config_theta)

}

#' @noRd
getBayesianConstants <- function(config, constants) {

  o <- list()

  interim_method <- toupper(config@interim_theta$method)
  final_method   <- toupper(config@final_theta$method)

  if (any(c(interim_method, final_method) %in% c("EB", "FB"))) {
    o$n_sample    <- config@MCMC$burn_in + config@MCMC$post_burn_in
    o$burn_in     <- config@MCMC$burn_in
    o$thin        <- config@MCMC$thin
    o$jump_factor <- config@MCMC$jump_factor
  }

  # generate prior densities from config
  o$final_theta_prior_densities <- generateDensityFromPriorPar(
    config@final_theta,
    constants$theta_q
  )

  return(o)

}

#' Generate item parameter samples using standard errors
#'
#' \code{\link{iparPosteriorSample}} is a function for generating item parameter samples.
#'
#' @param pool an \code{\linkS4class{item_pool}} object.
#' @param n_sample the number of samples to draw.
#'
#' @return \code{\link{iparPosteriorSample}} returns a length-\emph{ni} list of item parameter matrices, with each matrix having \code{n_sample} rows.
#'
#' @examples
#' ipar <- iparPosteriorSample(itempool_bayes, 5)
#'
#' @export
iparPosteriorSample <- function(pool, n_sample = 500) {

  requireNamespace("logitnorm")
  ipar_list <- vector(mode = "list", length = pool@ni)

  for (i in 1:pool@ni) {

    if (pool@model[i] == "item_1PL") {
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 1)
      ipar_list[[i]][, 1] <- rnorm(n_sample, pool@ipar[i, 1], pool@se[i, 1])
      next
    }
    if (pool@model[i] == "item_2PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 2)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])
      next
    }
    if (pool@model[i] == "item_3PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      c_hyp <- logitHyperPars(pool@ipar[i, 3], pool@se[i, 3])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 3)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])
      ipar_list[[i]][, 3] <- rlogitnorm(n_sample, mu = c_hyp[1], sigma = c_hyp[2])
      next
    }
    if (pool@model[i] == "item_PC") {
      ipar_list[[i]] <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k] <- rnorm(n_sample, pool@ipar[i, k], pool@se[i, k])
      }
      next
    }
    if (pool@model[i] == "item_GPC") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i])
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k + 1] <- rnorm(n_sample, pool@ipar[i, k + 1], pool@se[i, k + 1])
      }
      next
    }
    if (pool@model[i] == "item_GR") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i])
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k + 1] <- rnorm(n_sample, pool@ipar[i, k + 1], pool@se[i, k + 1])
      }
      for (s in 1:n_sample) {
        if (is.unsorted(ipar_list[[i]][s, 2:pool@NCAT[i]])) {
          ipar_list[[i]][s, 2:pool@NCAT[i]] <- sort(ipar_list[[i]][s, 2:pool@NCAT[i]])
        }
      }
      next
    }
  }
  return(ipar_list)
}

#' Convert mean and standard deviation into log-normal distribution parameters
#'
#' \code{\link{lnHyperPars}} is a function for calculating parameters for a log-normal distribution, such that the distribution yields desired mean and standard deviation.
#' Used for sampling the a-parameter.
#'
#' @param mean the desired mean.
#' @param sd the desired standard deviation.
#'
#' @return \code{\link{lnHyperPars}} returns two values. These can be directly supplied to \code{\link{rlnorm}}.
#'
#' @examples
#' pars <- lnHyperPars(2, 4)
#' x <- rlnorm(1000000, pars[1], pars[2])
#' mean(x) # close to 2
#' sd(x)   # close to 4
#'
#' @export
lnHyperPars <- function(mean, sd) {
  location <- log(mean^2 / sqrt(sd^2 + mean^2))
  scale    <- sqrt(log(1 + sd^2 / mean^2))
  return(c(location, scale))
}

#' Convert mean and standard deviation into logit-normal distribution parameters
#'
#' \code{\link{logitHyperPars}} is a function for calculating parameters for a logit-normal distribution, such that the distribution yields desired mean and standard deviation.
#' Used for sampling the c-parameter.
#'
#' @param mean the desired mean.
#' @param sd the desired standard deviation.
#'
#' @return \code{\link{logitHyperPars}} returns two values. These can be directly supplied to \code{\link[logitnorm]{rlogitnorm}}.
#'
#' @examples
#' pars <- logitHyperPars(0.2, 0.1)
#' x <- logitnorm::rlogitnorm(1000000, pars[1], pars[2])
#' mean(x) # close to 0.2
#' sd(x)   # close to 0.1
#'
#' @export
logitHyperPars <- function(mean, sd) {

  n_max <- 10000
  n     <- 0
  logit_samples <- numeric(n_max)

  while (n_max - n > 0) {
    norm_sample <- rnorm(n_max - n, mean, sd)
    idx <- (norm_sample >= 0) & (norm_sample <= 1)
    norm_sample <- norm_sample[idx]
    n_new <- n + length(norm_sample)
    if (length(norm_sample) > 0) {
      logit_samples[(n + 1):n_new] <- logitnorm::logit(norm_sample)
    }
    n <- n_new
  }

  return(c(mean(logit_samples), sd(logit_samples)))
}

#' @noRd
generateItemParameterSample <- function(config, item_pool, bayesian_constants) {

  o <- NULL

  interim_method <- toupper(config@interim_theta$method)
  final_method   <- toupper(config@final_theta$method)
  if (any(c(interim_method, final_method) %in% c("FB"))) {
    o <- iparPosteriorSample(item_pool, bayesian_constants$n_sample)
  }

  return(o)

}

#' @noRd
generateDensityFromPriorPar <- function(config_theta, theta_q) {

  nq <- nrow(theta_q)
  nj <- length(config_theta$prior_par) # this is an examinee-wise list
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
generateSampleFromPriorPar <- function(config_theta, j, bayesian_constants) {

  if (config_theta$prior_dist == "NORMAL") {
    prior_sample <- rnorm(
      bayesian_constants$n_sample,
      mean = config_theta$prior_par[[j]][1],
      sd   = config_theta$prior_par[[j]][2]
    )
    return(prior_sample)
  }
  if (config_theta$prior_dist == "UNIFORM") {
    prior_sample <- runif(
      bayesian_constants$n_sample,
      min = config_theta$prior_par[[j]][1],
      max = config_theta$prior_par[[j]][2]
    )
    return(prior_sample)
  }

}

#' @noRd
applyThin <- function(posterior_sample, bayesian_constants) {

  posterior_sample <- posterior_sample[seq(
    from = bayesian_constants$burn_in + 1,
    to   = bayesian_constants$n_sample,
    by   = bayesian_constants$thin
  )]

  return(posterior_sample)

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
