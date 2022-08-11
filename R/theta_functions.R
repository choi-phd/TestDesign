#' @include shadow_functions.R
NULL

#' @noRd
estimateInterimTheta <- function(
  o, j, position,
  current_theta,
  augmented_posterior_record, posterior_record,
  augmented_item_pool, item_pool, model,
  augmented_item_index,
  augmented_item_resp,
  include_items_for_estimation,
  config,
  constants,
  posterior_constants
) {

  if (constants$use_hand_scored) {
    if (
      constants$item_is_hand_scored[o@administered_item_index[position]] &
      position > 1
    ) {
      # skip update and reuse previous interim theta estimate
      o@interim_theta_est[position, ] <- o@interim_theta_est[position - 1, ]
      o@interim_se_est[position, ]    <- o@interim_se_est[position - 1, ]
      return(o)
    }
    if (
      constants$item_is_hand_scored[o@administered_item_index[position]] &
      position == 1
    ) {
      # skip update and reuse previous interim theta estimate (starting theta)
      o@interim_theta_est[position, ] <- o@initial_theta_est
      o@interim_se_est[position, ]    <- NA
      return(o)
    }
  }

  if (toupper(config@interim_theta$method) == "EAP") {

    interim_EAP <- computeEAPFromPosterior(augmented_posterior_record$posterior[j, ], constants$theta_q)
    interim_EAP <- applyShrinkageCorrection(interim_EAP, config@interim_theta, j)
    o@interim_theta_est[position, ] <- interim_EAP$theta
    o@interim_se_est[position, ]    <- interim_EAP$se

    return(o)

  }

  if (toupper(config@interim_theta$method) == "MLE") {

    interim_EAP <- computeEAPFromPosterior(augmented_posterior_record$posterior[j, ], constants$theta_q)
    interim_MLE <- mle(augmented_item_pool,
      select        = augmented_item_index,
      resp          = augmented_item_resp,
      start_theta   = interim_EAP$theta,
      max_iter      = config@interim_theta$max_iter,
      crit          = config@interim_theta$crit,
      theta_range   = config@interim_theta$bound_ML,
      truncate      = config@interim_theta$truncate_ML,
      max_change    = config@interim_theta$max_change,
      use_step_size = config@interim_theta$use_step_size,
      step_size     = config@interim_theta$step_size,
      do_Fisher     = config@interim_theta$do_Fisher
    )

    o@interim_theta_est[position, ] <- interim_MLE$th
    o@interim_se_est[position, ]    <- interim_MLE$se

    return(o)

  }

  if (toupper(config@interim_theta$method) == "MLEF") {

    interim_EAP <- computeEAPFromPosterior(augmented_posterior_record$posterior[j, ], constants$theta_q)
    interim_MLEF <- mlef(augmented_item_pool,
      select           = augmented_item_index,
      resp             = augmented_item_resp,
      fence_slope      = config@interim_theta$fence_slope,
      fence_difficulty = config@interim_theta$fence_difficulty,
      start_theta      = interim_EAP$theta,
      max_iter         = config@interim_theta$max_iter,
      crit             = config@interim_theta$crit,
      theta_range      = config@interim_theta$bound_ML,
      truncate         = config@interim_theta$truncate_ML,
      max_change       = config@interim_theta$max_change,
      use_step_size    = config@interim_theta$use_step_size,
      step_size        = config@interim_theta$step_size,
      do_Fisher        = config@interim_theta$do_Fisher
    )

    o@interim_theta_est[position, ] <- interim_MLEF$th
    o@interim_se_est[position, ]    <- interim_MLEF$se

    return(o)

  }

  if (toupper(config@interim_theta$method) == "EB") {

    # TODO: needs to work with include_items_for_estimation
    if (!is.null(include_items_for_estimation)) {
      stop("EB with include_items_for_estimation is not available")
    }

    current_item <- o@administered_item_index[position]

    interim_EB <- theta_EB_single(
      posterior_constants$n_sample, current_theta$theta, current_theta$se,
      item_pool@ipar[current_item, ],
      o@administered_item_resp[position], item_pool@NCAT[current_item],
      model[current_item], 1, c(current_theta$theta, current_theta$se)
    )[, 1]

    interim_EB <- applyThin(interim_EB, posterior_constants)

    o@posterior_sample <- interim_EB
    o@interim_theta_est[position, ] <- mean(interim_EB)
    o@interim_se_est[position, ]    <- sd(interim_EB)

    return(o)

  }

  if (toupper(config@interim_theta$method) == "FB") {

    # TODO: needs to work with include_items_for_estimation
    if (!is.null(include_items_for_estimation)) {
      stop("FB with include_items_for_estimation is not available")
    }

    current_item <- o@administered_item_index[position]

    interim_FB <- theta_FB_single(
      posterior_constants$n_sample, current_theta$theta, current_theta$se,
      posterior_record$ipar_list[[current_item]],
      item_pool@ipar[current_item, ],
      o@administered_item_resp[position], item_pool@NCAT[current_item],
      model[current_item], 1, c(current_theta$theta, current_theta$se)
    )[, 1]

    interim_FB <- applyThin(interim_FB, posterior_constants)

    o@posterior_sample <- interim_FB
    o@interim_theta_est[position, ] <- mean(interim_FB)
    o@interim_se_est[position, ]    <- sd(interim_FB)

    return(o)

  }

}

#' @noRd
estimateFinalTheta <- function(
  o, j, position,
  augmented_item_pool, item_pool, model,
  augment_item_index,
  augment_item_resp,
  include_items_for_estimation,
  posterior_record,
  config,
  constants,
  posterior_constants
) {

  if (identical(config@final_theta, config@interim_theta)) {

    # Skip final theta estimation if methods are identical

    o@final_theta_est <- o@interim_theta_est[position, ]
    o@final_se_est    <- o@interim_se_est[position, ]

    return(o)

  }

  if (toupper(config@final_theta$method == "EAP")) {

    final_prior <- generateDensityFromPriorPar(
      config@final_theta,
      constants$theta_q,
      nj = 1
    )[1, ]

    o@posterior       <- o@likelihood * final_prior
    final_EAP <- computeEAPFromPosterior(o@posterior, constants$theta_q)
    final_EAP <- applyShrinkageCorrection(final_EAP, config@final_theta, j)
    o@final_theta_est <- final_EAP$theta
    o@final_se_est    <- final_EAP$se

    return(o)

  }

  if (toupper(config@final_theta$method) == "MLE") {

    if (!is.null(include_items_for_estimation)) {

      final_MLE <- mle(
        augmented_item_pool,
        select        = c(augment_item_index, o@administered_item_index[1:constants$max_ni]),
        resp          = c(augment_item_resp,  o@administered_item_resp[1:constants$max_ni]),
        start_theta   = o@interim_theta_est[constants$max_ni, ],
        max_iter      = config@final_theta$max_iter,
        crit          = config@final_theta$crit,
        theta_range   = config@final_theta$bound_ML,
        truncate      = config@final_theta$truncate_ML,
        max_change    = config@final_theta$max_change,
        use_step_size = config@final_theta$use_step_size,
        step_size     = config@final_theta$step_size,
        do_Fisher     = config@final_theta$do_Fisher
      )

    }

    if (is.null(include_items_for_estimation)) {

      final_MLE <- mle(
        item_pool,
        select        = o@administered_item_index[1:constants$max_ni],
        resp          = o@administered_item_resp[1:constants$max_ni],
        start_theta   = o@interim_theta_est[constants$max_ni, ],
        max_iter      = config@final_theta$max_iter,
        crit          = config@final_theta$crit,
        theta_range   = config@final_theta$bound_ML,
        truncate      = config@final_theta$truncate_ML,
        max_change    = config@final_theta$max_change,
        use_step_size = config@final_theta$use_step_size,
        step_size     = config@final_theta$step_size,
        do_Fisher     = config@final_theta$do_Fisher
      )

    }

    o@final_theta_est <- final_MLE$th
    o@final_se_est    <- final_MLE$se

    return(o)

  }

  if (toupper(config@final_theta$method) == "MLEF") {

    if (!is.null(include_items_for_estimation)) {

      final_MLEF <- mlef(
        augmented_item_pool,
        select           = c(augment_item_index, o@administered_item_index[1:constants$max_ni]),
        resp             = c(augment_item_resp,  o@administered_item_resp[1:constants$max_ni]),
        fence_slope      = config@final_theta$fence_slope,
        fence_difficulty = config@final_theta$fence_difficulty,
        start_theta      = o@interim_theta_est[constants$max_ni, ],
        max_iter         = config@final_theta$max_iter,
        crit             = config@final_theta$crit,
        theta_range      = config@final_theta$bound_ML,
        truncate         = config@final_theta$truncate_ML,
        max_change       = config@final_theta$max_change,
        use_step_size    = config@final_theta$use_step_size,
        step_size        = config@final_theta$step_size,
        do_Fisher        = config@final_theta$do_Fisher
      )

    }

    if (is.null(include_items_for_estimation)) {

      final_MLEF <- mlef(
        item_pool,
        select           = o@administered_item_index[1:constants$max_ni],
        resp             = o@administered_item_resp[1:constants$max_ni],
        fence_slope      = config@final_theta$fence_slope,
        fence_difficulty = config@final_theta$fence_difficulty,
        start_theta      = o@interim_theta_est[constants$max_ni, ],
        max_iter         = config@final_theta$max_iter,
        crit             = config@final_theta$crit,
        theta_range      = config@final_theta$bound_ML,
        truncate         = config@final_theta$truncate_ML,
        max_change       = config@final_theta$max_change,
        use_step_size    = config@final_theta$use_step_size,
        step_size        = config@final_theta$step_size,
        do_Fisher        = config@final_theta$do_Fisher
      )

    }

    o@final_theta_est <- final_MLEF$th
    o@final_se_est    <- final_MLEF$se

    return(o)

  }

  if (toupper(config@final_theta$method) == "EB") {

    final_prior <- getInitialThetaPrior(
      config@final_theta,
      j,
      posterior_constants
    )

    final_EB <- theta_EB(
      posterior_constants$n_sample, final_prior$theta, final_prior$se,
      item_pool@ipar[o@administered_item_index[1:position], ],
      o@administered_item_resp[1:position], item_pool@NCAT[o@administered_item_index[1:position]],
      model[o@administered_item_index[1:position]], 1, c(final_prior$theta, final_prior$se)
    )

    final_EB           <- applyThin(final_EB, posterior_constants)

    o@prior_par        <- config@final_theta$prior_par[[j]]
    o@posterior_sample <- final_EB
    o@final_theta_est  <- mean(final_EB)
    o@final_se_est     <- sd(final_EB)

    return(o)

  }

  if (toupper(config@final_theta$method) == "FB") {

    final_prior <- getInitialThetaPrior(
      config@final_theta,
      j,
      posterior_constants
    )

    final_FB <- theta_FB(
      posterior_constants$n_sample, final_prior$theta, final_prior$se,
      posterior_record$ipar_list[o@administered_item_index[1:position]],
      item_pool@ipar[o@administered_item_index[1:position], ],
      o@administered_item_resp[1:position], item_pool@NCAT[o@administered_item_index[1:position]],
      model[o@administered_item_index[1:position]], 1, c(final_prior$theta, final_prior$se)
    )

    final_FB           <- applyThin(final_FB, posterior_constants)

    o@prior_par        <- config@final_theta$prior_par[[j]]
    o@posterior_sample <- final_FB
    o@final_theta_est  <- mean(final_FB)
    o@final_se_est     <- sd(final_FB)

    return(o)

  }

}

#' Compute maximum likelihood estimates of theta
#'
#' \code{\link{mle}} is a function to compute maximum likelihood estimates of theta.
#'
#' @param object an \code{\linkS4class{item_pool}} object.
#' @param select (optional) if item indices are supplied, only the specified items are used.
#' @param resp item response on all (or selected) items in the \code{object} argument. Can be a vector, a matrix, or a data frame. \code{length(resp)} or \code{ncol(resp)} must be equal to the number of all (or selected) items.
#' @param start_theta (optional) initial theta values. If not supplied, EAP estimates using uniform priors are used as initial values. Uniform priors are computed using the \code{theta_range} argument below, with increments of \code{.1}.
#' @param max_iter maximum number of iterations. (default = \code{100})
#' @param crit convergence criterion to use. (default = \code{0.001})
#' @param truncate set \code{TRUE} to impose a bound using \code{theta_range} on the estimate. (default = \code{FALSE})
#' @param theta_range a range of theta values to bound the estimate. Only effective when \code{truncate} is \code{TRUE}. (default = \code{c(-4, 4)})
#' @param max_change upper bound to impose on the absolute change in theta between iterations. Absolute changes exceeding this value will be capped to \code{max_change}. (default = \code{1.0})
#' @param use_step_size set \code{TRUE} to use \code{step_size}. (default = \code{FALSE})
#' @param step_size upper bound to impose on the absolute change in initial theta and estimated theta. Absolute changes exceeding this value will be capped to \code{step_size}. (default = \code{0.5})
#' @param do_Fisher set \code{TRUE} to use Fisher scoring instead of Newton-Raphson method. (default = \code{TRUE})
#'
#' @return \code{\link{mle}} returns a list containing estimated values.
#'
#' \itemize{
#'   \item{\code{th}} theta value.
#'   \item{\code{se}} standard error.
#'   \item{\code{conv}} \code{TRUE} if estimation converged.
#'   \item{\code{trunc}} \code{TRUE} if truncation was applied on \code{th}.
#' }
#'
#' @docType methods
#' @rdname mle-methods
#' @examples
#' mle(itempool_fatigue, resp = resp_fatigue_data[10, ])
#' mle(itempool_fatigue, select = 1:20, resp = resp_fatigue_data[10, 1:20])
#' @export
setGeneric(
  name = "mle",
  def = function(object, select = NULL, resp, start_theta = NULL, max_iter = 100, crit = 0.001, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, use_step_size = FALSE, step_size = 0.5, do_Fisher = TRUE) {
    standardGeneric("mle")
  }
)

#' @docType methods
#' @rdname mle-methods
setMethod(
  f = "mle",
  signature = "item_pool",
  definition = function(object, select = NULL, resp, start_theta = NULL, max_iter = 50, crit = 0.005, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, use_step_size = FALSE, step_size = 0.5, do_Fisher = TRUE) {

    ni         <- object@ni
    theta_grid <- seq(min(theta_range), max(theta_range), .1)
    nq         <- length(theta_grid)

    if (is.vector(resp)) {
      nj <- 1
      resp <- matrix(resp, 1)
    } else if (is.matrix(resp)) {
      nj <- nrow(resp)
    } else if (is.data.frame(resp)) {
      nj <- nrow(resp)
      resp <- as.matrix(resp)
    } else {
      stop("'resp' must be a vector, a matrix, or a data.frame")
    }

    if (!is.null(select)) {
      if (!all(select %in% 1:ni)) {
        stop("'select' contains invalid indices not present in item pool")
      }
      items <- select
    } else {
      items <- 1:ni
    }

    if (ncol(resp) != length(items)) {
      stop("ncol(resp) or length(resp) must be equal to the number of all (or selected) items")
    }

    if (anyDuplicated(select) > 0) {
      warning("'select' contains duplicate item indices. Removed duplicates from 'select' and 'resp'")
      select <- select[-duplicated(select)]
      resp   <- resp[-duplicated(select)]
    }

    if (is.null(start_theta)) {
      start_theta <- eap(object,
        select     = select,
        resp       = resp,
        theta_grid = theta_grid,
        prior      = rep(1 / nq, nq)
      )
      start_theta <- start_theta$th
    } else if (length(start_theta) == 1) {
      start_theta <- rep(start_theta, nj)
    } else if (length(start_theta) != nj) {
      stop("'start_theta' must be a single value or have a value for each examinee.")
    }

    th    <- numeric(nj)
    se    <- numeric(nj)
    conv  <- logical(nj)
    trunc <- logical(nj)

    for (j in 1:nj) {
      theta_1 <- start_theta[j]
      max_raw_score <- sum(object@NCAT[items[!is.na(resp[j, ])]] - 1)
      raw_score <- sum(resp[j, ], na.rm = TRUE)
      if (raw_score > 0 && raw_score < max_raw_score) {
        converged <- FALSE
        done <- FALSE
        iteration <- 0
        while (!converged && !done && iteration <= max_iter) {
          iteration <- iteration + 1
          theta_0 <- theta_1
          deriv1 <- 0
          deriv2 <- 0
          for (i in 1:length(items)) {
            if (!is.na(resp[j, i])) {
              deriv1 <- deriv1 + calcJacobian(object@parms[[items[i]]], theta_0, resp[j, i])
              if (do_Fisher) {
                deriv2 <- deriv2 + calcFisher(object@parms[[items[i]]], theta_0)[, 1]
              } else {
                deriv2 <- deriv2 - calcHessian(object@parms[[items[i]]], theta_0, resp[j, i])
              }
            }
          }
          change <- deriv1 / deriv2
          if (is.nan(change)) {
            done <- TRUE
          } else {
            if (abs(change) > max_change) {
              change <- sign(change) * max_change
            } else if (abs(change) < crit) {
              converged <- conv[j] <- TRUE
            }
            theta_1 <- theta_0 + change
          }
        }
      }
      if (conv[j]) {
        th[j] <- theta_1
        se[j] <- 1 / sqrt(abs(deriv2))
      } else {
        th[j] <- start_theta[j]
        sum_fisher <- 0
        for (i in 1:length(items)) {
          sum_fisher <- sum_fisher + calcFisher(object@parms[[items[i]]], th[j])
        }
        se[j] <- 1 / sqrt(sum_fisher)
      }
    }
    if (truncate) {
      min_theta <- min(theta_range)
      max_theta <- max(theta_range)
      th[th > max_theta] <- max_theta
      th[th < min_theta] <- min_theta
    }
    if (use_step_size) {
      th_change <- th - start_theta
      idx       <- abs(th_change) >= step_size
      th[idx]   <-
        start_theta[idx] +
        (sign(th_change[idx]) * step_size)
    }
    return(list(th = th, se = se, conv = conv, trunc = trunc))
  }
)

#' @docType methods
#' @rdname mle-methods
setGeneric(
  name = "MLE",
  def = function(object, select = NULL, start_theta = NULL, max_iter = 100, crit = 0.001, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    standardGeneric("MLE")
  }
)

#' @docType methods
#' @rdname mle-methods
setMethod(
  f = "MLE",
  signature = "test",
  definition = function(
    object, select = NULL,
    start_theta = NULL, max_iter = 100, crit = 0.001,
    theta_range = c(-4, 4), truncate = FALSE,
    max_change = 1.0,
    do_Fisher = TRUE
  ) {

    # 'test' class is superseded by 'simulation_data_cache'
    # maintainer preference: keep this function

    ni <- ncol(object@data)
    nj <- nrow(object@data)
    nq <- length(object@theta)

    if (is.null(select)) {
      select <- 1:object@pool@ni
      resp <- object@data
    } else {
      if (!all(select %in% 1:object@pool@ni)) {
        stop("MLE: 'select' contains invalid item indices")
      }
      resp <- object@data[, unique(select)]
    }

    if (!is.null(select)) {
      if (anyDuplicated(select) > 0) {
        warning("MLE: 'select' contains duplicate indices")
        select <- select[-duplicated(select)]
      }
      if (!all(select %in% 1:ni)) {
        stop("MLE: 'select' contains invalid indices")
      }
      items <- select
    } else {
      items <- 1:ni
    }

    if (is.null(start_theta)) {
      prior <- rep(1 / nq, nq)
      start_theta <- EAP(object,
        select = select,
        prior  = prior
      )
      start_theta <- start_theta$th
    } else if (length(start_theta) == 1) {
      start_theta <- rep(start_theta, nj)
    } else if (length(start_theta) != nj) {
      stop("MLE: 'start_theta' must be of length 1 or the number of examinees")
    }

    th <- numeric(nj)
    se <- numeric(nj)
    conv  <- logical(nj)
    trunc <- logical(nj)

    for (j in 1:nj) {
      theta_1 <- start_theta[j]
      max_raw_score <- sum(object@pool@NCAT[items[!is.na(object@data[j, items])]] - 1)
      raw_score <- sum(object@data[j, items], na.rm = TRUE)

      if (raw_score > 0 && raw_score < max_raw_score) {

        converged <- FALSE
        done <- FALSE
        iteration <- 0

        while (!converged && !done && iteration <= max_iter) {
          iteration <- iteration + 1
          theta_0 <- theta_1
          deriv1 <- 0
          deriv2 <- 0
          for (i in items) {
            resp <- object@data[j, i]
            deriv1 <- deriv1 + calcJacobian(object@pool@parms[[i]], theta_0, resp)
            if (do_Fisher) {
              deriv2 <- deriv2 + calcFisher(object@pool@parms[[i]], theta_0)
            } else {
              deriv2 <- deriv2 - calcHessian(object@pool@parms[[i]], theta_0, resp)
            }
          }
          change <- deriv1 / deriv2
          if (is.nan(change)) {
            done <- TRUE
          } else {
            if (abs(change) > max_change) {
              change <- sign(change) * max_change
            } else if (abs(change) < crit) {
              converged <- conv[j] <- TRUE
            }
            theta_1 <- theta_0 + change
          }
        }
      }

      if (conv[j]) {
        th[j] <- theta_1
        se[j] <- 1 / sqrt(abs(deriv2))
      } else {
        th[j] <- start_theta[j]
        sum_fisher <- 0
        for (i in 1:length(items)) {
          sum_fisher <- sum_fisher + calcFisher(object@parms[[items[i]]], th[j])
        }
        se[j] <- 1 / sqrt(sum_fisher)
      }

    }
    if (truncate) {
      min_theta <- min(theta_range)
      max_theta <- max(theta_range)
      th[th > max_theta] <- max_theta
      th[th < min_theta] <- min_theta
    }
    RMSE <- NULL
    if (!is.null(object@true_theta)) {
      RMSE <- sqrt(mean((th - object@true_theta)^2))
    }
    return(list(th = th, se = se, conv = conv, trunc = trunc, RMSE = RMSE))
  }
)

#' @docType methods
#' @rdname mle-methods
setMethod(
  f = "MLE",
  signature = "test_cluster",
  definition = function(object, select = NULL, start_theta = NULL, max_iter = 100, crit = 0.001) {
    # maintainer preference: keep this function
    MLE_cluster <- vector(mode = "list", length = object@nt)
    for (t in 1:object@nt) {
      MLE_cluster[[t]] <- MLE(object@tests[[t]], select = NULL, start_theta = start_theta, max_iter = max_iter, crit = crit)
    }
    return(MLE_cluster)
  }
)

#' Compute maximum likelihood estimates of theta using fence items
#'
#' \code{\link{mlef}} is a function to compute maximum likelihood estimates of theta using fence items.
#'
#' @param object an \code{\linkS4class{item_pool}} object.
#' @param select (optional) if item indices are supplied, only the specified items are used.
#' @param resp item response on all (or selected) items in the \code{object} argument. Can be a vector, a matrix, or a data frame. \code{length(resp)} or \code{ncol(resp)} must be equal to the number of all (or selected) items.
#' @param fence_slope the slope parameter to use on fence items. Can be one value, or two values for the lower and the upper fence respectively. (default = \code{5})
#' @param fence_difficulty the difficulty parameter to use on fence items. Must have two values for the lower and the upper fence respectively. (default = \code{c(-5, 5)})
#' @param start_theta (optional) initial theta values. If not supplied, EAP estimates using uniform priors are used as initial values. Uniform priors are computed using the \code{theta_range} argument below, with increments of \code{.1}.
#' @param max_iter maximum number of iterations. (default = \code{100})
#' @param crit convergence criterion to use. (default = \code{0.001})
#' @param truncate set \code{TRUE} to impose a bound using \code{theta_range} on the estimate. (default = \code{FALSE})
#' @param theta_range a range of theta values to bound the estimate. Only effective when \code{truncate} is \code{TRUE}. (default = \code{c(-4, 4)})
#' @param max_change upper bound to impose on the absolute change in theta between iterations. Absolute changes exceeding this value will be capped to \code{max_change}. (default = \code{1.0})
#' @param use_step_size set \code{TRUE} to use \code{step_size}. (default = \code{FALSE})
#' @param step_size upper bound to impose on the absolute change in initial theta and estimated theta. Absolute changes exceeding this value will be capped to \code{step_size}. (default = \code{0.5})
#' @param do_Fisher set \code{TRUE} to use Fisher scoring instead of Newton-Raphson method. (default = \code{TRUE})
#'
#' @return \code{\link{mlef}} returns a list containing estimated values.
#'
#' \itemize{
#'   \item{\code{th}} theta value.
#'   \item{\code{se}} standard error.
#'   \item{\code{conv}} \code{TRUE} if estimation converged.
#'   \item{\code{trunc}} \code{TRUE} if truncation was applied on \code{th}.
#' }
#'
#' @template mlef-ref
#'
#' @docType methods
#' @rdname mlef-methods
#' @examples
#' mlef(itempool_fatigue, resp = resp_fatigue_data[10, ])
#' mlef(itempool_fatigue, select = 1:20, resp = resp_fatigue_data[10, 1:20])
#' @export
setGeneric(
  name = "mlef",
  def = function(object, select = NULL, resp, fence_slope = 5, fence_difficulty = c(-5, 5), start_theta = NULL, max_iter = 100, crit = 0.001, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, use_step_size = FALSE, step_size = 0.5, do_Fisher = TRUE) {
    standardGeneric("mlef")
  }
)

#' @docType methods
#' @rdname mlef-methods
setMethod(
  f = "mlef",
  signature = "item_pool",
  definition = function(object, select = NULL, resp, fence_slope = 5, fence_difficulty = c(-5, 5), start_theta = NULL, max_iter = 50, crit = 0.005, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, use_step_size = FALSE, step_size = 0.5, do_Fisher = TRUE) {

    ni         <- object@ni
    theta_grid <- seq(min(theta_range), max(theta_range), .1)
    nq         <- length(theta_grid)

    if (is.vector(resp)) {
      nj <- 1
      resp <- matrix(resp, 1)
    } else if (is.matrix(resp)) {
      nj <- nrow(resp)
    } else if (is.data.frame(resp)) {
      nj <- nrow(resp)
      resp <- as.matrix(resp)
    } else {
      stop("'resp' must be a vector, a matrix, or a data.frame")
    }

    if (!is.null(select)) {
      if (!all(select %in% 1:ni)) {
        stop("'select' contains invalid indices not present in item pool")
      }
      items <- select
    } else {
      items <- 1:ni
    }

    if (ncol(resp) != length(items)) {
      stop("ncol(resp) or length(resp) must be equal to the number of all (or selected) items")
    }

    if (anyDuplicated(select) > 0) {
      warning("'select' contains duplicate item indices. Removed duplicates from 'select' and 'resp'")
      select <- select[-duplicated(select)]
      resp   <- resp[-duplicated(select)]
    }

    if (is.null(start_theta)) {
      start_theta <- eap(
        object,
        select     = select,
        resp       = resp,
        theta_grid = theta_grid,
        prior      = rep(1 / nq, nq)
      )
      start_theta <- start_theta$th
    } else if (length(start_theta) == 1) {
      start_theta <- rep(start_theta, nj)
    } else if (length(start_theta) != nj) {
      stop("'start_theta' must be a single value or have a value for each examinee.")
    }

    # add fence items
    if (nj == 1) {

      is_extreme <-
        all(resp[1, ] == object[items]@NCAT - 1) |
        all(resp[1, ] == object[items]@NCAT * 0)

      if (is_extreme) {

        ipar_fence <- data.frame(
          ID    = c("FENCE_LB", "FENCE_UB"),
          MODEL = rep("2PL", 2),
          PAR1  = rep(fence_slope, length.out = 2),
          PAR2  = fence_difficulty,
          stringsAsFactors = FALSE
        )
        item_fence <- loadItemPool(ipar_fence)
        object     <- object + item_fence
        resp       <- cbind(resp, 1)
        resp       <- cbind(resp, 0)
        items      <- c(items, ni + 1:2)
        ni         <- ni + 2

      }

    }

    if (nj > 1) {
      ipar_fence <- data.frame(
        ID    = c("FENCE_LB", "FENCE_UB"),
        MODEL = rep("2PL", 2),
        PAR1  = rep(fence_slope, length.out = 2),
        PAR2  = fence_difficulty
      )
      item_fence <- loadItemPool(ipar_fence)
      object     <- object + item_fence
      resp       <- cbind(resp, 1)
      resp       <- cbind(resp, 0)
      items      <- c(items, ni + 1:2)
      ni         <- ni + 2
    }

    th    <- numeric(nj)
    se    <- numeric(nj)
    conv  <- logical(nj)
    trunc <- logical(nj)

    for (j in 1:nj) {
      theta_1 <- start_theta[j]
      max_raw_score <- sum(object@NCAT[items[!is.na(resp[j, ])]] - 1)
      raw_score <- sum(resp[j, ], na.rm = TRUE)
      if (raw_score > 0 && raw_score < max_raw_score) {
        converged <- FALSE
        done <- FALSE
        iteration <- 0
        while (!converged && !done && iteration <= max_iter) {
          iteration <- iteration + 1
          theta_0 <- theta_1
          deriv1 <- 0
          deriv2 <- 0
          for (i in 1:length(items)) {
            if (!is.na(resp[j, i])) {
              deriv1 <- deriv1 + calcJacobian(object@parms[[items[i]]], theta_0, resp[j, i])
              if (do_Fisher) {
                deriv2 <- deriv2 + calcFisher(object@parms[[items[i]]], theta_0)[, 1]
              } else {
                deriv2 <- deriv2 - calcHessian(object@parms[[items[i]]], theta_0, resp[j, i])
              }
            }
          }
          change <- deriv1 / deriv2
          if (is.nan(change)) {
            done <- TRUE
          } else {
            if (abs(change) > max_change) {
              change <- sign(change) * max_change
            } else if (abs(change) < crit) {
              converged <- conv[j] <- TRUE
            }
            theta_1 <- theta_0 + change
          }
        }
      }
      if (conv[j]) {
        th[j] <- theta_1
        se[j] <- 1 / sqrt(abs(deriv2))
      } else {
        th[j] <- start_theta[j]
        sum_fisher <- 0
        for (i in 1:length(items)) {
          sum_fisher <- sum_fisher + calcFisher(object@parms[[items[i]]], th[j])
        }
        se[j] <- 1 / sqrt(sum_fisher)
      }
    }
    if (truncate) {
      min_theta <- min(theta_range)
      max_theta <- max(theta_range)
      th[th > max_theta] <- max_theta
      th[th < min_theta] <- min_theta
    }
    if (use_step_size) {
      th_change <- th - start_theta
      idx       <- abs(th_change) >= step_size
      th[idx]   <-
        start_theta[idx] +
        (sign(th_change[idx]) * step_size)
    }
    return(list(th = th, se = se, conv = conv, trunc = trunc))
  }
)

#' Compute expected a posteriori estimates of theta
#'
#' \code{\link{eap}} is a function to compute expected a posteriori estimates of theta.
#'
#' @param object an \code{\linkS4class{item_pool}} object.
#' @param select (optional) if item indices are supplied, only the specified items are used.
#' @param resp item response on all (or selected) items in the \code{object} argument. Can be a vector, a matrix, or a data frame. \code{length(resp)} or \code{ncol(resp)} must be equal to the number of all (or selected) items.
#' @param theta_grid the theta grid to use as quadrature points. (default = \code{seq(-4, 4, .1)})
#' @param prior a prior distribution, a numeric vector for a common prior or a matrix for individualized priors. (default = \code{rep(1 / 81, 81)})
#' @param reset_prior used for \code{\linkS4class{test_cluster}} objects. If \code{TRUE}, reset the prior distribution for each \code{\linkS4class{test}} object.
#'
#' @return \code{\link{eap}} returns a list containing estimated values.
#' \itemize{
#'   \item{\code{th}} theta value.
#'   \item{\code{se}} standard error.
#' }
#'
#' @examples
#' eap(itempool_fatigue, resp = resp_fatigue_data[10, ])
#' eap(itempool_fatigue, select = 1:20, resp = resp_fatigue_data[10, 1:20])
#'
#' @docType methods
#' @rdname eap-methods
#' @export
setGeneric(
  name = "eap",
  def = function(object, select = NULL, resp, theta_grid = seq(-4, 4, .1), prior = rep(1 / 81, 81)) {
    standardGeneric("eap")
  }
)

#' @docType methods
#' @rdname eap-methods
#' @export
setMethod(
  f = "eap",
  signature = "item_pool",
  definition = function(object, select = NULL, resp, theta_grid = seq(-4, 4, .1), prior = rep(1 / 81, 81)) {
    ni <- object@ni
    nq <- length(theta_grid)
    prob <- calcProb(object, theta_grid)

    if (is.vector(resp)) {
      nj <- 1
      resp <- matrix(resp, 1)
    } else if (is.matrix(resp)) {
      nj <- nrow(resp)
    } else if (is.data.frame(resp)) {
      nj <- nrow(resp)
      resp <- as.matrix(resp)
    } else {
      stop("'resp' must be a vector or a matrix")
    }

    posterior <- matrix(rep(prior, nj), nj, nq, byrow = TRUE)

    if (length(prior) != nq) {
      stop("length(theta_grid) and length(prior) must be equal")
    }

    if (!is.null(select)) {
      if (!all(select %in% 1:ni)) {
        stop("item indices in 'select' must be in 1:ni")
      }
      items <- select
    } else {
      items <- 1:ni
    }

    if (ncol(resp) != length(items)) {
      stop("ncol(resp) or length(resp) must be equal to the number of all (or selected) items")
    }

    if (anyDuplicated(select) > 0) {
      warning("'select' contains duplicated item indices. Removed duplicates from 'select' and 'resp'")
      select <- select[-duplicated(select)]
      resp   <- resp[-duplicated(select)]
    }

    if (nj == 1) {
      for (i in 1:length(items)) {
        if (resp[i] >= 0 && resp[i] < object@max_cat) {
          posterior <- posterior * prob[[items[i]]][, resp[i] + 1]
        }
      }
      th <- sum(posterior * theta_grid) / sum(posterior)
      se <- sqrt(sum(posterior * (theta_grid - th)^2) / sum(posterior))
    } else {
      for (i in items) {
        response <- matrix(resp[, i] + 1, nj, 1)
        if (!all(is.na(response))) {
          prob <- t(prob[[items[i]]][, response])
          prob[is.na(prob)] <- 1
          posterior <- posterior * prob
        }
      }
      th <- as.vector(posterior %*% theta_grid / rowSums(posterior))
      se <- as.vector(sqrt(rowSums(posterior * (matrix(theta_grid, nj, nq, byrow = TRUE) - matrix(th, nj, nq))^2) / rowSums(posterior)))
    }
    return(list(th = th, se = se))
  }
)

#' @docType methods
#' @rdname eap-methods
setGeneric(
  name = "EAP",
  def = function(object, select = NULL, prior, reset_prior = FALSE) {
    standardGeneric("EAP")
  }
)

#' @docType methods
#' @rdname eap-methods
setMethod(
  f = "EAP",
  signature = "test",
  definition = function(object, select = NULL, prior, reset_prior = FALSE) {

    # 'test' class is superseded by 'simulation_data_cache'
    # maintainer preference: keep this function

    nj <- nrow(object@data)
    if (is.matrix(prior)) {
      nq <- ncol(prior)
      if (nj != nrow(prior)) stop("EAP: nrow(prior) is not equal to nrow(data)")
      posterior <- prior
    } else {
      nq <- length(prior)
      posterior <- matrix(rep(prior, nj), nj, nq, byrow = TRUE)
    }
    if (is.null(select)) {
      select <- 1:object@pool@ni
    } else {
      if (!all(select %in% 1:object@pool@ni)) {
        stop("EAP: 'select' contains invalid item indices")
      }
    }
    for (i in unique(select)) {
      resp <- matrix(object@data[, i] + 1, nj, 1)
      if (!all(is.na(resp))) {
        prob <- t(object@prob[[i]][, resp])
        prob[is.na(prob)] <- 1
        posterior <- posterior * prob
      }
    }
    th <- as.vector(posterior %*% object@theta / rowSums(posterior))
    se <- as.vector(sqrt(rowSums(posterior * (matrix(object@theta, nj, nq, byrow = TRUE) - matrix(th, nj, nq))^2) / rowSums(posterior)))
    if (is.null(object@true_theta)) {
      RMSE <- NULL
    } else {
      RMSE <- sqrt(mean((th - object@true_theta)^2))
    }
    return(list(th = th, se = se, prior = prior, posterior = posterior, RMSE = RMSE))
  }
)

#' @docType methods
#' @rdname eap-methods
setMethod(
  f = "EAP",
  signature = "test_cluster",
  definition = function(object, select = NULL, prior, reset_prior = FALSE) {
    # maintainer preference: keep this function
    EAP_cluster <- vector(mode = "list", length = object@nt)
    EAP_cluster[[1]] <- EAP(object@tests[[1]], select = select, prior = prior)
    if (reset_prior) {
      for (t in 2:object@nt) {
        EAP_cluster[[t]] <- EAP(object@tests[[t]], select = select, prior = prior)
      }
    } else {
      for (t in 2:object@nt) {
        EAP_cluster[[t]] <- EAP(object@tests[[t]], select = select, prior = EAP_cluster[[t - 1]]@posterior)
      }
    }
    return(EAP_cluster)
  }
)

#' @noRd
parseInitialTheta <- function(config, constants, item_pool, posterior_constants) {

  o <- list()

  o$likelihood <- matrix(1, constants$nj, constants$nq)

  o$posterior <- generateDensityFromPriorPar(
    config@interim_theta,
    constants$theta_q,
    constants$nj
  )

  interim_method <- toupper(config@interim_theta$method)
  final_method   <- toupper(config@final_theta$method)
  if (any(c(interim_method, final_method) %in% c("FB"))) {
    o$ipar_list <- iparPosteriorSample(item_pool, posterior_constants$n_sample)
    # TODO: needs to be migrated to its own function
  }

  config_value <- config@item_selection$initial_theta

  if (!is.null(config_value)) {
    if (inherits(config_value, "numeric")) {
      config_value <- matrix(config_value, , 1)
    }
    if (nrow(config_value) == 1) {
      o$theta <- matrix(config_value, constants$nj, ncol(config_value), byrow = TRUE)
    }
    if (nrow(config_value) == constants$nj) {
      o$theta <- config_value
    }
  }
  if (is.null(config_value)) {
    o$theta <- o$posterior %*% constants$theta_q
  }

  return(o)

}

#' @noRd
getInitialThetaPrior <- function(config_theta, j, posterior_constants) {

  o <- list()

  o$posterior_sample <- generateSampleFromPriorPar(
    config_theta,
    j,
    posterior_constants
  )

  o$posterior_sample <- applyThin(o$posterior_sample, posterior_constants)
  o$theta            <- mean(o$posterior_sample)
  o$se               <- sd(o$posterior_sample) * posterior_constants$jump_factor

  return(o)

}

#' @noRd
parseInitialThetaOfThisExaminee <- function(config_theta, initial_theta, j, posterior_constants) {

  o <- list()
  theta_method <- toupper(config_theta$method)
  if (theta_method %in% c("EAP", "MLE", "MLEF")) {
    o$theta <- initial_theta[j, ]
  }
  if (theta_method %in% c("EB", "FB")) {
    o <- getInitialThetaPrior(config_theta, j, posterior_constants)
  }

  return(o)

}

#' @noRd
parseThetaSegment <- function(current_theta, position, exposure_control, constants) {

  n_segment   <- constants$n_segment
  segment_cut <- constants$segment_cut

  if (isFirstSegmentValid(exposure_control$first_segment, n_segment, position)) {
    segment <- exposure_control$first_segment[position]
    return(segment)
  }

  if (constants$exposure_control_method %in% c("NONE", "ELIGIBILITY", "BIGM")) {
    # find_segment() needs to be updated for multidimensional segments
    segment <- find_segment(current_theta$theta, segment_cut)
    return(segment)
  }

  if (constants$exposure_control_method %in% c("BIGM-BAYESIAN")) {
    segment_prob <- getSegmentProb(current_theta$posterior_sample, constants)
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
computeEAPFromPosterior <- function(posterior, theta_grid) {
  o <- list()
  o$theta <- sum(posterior * theta_grid) / sum(posterior)
  o$se    <- sqrt(sum(posterior * (theta_grid - o$theta)^2) / sum(posterior))
  return(o)
}

#' @noRd
applyShrinkageCorrection <- function(EAP, config_theta, j) {

  if (toupper(config_theta$prior_dist) == "NORMAL" && config_theta$shrinkage_correction) {
    prior_sd <- config_theta$prior_par[[j]][2]
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
