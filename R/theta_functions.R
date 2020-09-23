#' @include shadow_functions.R
NULL

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
#' @param truncate set \code{TRUE} to impose a bound on the estimate. (default = \code{FALSE})
#' @param theta_range a range of theta values to bound the estimate. Only effective when \code{truncate} is \code{TRUE}. (default = \code{c(-4, 4)})
#' @param max_change upper bound to impose on the absolute change in theta between iterations. Absolute changes exceeding this value will be capped to \code{max_change}. (default = \code{1.0})
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
  def = function(object, select = NULL, resp, start_theta = NULL, max_iter = 100, crit = 0.001, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, do_Fisher = TRUE) {
    standardGeneric("mle")
  }
)

#' @docType methods
#' @rdname mle-methods
setMethod(
  f = "mle",
  signature = "item_pool",
  definition = function(object, select = NULL, resp, start_theta = NULL, max_iter = 50, crit = 0.005, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, do_Fisher = TRUE) {

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
  definition = function(object, select = NULL, start_theta = NULL, max_iter = 100, crit = 0.001, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    ni <- ncol(object@data)
    nj <- nrow(object@data)
    nq <- length(object@theta)

    if (is.null(select)) {
      select <- 1:object@pool@ni
      resp <- object@data
    } else {
      if (!all(select %in% 1:object@pool@ni)) {
        stop("select contains invalid item indices")
      }
      resp <- object@data[, unique(select)]
    }

    if (!is.null(select)) {
      if (anyDuplicated(select) > 0) {
        warning("select contains duplicated indices")
        select <- select[-duplicated(select)]
      }
      if (!all(select %in% 1:ni)) {
        stop("select contains invalid indices")
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
      stop("start_theta must be of length 1 or the number of examinees")
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
#' @param truncate set \code{TRUE} to impose a bound on the estimate. (default = \code{FALSE})
#' @param theta_range a range of theta values to bound the estimate. Only effective when \code{truncate} is \code{TRUE}. (default = \code{c(-4, 4)})
#' @param max_change upper bound to impose on the absolute change in theta between iterations. Absolute changes exceeding this value will be capped to \code{max_change}. (default = \code{1.0})
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
  def = function(object, select = NULL, resp, fence_slope = 5, fence_difficulty = c(-5, 5), start_theta = NULL, max_iter = 100, crit = 0.001, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, do_Fisher = TRUE) {
    standardGeneric("mlef")
  }
)

#' @docType methods
#' @rdname mlef-methods
setMethod(
  f = "mlef",
  signature = "item_pool",
  definition = function(object, select = NULL, resp, fence_slope = 5, fence_difficulty = c(-5, 5), start_theta = NULL, max_iter = 50, crit = 0.005, truncate = FALSE, theta_range = c(-4, 4), max_change = 1.0, do_Fisher = TRUE) {

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
#' @param reset_prior used for \code{\linkS4class{test_cluster}} objects. If \code{TRUE}, to reset the prior distribution before each test.
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
    nj <- nrow(object@data)
    if (is.matrix(prior)) {
      nq <- ncol(prior)
      if (nj != nrow(prior)) stop("nrow(prior) is not equal to nrow(data)")
      posterior <- prior
    } else {
      nq <- length(prior)
      posterior <- matrix(rep(prior, nj), nj, nq, byrow = TRUE)
    }
    if (is.null(select)) {
      select <- 1:object@pool@ni
    } else {
      if (!all(select %in% 1:object@pool@ni)) {
        stop("select contains invalid item indices")
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
  if (theta_method %in% c("EAP", "MLE", "MLEF")) {
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

  if (exposure_control_method %in% c("NONE", "ELIGIBILITY", "BIGM")) {
    if (isFirstSegmentValid(exposure_control$first_segment, n_segment, position)) {
      segment <- exposure_control$first_segment[position]
      return(segment)
    } else {
      segment <- find_segment(current_theta$theta, segment_cut)
      return(segment)
    }
  }

  if (exposure_control_method %in% c("BIGM-BAYESIAN")) {
    # maybe not necessary to make this into a function
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
