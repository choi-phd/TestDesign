#' @include static_functions.R
NULL

#' Find matching theta to supplied probability
#'
#' Find theta corresponding to a response probability value for each item.
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param rp A response probability value.
#' @param max_iter A maximum number of iterations.
#' @param conv A convergence criterion.
#' @param start_theta A starting theta value.
calcRP <- function(object, rp = .50, max_iter = 100, conv = 0.0001, start_theta = 0) {
  RP <- numeric(object@ni)
  for (i in 1:object@ni) {
    max_score <- object@NCAT[i] - 1
    theta <- start_theta
    ep    <- as.vector(calcEscore(object@parms[[i]], theta)) / max_score
    gap   <- abs(rp - ep)
    done  <- gap < conv
    iter  <- 0
    while (!done && iter < max_iter) {
      iter  <- iter + 1
      h     <- gap / -calcFisher(object@parms[[i]], theta)
      theta <- theta - h
      ep    <- as.vector(calcEscore(object@parms[[i]], theta)) / max_score
      gap   <- abs(rp - ep)
      done  <- gap < conv
    }
    RP[i] <- theta
  }
  return(RP)
}

#' @rdname simResp-methods
#' @aliases simResp,pool_cluster,numeric-method
setMethod(
  f = "simResp",
  signature = c("pool_cluster", "list"),
  definition = function(object, theta) {
    if (length(theta) != length(object@np)) {
      data <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        if (all(!is.na(theta[[i]]))) {
          data[[i]] <- simResp(object@pools[[i]], theta[[i]])
        } else {
          stop(paste0("invalid values in theta", "[[", i, "]]"))
        }
      }
      return(data)
    } else {
      stop("length of theta not equal to np")
    }
  }
)

#' @description \code{pool_cluster1 == pool_cluster2} tests equality of the two pool_cluster objects.
#'
#' @param pool_cluster1 A \code{\linkS4class{pool_cluster}} object.
#' @param pool_cluster2 A \code{\linkS4class{pool_cluster}} object.
#'
#' @examples
#' cluster1 <- makeItemPoolCluster(c(itempool_science, itempool_reading))
#' cluster2 <- makeItemPoolCluster(c(cluster1@pools[[1]], cluster1@pools[[2]]))
#' cluster1 == cluster2  ## TRUE
#'
#' @rdname item_pool.operators
#' @export
`==.pool_cluster` <- function(pool_cluster1, pool_cluster2) {
  if (!inherits(pool_cluster1, "pool_cluster") || !inherits(pool_cluster2, "pool_cluster")) stop("Operands must be 'pool_cluster' objects.")
  return(identical(pool_cluster1, pool_cluster2))
}

#' Extract
#'
#' @param x x
#' @param i i
#' @param j j
#' @param ... ...
#' @param drop drop
#'
#' @name extract-methods
#' @aliases [,test,ANY,ANY,ANY-method
#' @docType methods
setMethod(
  f = "[",
  signature = "test",
  definition = function(x, i, j, ...) {
    if (i == "pool") {
      return(x@pool)
    }
    if (i == "theta") {
      return(x@theta)
    }
    if (i == "prob") {
      return(x@prob)
    }
    if (i == "info") {
      return(x@info)
    }
    if (i == "true_theta") {
      return(x@true_theta)
    }
    if (i == "data") {
      return(x@data)
    }
  }
)

#' Create a subset of a test object
#'
#' Create a subset of a test object.
#'
#' @param test An \code{\linkS4class{test}} object.
#' @param select A vector of item indices to subset.
#'
#' @examples
#' test <- makeTest(itempool_science, seq(-3, 3, 1))
#' subtest <- subsetTest(test, 1:100)
#' @export
subsetTest <- function(test, select = NULL) {
  if (!inherits(test, "test")) {
    stop("'test' must be a 'test' object.")
  }
  if (is.null(select)) {
    return(test)
  } else if (all(select %in% 1:test@pool@ni) && anyDuplicated(select) == 0) {
    n_select            <- length(select)
    sub_test            <- new("test")
    sub_test@pool       <- subsetItemPool(test@pool, select = select)
    sub_test@theta      <- test@theta
    sub_test@prob       <- test@prob[select]
    sub_test@info       <- test@info[, select, drop = FALSE]
    sub_test@true_theta <- test@true_theta
    sub_test@data       <- test@data[, select, drop = FALSE]
    return(sub_test)
  } else {
    stop("'select' contains invalid values.")
  }
}

#' Generate a test object
#'
#' Generate a \code{\linkS4class{test}} object
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param theta A grid of theta values.
#' @param info_type An information type.
#' @param true_theta An optional vector of true theta values to simulate response data.
#'
#' @docType methods
#' @rdname makeTest-methods
#'
#' @examples
#' test <- makeTest(itempool_science, seq(-3, 3, 1))
#' @export
setGeneric(
  name = "makeTest",
  def = function(object, theta = seq(-4, 4, .1), info_type = "FISHER", true_theta = NULL) {
    standardGeneric("makeTest")
  }
)

#' @docType methods
#' @rdname makeTest-methods
#' @export
setMethod(
  f = "makeTest",
  signature = "item_pool",
  definition = function(object, theta = seq(-4, 4, .1), info_type = "FISHER", true_theta = NULL) {
    prob <- calcProb(object, theta)
    if (toupper(info_type) == "FISHER") {
      info <- calcFisher(object, theta)
    } else {
      stop("'info_type' must be FISHER.")
    }
    if (!is.null(true_theta)) {
      data <- simResp(object, true_theta)
    } else {
      data <- NULL ## this is a provision for cases where data is imported from elsewhere
    }
    return(new("test", pool = object, theta = theta, prob = prob, info = info, true_theta = true_theta, data = data))
  }
)

#' Generate a test cluster object
#'
#' Generate a \code{\linkS4class{test_cluster}} object
#'
#' @param object An \code{\linkS4class{pool_cluster}} object
#' @param theta A grid of theta values
#' @param true_theta An optional vector of true theta values to simulate response data
#'
#' @docType methods
#' @rdname makeTestCluster-methods
setGeneric(
  name = "makeTestCluster",
  def = function(object, theta, true_theta) {
    standardGeneric("makeTestCluster")
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods
setMethod(
  f = "makeTestCluster",
  signature = c("pool_cluster", "numeric", "numeric"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta)
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods
setMethod(
  f = "makeTestCluster",
  signature = c("pool_cluster", "numeric", "list"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta[[p]])
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)

#' Generate maximum likelihood estimates of theta
#'
#' Generate maximum likelihood estimates of theta.
#'
#' @param object A \code{\linkS4class{item_pool}} object.
#' @param resp A vector (or matrix) of item responses.
#' @param start_theta An optional vector of start theta values.
#' @param max_iter Maximum number of iterations.
#' @param crit Convergence criterion.
#' @param select A vector of indices identifying the items to subset.
#' @param theta_range A range of theta values.
#' @param truncate Set \code{TRUE} to bound MLE to theta_range: c(minTheta, maxTheta).
#' @param max_change Maximum change between iterations.
#' @param do_Fisher \code{TRUE} to use Fisher's method of scoring.
#'
#' @docType methods
#' @rdname mle-methods
#' @examples
#' mle(itempool_fatigue, resp_fatigue_data[10,])
#' @export
setGeneric(
  name = "mle",
  def = function(object, resp, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    standardGeneric("mle")
  }
)

#' @docType methods
#' @rdname mle-methods
setMethod(
  f = "mle",
  signature = "item_pool",
  definition = function(object, resp, start_theta = NULL, max_iter = 50, crit = 0.005, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    ni <- object@ni
    theta <- seq(min(theta_range), max(theta_range), .1)
    nq <- length(theta)
    if (is.vector(resp)) {
      nj <- 1
      resp <- matrix(resp, 1)
    } else if (is.matrix(resp)) {
      nj <- nrow(resp)
    } else if (is.data.frame(resp)) {
      nj <- nrow(resp)
      resp <- as.matrix(resp)
    } else {
      stop("'resp' must be a vector, a matrix, or a data.frame.")
    }
    if (!is.null(select)) {
      if (length(resp) != length(select)) {
        stop("'resp' and 'select' must have equal length when 'select' is supplied.")
      }
      if (anyDuplicated(select) > 0) {
        warning("'select' contains duplicate indices.")
        select <- select[-duplicated(select)]
      }
      if (!all(select %in% 1:ni)) {
        stop("'select' contains invalid indices.")
      }
      items <- select
    } else {
      items <- 1:ni
    }
    if (ncol(resp) != length(items)) {
      stop("'resp' must be of length ni or match the length of select")
    }
    if (is.null(start_theta)) {
      start_theta <- eap(object, theta, rep(1 / nq, nq), resp, select = select)$th
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
                deriv2 <- deriv2 + calcFisher(object@parms[[items[i]]], theta_0)
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

#' Generate maximum likelihood estimates of theta
#'
#' Generate maximum likelihood estimates of theta.
#'
#' @param object A \code{\linkS4class{test}} object.
#' @param start_theta An optional vector of start theta values.
#' @param max_iter Maximum number of iterations.
#' @param crit Convergence criterion.
#' @param select A vector of indices identifying the items to subset.
#' @param theta_range A range of theta values: c(minTheta, maxTheta).
#' @param truncate Set \code{TRUE} to bound MLE to theta_range.
#' @param max_change Maximum change between iterations.
#' @param do_Fisher Set \code{TRUE} to use Fisher's method of scoring.
#'
#' @docType methods
#' @rdname mlearray-methods
setGeneric(
  name = "MLE",
  def = function(object, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    standardGeneric("MLE")
  }
)

#' @docType methods
#' @rdname mlearray-methods
setMethod(
  f = "MLE",
  signature = "test",
  definition = function(object, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
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
      start_theta <- EAP(object, prior, select = select)$th
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
#' @rdname mlearray-methods
setMethod(
  f = "MLE",
  signature = "test_cluster",
  definition = function(object, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL) {
    MLE_cluster <- vector(mode = "list", length = object@nt)
    for (t in 1:object@nt) {
      MLE_cluster[[t]] <- MLE(object@tests[[t]], start_theta = start_theta, max_iter = max_iter, crit = crit, select = NULL)
    }
    return(MLE_cluster)
  }
)

#' Generate expected a posteriori estimates of theta
#'
#' Generate expected a posteriori estimates of theta.
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param theta A theta grid.
#' @param prior A prior distribution, a numeric vector for a common prior or a matrix for individualized priors.
#' @param resp A numeric matrix of item responses, one row per examinee.
#' @param select A vector of indices identifying the items to subset.
#'
#' @docType methods
#' @rdname eap-methods
#' @export
setGeneric(
  name = "eap",
  def = function(object, theta, prior, resp, select = NULL) {
    standardGeneric("eap")
  }
)

#' @docType methods
#' @rdname eap-methods
#' @export
setMethod(
  f = "eap",
  signature = "item_pool",
  definition = function(object, theta, prior, resp, select = NULL) {
    ni <- object@ni
    nq <- length(theta)
    prob <- calcProb(object, theta)
    if (is.vector(resp)) {
      nj <- 1
    } else if (is.matrix(resp)) {
      nj <- nrow(resp)
    } else if (is.data.frame(resp)) {
      nj <- nrow(resp)
      resp <- as.matrix(resp)
    } else {
      stop("resp must be of class either vector or matrix")
    }
    posterior <- matrix(rep(prior, nj), nj, nq, byrow = TRUE)
    if (length(prior) != nq) {
      stop("theta and prior must be equal in length")
    }
    if (!is.null(select)) {
      if (length(resp) != length(select)) {
        stop("resp and select must be equal in length when select is not NULL")
      }
      if (anyDuplicated(select) > 0) {
        warning("select contains duplicated indices")
        select <- select[-duplicated(select)]
        response <- response[-duplicated(select)]
      }
      if (!all(select %in% 1:ni)) {
        stop("select contains invalid indices")
      }
      items <- select
    } else {
      items <- 1:ni
    }
    if (nj == 1) {
      for (i in 1:length(items)) {
        if (resp[i] >= 0 && resp[i] < object@max_cat) {
          posterior <- posterior * prob[[items[i]]][, resp[i] + 1]
        }
      }
      th <- sum(posterior * theta) / sum(posterior)
      se <- sqrt(sum(posterior * (theta - th)^2) / sum(posterior))
    } else {
      for (i in items) {
        response <- matrix(resp[, i] + 1, nj, 1)
        if (!all(is.na(response))) {
          prob <- t(prob[[items[i]]][, response])
          prob[is.na(prob)] <- 1
          posterior <- posterior * prob
        }
      }
      th <- as.vector(posterior %*% theta / rowSums(posterior))
      se <- as.vector(sqrt(rowSums(posterior * (matrix(theta, nj, nq, byrow = TRUE) - matrix(th, nj, nq))^2) / rowSums(posterior)))
    }
    return(list(th = th, se = se))
  }
)

#' Generate expected a posteriori estimates of theta
#'
#' Generate expected a posteriori estimates of theta.
#'
#' @param object A \code{\linkS4class{test}} or a \code{\linkS4class{test_cluster}} object.
#' @param prior A prior distribution, a numeric vector for a common prior or a matrix for individualized priors.
#' @param select A vector of indices identifying the items to subset.
#' @param reset_prior Set \code{TRUE} to reset the prior distribution for each test when object is of class \code{\linkS4class{test_cluster}}.
#'
#' @docType methods
#' @rdname eaparray-methods
setGeneric(
  name = "EAP",
  def = function(object, prior, select = NULL, reset_prior = FALSE) {
    standardGeneric("EAP")
  }
)

#' @docType methods
#' @rdname eaparray-methods
setMethod(
  f = "EAP",
  signature = "test",
  definition = function(object, prior, select = NULL, reset_prior = FALSE) {
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
#' @rdname eaparray-methods
setMethod(
  f = "EAP",
  signature = "test_cluster",
  definition = function(object, prior, select = NULL, reset_prior = FALSE) {
    EAP_cluster <- vector(mode = "list", length = object@nt)
    EAP_cluster[[1]] <- EAP(object@tests[[1]], prior, select)
    if (reset_prior) {
      for (t in 2:object@nt) {
        EAP_cluster[[t]] <- EAP(object@tests[[t]], prior, select)
      }
    } else {
      for (t in 2:object@nt) {
        EAP_cluster[[t]] <- EAP(object@tests[[t]], EAP_cluster[[t - 1]]@posterior, select)
      }
    }
    return(EAP_cluster)
  }
)

#' Create an item pool cluster object
#'
#' Create a \code{\linkS4class{pool_cluster}} object.
#'
#' @param pools A list of \code{\linkS4class{item_pool}} objects.
#' @param names An optional vector of \code{\linkS4class{item_pool}} names.
#' @examples
#'
#' cluster <- makeItemPoolCluster(c(itempool_science, itempool_reading))
#' @export
makeItemPoolCluster <- function(pools, names = NULL) {
  np <- length(pools)
  if (np == 0) {
    stop("pools is empty")
  } else if (np == 1) {
    stop("only one pool found in pools - expecting 2 or more")
  }
  if (is.null(names)) {
    names <- paste0("Pool_", 1:np)
  } else {
    if (length(names) != np) stop("pools and names are of different lengths")
  }
  pool_cluster <- new("pool_cluster")
  pool_cluster@np <- np
  pool_cluster@pools <- vector(mode = "list", length = np)
  pool_cluster@names <- names
  for (i in 1:np) {
    if (!inherits(pools[[i]], "item_pool")) {
      stop(paste0("pool.list[[", i, "]] is not of class \"item_pool\""))
    }
    pool_cluster@pools[[i]] <- pools[[i]]
  }
  if (validObject(pool_cluster)) {
    return(pool_cluster)
  }
}

#' Run adaptive test assembly.
#'
#' Perform adaptive test assembly based on generalized shadow-test approach, with specified configurations.
#'
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param constraints A list representing optimization constraints. Use \code{\link{loadConstraints}} for this.
#' @param true_theta Numeric. A vector of true theta values to be used in simulation.
#' @param data Numeric. A matrix containing item response data.
#' @param prior Numeric. A matrix or a vector containing priors.
#' @param prior_par Numeric. A vector of parameters for prior distribution.
#' @param session Used to communicate with a Shiny session.
#'
#' @return An \code{\linkS4class{output_Shadow_all}} object containing results.
#'
#' @references{
#'   \insertRef{van_der_linden_model_1998}{TestDesign}
#'
#'   \insertRef{van_der_linden_optimal_1998}{TestDesign}
#'
#'   \insertRef{van_der_linden_optimal_2000}{TestDesign}
#'
#'   \insertRef{van_der_linden_linear_2005}{TestDesign}
#' }
#' @rdname Shadow-methods
#'
#' @examples
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(config, constraints_science, true_theta)
#' solution@output
#' @export
setGeneric(
  name = "Shadow",
  def = function(config, constraints = NULL, true_theta = NULL, data = NULL, prior = NULL, prior_par = NULL, session = NULL) {
    standardGeneric("Shadow")
  }
)

#' @rdname Shadow-methods
#' @export
setMethod(
  f = "Shadow",
  signature = "config_Shadow",
  definition = function(config, constraints, true_theta, data, prior, prior_par, session) {

    if (!validObject(config)) {
      stop("invalid configuration options specified")
    }

    if (is.null(constraints)) {
      stop("'constraints' must be supplied.")
    }

    pool      <- constraints@pool
    model     <- sanitizeModel(pool@model)
    constants <- getConstants(constraints, config, data, true_theta)
    all_data  <- makeData(pool, true_theta, data, constants)
    exposure_control  <- toupper(config@exposure_control$method)

    if (constants$use_shadow) {
      refresh_shadow <- initializeShadowEngine(constants, config@refresh_policy)
    }

    #####
    ###    Initialize bayesian stuff
    #####

    if (is.null(prior)) {
      if (!is.null(prior_par)) {
        if (is.vector(prior_par) && length(prior_par) == 2) {
          posterior <- matrix(dnorm(constants$theta_q, mean = prior_par[1], sd = prior_par[2]), nj, nq, byrow = TRUE)
        } else if (is.matrix(prior_par) && all(dim(prior_par) == c(nj, 2))) {
          posterior <- matrix(NA, nj, nq)
          for (j in 1:nj) {
            posterior[j, ] <- dnorm(constants$theta_q, mean = prior_par[j, 1], sd = prior_par[j, 2])
          }
        } else {
          stop("prior_par must be a vector of length 2, c(mean, sd), or a matrix of dim c(nj x 2)")
        }
      } else if (toupper(config@interim_theta$prior_dist) == "NORMAL") {
        posterior <- matrix(dnorm(constants$theta_q, mean = config@interim_theta$prior_par[1], sd = config@interim_theta$prior_par[2]), nj, nq, byrow = TRUE)
      } else if (toupper(config@interim_theta$prior_dist) == "UNIFORM") {
        posterior <- matrix(1, nj, nq)
      } else {
        stop("invalid configuration option for interim_theta$prior_dist")
      }
    } else if (is.vector(prior) && length(prior) == nq) {
      posterior <- matrix(prior, nj, nq, byrow = TRUE)
    } else if (is.matrix(prior) && all(dim(prior) == c(nj, nq))) {
      posterior <- prior
    } else {
      stop("misspecification for prior or prior_par")
    }

    if (toupper(config@interim_theta$method) %in% c("EB", "FB") || toupper(config@final_theta$method) %in% c("EB", "FB")) {
      n_sample <- config@MCMC$burn_in + config@MCMC$post_burn_in
    }
    if (toupper(config@interim_theta$method) == "FB" || toupper(config@final_theta$method) == "FB") {
      ipar_list <- iparPosteriorSample(pool, n_sample)
    }

    #####
    ###    Get initial theta estimate
    #####

    if (!is.null(config@item_selection$initial_theta)) {
      initial_theta <- rep(config@item_selection$initial_theta, constants$nj)
    } else {
      initial_theta <- as.vector(posterior %*% matrix(constants$theta_q, ncol = 1))
    }


    #####
    ###    Initialize exposure rate control
    #####

    items_administered <- matrix(FALSE, constants$nj, constants$ni)
    output_list <- vector(mode = "list", length = constants$nj)

    if (exposure_control %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {

      #####
      ###    Initialize segment-wise exposure rates
      #####

      item_eligibility_control <- TRUE
      max_exposure_rate   <- config@exposure_control$max_exposure_rate
      fading_factor       <- config@exposure_control$fading_factor
      acceleration_factor <- config@exposure_control$acceleration_factor

      n_segment <- config@exposure_control$n_segment
      if (!length(max_exposure_rate) %in% c(1, n_segment)) {
        stop("length(max_exposure_rate) must be 1 or n_segment")
      }

      true_segment_freq  <- numeric(n_segment)
      est_segment_freq   <- numeric(n_segment)
      true_segment_count <- numeric(nj)
      est_segment_count  <- numeric(nj)
      segment_cut  <- config@exposure_control$segment_cut
      cut_lower    <- segment_cut[1:n_segment]
      cut_upper    <- segment_cut[2:(n_segment + 1)]
      pe_i <- matrix(1, n_segment, constants$ni)

      if (constants$set_based) {
        pe_s <- matrix(1, n_segment, constants$ns)
      } else {
        pe_s <- NULL
        alpha_sjk <- NULL
        rho_sjk   <- NULL
      }

      # Initialize diagnostic stats

      if (config@exposure_control$diagnostic_stats) {

        alpha_g_i   <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ni)
        epsilon_g_i <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ni)

        if (constants$set_based) {
          alpha_g_s   <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ns)
          epsilon_g_s <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ns)
        }

        if (fading_factor != 1) {
          no_fading_alpha_g_i   <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ni)
          no_fading_epsilon_g_i <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ni)
          if (constants$set_based) {
            no_fading_alpha_g_s   <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ns)
            no_fading_epsilon_g_s <- matrix(0, nrow = constants$nj, ncol = n_segment * constants$ns)
          }
        }
      }

      # Initialize eligibility parameters

      if (!is.null(config@exposure_control$initial_eligibility_stats)) {
        n_jk      <- config@exposure_control$initial_eligibility_stats$n_jk
        alpha_ijk <- config@exposure_control$initial_eligibility_stats$alpha_ijk
        phi_jk    <- config@exposure_control$initial_eligibility_stats$phi_jk
        rho_ijk   <- config@exposure_control$initial_eligibility_stats$rho_ijk
        if (constants$set_based) {
          alpha_sjk <- config@exposure_control$initial_eligibility_stats$alpha_sjk
          rho_sjk   <- config@exposure_control$initial_eligibility_stats$rho_sjk
        }
      } else {
        n_jk      <- numeric(n_segment)
        alpha_ijk <- matrix(0, n_segment, constants$ni)
        phi_jk    <- numeric(n_segment)
        rho_ijk   <- matrix(0, n_segment, constants$ni)
        if (constants$set_based) {
          alpha_sjk <- matrix(0, n_segment, constants$ns)
          rho_sjk   <- matrix(0, n_segment, constants$ns)
        }
      }

      if (fading_factor != 1) {
        no_fading_n_jk      <- n_jk
        no_fading_alpha_ijk <- alpha_ijk
        no_fading_rho_ijk   <- rho_ijk
        if (constants$set_based) {
          no_fading_alpha_sjk <- alpha_sjk
          no_fading_rho_sjk   <- rho_sjk
        }
      }

    } else {

      item_eligibility_control <- FALSE
      true_segment_count <- NULL
      est_segment_count  <- NULL

    }

    #####
    ###    Use a fixed theta value throughout selection, if supplied
    #####

    if (!is.null(config@item_selection$fixed_theta)) {
      if (length(config@item_selection$fixed_theta) == 1) {
        info_fixed_theta <- vector(mode = "list", length = constants$nj)
        info_fixed_theta[1:constants$nj] <- all_data$test@info[which.min(abs(config@theta_grid - config@item_selection$fixed_theta)), ]
        config@item_selection$fixed_theta <- rep(config@item_selection$fixed_theta, constants$nj)
        select_at_fixed_theta <- TRUE
      } else if (length(config@item_selection$fixed_theta) == constants$nj) {
        info_fixed_theta <- lapply(seq_len(constants$nj), function(j) calc_info(config@item_selection$fixed_theta[j], pool@ipar, pool@NCAT, model))
        select_at_fixed_theta <- TRUE
      } else {
        stop("length of config@item_selection$fixed_theta must be either 1 or nj")
      }
    } else {
      select_at_fixed_theta <- FALSE
    }

    #####
    ###    Initialize usage matrix
    #####

    if (constants$set_based) {
      usage_matrix <- matrix(FALSE, nrow = constants$nj, ncol = constants$nv)
    } else {
      usage_matrix <- matrix(FALSE, nrow = constants$nj, ncol = constants$ni)
    }

    #####
    ###    Get info
    #####

    getInfo <- function() {
      if (select_at_fixed_theta) {
        info <- info_fixed_theta[[j]]
      } else if (config@item_selection$method == "MPWI") {
        info <- as.vector(matrix(posterior[j, ], nrow = 1) %*% all_data$test@info)
      } else if (config@item_selection$method == "MFI") {
        info <- calc_info(current_theta, pool@ipar, pool@NCAT, model)
      } else if (config@item_selection$method == "EB") {
        info <- calc_info_EB(output@posterior_sample, pool@ipar, pool@NCAT, model)
      } else if (config@item_selection$method == "FB") {
        if (config@item_selection$info_type == "FISHER") {
          info <- calc_info_FB(output@posterior_sample, ipar_list, pool@NCAT, model)
        } else if (config@item_selection$info_type %in% c("MI", "MUTUAL")) {
          info <- calc_MI_FB(output@posterior_sample, ipar_list, pool@NCAT, model)
        }
      }
      return(info)
    }

    #####
    ###    select a non-administered item with the largest information
    #####

    selectItem <- function() {

      if (position > 1) {
        info[output@administered_item_index[1:(position - 1)]] <- -1
      }

      info_index    <- order(info, decreasing = TRUE)
      item_selected <- info_index[1]

      if (item_selected %in% output@administered_item_index[1:(position - 1)]) {
        stop(sprintf("the selected item %i has been already administered", item_selected))
      }

      return(item_selected)
    }

    #####
    ###    select a non-administered item with the largest information
    #####

    selectItemShadowTest <- function() {

      n_remaining <- constants$test_length - position
      new_stimulus_selected <- FALSE
      last_stimulus_index <- 0

      if (!constants$set_based) {
        stimulus_selected <- NA
        stimulus_finished <- FALSE
      }

      if (position == 1) {
        selected <- 1

        if (constants$set_based) {
          stimulus_selected <- optimal$shadow_test[["STINDEX"]][1]
          new_stimulus_selected <- TRUE

          if (!is.na(stimulus_selected)) {
            # If the selected item is not a discrete item
            if (sum(optimal$shadow_test[["STINDEX"]] == stimulus_selected, na.rm = TRUE) == 1) {
              stimulus_finished <- TRUE
            } else {
              stimulus_finished <- FALSE
            }
          } else {
            # If the selected item is a discrete item, mark it as the last item of its belonging stimulus
            stimulus_finished <- TRUE
          }

        }

      } else {

        remaining <- which(!optimal$shadow_test[["INDEX"]] %in% output@administered_item_index[1:(position - 1)])

        if (!constants$set_based) {

          selected <- remaining[1]

        } else {

          last_stimulus_index <- output@administered_stimulus_index[position - 1]

          if (!is.na(last_stimulus_index)) {
            # If the previous item was not a discrete item

            # If there are any items left within the stimulus
            if (any(optimal$shadow_test[["STINDEX"]][remaining] == last_stimulus_index, na.rm = TRUE)) {
              remaining_in_stimulus <- remaining[which(optimal$shadow_test[["STINDEX"]][remaining] == last_stimulus_index)]
              selected <- remaining_in_stimulus[1]
            } else {
              selected <- remaining[1]
            }
          } else {
            # If the previous item was a discrete item
            selected <- remaining[1]
          }

          stimulus_selected <- optimal$shadow_test[["STINDEX"]][selected]

          if (last_stimulus_index != stimulus_selected | is.na(last_stimulus_index != stimulus_selected)) {
            new_stimulus_selected <- TRUE
          }

          if (n_remaining == 0) {
            stimulus_finished <- TRUE
          } else {

            if (!is.na(stimulus_selected)) {

              # If the selected item is not a discrete item
              if (sum(optimal$shadow_test[["STINDEX"]][remaining] == stimulus_selected, na.rm = TRUE) == 1) {
                stimulus_finished <- TRUE
              } else {
                stimulus_finished <- FALSE
              }
            } else {
              stimulus_finished <- TRUE
            }
          }
        }
      }

      item_selected <- optimal$shadow_test[["INDEX"]][selected]

      return(
        list(
          item_selected = item_selected,
          stimulus_selected = stimulus_selected,
          stimulus_finished = stimulus_finished,
          last_stimulus_index = last_stimulus_index,
          new_stimulus_selected = new_stimulus_selected,
          n_remaining = n_remaining
        )
      )

    }

    #####
    ###    plot audit trail
    #####

    plotAuditTrail <- function() {

      old_mar   <- par()$mar
      old_mfrow <- par()$mfrow
      on.exit(par(mar = old_mar, mfrow = old_mfrow))
      par(mar = c(2, 3, 1, 1) + 0.1, mfrow = c(2, 1))

      plot(1:max_ni, seq(min_theta, max_theta, length = max_ni), main = paste0("Examinee ", j), xlab = "Items Administered", ylab = "Theta", type = "n", las = 1)
      points(1:max_ni, output@interim_theta_est, type = "b", pch = 9, col = "blue")

      if (!is.null(true_theta)) {
        abline(h = output@true_theta, lty = 2, col = "red")
      } else {
        abline(h = output@final_theta_est, lty = 2, col = "red")
      }

      item_string <- paste(output@administered_item_index, collapse = ",")
      text(1, max_theta, paste0("Items: ", item_string), cex = 0.7, adj = 0)
      text(1, min_theta + 0.3, paste("Theta: ", round(output@final_theta_est, digits = 2), " SE: ", round(output@final_se_est, digits = 2)), cex = 0.8, adj = 0)

      for (i in 1:max_ni) {
        lines(rep(i, 2), c(output@interim_theta_est[i] - 1.96 * output@interim_se_est[i], output@interim_theta_est[i] + 1.96 * output@interim_se_est[i]))
        if (constants$use_shadow) {
          if (output@shadow_test_refreshed[i]) {
            points(i, output@interim_theta_est[i], pch = 18, col = "red")
          }
        }
      }

      resp_string <- paste(output@administered_item_resp, collapse = ",")
      plot(constants$theta_q, output@posterior, main = "Final Posterior Distribution", xlab = "Theta", ylab = "Posterior", type = "l", col = "blue", yaxt = "n")
      text(min_theta, max(output@posterior), paste0("Responses: ", resp_string), cex = 0.7, adj = 0)

    }

    #####
    ###    Loop over nj simulees
    #####

    has_progress_pkg <- requireNamespace("progress")
    if (has_progress_pkg) {
      pb <- progress::progress_bar$new(
        format = "[:bar] :spin :current/:total (:percent) eta :eta",
        total = constants$nj, clear = FALSE)
      pb$tick(0)
    } else {
      pb <- txtProgressBar(0, constants$nj, char = "|", style = 3)
    }

    for (j in 1:constants$nj) {

      output <- new("output_Shadow")
      output@simulee_id <- j

      if (!is.null(true_theta)) {
        output@true_theta <- true_theta[j]
      } else {
        output@true_theta <- NULL
      }

      output@prior <- posterior[j, ]
      output@administered_item_index <- rep(NA_real_, constants$max_ni)
      output@administered_item_resp  <- rep(NA_real_, constants$max_ni)
      output@theta_segment_index     <- rep(NA_real_, constants$max_ni)
      output@interim_theta_est       <- rep(NA_real_, constants$max_ni)
      output@interim_se_est          <- rep(NA_real_, constants$max_ni)
      output@administered_stimulus_index <- NaN
      output@shadow_test <- vector(mode = "list", length = constants$max_ni)
      output@max_cat_pool <- pool@max_cat

      ##
      #  Simulee: set initial theta estimate
      ##

      if (config@interim_theta$method %in% c("EAP", "MLE")) {
        current_theta <- initial_theta[j]
      } else if (toupper(config@interim_theta$method) %in% c("EB", "FB")) {
        if (is.vector(prior_par) && length(prior_par) == 2) {
          output@prior_par <- prior_par
        } else if (is.matrix(prior_par) && all(dim(prior_par) == c(constants$nj, 2))) {
          output@prior_par <- prior_par[j, ]
        } else {
          output@prior_par <- config@interim_theta$prior_par
        }
        output@posterior_sample <- rnorm(n_sample, mean = output@prior_par[1], sd = output@prior_par[2])
        output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
        current_theta <- mean(output@posterior_sample)
        current_se <- sd(output@posterior_sample) * config@MCMC$jump_factor
      }

      ##
      #  Simulee: initialize stimulus tracking
      ##

      if (constants$set_based) {
        output@administered_stimulus_index <- rep(NA_real_, constants$max_ni)
        end_set <- TRUE
        finished_stimulus_index      <- NULL
        finished_stimulus_item_count <- NULL
      }

      ##
      #  Simulee: initialize shadow test stuff
      ##

      if (constants$use_shadow) {
        output@shadow_test_feasible  <- logical(constants$test_length)
        output@shadow_test_refreshed <- logical(constants$test_length)
        imat <- NULL
        idir <- NULL
        irhs <- NULL
        if (constants$set_based) {
          smat <- NULL
          sdir <- NULL
          srhs <- NULL
        }
      }

      likelihood   <- rep(1, nq)
      theta_change <- 10000
      done         <- FALSE
      position     <- 0

      ##
      #  Simulee: flag ineligibile items
      ##

      if (exposure_control %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {

        # Randomly flag items in each segment to be ineligible

        ineligible_i <- matrix(0, n_segment, ni)
        prob_random  <- matrix(runif(n_segment * ni), n_segment, ni)
        ineligible_i[prob_random >= pe_i] <- 1

        # Randomly flag stimuli in each segment to be ineligible

        if (constants$set_based) {
          ineligible_s <- matrix(0, n_segment, ns)
          prob_random <- matrix(runif(n_segment * ns), n_segment, ns)
          ineligible_s[prob_random >= pe_s] <- 1

          for (k in 1:n_segment) {
            for (s in which(ineligible_s[k, ] == 1)) {
              ineligible_i[k, constraints@item_index_by_stimulus[[s]]] <- 1
            }
            for (s in which(ineligible_s[k, ] == 0)) {
              ineligible_i[k, constraints@item_index_by_stimulus[[s]]] <- 0
            }
          }

        }

        if (exposure_control %in% c("ELIGIBILITY")) {
          xmat <- NULL
          xdir <- NULL
          xrhs <- NULL
        }

      }

      ##
      #  Simulee: administer (test_length) items
      ##

      position = 0

      while (!done) {

        position <- position + 1
        info     <- getInfo()

        # Item position / simulee: do shadow test stuff

        if (constants$use_shadow) {

          if (exposure_control %in% c("ELIGIBILITY", "BIGM")) {

            # Item position / simulee: get which theta segment are we on

            if (!is.null(config@exposure_control$first_segment) &&
              length(config@exposure_control$first_segment) >= position &&
              all(config@exposure_control$first_segment >= 1) &&
              all(config@exposure_control$first_segment <= n_segment)) {
              output@theta_segment_index[position] <- config@exposure_control$first_segment[position]
            } else {
              output@theta_segment_index[position] <- find_segment(current_theta, segment_cut)
            }

          } else if (exposure_control %in% c("BIGM-BAYESIAN")) {

            sample_segment <- find_segment(output@posterior_sample, segment_cut)
            segment_distribution <- table(sample_segment) / length(sample_segment)
            segment_classified <- as.numeric(names(segment_distribution))
            segment_prob <- numeric(n_segment)
            segment_prob[segment_classified] <- segment_distribution
            output@theta_segment_index[position] <- which.max(segment_prob)

          }

          # Item position / simulee: refresh shadow test

          if (position == 1 ||
            (refresh_policy == "ALWAYS") ||
            (refresh_policy %in% c("POSITION", "INTERVAL") && refresh_shadow[position]) ||
            (refresh_policy == "THRESHOLD" && abs(theta_change) > config@refresh_policy$threshold) ||
            (refresh_policy == "INTERVAL-THRESHOLD" && refresh_shadow[position] && abs(theta_change) > config@refresh_policy$threshold) ||
            (constants$set_based_refresh && constants$set_based && end_set)) {

            output@shadow_test_refreshed[position] <- TRUE

            administered_stimulus_index <- na.omit(unique(output@administered_stimulus_index))

            if (position > 1) {

              # Include administered items in selection

              imat <- matrix(0, nrow = position - 1, ncol = nv)

              for (p in 1:(position - 1)) {
                imat[p, output@administered_item_index[p]] <- 1
              }

              idir <- rep("==", position - 1)
              irhs <- rep(1   , position - 1)

              # Include administered stimulus in selection

              if (constants$set_based) {

                if (length(administered_stimulus_index) > 0) {

                  smat <- matrix(0, nrow = length(administered_stimulus_index), ncol = nv)

                  for (s in 1:length(administered_stimulus_index)) {
                    smat[s, ni + administered_stimulus_index[s]] <- 1
                  }

                  sdir <- rep("==", length(administered_stimulus_index))
                  srhs <- rep(1, length(administered_stimulus_index))

                  imat <- rbind(imat, smat)
                  idir <-     c(idir, sdir)
                  irhs <-     c(irhs, srhs)

                  if (constants$set_based_refresh && constants$set_based && end_set) {

                    n_administered_stimulus <- length(administered_stimulus_index)
                    if (n_administered_stimulus > 0) {
                      smat <- matrix(0, nrow = n_administered_stimulus, ncol = nv)
                      sdir <- rep("==", n_administered_stimulus)
                      srhs <- numeric(n_administered_stimulus)
                      for (s in 1:n_administered_stimulus) {
                        smat[s, constraints@item_index_by_stimulus[[administered_stimulus_index[s]]]] <- 1
                        srhs[s] <- sum(output@administered_stimulus_index[1:(position - 1)] == administered_stimulus_index[s], na.rm = TRUE)
                      }
                      imat <- rbind(imat, smat)
                      idir <-     c(idir, sdir)
                      irhs <-     c(irhs, srhs)
                    }

                  } else {

                    n_finished_stimulus <- length(finished_stimulus_index)

                    if (n_finished_stimulus > 0) {
                      smat <- matrix(0, nrow = n_finished_stimulus, ncol = nv)
                      sdir <- rep("==", n_finished_stimulus)
                      srhs <- finished_stimulus_item_count
                      for (s in 1:n_finished_stimulus) {
                        smat[s, constraints@item_index_by_stimulus[[finished_stimulus_index[s]]]] <- 1
                      }
                      imat <- rbind(imat, smat)
                      idir <-     c(idir, sdir)
                      irhs <-     c(irhs, srhs)
                    }

                  }
                }
              }
            }

            # Do exposure control stuff

            if (item_eligibility_control) {

              # Get ineligibile items in the current theta segment

              item_ineligible <- ineligible_i[output@theta_segment_index[position], ]
              if (constants$set_based) {
                stimulus_ineligible <- ineligible_s[output@theta_segment_index[position], ]
              }

              if (position > 1) {
                item_ineligible[output@administered_item_index[1:(position - 1)]] <- 0
                if (constants$set_based) {
                  stimulus_ineligible[output@administered_stimulus_index[1:(position - 1)]] <- 0
                }
              }

              if (exposure_control %in% c("ELIGIBILITY")) {

                # Do eligibility-based exposure control
                # get xmat representing ineligible items

                if (any(item_ineligible == 1)) {

                  xmat <- numeric(nv)
                  xmat[1:ni] <- item_ineligible
                  xdir <- "=="
                  xrhs <- 0

                  if (constants$set_based) {
                    if (any(stimulus_ineligible == 1)) {
                      xmat[(ni + 1):nv] <- stimulus_ineligible
                      for (s in which(stimulus_ineligible == 1)) {
                        xmat[constraints@item_index_by_stimulus[[s]]] <- 1
                      }
                      for (s in which(stimulus_ineligible == 0)) {
                        xmat[constraints@item_index_by_stimulus[[s]]] <- 0
                      }
                    }
                  }

                }

                xdata = list(xmat = rbind(xmat, imat),
                             xdir =     c(xdir, idir),
                             xrhs =     c(xrhs, irhs))

                optimal    <- runAssembly(config, constraints, xdata = xdata, objective = info)
                is_optimal <- isOptimal(optimal$status, config@MIP$solver)

                # If not optimal, retry without xmat

                if (is_optimal) {

                  output@shadow_test_feasible[position] <- TRUE

                } else {

                  output@shadow_test_feasible[position] <- FALSE

                  xdata = list(xmat = imat,
                               xdir = idir,
                               xrhs = irhs)

                  optimal <- runAssembly(config, constraints, xdata = xdata, objective = info)

                }

              } else if (exposure_control %in% c("BIGM", "BIGM-BAYESIAN")) {

                # Do Big-M based exposure control
                # Penalize item info

                if (!is.null(config@exposure_control$M)) {
                  info[item_ineligible == 1] <- info[item_ineligible == 1] - config@exposure_control$M
                } else {
                  info[item_ineligible == 1] <- -1 * all_data$max_info - 1
                }

                xdata = list(xmat = imat,
                             xdir = idir,
                             xrhs = irhs)

                optimal <- runAssembly(config, constraints, xdata = xdata, objective = info)
                output@shadow_test_feasible[position] <- TRUE

              }

            } else {

              # No exposure control

              xdata = list(xmat = imat,
                           xdir = idir,
                           xrhs = irhs)

              optimal <- runAssembly(config, constraints, xdata = xdata, objective = info)
              output@shadow_test_feasible[position] <- TRUE

            }

            is_optimal <- isOptimal(optimal$status, config@MIP$solver)
            if (!is_optimal) {
              warning(notOptimal(optimal$status, config@MIP$solver))
              stop(sprintf("MIP solver returned non-zero status at examinee %i position %i", j, position))
            }

            output@solve_time[position] <- optimal$solve_time

          } else {

            # Do not refresh shadow test

            output@shadow_test_refreshed[position] <- FALSE
            output@shadow_test_feasible[position]  <- TRUE

          }

          # Select an item from shadow test

          selection <- selectItemShadowTest()
          output@administered_item_index[position] <- selection$item_selected
          output@shadow_test[[position]]           <- optimal$shadow_test[["INDEX"]]

        } else {

          # If not doing shadow

          output@administered_item_index[position] <- selectItem()

        }

        # Item position / simulee: record which stimulus was administered

        if (constants$set_based) {
          output@administered_stimulus_index[position] <- selection$stimulus_selected
          if (selection$stimulus_finished) {
            end_set <- TRUE
          } else {
            end_set <- FALSE
          }

          # TODO: why is selection$last_stimulus_index == 0 at position 1?

          if (!is.na(selection$last_stimulus_index)) {
            if (selection$new_stimulus_selected && selection$last_stimulus_index > 0) {
              finished_stimulus_index      <- c(finished_stimulus_index, selection$last_stimulus_index)
              finished_stimulus_item_count <- c(finished_stimulus_item_count, sum(output@administered_stimulus_index[1:(position - 1)] == selection$last_stimulus_index, na.rm = TRUE))
            }
          }

        }

        # Item position / simulee: record which item was administered

        output@administered_item_resp[position] <- all_data$test@data[j, output@administered_item_index[position]]
        output@administered_item_ncat[position] <- pool@NCAT[output@administered_item_index[position]]
        items_administered[j, output@administered_item_index[position]] <- TRUE

        # Item position / simulee: update posterior and likelihood
        prob_resp      <- all_data$test@prob[[output@administered_item_index[position]]][, output@administered_item_resp[position] + 1]
        posterior[j, ] <- posterior[j, ] * prob_resp
        likelihood     <- likelihood     * prob_resp

        # Item position / simulee: estimate theta

        if (toupper(config@interim_theta$method) == "EAP") {
          output@interim_theta_est[position] <- sum(posterior[j, ] * config@theta_grid) / sum(posterior[j, ])
          output@interim_se_est[position]    <- sqrt(sum(posterior[j, ] * (config@theta_grid - output@interim_theta_est[position])^2) / sum(posterior[j, ]))
          if (toupper(config@interim_theta$prior_dist) == "NORMAL" && config@interim_theta$shrinkage_correction) {
            output@interim_theta_est[position] <- output@interim_theta_est[position] * (1 + output@interim_se_est[position]^2)
            if (output@interim_se_est[position] < config@interim_theta$prior_par[2]) {
              output@interim_se_est[position] <- 1 / sqrt(1 / output@interim_se_est[position]^2 - 1 / config@interim_theta$prior_par[2]^2)
            }
          }
        } else if (toupper(config@interim_theta$method) == "MLE") {
          interim_EAP <- sum(posterior[j, ] * config@theta_grid) / sum(posterior[j, ])
          interim_MLE <- mle(pool, output@administered_item_resp[1:position], start_theta = interim_EAP, theta_range = config@interim_theta$bound_ml, max_iter = config@interim_theta$max_iter, crit = config@interim_theta$crit, select = output@administered_item_index[1:position])
          output@interim_theta_est[position] <- interim_MLE$th
          output@interim_se_est[position]    <- interim_MLE$se
        } else if (toupper(config@interim_theta$method) %in% c("EB", "FB")) {
          current_item <- output@administered_item_index[position]
          if (toupper(config@interim_theta$method == "EB")) {
            output@posterior_sample <- theta_EB_single(
              n_sample, current_theta, current_se,
              pool@ipar[current_item, ],
              output@administered_item_resp[position], pool@NCAT[current_item],
              model[current_item], 1, c(current_theta, current_se)
            )
          } else {
            output@posterior_sample <- theta_FB_single(
              n_sample, current_theta, current_se, ipar_list[[current_item]],
              pool@ipar[current_item, ],
              output@administered_item_resp[position], pool@NCAT[current_item],
              model[current_item], 1, c(current_theta, current_se)
            )
          }
          output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
          output@interim_theta_est[position] <- mean(output@posterior_sample)
          output@interim_se_est[position] <- sd(output@posterior_sample)
        }

        theta_change  <- output@interim_theta_est[position] - current_theta
        current_theta <- output@interim_theta_est[position]
        current_se    <- output@interim_se_est[position]


        # Item position / simulee: trigger shadow test refresh if theta change is sufficient

        if (toupper(config@refresh_policy$method) == "THRESHOLD") {
          if ((abs(theta_change) > config@refresh_policy$threshold) && (position < constants$test_length)) {
            refresh_shadow[position + 1] <- TRUE
          }
        }

        # Item position / simulee: prepare for the next item position

        if (position == constants$max_ni) {
          done <- TRUE
          output@likelihood <- likelihood
          output@posterior  <- posterior[j, ]
        }

        if (has_progress_pkg) {
          pb$tick(0)
        }

      }

      ##
      #  Simulee: test complete, estimate theta
      ##

      if (identical(config@final_theta, config@interim_theta)) {

        # Skip final theta estimation if methods are identical

        output@final_theta_est <- output@interim_theta_est[position]
        output@final_se_est    <- output@interim_se_est[position]

      } else if (toupper(config@final_theta$method == "EAP")) {

        if (toupper(config@final_theta$prior_dist) == "NORMAL") {
          final_prior <- dnorm(constants$theta_q, mean = config@final_theta$prior_par[1], sd = config@final_theta$prior_par[2])
        } else if (toupper(config@final_theta$prior_dist) == "UNIFORM") {
          final_prior <- rep(1, nq)
        }

        output@posterior       <- output@likelihood * final_prior
        output@final_theta_est <- sum(output@posterior * constants$theta_q) / sum(output@posterior)
        output@final_se_est    <- sqrt(sum(output@posterior * (constants$theta_q - output@final_theta_est)^2) / sum(output@posterior))

        if (toupper(config@final_theta$prior_dist) == "NORMAL" && config@final_theta$shrinkage_correction) {
          output@final_theta_est <- output@final_theta_est * (1 + output@final_se_est^2)
          if (output@final_se_est < config@final_theta$prior_par[2]) {
            output@final_se_est <- 1 / sqrt(1 / output@final_se_est^2 - 1 / config@final_theta$prior_par[2]^2)
          }
        }

      } else if (toupper(config@final_theta$method) == "MLE") {

        final_MLE <- mle(pool, output@administered_item_resp[1:constants$max_ni], start_theta = output@interim_theta_est[constants$max_ni], theta_range = config@final_theta$bound_ml, max_iter = config@final_theta$max_iter, crit = config@final_theta$crit, select = output@administered_item_index[1:constants$max_ni], truncate = config@final_theta$truncateML)
        output@final_theta_est <- final_MLE$th
        output@final_se_est    <- final_MLE$se

      } else if (toupper(config@final_theta$method) %in% c("EB", "FB")) {

        if (toupper(config@interim_theta$method) == toupper(config@final_theta$method) && identical(config@interim_theta$prior_par, config@final_theta$prior_par)) {

          output@final_theta_est <- output@interim_theta_est[position]
          output@final_se_est    <- output@interim_se_est[position]

        } else {

          if (toupper(config@final_theta$method) %in% c("EB", "FB")) {
            if (is.vector(prior_par) && length(prior_par) == 2) {
              output@prior_par <- prior_par
            } else if (is.matrix(prior_par) && all(dim(prior_par) == c(constants$nj, 2))) {
              output@prior_par <- prior_par[j, ]
            } else {
              output@prior_par <- config@final_theta$prior_par
            }
          }

          output@posterior_sample <- rnorm(n_sample, mean = output@prior_par[1], sd = output@prior_par[2])
          output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
          current_theta <- mean(output@posterior_sample)
          current_se    <- sd(output@posterior_sample) * config@MCMC$jump_factor

          if (toupper(config@final_theta$method == "EB")) {
            output@posterior_sample <- theta_EB(
              n_sample, current_theta, current_se,
              pool@ipar[output@administered_item_index[1:position], ],
              output@administered_item_resp[1:position], pool@NCAT[output@administered_item_index[1:position]],
              model[output@administered_item_index[1:position]], 1, c(current_theta, current_se)
            )
          } else if (toupper(config@final_theta$method == "FB")) {
            output@posterior_sample <- theta_FB(
              n_sample, current_theta, current_se, ipar_list[output@administered_item_index[1:position]],
              pool@ipar[output@administered_item_index[1:position], ],
              output@administered_item_resp[1:position], pool@NCAT[output@administered_item_index[1:position]],
              model[output@administered_item_index[1:position]], 1, c(current_theta, current_se)
            )
          }

          output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
          output@final_theta_est  <- mean(output@posterior_sample)
          output@final_se_est     <- sd(output@posterior_sample)

        }

      }

      ##
      #  Simulee: record item usage
      ##

      usage_matrix[j, output@administered_item_index] <- TRUE
      if (constants$set_based) {
        usage_matrix[j, constants$ni + output@administered_stimulus_index] <- TRUE
      }

      output_list[[j]] <- output

      ##
      #  Simulee: do exposure control
      ##

      if (item_eligibility_control) {
        if (!is.null(true_theta)) {
          segment_true <- find_segment(output@true_theta, segment_cut)
          output_list[[j]]@true_theta_segment <- segment_true
          true_segment_freq[segment_true] <- true_segment_freq[segment_true] + 1
          true_segment_count[j]           <- true_segment_freq[segment_true]
        }
        segment_final <- find_segment(output@final_theta_est, segment_cut)
        eligible_in_final_segment <- ineligible_i[segment_final, ] == 0
        est_segment_freq[segment_final] <- est_segment_freq[segment_final] + 1
        est_segment_count[j]            <- est_segment_freq[segment_final]

        # TODO: Why are we sorting segment_visited? might be better to not sort
        segment_visited <- sort(unique(output@theta_segment_index))
        segment_other   <- segment_visited[segment_visited != segment_final]

        if (constants$set_based) {
          eligible_set_in_final_segment <- ineligible_s[segment_final, ] == 0
        }

        if (exposure_control %in% c("ELIGIBILITY")) {
          n_jk[segment_final] <- fading_factor * n_jk[segment_final] + 1
          alpha_ijk[segment_final, ] <- fading_factor * alpha_ijk[segment_final, ]
          alpha_ijk[segment_final, output@administered_item_index] <-
          alpha_ijk[segment_final, output@administered_item_index] + 1

          if (length(segment_other) > 0) {
            if (any(!eligible_in_final_segment[output@administered_item_index])) {
              for (k in segment_other) {
                for (i in output@administered_item_index[output@theta_segment_index == k]) {
                  if (!eligible_in_final_segment[i]) {
                    alpha_ijk[k, i] <- alpha_ijk[k, i] + 1
                  }
                }
              }
            }
          }

          if (fading_factor != 1) {
            no_fading_n_jk[segment_final] <- no_fading_n_jk[segment_final] + 1
            no_fading_alpha_ijk[segment_final, output@administered_item_index] <-
            no_fading_alpha_ijk[segment_final, output@administered_item_index] + 1
          }

          segment_feasible   <- unique(output@theta_segment_index[output@shadow_test_feasible == TRUE])
          segment_infeasible <- unique(output@theta_segment_index[output@shadow_test_feasible == FALSE])
          phi_jk[segment_final]    <- fading_factor * phi_jk[segment_final]
          rho_ijk[segment_final, ] <- fading_factor * rho_ijk[segment_final, ]

          if (segment_final %in% segment_feasible) {
            phi_jk[segment_final] <- phi_jk[segment_final] + 1
            rho_ijk[segment_final, eligible_in_final_segment] <-
            rho_ijk[segment_final, eligible_in_final_segment] + 1
            if (fading_factor != 1) {
              no_fading_rho_ijk[segment_final, eligible_in_final_segment] <-
              no_fading_rho_ijk[segment_final, eligible_in_final_segment] + 1
            }
          } else {
            rho_ijk[segment_final, ] <- rho_ijk[segment_final, ] + 1
            if (fading_factor != 1) {
              no_fading_rho_ijk[segment_final, ] <-
              no_fading_rho_ijk[segment_final, ] + 1
            }
          }

          nf_ijk <- matrix(n_jk / phi_jk, n_segment, ni)

          if (acceleration_factor > 1) {
            p_alpha_ijk <- alpha_ijk / matrix(n_jk, n_segment, ni)
            p_rho_ijk <- rho_ijk / matrix(n_jk, n_segment, ni)
            p_alpha_ijk[is.na(p_alpha_ijk)] <- 0
            p_rho_ijk[is.na(p_rho_ijk)] <- 1
            flag_alpha_ijk <- p_alpha_ijk > max_exposure_rate
            if (length(max_exposure_rate) == n_segment) {
              for (k in 1:n_segment) {
                pe_i[k,  flag_alpha_ijk[k, ]] <- 1 - nf_ijk[k,  flag_alpha_ijk[k, ]] + (max_exposure_rate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^acceleration_factor * nf_ijk[k, flag_alpha_ijk[k, ]] * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                pe_i[k, !flag_alpha_ijk[k, ]] <- 1 - nf_ijk[k, !flag_alpha_ijk[k, ]] + max_exposure_rate[k] * nf_ijk[k, !flag_alpha_ijk[k, ]] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
              }
            } else {
              pe_i[ flag_alpha_ijk] <- 1 - nf_ijk[ flag_alpha_ijk] + (max_exposure_rate / p_alpha_ijk[flag_alpha_ijk])^acceleration_factor * nf_ijk[flag_alpha_ijk] * p_rho_ijk[flag_alpha_ijk]
              pe_i[!flag_alpha_ijk] <- 1 - nf_ijk[!flag_alpha_ijk] + max_exposure_rate * nf_ijk[!flag_alpha_ijk] * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
            }
          } else {
            pe_i <- 1 - nf_ijk + max_exposure_rate * nf_ijk * rho_ijk / alpha_ijk
          }

          pe_i[is.na(pe_i) | alpha_ijk == 0] <- 1
          pe_i[pe_i > 1] <- 1

          if (constants$set_based) {
            alpha_sjk[segment_final, ] <- fading_factor * alpha_sjk[segment_final, ]
            alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] <-
            alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] + 1

            if (length(segment_other) > 0) {
              if (any(!eligible_set_in_final_segment[administered_stimulus_index])) {
                for (k in segment_other) {
                  for (s in unique(output@administered_stimulus_index[output@theta_segment_index == k & output@administered_stimulus_index %in% administered_stimulus_index])) {
                    if (!eligible_set_in_final_segment[s]) {
                      alpha_sjk[k, s] <- alpha_sjk[k, s] + 1
                    }
                  }
                }
              }
            }

            if (fading_factor != 1) {
              no_fading_alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] <-
              no_fading_alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] + 1
            }
            rho_sjk[segment_final, ] <- fading_factor * rho_sjk[segment_final, ]
            if (segment_final %in% segment_feasible) {
              rho_sjk[segment_final, eligible_set_in_final_segment] <-
              rho_sjk[segment_final, eligible_set_in_final_segment] + 1
              if (fading_factor != 1) {
                no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] <-
                no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] + 1
              }
            } else {
              rho_sjk[segment_final, ] <- rho_sjk[segment_final, ] + 1
              if (fading_factor != 1) {
                no_fading_rho_sjk[segment_final, ] <- no_fading_rho_sjk[segment_final, ] + 1
              }
            }
            nf_sjk <- matrix(n_jk / phi_jk, n_segment, ns)
            if (acceleration_factor > 1) {
              p_alpha_sjk <- alpha_sjk / matrix(n_jk, n_segment, ns)
              p_rho_sjk   <- rho_sjk / matrix(n_jk, n_segment, ns)
              p_alpha_sjk[is.na(p_alpha_sjk)] <- 0
              p_rho_sjk[is.na(p_rho_sjk)]     <- 1
              flag_alpha_sjk <- p_alpha_sjk > max_exposure_rate
              if (length(max_exposure_rate) == n_segment) {
                for (k in 1:n_segment) {
                  pe_s[k,  flag_alpha_sjk[k, ]] <- 1 - nf_sjk[k,  flag_alpha_sjk[k, ]] +
                    (max_exposure_rate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^acceleration_factor * nf_sjk[k, flag_alpha_sjk[k, ]] * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                  pe_s[k, !flag_alpha_sjk[k, ]] <- 1 - nf_sjk[k, !flag_alpha_sjk[k, ]] +
                    max_exposure_rate[k] * nf_sjk[k, !flag_alpha_sjk[k, ]] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                }
              } else {
                pe_s[ flag_alpha_sjk] <- 1 - nf_sjk[ flag_alpha_sjk] + (max_exposure_rate / p_alpha_sjk[flag_alpha_sjk])^acceleration_factor * nf_sjk[flag_alpha_sjk] * p_rho_sjk[flag_alpha_sjk]
                pe_s[!flag_alpha_sjk] <- 1 - nf_sjk[!flag_alpha_sjk] + max_exposure_rate * nf_sjk[!flag_alpha_sjk] * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
              }
            } else {
              pe_s <- 1 - nf_sjk + max_exposure_rate * nf_sjk * rho_sjk / alpha_sjk
            }
            pe_s[is.na(pe_s) | alpha_sjk == 0] <- 1
            pe_s[pe_s > 1] <- 1
          }

        } else if (exposure_control %in% c("BIGM")) {

          n_jk[segment_final] <- fading_factor * n_jk[segment_final] + 1
          alpha_ijk[segment_final, ] <- fading_factor * alpha_ijk[segment_final, ]
          alpha_ijk[segment_final, output@administered_item_index] <-
          alpha_ijk[segment_final, output@administered_item_index] + 1

          # Visited segments that are not the final segment
          if (length(segment_other) > 0) {
            if (any(!eligible_in_final_segment[output@administered_item_index])) {
              for (k in segment_other) {
                for (i in output@administered_item_index[output@theta_segment_index == k]) {
                  if (!eligible_in_final_segment[i]) {
                    alpha_ijk[k, i] <- alpha_ijk[k, i] + 1
                  }
                }
              }
            }
          }

          rho_ijk[segment_final, ] <- fading_factor * rho_ijk[segment_final, ]
          rho_ijk[segment_final, eligible_in_final_segment] <-
          rho_ijk[segment_final, eligible_in_final_segment] + 1

          if (fading_factor != 1) {
            no_fading_n_jk[segment_final] <- no_fading_n_jk[segment_final] + 1
            no_fading_alpha_ijk[segment_final, output@administered_item_index] <-
            no_fading_alpha_ijk[segment_final, output@administered_item_index] + 1
            no_fading_rho_ijk[segment_final, eligible_in_final_segment] <-
            no_fading_rho_ijk[segment_final, eligible_in_final_segment] + 1
          }

          if (acceleration_factor > 1) {

            p_alpha_ijk <- alpha_ijk / matrix(n_jk, n_segment, ni)
            p_rho_ijk   <- rho_ijk   / matrix(n_jk, n_segment, ni)
            p_alpha_ijk[is.na(p_alpha_ijk)] <- 0
            p_rho_ijk[is.na(p_rho_ijk)]     <- 1
            flag_alpha_ijk <- p_alpha_ijk > max_exposure_rate
            if (length(max_exposure_rate) == n_segment) {
              for (k in 1:n_segment) {
                pe_i[k, flag_alpha_ijk[k, ]]  <- (max_exposure_rate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^acceleration_factor * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                pe_i[k, !flag_alpha_ijk[k, ]] <- max_exposure_rate[k] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
              }
            } else {
              pe_i[flag_alpha_ijk]  <- (max_exposure_rate / p_alpha_ijk[flag_alpha_ijk])^acceleration_factor * p_rho_ijk[flag_alpha_ijk]
              pe_i[!flag_alpha_ijk] <- max_exposure_rate * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
            }

          } else {
            pe_i <- max_exposure_rate * rho_ijk / alpha_ijk
          }

          pe_i[is.na(pe_i) | alpha_ijk == 0] <- 1
          pe_i[pe_i > 1] <- 1

          if (constants$set_based) {
            alpha_sjk[segment_final, ] <- fading_factor * alpha_sjk[segment_final, ]
            alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] <-
            alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] + 1
            rho_sjk[segment_final, ] <- fading_factor * rho_sjk[segment_final, ]
            rho_sjk[segment_final, eligible_set_in_final_segment] <-
            rho_sjk[segment_final, eligible_set_in_final_segment] + 1

            if (length(segment_other) > 0) {
              if (any(!eligible_set_in_final_segment[administered_stimulus_index], na.rm = T)) {
                for (k in segment_other) {
                  for (s in unique(output@administered_stimulus_index[output@theta_segment_index == k & output@administered_stimulus_index %in% administered_stimulus_index])) {
                    if (!eligible_set_in_final_segment[s]) {
                      alpha_sjk[k, s] <- alpha_sjk[k, s] + 1
                    }
                  }
                }
              }
            }

            if (fading_factor != 1) {
              no_fading_alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] <-
              no_fading_alpha_sjk[segment_final, na.omit(output@administered_stimulus_index)] + 1
              no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] <-
              no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] + 1
            }

            if (acceleration_factor > 1) {
              p_alpha_sjk <- alpha_sjk / matrix(n_jk, n_segment, constants$ns)
              p_rho_sjk   <- rho_sjk / matrix(n_jk, n_segment, constants$ns)
              p_alpha_sjk[is.na(p_alpha_sjk)] <- 0
              p_rho_sjk[is.na(p_rho_sjk)]     <- 1
              flag_alpha_sjk <- p_alpha_sjk > max_exposure_rate
              if (length(max_exposure_rate) == n_segment) {
                for (k in 1:n_segment) {
                  pe_s[k, flag_alpha_sjk[k, ]]  <- (max_exposure_rate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^acceleration_factor * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                  pe_s[k, !flag_alpha_sjk[k, ]] <- max_exposure_rate[k] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                }
              } else {
                pe_s[flag_alpha_sjk]  <- (max_exposure_rate / p_alpha_sjk[flag_alpha_sjk])^acceleration_factor * p_rho_sjk[flag_alpha_sjk]
                pe_s[!flag_alpha_sjk] <- max_exposure_rate * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
              }

            } else {
              pe_s <- max_exposure_rate * rho_sjk / alpha_sjk
            }
            pe_s[is.na(pe_s) | alpha_sjk == 0] <- 1
            pe_s[pe_s > 1] <- 1
          }

        } else if (exposure_control %in% c("BIGM-BAYESIAN")) {

          segment_visited <- sort(unique(output@theta_segment_index))
          sample_segment  <- find_segment(output@posterior_sample, segment_cut)
          segment_distribution <- table(sample_segment) / length(sample_segment)
          segment_classified   <- as.numeric(names(segment_distribution))
          segment_prob <- numeric(n_segment)
          segment_prob[segment_classified] <- segment_distribution

          n_jk      <- fading_factor * n_jk + segment_prob
          rho_ijk   <- fading_factor * rho_ijk
          alpha_ijk <- fading_factor * alpha_ijk
          alpha_ijk[, output@administered_item_index] <- alpha_ijk[, output@administered_item_index] + segment_prob

          if (length(segment_other) > 0) {
            if (any(!eligible_in_final_segment[output@administered_item_index])) {
              for (k in segment_other) {
                for (i in output@administered_item_index[output@theta_segment_index == k]) {
                  if (!eligible_in_final_segment[i]) {
                    alpha_ijk[k, i] <- alpha_ijk[k, i] + segment_prob[k]
                  }
                }
              }
            }
          }

          for (segment in 1:n_segment) {
            eligible <- ineligible_i[segment, ] == 0
            rho_ijk[segment, eligible] <- rho_ijk[segment, eligible] + segment_prob[segment]
          }
          if (fading_factor != 1) {
            no_fading_n_jk <- no_fading_n_jk + segment_prob
            no_fading_alpha_ijk[, output@administered_item_index] <- no_fading_alpha_ijk[, output@administered_item_index] + segment_prob
            for (segment in 1:n_segment) {
              eligible <- ineligible_i[segment, ] == 0
              no_fading_rho_ijk[segment, eligible] <- no_fading_rho_ijk[segment, eligible] + segment_prob[segment]
            }
          }
          if (acceleration_factor > 1) {
            p_alpha_ijk <- alpha_ijk / matrix(n_jk, n_segment, ni)
            p_rho_ijk   <- rho_ijk / matrix(n_jk, n_segment, ni)
            p_alpha_ijk[is.na(p_alpha_ijk)] <- 0
            p_rho_ijk[is.na(p_rho_ijk)]     <- 1
            flag_alpha_ijk <- p_alpha_ijk > max_exposure_rate
            if (length(max_exposure_rate) == n_segment) {
              for (k in 1:n_segment) {
                pe_i[k, flag_alpha_ijk[k, ]] <- (max_exposure_rate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^acceleration_factor * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                pe_i[k, !flag_alpha_ijk[k, ]] <- max_exposure_rate[k] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
              }
            } else {
              pe_i[flag_alpha_ijk]  <- (max_exposure_rate / p_alpha_ijk[flag_alpha_ijk])^acceleration_factor * p_rho_ijk[flag_alpha_ijk]
              pe_i[!flag_alpha_ijk] <- max_exposure_rate * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
            }

          } else {
            pe_i <- max_exposure_rate * rho_ijk / alpha_ijk
          }

          pe_i[is.na(pe_i) | alpha_ijk == 0] <- 1
          pe_i[pe_i > 1] <- 1

          if (constants$set_based) {
            alpha_sjk <- fading_factor * alpha_sjk
            rho_sjk   <- fading_factor * rho_sjk
            alpha_sjk[, output@administered_stimulus_index] <- alpha_sjk[, output@administered_stimulus_index] + segment_prob

            if (length(segment_other) > 0) {
              if (any(!eligible_set_in_final_segment[administered_stimulus_index])) {
                for (k in segment_other) {
                  for (s in unique(output@administered_stimulus_index[output@theta_segment_index == k & output@administered_stimulus_index %in% administered_stimulus_index])) {
                    if (!eligible_set_in_final_segment[s]) {
                      alpha_sjk[k, s] <- alpha_sjk[k, s] + segment_prob[k]
                    }
                  }
                }
              }
            }

            for (segment in 1:n_segment) {
              rho_sjk[segment, ineligible_s[segment, ] == 0] <- rho_sjk[segment, ineligible_s[segment, ] == 0] + segment_prob[segment]
            }
            if (fading_factor != 1) {
              no_fading_alpha_sjk[, output@administered_stimulus_index] <- no_fading_alpha_sjk[, output@administered_stimulus_index] + segment_prob
              for (segment in 1:n_segment) {
                no_fading_rho_sjk[segment, ineligible_s[segment, ] == 0] <- no_fading_rho_sjk[segment, ineligible_s[segment, ] == 0] + segment_prob[k]
              }
            }
            if (acceleration_factor > 1) {
              p_alpha_sjk <- alpha_sjk / matrix(n_jk, n_segment, constants$ns)
              p_rho_sjk <- rho_sjk / matrix(n_jk, n_segment, constants$ns)
              p_alpha_sjk[is.na(p_alpha_sjk)] <- 0
              p_rho_sjk[is.na(p_rho_sjk)] <- 1
              flag_alpha_sjk <- p_alpha_sjk > max_exposure_rate
              if (length(max_exposure_rate) == n_segment) {
                for (k in 1:n_segment) {
                  pe_s[k, flag_alpha_sjk[k, ]]  <- (max_exposure_rate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^acceleration_factor * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                  pe_s[k, !flag_alpha_sjk[k, ]] <- max_exposure_rate[k] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                }
              } else {
                pe_s[flag_alpha_sjk]  <- (max_exposure_rate / p_alpha_sjk[flag_alpha_sjk])^acceleration_factor * p_rho_sjk[flag_alpha_sjk]
                pe_s[!flag_alpha_sjk] <- max_exposure_rate * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
              }
            } else {
              pe_s <- max_exposure_rate * rho_sjk / alpha_sjk
            }
            pe_s[is.na(pe_s) | alpha_sjk == 0] <- 1
            pe_s[pe_s > 1] <- 1
          }
        }

        if (config@exposure_control$diagnostic_stats) {

          for (g in 1:n_segment) {
            alpha_g_i[j, (g - 1) * constants$ni + 1:constants$ni]   <- alpha_ijk[g, ]
            epsilon_g_i[j, (g - 1) * constants$ni + 1:constants$ni] <- rho_ijk[g, ]
            if (constants$set_based) {
              alpha_g_s[j, (g - 1) * constants$ns + 1:constants$ns]   <- alpha_sjk[g, ]
              epsilon_g_s[j, (g - 1) * constants$ns + 1:constants$ns] <- rho_sjk[g, ]
            }
          }

          if (fading_factor != 1) {
            for (g in 1:n_segment) {
              no_fading_alpha_g_i[j, (g - 1) * constants$ni + 1:constants$ni]   <- no_fading_alpha_ijk[g, ]
              no_fading_epsilon_g_i[j, (g - 1) * constants$ni + 1:constants$ni] <- no_fading_rho_ijk[g, ]
              if (constants$set_based) {
                no_fading_alpha_g_s[j, (g - 1) * constants$ns + 1:constants$ns]   <- no_fading_alpha_sjk[g, ]
                no_fading_epsilon_g_s[j, (g - 1) * constants$ns + 1:constants$ns] <- no_fading_rho_sjk[g, ]
              }
            }
          }

        }

      }

      if (config@audit_trail) {
        plotAuditTrail()
      }

      if (!is.null(session)) {
        shinyWidgets::updateProgressBar(session = session, id = "pb", value = j, total = constants$nj)
      } else {
        if (has_progress_pkg) {
          pb$tick()
        } else {
          setTxtProgressBar(pb, j)
        }
      }

      ##
      #  Simulee: go to next simulee
      ##

    }

    if (!has_progress_pkg) {
      close(pb)
    }

    final_theta_est <- unlist(lapply(1:constants$nj, function(j) output_list[[j]]@final_theta_est))
    final_se_est    <- unlist(lapply(1:constants$nj, function(j) output_list[[j]]@final_se_est))

    #####
    ###    Get exposure rate from everyone
    #####

    if (!constants$set_based) {
      exposure_rate <- matrix(NA, constants$ni, 2)
      colnames(exposure_rate) <- c('Item', 'Item ER')
      exposure_rate[, 1] <- 1:constants$ni
      exposure_rate[, 2] <- colSums(usage_matrix) / constants$nj
    } else {
      exposure_rate <- matrix(NA, constants$ni, 4)
      colnames(exposure_rate) <- c('Item', 'Stimulus', 'Item ER', 'Stimulus ER')
      exposure_rate_raw <- colSums(usage_matrix) / constants$nj
      exposure_rate[, 1] <- 1:constants$ni
      exposure_rate[, 2] <- constraints@stimulus_index_by_item
      exposure_rate[, 3] <- exposure_rate_raw[1:constants$ni]
      exposure_rate[, 4] <- exposure_rate_raw[(constants$ni + 1):constants$nv][constraints@stimulus_index_by_item]
    }

    eligibility_stats           <- NULL
    check_eligibility_stats     <- NULL
    no_fading_eligibility_stats <- NULL

    #####
    ###    Get exposure control diagnostic stats
    #####

    if (item_eligibility_control) {

      eligibility_stats <- list(
        pe_i = pe_i, n_jk = n_jk,
        alpha_ijk = alpha_ijk, phi_jk = phi_jk, rho_ijk = rho_ijk,
        pe_s = pe_s,
        alpha_sjk = alpha_sjk, rho_sjk = rho_sjk)

      if (config@exposure_control$diagnostic_stats) {

        check_eligibility_stats <- as.data.frame(
          cbind(1:constants$nj, true_theta, find_segment(true_theta, segment_cut), true_segment_count, alpha_g_i, epsilon_g_i),
          row.names = NULL)

        names(check_eligibility_stats) <- c("Examinee", "TrueTheta", "TrueSegment", "TrueSegmentCount",
          paste("a", "g", rep(1:n_segment, rep(constants$ni, n_segment)), "i", rep(1:constants$ni, n_segment), sep = "_"),
          paste("e", "g", rep(1:n_segment, rep(constants$ni, n_segment)), "i", rep(1:constants$ni, n_segment), sep = "_"))

        if (constants$set_based) {
          check_eligibility_stats_stimulus <- as.data.frame(cbind(alpha_g_s, epsilon_g_s), row.names = NULL)
          names(check_eligibility_stats_stimulus) <- c(
            paste("a", "g", rep(1:n_segment, rep(constants$ns, n_segment)), "s", rep(1:constants$ns, n_segment), sep = "_"),
            paste("e", "g", rep(1:n_segment, rep(constants$ns, n_segment)), "s", rep(1:constants$ns, n_segment), sep = "_"))
          check_eligibility_stats <- cbind(check_eligibility_stats, check_eligibility_stats_stimulus)
        }

        if (fading_factor != 1) {
          no_fading_eligibility_stats <- as.data.frame(cbind(1:constants$nj, true_theta, find_segment(true_theta, segment_cut), true_segment_count, no_fading_alpha_g_i, no_fading_epsilon_g_i), row.names = NULL)
          names(no_fading_eligibility_stats) <- c("Examinee", "TrueTheta", "TrueSegment", "TrueSegmentCount",
            paste("a", "g", rep(1:n_segment, rep(constants$ni, n_segment)), "i", rep(1:constants$ni, n_segment), sep = "_"),
            paste("e", "g", rep(1:n_segment, rep(constants$ni, n_segment)), "i", rep(1:constants$ni, n_segment), sep = "_"))
          if (constants$set_based) {
            no_fading_eligibility_stats_stimulus <- as.data.frame(cbind(no_fading_alpha_g_s, no_fading_epsilon_g_s), row.names = NULL)
            names(no_fading_eligibility_stats_stimulus) <- c(
              paste("a", "g", rep(1:n_segment, rep(constants$ns, n_segment)), "s", rep(1:constants$ns, n_segment), sep = "_"),
              paste("e", "g", rep(1:n_segment, rep(constants$ns, n_segment)), "s", rep(1:constants$ns, n_segment), sep = "_"))
            no_fading_eligibility_stats <- cbind(no_fading_eligibility_stats, no_fading_eligibility_stats_stimulus)
          }
        }

      }
    }

    if (constants$use_shadow) {
      freq_infeasible <- table(unlist(lapply(1:constants$nj, function(j) sum(!output_list[[j]]@shadow_test_feasible))))
    } else {
      freq_infeasible <- NULL
    }

    out                             <- new("output_Shadow_all")
    out@output                      <- output_list
    out@pool                        <- pool
    out@config                      <- config
    out@true_theta                  <- true_theta
    out@constraints                 <- constraints
    out@prior                       <- prior
    out@prior_par                   <- prior_par
    out@data                        <- all_data$test@data
    out@final_theta_est             <- final_theta_est
    out@final_se_est                <- final_se_est
    out@exposure_rate               <- exposure_rate
    out@usage_matrix                <- usage_matrix
    out@true_segment_count          <- true_segment_count
    out@est_segment_count           <- est_segment_count
    out@eligibility_stats           <- eligibility_stats
    out@check_eligibility_stats     <- check_eligibility_stats
    out@no_fading_eligibility_stats <- no_fading_eligibility_stats
    out@freq_infeasible             <- freq_infeasible

    return(out)
  }
)

#' Add transparancy to color
#'
#' Add transparancy to color.
#'
#' @param color A vector of color names or RGB color codes.
#' @param alpha A vector of integers between 0 and 255 (0 = fully transparent, 255 = fully visible).
addTrans <- function(color, alpha) {
  if (length(color) != length(alpha) & !any(c(length(color), length(alpha)) == 1)) {
    stop("Vector lengths not correct")
  }
  if (length(color) == 1 & length(alpha) > 1) {
    color <- rep(color, length(alpha))
  }
  if (length(alpha) == 1 & length(color) > 1) {
    alpha <- rep(alpha, length(color))
  }
  num2hex <- function(x) {
    hex <- unlist(strsplit("0123456789ABCDEF", split = ""))
    return(paste(hex[(x - x %% 16) / 16 + 1], hex[x %% 16 + 1], sep = ""))
  }
  rgb <- rbind(col2rgb(color), alpha)
  res <- paste("#", apply(apply(rgb, 2, num2hex), 2, paste, collapse = ""), sep = "")
  return(res)
}

#' Draw item eligibility statistics plots
#'
#' Draw item eligibility statistics plots.
#'
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param object An object containing eligibility statistics generated by \code{\link{Shadow}}.
#' @param object_no_fading An object containing eligibility statistics generated without fading.
#' @param file The filename of an object containing eligibility statistics generated by \code{\link{Shadow}}.
#' @param file_no_fading The filename of an object containing eligibility statistics generated without fading.
#' @param segment A theta segment index.
#' @param items A vector of item indices to generate the plots.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param max_rate A target item exposure rate.
#' @param discard_first A integer identifying the first x simulees to discard as burn-in.
plotEligibilityStats <- function(config, object = NULL, object_no_fading = NULL, file = NULL, file_no_fading = NULL, segment = 1, items = c(1), file_pdf = NULL, max_rate = 0.25, discard_first = NULL) {
  fading_factor <- config@exposure_control$fading_factor
  if (!is.null(file_pdf)) {
    pdf(file = file_pdf)
  }
  if (is.null(object) && is.null(file)) {
    stop("Both object and file are NULL")
  } else if (!is.null(object)) {
    eligibility_stats <- object
  } else if (!is.null(file)) {
    eligibility_stats <- read.csv(file, header = TRUE, sep = ",")
  }

  eligibility_stats_no_fading <- NULL
  if (!is.null(object_no_fading)) {
    eligibility_stats_no_fading <- object_no_fading
  } else if (!is.null(file_no_fading)) {
    eligibility_stats_no_fading <- read.csv(file_no_fading, header = TRUE, sep = ",")
  }

  eligibility_stats_segment <- split(eligibility_stats, eligibility_stats$TrueSegment)[[segment[1]]]
  if (!is.null(eligibility_stats_no_fading)) {
    eligibility_stats_segment_no_fading <- split(eligibility_stats_no_fading, eligibility_stats_no_fading$TrueSegment)[[segment[1]]]
  }
  if (!is.null(discard_first) && discard_first < nrow(eligibility_stats_segment)) {
    eligibility_stats_segment <- eligibility_stats_segment[eligibility_stats_segment$TrueSegmentCount > discard_first, ]
    if (!is.null(eligibility_stats_no_fading)) {
      eligibility_stats_segment_no_fading <- eligibility_stats_segment_no_fading[eligibility_stats_segment_no_fading$TrueSegmentCount > discard_first, ]
    }
  }

  examinee <- eligibility_stats_segment$TrueSegmentCount
  n_examinee <- length(examinee)
  fading_examinee <- examinee
  for (j in 2:length(examinee)) {
    fading_examinee[j] <- fading_examinee[j - 1] * fading_factor + 1
  }

  for (i in items) {
    alpha <- eligibility_stats_segment[[paste("a_g", segment, "i", i, sep = "_")]]
    epsilon <- eligibility_stats_segment[[paste("e_g", segment, "i", i, sep = "_")]]
    p_alpha <- alpha / fading_examinee
    p_epsilon <- epsilon / fading_examinee
    p_epsilon[p_epsilon > 1] <- 1
    p_eligibility <- rep(1, n_examinee)
    for (j in 2:n_examinee) {
      if (alpha[j - 1] > 0) {
        p_eligibility[j] <- min(epsilon[j - 1] * max_rate / alpha[j - 1], 1)
      }
    }
    if (!is.null(eligibility_stats_no_fading)) {
      alpha_no_fading <- eligibility_stats_segment_no_fading[[paste("a_g", segment, "i", i, sep = "_")]]
      epsilon_no_fading <- eligibility_stats_segment_no_fading[[paste("e_g", segment, "i", i, sep = "_")]]
      p_alpha_no_fading <- alpha_no_fading / examinee
      p_epsilon_no_fading <- epsilon_no_fading / examinee
    }
    plot(examinee, p_alpha, main = paste("Segment", segment, "- Item", i), type = "n", ylim = c(0, 1), xlab = "Examinees", ylab = "Rate")
    lines(examinee, p_alpha, col = "red", lty = 1, lwd = 3)
    lines(examinee, p_epsilon, col = "blue", lty = 2, lwd = 3)
    lines(examinee, p_eligibility, col = "purple", lty = 3, lwd = 3)
    if (is.null(eligibility_stats_no_fading)) {
      legend("topright", c("alpha", "epsilon", "Pr{eligible}"), lty = c(1, 2, 3), col = c("red", "blue", "purple"), lwd = c(2, 2, 2), bg = "white")
    } else {
      lines(examinee, p_epsilon_no_fading, col = addTrans("blue", 20), lty = 1, type = "h")
      lines(examinee, p_alpha_no_fading, col = addTrans("red", 20), lty = 1, type = "h")
      legend("topright", c("alpha", "epsilon", "Pr{eligible}", "alpha empirical", "epsilon empirical"), lty = c(1, 2, 3, 1, 1), lwd = c(2, 2, 2, 5, 5), col = c("red", "blue", "purple", addTrans("red", 100), addTrans("blue", 100)))
    }
    abline(h = max_rate, col = "gray")
  }
  if (!is.null(file_pdf)) {
    dev.off()
  }
}

#' Calculate Root Mean Squared Error
#'
#' Calculate Root Mean Squared Error.
#'
#' @param x A vector of values.
#' @param y A vector of values.
#' @param conditional If \code{TRUE}, calculate RMSE conditional on x.
RMSE <- function(x, y, conditional = TRUE) {
  if (length(x) != length(y)) {
    stop("length(x) and length(y) are not equal")
  }
  if (conditional) {
    MSE <- tapply((x - y)^2, x, mean)
  } else {
    MSE <- mean((x - y)^2)
  }
  return(sqrt(MSE))
}

#' Calculate Relative Errors
#'
#' Calculate Relative Errors.
#'
#' @param RMSE_foc A vector of RMSE values for the focal group.
#' @param RMSE_ref A vector of RMSE values for the reference group.
RE <- function(RMSE_foc, RMSE_ref) {
  if (length(RMSE_foc) != length(RMSE_ref)) {
    stop("length(x) and length(y) are not equal")
  }
  RE <- RMSE_ref^2 / RMSE_foc^2
  return(RE)
}

#' Check the consistency of constraints and item usage
#'
#' Check the consistency of constraints and item usage.
#'
#' @param constraints A \code{\linkS4class{constraints}} object generated by \code{\link{loadConstraints}}.
#' @param usage_matrix A matrix of item usage data from \code{\link{Shadow}}.
#' @param true_theta A vector of true theta values.
checkConstraints <- function(constraints, usage_matrix, true_theta = NULL) {


  raw_constraints <- constraints@constraints
  list_constraints <- constraints@list_constraints

  nc <- nrow(raw_constraints)
  nj <- nrow(usage_matrix)
  ni <- ncol(usage_matrix)

  MET <- matrix(FALSE, nrow = nj, ncol = nc)
  COUNT <- matrix(NA, nrow = nj, ncol = nc)
  if (ni != constraints@ni) {
    stop("unequal number of items in constraints and usage_matrix ")
  }
  byTheta <- FALSE
  MEAN <- rep(NA, nc)
  SD   <- rep(NA, nc)
  MIN  <- rep(NA, nc)
  MAX  <- rep(NA, nc)
  HIT  <- rep(NA, nc)
  if (!is.null(true_theta)) {
    if (length(true_theta) != nj) {
      stop("length of true_theta is not equal to nrow of usage_matrix")
    }
    byTheta <- TRUE
    groupMEAN <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupSD   <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupMIN  <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupMAX  <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupHIT  <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
  } else {
    groupMEAN <- NULL
    groupSD   <- NULL
    groupMIN  <- NULL
    groupMAX  <- NULL
    groupHIT  <- NULL
  }
  nEnemy <- sum(raw_constraints$TYPE == "ENEMY")
  if (nEnemy > 0) {
    enemyIndex <- which(raw_constraints$TYPE == "ENEMY")
    raw_constraints$LB[enemyIndex] <- 0
    raw_constraints$UB[enemyIndex] <- 1
  }
  numberIndex <- which(raw_constraints$TYPE == "NUMBER")
  for (index in 1:nc) {
    if (raw_constraints$WHAT[index] == "ITEM") {
      if (raw_constraints$TYPE[index] %in% c("NUMBER", "ENEMY")) {
        items <- which(list_constraints[[index]]@mat[1, ] == 1)
        COUNT[, index] <- rowSums(usage_matrix[, items])
        MET[, index] <- COUNT[, index] >= raw_constraints$LB[index] & COUNT[, index] <= raw_constraints$UB[index]
        if (byTheta) {
          groupMEAN[index, ] <- round(tapply(COUNT[, index], true_theta, mean), 3)
          groupSD[index, ] <- round(tapply(COUNT[, index], true_theta, sd), 3)
          groupMIN[index, ] <- tapply(COUNT[, index], true_theta, min)
          groupMAX[index, ] <- tapply(COUNT[, index], true_theta, max)
          groupHIT[index, ] <- round(tapply(MET[, index], true_theta, mean), 3)
        }
        MEAN[index] <- round(mean(COUNT[, index]), 2)
        SD[index] <- round(sd(COUNT[, index]), 2)
        MIN[index] <- min(COUNT[, index])
        MAX[index] <- max(COUNT[, index])
        HIT[index] <- round(mean(MET[, index]), 3)
      }
    }
  }
  LD <- NULL
  if (nEnemy > 0) {
    LD <- rowSums(COUNT[, enemyIndex] > 1)
  }
  Check <- data.frame(raw_constraints, MEAN = MEAN, SD = SD, MIN = MIN, MAX = MAX, HIT = HIT)
  return(list(
    Check = Check[raw_constraints[["TYPE"]] == "NUMBER", ],
    LD = LD,
    groupMEAN = groupMEAN[numberIndex, ], groupSD = groupSD[numberIndex, ],
    groupMIN = groupMIN[numberIndex, ], groupMAX = groupMAX[numberIndex, ],
    groupHIT = groupHIT[numberIndex, ]))
}

#' Draw RMSE plots
#'
#' Draw RMSE plots.
#'
#' @param ... A series of RMSE values.
#' @param title A plot title.
#' @param legend_title A legend title.
#' @param legend_labels A vector of labels for the series.
#' @param lty_set A vector of line types for the series.
#' @param col_set A vector of colors for the series.
#' @param theta A theta grid.
plotRMSE <- function(..., title = NULL, legend_title = NULL, legend_labels = NULL, lty_set = NULL, col_set = NULL, theta = seq(-2, 2, 1)) {

  output_list <- list(...)
  n_output <- length(output_list)

  if (is.null(lty_set)) {
    lty_set <- 1:n_output
  } else if (length(lty_set) != n_output) {
    warning("... and lty_set are of different lengths")
    lty_set <- 1:n_output
  }

  if (is.null(col_set)) {
    col_set <- 1:n_output
  } else if (length(col_set) != n_output) {
    warning("... and col_set are of different lengths")
    col_set <- 1:n_output
  }

  plot(unique(output_list[[1]]$true_theta), RMSE(output_list[[1]]$true_theta, output_list[[1]]$final_theta_est), xlim = range(theta), ylim = c(0, 1), xlab = "Theta", ylab = "RMSE", type = "n", xaxt = "n", yaxt = "n", main = title)
  axis(1, at = theta, labels = theta)
  axis(2, at = seq(0, 1.0, .2), labels = format(seq(0, 1.0, .2), digits = 1), las = 2)
  grid()

  for (i in 1:n_output) {
    lines(unique(output_list[[i]]$true_theta), RMSE(output_list[[i]]$true_theta, output_list[[i]]$final_theta_est), lty = lty_set[i], col = col_set[i], lwd = 2)
  }

  if (!is.null(legend_labels)) {
    if (length(legend_labels) != n_output) {
      warning("... and legend_labels are of different lengths")
      legend_labels <- 1:n_output
    }
    legend("top", labels, lty = lty_set, col = col_set, title = legend_title, bg = "white")
  }
}

#' @noRd
plotER <- function(
  item_exposure_rate, item_exposure_rate_final = NULL,
  stim_exposure_rate = NULL, stim_index = NULL,
  max_rate = max_rate, title = NULL, color = "blue", color_final = "yellow", color_stim = "red", color_threshold = "dark gray", simple = FALSE) {

  if (!is.null(stim_index)) {
    idx_sort <- order(stim_exposure_rate, stim_index, item_exposure_rate, decreasing = TRUE)
    item_exposure_rate_ordered <- item_exposure_rate[idx_sort]
    stim_exposure_rate_ordered <- stim_exposure_rate[idx_sort]
    stim_index_ordered         <- stim_index[idx_sort]
  } else {
    idx_sort <- order(item_exposure_rate, decreasing = TRUE)
    item_exposure_rate_ordered <- item_exposure_rate[idx_sort]
  }

  ni <- length(item_exposure_rate)

  if (!simple) {
    xlab = "Item"
    ylab = "Exposure Rate"
  } else {
    xlab = ""
    ylab = ""
  }

  plot(1:ni, item_exposure_rate_ordered, type = "n", lwd = 2, ylim = c(0, 1), xlab = "Item", ylab = "Exposure Rate", main = title)
  points(1:ni, item_exposure_rate_ordered, type = "h", lwd = 1, col = color)
  if (!is.null(stim_exposure_rate)) {
    lines(1:ni, stim_exposure_rate_ordered, col = color_stim, type = "s")
    for (stim_id in unique(stim_index_ordered)) {
      x <- mean((1:ni)[which(stim_index_ordered == stim_id)])
      y <- stim_exposure_rate_ordered[which(stim_index_ordered == stim_id)][1]
      points(x, y, col = color_stim, pch = 21, bg = 'white', cex = .75)
    }
  }
  abline(h = max_rate, col = color_threshold, lty = 2)

  if (!is.null(item_exposure_rate_final)) {
    item_exposure_rate_final_ordered <- item_exposure_rate_final[idx_sort]
    points(1:ni, item_exposure_rate_final_ordered, type = "h", lwd = 1, lty = 1, col = color_final)
  }
}

#' Draw exposure rate plots by theta segment
#'
#' Draw exposure rate plots by theta segment.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param max_rate A target item exposure rate.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of the graphics device.
#' @param height Height of the graphics device.
#' @param mfrow Number of multiple figures defined as c(nrow, ncol).
plotExposureRateBySegment <- function(object, config, max_rate = 0.25, file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4)) {

  ## FIX THIS: WHAT IF TRUE_THETA IS NOT AVAILABLE

  nj <- length(object$true_theta)
  ni <- object$pool@ni
  segment_cut <- config@exposure_control$segment_cut
  n_segment   <- config@exposure_control$n_segment
  cut_lower   <- segment_cut[1:n_segment]
  cut_upper   <- segment_cut[2:(n_segment + 1)]
  segment_label <- character(n_segment)

  for (k in 1:n_segment) {
    if (k < n_segment) {
      segment_label[k] <- paste0("(", round(cut_lower[k], 1), ",", round(cut_upper[k], 1), "]")
    } else {
      segment_label[k] <- paste0("(", round(cut_lower[k], 1), ",", round(cut_upper[k], 1), ")")
    }
  }

  exposure_rate <- colSums(object$usage_matrix) / nj
  exposure_rate_segment <- vector("list", n_segment)

  item_exposure_rate <- exposure_rate[1:ni]
  item_exposure_rate_segment <- vector("list", n_segment)
  names(exposure_rate_segment) <- segment_label

  for (k in 1:n_segment) {
    if (object$eligibility_stats$n_jk[k] == 0) {
      exposure_rate_segment[[k]] <- numeric(ni)
    } else {
      exposure_rate_segment[[k]] <- object$eligibility_stats$alpha_ijk[k, ] / object$eligibility_stats$n_jk[k]
      item_exposure_rate_segment[[k]] <- exposure_rate_segment[[k]][1:ni]
    }
  }

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  old_mfrow <- par()$mfrow
  on.exit(par(mfrow = old_mfrow))
  par(mfrow = mfrow)

  ## FIX THIS: split EXPOSURE_RATE into ITEM_EXPOSURE_RATE and STIM_EXPOSURE_RATE

  plotER(exposure_rate, max_rate = max_rate, title = paste0("Overall (N = ", nj, ")"), color = "blue")

  for (k in 1:config@exposure_control$n_segment) {
    plotER(
      exposure_rate_segment[[k]], NULL, max_rate = max_rate,
      title = paste0(segment_label[k], " (n = ", round(object$eligibility_stats$n_jk[k], 1), ")"),
      color = "blue"
    )
  }
  if (!is.null(file_pdf)) {
    dev.off()
  }

  return(exposure_rate_segment)
}

#' Draw exposure rate plots by final theta segment
#'
#' Draw exposure rate plots by final theta segment.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param max_rate A target item exposure rate.
#' @param theta By which theta to base the segments, either "Estimated" or "True".
#' @param segment_cut A vector of cut values defining theta segments.
#' @param color A vector of colors.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of the graphics object.
#' @param height Height of the graphics object.
#' @param mfrow Number of multiple figures defined as c(nrow, ncol).
#' @param burn An integer identifying the first x simulees to discard as burn-in.
#' @param retain An optional vector of indices identifying the simulees to retain.
#'
#' @examples
#' \donttest{
#' true_theta <- runif(10, min = -3.5, max = 3.5)
#' resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
#' constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))
#' config_science <- createShadowTestConfig(
#'   MIP = list(solver = "LPSOLVE"),
#'   exposure_control = list(method = "ELIGIBILITY")
#' )
#' solution <- Shadow(config_science, constraints_science2, true_theta, data = resp_science)
#' p <- plotExposureRateFinal(solution, config_science, 0.25)
#' }
#' @export
plotExposureRateFinal <- function(object, config = NULL, max_rate = 0.25, theta = "Estimated", segment_cut = NULL, color = "red", file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4), burn = 0, retain = NULL) {

  if (toupper(theta) == "TRUE") {
    theta_values <- object@true_theta
    nj           <- length(theta_values)
  } else if (toupper(theta) == "ESTIMATED") {
    theta_values <- object@final_theta_est
    nj           <- length(theta_values)
  } else {
    stop(sprintf("unknown theta_segment '%s' specified: must be 'True' or 'Estimated'", theta))
  }

  if (burn > 0) {
    if (toupper(theta) == "TRUE") {
      retained <- object@true_segment_count > burn  ## CHECK THIS IF THIS IS WORKING AS INTENDED
    } else {
      retained <- object@est_segment_count > burn   ## CHECK THIS IF THIS IS WORKING AS INTENDED
    }
  } else if (!is.null(retain)) {
    retained <- (1:nj) %in% retain
  } else {
    retained <- rep(TRUE, nj)
  }
  n_retained <- sum(retained)

  ni <- ncol(object@usage_matrix)

  if (is.null(config)) {
    config <- object@config
  }
  if (is.null(segment_cut)) {
    segment_cut <- config@exposure_control$segment_cut
  }

  n_segment <- length(segment_cut) - 1
  cut_lower <- segment_cut[1:n_segment]
  cut_upper <- segment_cut[2:(n_segment + 1)]
  segment_label <- character(n_segment)
  theta_segment_index <- numeric(sum(retained))
  theta_segment_index <- find_segment(theta_values[retained], segment_cut)

  segment_n    <- numeric(n_segment)
  segment_dist <- table(theta_segment_index)
  segment_n[as.numeric(names(segment_dist))] <- segment_dist
  segment_index_table <- matrix(NA, n_retained, object@constraints@test_length)
  for (k in 1:n_segment) {
    if (k < n_segment) {
      segment_label[k] <- paste0("(", cut_lower[k], ",", cut_upper[k], "]")
    } else {
      segment_label[k] <- paste0("(", cut_lower[k], ",", cut_upper[k], ")")
    }
  }

  usage_matrix       <- object@usage_matrix[retained, ]
  usage_matrix_final <- object@usage_matrix[retained, ]
  idx <- 0
  for (j in 1:nj) {
    if (retained[j]) {
      idx <- idx + 1
      usage_matrix_final[idx, object@output[[j]]@administered_item_index[object@output[[j]]@theta_segment_index != theta_segment_index[idx]]] <- FALSE
      segment_index_table[idx, ] <- object@output[[j]]@theta_segment_index
    }
  }

  segment_freq <- matrix(0, n_segment, n_segment)
  for (i in 1:object@constraints@test_length) {
    factor(segment_index_table[, i], levels = 1:n_segment)
    segment_table <- tapply(factor(segment_index_table[, i], levels = 1:n_segment), theta_segment_index, table)
    for (s in 1:length(segment_table)) {
      idx_r <- as.numeric(names(segment_table)[s])
      idx_c <- as.numeric(names(segment_table[[s]]))
      segment_freq[idx_r, idx_c] <- segment_freq[idx_r, idx_c] + segment_table[[s]]
    }
  }

  segment_rate <- segment_freq / segment_n
  segment_rate_table <- data.frame(
    segment_class = factor(rep(segment_label, rep(n_segment, n_segment)),
    levels = segment_label),
    segment = rep(1:n_segment, n_segment),
    avg_visit = matrix(t(segment_rate),
    nrow = n_segment^2, ncol = 1
    )
  )

  exposure_rate               <- colSums(usage_matrix) / n_retained
  exposure_rate_final         <- colSums(usage_matrix_final) / n_retained
  exposure_rate_segment       <- vector("list", n_segment)
  exposure_rate_segment_final <- vector("list", n_segment)
  names(exposure_rate_segment)       <- segment_label
  names(exposure_rate_segment_final) <- segment_label

  for (k in 1:n_segment) {
    if (segment_n[k] > 2) {
      exposure_rate_segment[[k]]       <- colMeans(usage_matrix[theta_segment_index == k, ])
      exposure_rate_segment_final[[k]] <- colMeans(usage_matrix_final[theta_segment_index == k, ])
    }
    if (is.null(exposure_rate_segment[[k]])) {
      exposure_rate_segment[[k]] <- numeric(ni)
    } else if (any(is.nan(exposure_rate_segment[[k]]))) {
      exposure_rate_segment[[k]][is.nan(exposure_rate_segment[[k]])] <- 0
    }
    if (is.null(exposure_rate_segment_final[[k]])) {
      exposure_rate_segment_final[[k]] <- numeric(ni)
    } else if (any(is.nan(exposure_rate_segment_final[[k]]))) {
      exposure_rate_segment_final[[k]][is.nan(exposure_rate_segment_final[[k]])] <- 0
    }
  }

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  old_mfrow <- par()$mfrow
  on.exit(par(mfrow = old_mfrow))
  par(mfrow = mfrow)

  plotER(exposure_rate, exposure_rate_final, max_rate = max_rate, title = paste0("Overall (N = ", n_retained, ")"), color = color)
  for (k in 1:n_segment) {
    plotER(
      exposure_rate_segment[[k]], exposure_rate_segment_final[[k]],
      max_rate = max_rate, title = paste0(segment_label[k], " (n = ", segment_n[k], ")"),
      color = color)
  }
  if (!is.null(file_pdf)) {
    dev.off()
  }

  return(
    list(
      exposure_rate               = exposure_rate,
      exposure_rate_segment       = exposure_rate_segment,
      exposure_rate_segment_final = exposure_rate_segment_final,
      segment_rate_table          = segment_rate_table,
      n_segment                   = n_segment,
      segment_n                   = segment_n,
      segment_cut                 = segment_cut,
      segment_label               = segment_label
    )
  )
}

#' Draw item information plots for flagged items by segment
#'
#' Draw item information plots for flagged items by segment.
#'
#' @param object A list object generated by \code{\link{plotExposureRateFinal}}.
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param theta A theta grid.
#' @param flag_from A flagging criterion.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of the graphics device.
#' @param height Height of the graphics device.
#' @param color Plotting color.
#' @param mfrow Number of multiple figures defined as c(nrow, ncol).
plotExposureRateFinalFlag <- function(object, pool, theta = seq(-3, 3, .1), flag_from = 0.4, file_pdf = NULL, width = 7, height = 6, color = "red", mfrow = c(2, 4)) {
  info <- calcFisher(pool, theta)
  ni <- pool@ni
  n_segment <- object$n_segment
  segment_cut <- object$segment_cut
  segment_cut[1] <- min(theta)
  segment_cut[length(segment_cut)] <- max(theta)
  segment_label <- object$segment_label
  items_flagged_segment <- lapply(seq_len(object$n_segment), function(j) which(object$exposure_rate_segment[[j]] > flag_from))

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  old_mfrow <- par()$mfrow
  on.exit(par(mfrow = old_mfrow))
  par(mfrow = mfrow)

  for (k in 1:n_segment) {
    theta_segment_range         <- which(theta >= segment_cut[k] & theta <= segment_cut[k + 1])
    theta_segment_range_outside <- which(theta <= segment_cut[k] | theta >= segment_cut[k + 1])
    plot(theta, info[, 1], xlab = "Theta", ylab = "Info", main = segment_label[k], type = "n", ylim = c(0, max(info)))
    for (i in 1:ni) {
      lines(theta, info[, i], col = "light grey", lwd = 0.5)
      lines(theta[theta_segment_range], info[theta_segment_range, i], col = "grey", lwd = 1.0)
    }
    items_flagged <- items_flagged_segment[[k]]
    if (length(items_flagged) > 0) {
      for (i in items_flagged) {
        lines(theta[theta_segment_range]        , info[theta_segment_range, i]        , col = color, lwd = 2)
        lines(theta[theta_segment_range_outside], info[theta_segment_range_outside, i], col = color, lwd = 1)
      }
    }
    abline(v = segment_cut[k]    , col = "dark grey")
    abline(v = segment_cut[k + 1], col = "dark grey")
  }

  if (!is.null(file_pdf)) {
    dev.off()
  }

  return(items_flagged_segment)
}

#' Overlay item information plots
#'
#' Overlay item information plots.
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param theta A theta grid.
#' @param info_type Type of information.
#' @param select A vector of indices identifying the items to subset.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param color Plotting color.
#' @param width Width of the graphics device.
#' @param height Height of the graphics device.
plotInfoOverlay <- function(object, theta, info_type = "FISHER", select = NULL, file_pdf = NULL, color = "red", width = 7, height = 6) {

  if (toupper(info_type) == "FISHER") {
    info <- calcFisher(object, theta)
  } else {
    stop("Invalid info_type specified")
  }

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  items <- 1:object@ni
  if (!is.null(select) && all(select %in% items)) {
    items <- select
  }

  plot(theta, info[, 1], xlab = "Theta", ylab = "Info", main = "", type = "n", ylim = c(0, max(info)))
  for (i in 1:object@ni) {
    lines(theta, info[, i], col = "light grey", lwd = 0.5)
  }
  for (i in items) {
    lines(theta, info[, i], col = color, lwd = 2)
  }

  if (!is.null(file_pdf)) {
    dev.off()
  }
}

#' Calculate hyperparameters for log-normal distribution
#'
#' Calculate hyperparameters for log-normal distribution.
#'
#' @param mean Mean of the distribution.
#' @param sd Standard deviation of the distribution.
#'
#' @examples
#' lnHyperPars(.5, 1)
#'
#' @export
lnHyperPars <- function(mean, sd) {
  location <- log(mean^2 / sqrt(sd^2 + mean^2))
  scale    <- sqrt(log(1 + sd^2 / mean^2))
  return(c(location, scale))
}

#' Calculate hyperparameters for logit-normal distribution
#'
#' Calculate hyperparameters for logit-normal distribution.
#'
#' @param mean Mean of the distribution.
#' @param sd Standard deviation of the distribution.
#'
#' @examples
#' logitHyperPars(.5, 1)
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

#' Sample item parameter estimates from their posterior distributions
#'
#' Sample item parameter estimates from their posterior distributions.
#'
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param n_sample An integer as the number of sampled parameters.
#'
#' @examples
#' ipar <- iparPosteriorSample(itempool_science, 5)
#'
#' @export
iparPosteriorSample <- function(pool, n_sample = 500) {

  requireNamespace("logitnorm")
  ipar_list <- vector(mode = "list", length = pool@ni)

  for (i in 1:pool@ni) {

    if (pool@model[i] == "item_1PL") {
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 1)
      ipar_list[[i]][, 1] <- rnorm(n_sample, pool@ipar[i, 1], pool@se[i, 1])

    } else if (pool@model[i] == "item_2PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 2)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])

    } else if (pool@model[i] == "item_3PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      c_hyp <- logitHyperPars(pool@ipar[i, 3], pool@se[i, 3])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 3)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])
      ipar_list[[i]][, 3] <- rlogitnorm(n_sample, mu = c_hyp[1], sigma = c_hyp[2])

    } else if (pool@model[i] == "item_PC") {
      ipar_list[[i]] <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k] <- rnorm(n_sample, pool@ipar[i, k], pool@se[i, k])
      }

    } else if (pool@model[i] == "item_GPC") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i])
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k + 1] <- rnorm(n_sample, pool@ipar[i, k + 1], pool@se[i, k + 1])
      }

    } else if (pool@model[i] == "item_GR") {
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

    }
  }
  return(ipar_list)
}

#' Save or print audit trails
#'
#' Save or print audit trails for all simulees.
#'
#' @param object_list A list of output objects generated from \code{STA}.
#' @param file An optional file name as a character string to save the output.
#'
#' @return None
saveOutput <- function(object_list, file = NULL) {
  nj <- length(object_list)
  for (j in 1:nj) {
    object <- object_list[[j]]
    output <- data.frame(
      simulee = object@simulee_id,
      true_theta = object@true_theta,
      true_theta_segment = object@true_theta_segment,
      stage = 1:length(object@administered_item_index),
      stimulus_index = ifelse(is.nan(object@administered_stimulus_index), rep(NA, length(object@administered_item_index)), object@administered_stimulus_index),
      item_index = object@administered_item_index,
      item_resp = object@administered_item_resp,
      interim_theta = object@interim_theta_est,
      interim_se = object@interim_se_est,
      interim_theta_segment = object@theta_segment_index
    )
    if (!is.null(file)) {
      write.table(output, file = file, append = j > 1, row.names = FALSE, col.names = j == 1, sep = ",")
    } else {
      print(output)
    }
  }
}

#' (deprecated) Plot a shadow test chart
#'
#' (deprecated) Use \code{\link[TestDesign:plot-methods]{plot}} with \code{type = 'shadow'} instead.
#'
#' @param object An output from \code{\link{Shadow}} function.
#' @param examinee_id Numeric ID of the examinee to draw the plot.
#' @param sort_by_difficulty Sort the items by difficulty. (not implemented)
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param simple If \code{TRUE}, simplity the chart by hiding unused items.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(config, constraints_science, true_theta)
#' plotShadow(solution, 1)
#' plotShadow(solution, 1, simple = TRUE)
#'
#' @docType methods
#' @rdname plotShadow-methods
#' @export
setGeneric(
  name = "plotShadow",
  def = function(object, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, simple = FALSE, ...) {
    standardGeneric("plotShadow")
  }
)

#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(
  f = "plotShadow",
  signature = "output_Shadow_all",
  definition = function(object, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, simple = FALSE, ...) {
    .Deprecated("plot", msg = "plotShadow() is deprecated. Use plot(type = 'shadow') instead.")
    p <- plot(
      object,
      type = "shadow",
      examinee_id = examinee_id,
      sort_by_difficulty = sort_by_difficulty,
      file_pdf = file_pdf,
      simple = simple,
      ...
    )
  }
)

#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(
  f = "plotShadow",
  signature = "list",
  definition = function(object, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, simple = FALSE, ...) {
    .Deprecated("plot", msg = "plotShadow() is deprecated. Use plot(type = 'shadow') instead.")
    message("Consider converting the object to 'output_Shadow_all' class.\n")
    new_object <- new("output_Shadow_all")
    for (n in names(object)) {
      slot(new_object, n) <- object[[n]]
    }
    p <- plot(
      new_object,
      type = "shadow",
      examinee_id = examinee_id,
      sort_by_difficulty = sort_by_difficulty,
      file_pdf = file_pdf,
      simple = simple,
      ...
    )
  }
)

#' (deprecated) Plot audit trail
#'
#' (deprecated) Use \code{\link[TestDesign:plot-methods]{plot}} with \code{type = 'audit'} instead.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param examinee_id Numeric ID of the examinee to draw the plot.
#' @param min_theta A lower bound of theta.
#' @param max_theta An upper bound of theta.
#' @param min_score A minimum item score.
#' @param max_score A maximum item score.
#' @param z_ci A quantile of the normal distribution for confidence intervals.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(config, constraints_science, true_theta)
#' plotCAT(solution, 1)
#'
#' @docType methods
#' @rdname plotCAT-methods
#' @export
setGeneric(
  name = "plotCAT",
  def = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    standardGeneric("plotCAT")
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "output_Shadow_all",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotCAT() is deprecated. Use plot(type = 'audit') instead.")
    p <- plot(object,
      type = 'audit',
      examinee_id = examinee_id,
      min_theta = min_theta,
      max_theta = max_theta,
      min_score = min_score,
      max_score = max_score,
      z_ci = z_ci,
      file_pdf = file_pdf,
      ...
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "list",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotCAT() is deprecated. Use plot(type = 'audit') instead.")
    message("Consider converting the object to 'output_Shadow_all' class.\n")
    new_object <- new("output_Shadow_all")
    for (n in names(object)) {
      slot(new_object, n) <- object[[n]]
    }
    p <- plot(new_object,
      type = 'audit',
      examinee_id = examinee_id,
      min_theta = min_theta,
      max_theta = max_theta,
      min_score = min_score,
      max_score = max_score,
      z_ci = z_ci,
      file_pdf = file_pdf,
      ...
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "output_Shadow",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotCAT() function is deprecated. Use plot(type = 'audit') instead.")
    plot(object,
      type = 'audit',
      examinee_id = examinee_id,
      min_theta = min_theta,
      max_theta = max_theta,
      min_score = min_score,
      max_score = max_score,
      z_ci = z_ci,
      file_pdf = file_pdf,
      ...
    )
  }
)

#' (deprecated) Plot item exposure rates
#'
#' (deprecated) Use \code{\link[TestDesign:plot-methods]{plot}} with \code{type = 'exposure'} instead.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param max_rate A target exposure rate.
#' @param theta_segment True or Estimated theta used to create segments ("Estimated" or "True").
#' @param color Color of item-wise exposure rates.
#' @param color_final Color of item-wise exposure rates, only counting the items while in the final theta segment as exposed.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' \donttest{
#' true_theta <- runif(10, min = -3.5, max = 3.5)
#' resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
#' constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))
#' config_science <- createShadowTestConfig(
#'   MIP = list(solver = "lpSolve"),
#'   exposure_control = list(method = "ELIGIBILITY")
#' )
#' solution <- Shadow(config_science, constraints_science2, true_theta, data = resp_science)
#' p <- plotExposure(solution)
#' }
#' @docType methods
#' @rdname plotExposure-methods
#' @export
setGeneric(
  name = "plotExposure",
  def = function(object, max_rate = 0.25, theta_segment = "Estimated", color = "blue", color_final = "blue", file_pdf = NULL, ...) {
    standardGeneric("plotExposure")
  }
)

#' @docType methods
#' @rdname plotExposure-methods
#' @export
setMethod(
  f = "plotExposure",
  signature = "list",
  definition = function(object, max_rate = 0.25, theta_segment = "estimated", color = "blue", color_final = "blue", file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotExposure() is deprecated. Use plot(type = 'exposure') instead.")
    message("Consider converting the object to 'output_Shadow_all' class.\n")
    new_object <- new("output_Shadow_all")
    for (n in names(object)) {
      slot(new_object, n) <- object[[n]]
    }
    p <- plot(new_object,
      type = "exposure",
      theta_segment = theta_segment,
      color = color,
      color_final = color_final,
      file_pdf = file_pdf,
      ...
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotExposure-methods
#' @export
setMethod(
  f = "plotExposure",
  signature = "output_Shadow_all",
  definition = function(object, max_rate = 0.25, theta_segment = "estimated", color = "blue", color_final = "blue", file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotExposure() is deprecated. Use plot(type = 'exposure') instead.")
    p <- plot(object,
      type = "exposure",
      theta_segment = theta_segment,
      color = color,
      color_final = color_final,
      file_pdf = file_pdf,
      ...
    )
    return(p)
  }
)
