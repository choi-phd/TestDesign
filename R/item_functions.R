#' @include item_class.R
NULL

#' Calculate first derivative of log-likelihood
#'
#' \code{\link{calcJacobian}} is a function to calculate the first derivative of the log-likelihood function.
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#' @param resp the response data to use.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcJacobian}} returns a length \emph{nq} vector containing the first derivative of the log-likelihood function, of observing the response at each theta.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcJacobian}} returns a (\emph{nq}, \emph{ni}) matrix containing the first derivative of the log-likelihood function, of observing the response at each theta.}
#' }
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{nq} denotes the number of theta values.}
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' @examples
#' item_1    <- new("item_1PL", difficulty = 0.5)
#' item_2    <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3    <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4    <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5    <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6    <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' j_item_1 <- calcJacobian(item_1, seq(-3, 3, 1), 0)
#' j_item_2 <- calcJacobian(item_2, seq(-3, 3, 1), 0)
#' j_item_3 <- calcJacobian(item_3, seq(-3, 3, 1), 0)
#' j_item_4 <- calcJacobian(item_4, seq(-3, 3, 1), 0)
#' j_item_5 <- calcJacobian(item_5, seq(-3, 3, 1), 0)
#' j_item_6 <- calcJacobian(item_6, seq(-3, 3, 1), 0)
#' j_pool   <- calcJacobian(
#'   itempool_science, seq(-3, 3, 1),
#'   rep(0, itempool_science@ni)
#' )
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @docType methods
#' @rdname calcJacobian-methods
#' @export
setGeneric(
  name = "calcJacobian",
  def = function(object, theta, resp) {
    standardGeneric("calcJacobian")
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_1PL,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_1PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(resp - calcEscore(object, theta))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_2PL,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_2PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(object@slope * (resp - calcEscore(object, theta)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_3PL,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_3PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    expected_score <- calcEscore(object, theta)
    return(object@slope * (resp - expected_score) * (expected_score - object@guessing) / (expected_score * (1.0 - object@guessing)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_PC,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_PC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(resp - calcEscore(object, theta))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_GPC,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_GPC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(object@slope * (resp - calcEscore(object, theta)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_GR,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_GR", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    resp <- resp + 1
    prob <- calcProb(object, theta)
    ps <- matrix(NA, length(theta), object@ncat + 1)
    ps[, 1] <- 1
    ps[, object@ncat + 1] <- 0
    for (k in 2:(object@ncat)) {
      ps[, k] <- ps[, k - 1] - prob[, k - 1]
    }
    return(object@slope * ((ps[, resp] * (1 - ps[, resp]) - ps[, resp + 1] * (1 - ps[, resp + 1])) / prob[, resp]))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_pool,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (length(resp) != object@ni) {
      stop("length(resp) does not match item_pool@ni")
    }
    if (length(theta) > 0 && all(!is.na(theta))) {
      mat_Jacobian <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        mat_Jacobian[, i] <- calcJacobian(object@parms[[i]], theta, resp[i])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Jacobian)
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item_pool_cluster,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("item_pool_cluster", "numeric", "list"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      mat_Jacobian <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        mat_Jacobian[[i]] <- calcJacobian(object@pools[[i]], theta, resp[[i]])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Jacobian)
  }
)

#' Calculate second derivative of log-likelihood
#'
#' \code{\link{calcHessian}} is a function to calculate the second derivative of the log-likelihood function.
#'
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{nq} denotes the number of theta values.}
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#' @param resp the response data to use. This must be a single value for an \code{\link{item}}, or a length \emph{ni} vector for an \code{\linkS4class{item_pool}}.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcHessian}} returns a length \emph{nq} vector containing the second derivative of the log-likelihood function, of observing the response at each theta.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcHessian}} returns a (\emph{nq}, \emph{ni}) matrix containing the second derivative of the log-likelihood function, of observing the response at each theta.}
#' }
#'
#' @examples
#'
#' item_1    <- new("item_1PL", difficulty = 0.5)
#' item_2    <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3    <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4    <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5    <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6    <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' h_item_1 <- calcHessian(item_1, seq(-3, 3, 1), 0)
#' h_item_2 <- calcHessian(item_2, seq(-3, 3, 1), 0)
#' h_item_3 <- calcHessian(item_3, seq(-3, 3, 1), 0)
#' h_item_4 <- calcHessian(item_4, seq(-3, 3, 1), 0)
#' h_item_5 <- calcHessian(item_5, seq(-3, 3, 1), 0)
#' h_item_6 <- calcHessian(item_6, seq(-3, 3, 1), 0)
#' h_pool   <- calcHessian(
#'   itempool_science, seq(-3, 3, 1),
#'   rep(0, itempool_science@ni)
#' )
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @docType methods
#' @rdname calcHessian-methods
#' @export
setGeneric(
  name = "calcHessian",
  def = function(object, theta, resp) {
    standardGeneric("calcHessian")
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_1PL,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_1PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_2PL,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_2PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_3PL,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_3PL", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    prob <- calcProb(object, theta)
    return(object@slope^2 * prob[, 1] * (prob[, 2] - object@guessing) * (object@guessing * resp - prob[, 2]^2) / (prob[, 2]^2 * (1 - object@guessing)^2))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_PC,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_PC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_GPC,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_GPC", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_GR,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_GR", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    resp <- resp + 1
    prob <- calcProb(object, theta)
    ps <- matrix(NA, length(theta), object@ncat + 1)
    ps[, 1] <- 1
    ps[, object@ncat + 1] <- 0
    for (k in 2:(object@ncat)) {
      ps[, k] <- ps[, k - 1] - prob[, k - 1]
    }
    mat_Hessian <- object@slope^2 * ((ps[, resp] * (1 - ps[, resp]) * ((1 - ps[, resp]) - ps[, resp]) - ps[, resp + 1] * (1 - ps[, resp + 1]) * ((1 - ps[, resp + 1]) - ps[, resp + 1])) / prob[, resp]
      - (ps[, resp] * (1 - ps[, resp]) - ps[, resp + 1] * (1 - ps[, resp + 1]))^2 / prob[, resp]^2)
    return(mat_Hessian)
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_pool,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      calcProb(object, theta)
      mat_Hessian <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        mat_Hessian[, i] <- calcHessian(object@parms[[i]], theta, resp[i])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Hessian)
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item_pool_cluster,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("item_pool_cluster", "numeric", "list"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      mat_Hessian <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        mat_Hessian[[i]] <- calcHessian(object@pools[[i]], theta, resp[[i]])
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(mat_Hessian)
  }
)

#' Simulate item response data
#'
#' \code{\link{simResp}} is a function to simulate item response data.
#'
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{nq} denotes the number of theta values.}
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#' @param theta theta values to use.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{simResp}} returns a length \emph{nq} vector containing simulated item response data.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{simResp}} returns a (\emph{nq}, \emph{ni}) matrix containing simulated item response data.}
#' }
#'
#' @examples
#'
#' item_1    <- new("item_1PL", difficulty = 0.5)
#' item_2    <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3    <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4    <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5    <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6    <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' sim_item_1 <- simResp(item_1, seq(-3, 3, 1))
#' sim_item_2 <- simResp(item_2, seq(-3, 3, 1))
#' sim_item_3 <- simResp(item_3, seq(-3, 3, 1))
#' sim_item_4 <- simResp(item_4, seq(-3, 3, 1))
#' sim_item_5 <- simResp(item_5, seq(-3, 3, 1))
#' sim_item_6 <- simResp(item_6, seq(-3, 3, 1))
#' sim_pool   <- simResp(itempool_science, seq(-3, 3, 1))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @docType methods
#' @rdname simResp-methods
#' @export
setGeneric(
  name = "simResp",
  def = function(object, theta) {
    standardGeneric("simResp")
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_1PL,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    prob   <- calcProb(object, theta)
    random <- runif(length(theta))
    resp   <- numeric(length(theta))
    resp[prob[, 2] > random] <- 1
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_2PL,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    prob   <- calcProb(object, theta)
    random <- runif(length(theta))
    resp   <- numeric(length(theta))
    resp[prob[, 2] > random] <- 1
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_3PL,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    prob   <- calcProb(object, theta)
    random <- runif(length(theta))
    resp   <- numeric(length(theta))
    resp[prob[, 2] > random] <- 1
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_PC,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    prob   <- calcProb(object, theta)
    random <- runif(length(theta))
    resp   <- numeric(length(theta))
    sump   <- numeric(length(theta))
    for (k in 1:(object@ncat - 1)) {
      sump <- sump + prob[, k]
      resp[random > sump] <- resp[random > sump] + 1
    }
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_GPC,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    prob   <- calcProb(object, theta)
    random <- runif(length(theta))
    resp   <- numeric(length(theta))
    sump   <- numeric(length(theta))
    for (k in 1:(object@ncat - 1)) {
      sump <- sump + prob[, k]
      resp[random > sump] <- resp[random > sump] + 1
    }
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_GR,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_GR", "numeric"),
  definition = function(object, theta) {
    prob   <- calcProb(object, theta)
    random <- runif(length(theta))
    resp   <- numeric(length(theta))
    sump   <- numeric(length(theta))
    for (k in 1:(object@ncat - 1)) {
      sump <- sump + prob[, k]
      resp[random > sump] <- resp[random > sump] + 1
    }
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_pool,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      data <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        data[, i] <- simResp(object@parms[[i]], theta)
      }
      return(data)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_pool_cluster,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      data <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        data[[i]] <- simResp(object@pools[[i]], theta)
      }
      return(data)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item_pool_cluster,list-method
setMethod(
  f = "simResp",
  signature = c("item_pool_cluster", "list"),
  definition = function(object, theta) {
    if (length(theta) != length(object@np)) {
      data <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        if (all(!is.na(theta[[i]]))) {
          data[[i]] <- simResp(object@pools[[i]], theta[[i]])
        } else {
          stop(sprintf("invalid values in theta[[%i]]", i))
        }
      }
      return(data)
    } else {
      stop("length of 'theta' must match object@np.")
    }
  }
)
