#' @include item_class.R
NULL

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
