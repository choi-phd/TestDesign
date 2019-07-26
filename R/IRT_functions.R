msg.missing.theta <- "Argument 'theta' is empty, or contains missing values."

#' Calculate item response probabilities
#'
#' An S4 generic and its methods to calculate item response probabilities for different item classes
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return A matrix of probability values with a dimension (nq, ncat) for a single item or a list of matrices for an instance of "item.pool".
#'
#' @export
#' @docType methods
#' @rdname calcProb-methods
setGeneric(
  name = "calcProb",
  def = function(object, theta) {
    standardGeneric("calcProb")
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' prob.item.1 = calcProb(item.1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcProb",
  signature = c("item.1pl", "numeric"),
  definition = function(object, theta) {
    prob <- matrix(NA, length(theta), 2)
    prob[, 2] <- array_p_1pl(theta, object@difficulty)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' prob.item.2 = calcProb(item.2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcProb",
  signature = c("item.2pl", "numeric"),
  definition = function(object, theta) {
    prob <- matrix(NA, length(theta), 2)
    prob[, 2] <- array_p_2pl(theta, object@slope, object@difficulty)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' prob.item.3 = calcProb(item.3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcProb",
  signature = c("item.3pl", "numeric"),
  definition = function(object, theta) {
    prob <- matrix(NA, length(theta), 2)
    prob[, 2] <- array_p_3pl(theta, object@slope, object@difficulty, object@guessing)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' prob.item.4 = calcProb(item.4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcProb",
  signature = c("item.pc", "numeric"),
  definition = function(object, theta) {
    prob <- array_p_pc(theta, object@threshold)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' prob.item.5 = calcProb(item.5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcProb",
  signature = c("item.gpc", "numeric"),
  definition = function(object, theta) {
    prob <- array_p_gpc(theta, object@slope, object@threshold)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' prob.item.6 = calcProb(item.6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcProb",
  signature = c("item.gr", "numeric"),
  definition = function(object, theta) {
    prob <- array_p_gr(theta, object@slope, object@category)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item.pool,numeric-method
#' @examples
#' prob.itempool <- calcProb(itempool.science, seq(-3, 3, 1))
setMethod(
  f = "calcProb",
  signature = c("item.pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      Prob <- lapply(object@parms, calcProb, theta)
    } else {
      stop(msg.missing.theta)
    }
    return(Prob)
  }
)

#' Calculate expected scores
#'
#' An S4 generic and its methods to calculate expected scores given a vector of thetas for different item classes.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return A vector of expected scores of length nq (the number of values on theta grid).
#'
#' @export
#' @docType methods
#' @rdname calcEscore-methods
setGeneric(
  name = "calcEscore",
  def = function(object, theta) {
    standardGeneric("calcEscore")
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' ICC.item.1 = calcEscore(item.1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcEscore",
  signature = c("item.1pl", "numeric"),
  definition = function(object, theta) {
    return(calcProb(object, theta)[, 2])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' ICC.item.2 = calcEscore(item.2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcEscore",
  signature = c("item.2pl", "numeric"),
  definition = function(object, theta) {
    return(calcProb(object, theta)[, 2])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' ICC.item.3 = calcEscore(item.3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcEscore",
  signature = c("item.3pl", "numeric"),
  definition = function(object, theta) {
    return(calcProb(object, theta)[, 2])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' ICC.item.4 = calcEscore(item.4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcEscore",
  signature = c("item.pc", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    expected.score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    return(expected.score)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' ICC.item.5 = calcEscore(item.5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcEscore",
  signature = c("item.gpc", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    expected.score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    return(expected.score)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' ICC.item.6 = calcEscore(item.6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcEscore",
  signature = c("item.gr", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    expected.score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    return(expected.score)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item.pool,numeric-method
#' @examples
#' TCC.itempool <- calcEscore(itempool.science, seq(-3, 3, 1))
setMethod(
  f = "calcEscore",
  signature = c("item.pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      expected.score <- as.vector(Reduce("+", lapply(object@parms, calcEscore, theta)))
    } else {
      stop(msg.missing.theta)
    }
    return(expected.score)
  }
)

#' Calculate Fisher information
#'
#' An S4 generic and its methods to calculate Fisher information given a vector of thetas for different item classes.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return A vector of Fisher information values over theta (nq values) for a single item or a matrix of dimension (nq, ni) for an "item.pool".
#'
#' @export
#' @docType methods
#' @rdname calcFisher-methods
setGeneric(
  name = "calcFisher",
  def = function(object, theta) {
    standardGeneric("calcFisher")
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' info.item.1 = calcFisher(item.1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcFisher",
  signature = c("item.1pl", "numeric"),
  definition = function(object, theta) {
    fisher.info <- array_info_1pl(theta, object@difficulty)
    return(fisher.info)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' info.item.2 = calcFisher(item.2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcFisher",
  signature = c("item.2pl", "numeric"),
  definition = function(object, theta) {
    fisher.info <- array_info_2pl(theta, object@slope, object@difficulty)
    return(fisher.info)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' info.item.3 = calcFisher(item.3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcFisher",
  signature = c("item.3pl", "numeric"),
  definition = function(object, theta) {
    fisher.info <- array_info_3pl(theta, object@slope, object@difficulty, object@guessing)
    return(fisher.info)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item.pc,numeric-method
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' info.item.4 = calcFisher(item.4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcFisher",
  signature = c("item.pc", "numeric"),
  definition = function(object, theta) {
    fisher.info <- array_info_pc(theta, object@threshold)
    return(fisher.info)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' info.item.5 = calcFisher(item.5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcFisher",
  signature = c("item.gpc", "numeric"),
  definition = function(object, theta) {
    fisher.info <- array_info_gpc(theta, object@slope, object@threshold)
    return(fisher.info)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' info.item.6 = calcFisher(item.6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcFisher",
  signature = c("item.gr", "numeric"),
  definition = function(object, theta) {
    fisher.info <- array_info_gr(theta, object@slope, object@category)
    return(fisher.info)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item.pool,numeric-method
#' @examples
#' info.itempool <- calcFisher(itempool.science, seq(-3, 3, 1))
setMethod(
  f = "calcFisher",
  signature = c("item.pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      fisher.info <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        fisher.info[, i] <- calcFisher(object@parms[[i]], theta)
      }
    } else {
      stop(msg.missing.theta)
    }
    return(fisher.info)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,pool.cluster,numeric-method
setMethod(
  f = "calcProb",
  signature = c("pool.cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      prob.list <- lapply(object@pools, calcProb, theta)
    } else {
      stop(msg.missing.theta)
    }
    return(prob.list)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,pool.cluster,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("pool.cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      expected.score <- lapply(object@pools, calcEscore, theta)
    } else {
      stop(msg.missing.theta)
    }
    return(expected.score)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,pool.cluster,numeric-method
#' @export
setMethod(
  f = "calcFisher",
  signature = c("pool.cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      fisher.info.list <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        fisher.info.list[[i]] <- calcFisher(object@pools[[i]], theta)
      }
    } else {
      stop(msg.missing.theta)
    }
    return(fisher.info.list)
  }
)

#' Calculate item location
#'
#' An S4 generic and its methods to calculate item location.
#'
#' @param object An instance of an item class.
#'
#' @return Item location values.
#'
#' @docType methods
#' @rdname calcLocation-methods
#' @export
setGeneric(
  name = "calcLocation",
  def = function(object) {
    standardGeneric("calcLocation")
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' theta.item.1 = calcLocation(item.1)
#' @template 1pl-ref
setMethod(
  f = "calcLocation",
  signature = c("item.1pl"),
  definition = function(object) {
    return(object@difficulty)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' theta.item.2 = calcLocation(item.2)
#' @template 2pl-ref
setMethod(
  f = "calcLocation",
  signature = c("item.2pl"),
  definition = function(object) {
    return(object@difficulty)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' theta.item.3 = calcLocation(item.3)
#' @template 3pl-ref
setMethod(
  f = "calcLocation",
  signature = c("item.3pl"),
  definition = function(object) {
    location <- object@difficulty + (log(0.5 + sqrt(1 + 8 * object@guessing) / 2) / object@slope) # Birnbaum 1968, p. 464
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' theta.item.4 = calcLocation(item.4)
#' @template pc-ref
setMethod(
  f = "calcLocation",
  signature = c("item.pc"),
  definition = function(object) {
    return(mean(object@threshold))
  }
)


#' @rdname calcLocation-methods
#' @aliases calcLocation,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' theta.item.5 = calcLocation(item.5)
#' @template gpc-ref
setMethod(
  f = "calcLocation",
  signature = c("item.gpc"),
  definition = function(object) {
    return(mean(object@threshold))
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' theta.item.6 = calcLocation(item.6)
#' @template gr-ref
setMethod(
  f = "calcLocation",
  signature = c("item.gr"),
  definition = function(object) {
    return(mean(object@category))
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item.pool,numeric-method
#' @examples
#' theta.itempool <- calcLocation(itempool.science)
setMethod(
  f = "calcLocation",
  signature = c("item.pool"),
  definition = function(object) {
    Location <- lapply(object@parms, calcLocation)
    return(Location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,pool.cluster,numeric-method
setMethod(
  f = "calcLocation",
  signature = c("pool.cluster"),
  definition = function(object) {
    LocationL <- lapply(object@pools, calcLocation)
    return(LocationL)
  }
)

#' Calculate first derivative
#'
#' An S4 generic and its methods to calculate the first derivative of the probability function.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return First derivative values.
#'
#' @docType methods
#' @rdname calcDerivative-methods
#' @export
setGeneric(
  name = "calcDerivative",
  def = function(object, theta) {
    standardGeneric("calcDerivative")
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' d.item.1 = calcDerivative(item.1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcDerivative",
  signature = c("item.1pl", "numeric"),
  definition = function(object, theta) {
    prob       <- calcProb(object, theta)
    derivative <- prob[, 1] * prob[, 2]
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' d.item.2 = calcDerivative(item.2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcDerivative",
  signature = c("item.2pl", "numeric"),
  definition = function(object, theta) {
    prob       <- calcProb(object, theta)
    derivative <- object@slope * prob[, 1] * prob[, 2]
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' d.item.3 = calcDerivative(item.3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcDerivative",
  signature = c("item.3pl", "numeric"),
  definition = function(object, theta) {
    prob       <- calcProb(object, theta)
    derivative <- object@slope * prob[, 1] * (prob[, 2] - object@guessing) / (1 - object@guessing)
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' d.item.4 = calcDerivative(item.4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcDerivative",
  signature = c("item.pc", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected.score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative     <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative <- derivative + prob[, k] * (k - expected.score) * k
    }
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' d.item.5 = calcDerivative(item.5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcDerivative",
  signature = c("item.gpc", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected.score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative     <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative <- derivative + object@slope * prob[, k] * (k - expected.score) * k
    }
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' d.item.6 = calcDerivative(item.6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcDerivative",
  signature = c("item.gr", "numeric"),
  definition = function(object, theta) {
    prob       <- calcProb(object, theta)
    derivative <- numeric(length(theta))
    ps <- matrix(NA, length(theta), object@ncat + 1)
    ps[, 1] <- 1
    ps[, object@ncat + 1] <- 0
    for (k in 2:(object@ncat)) {
      ps[, k] <- ps[, k - 1] - prob[, k - 1]
    }
    for (k in 1:object@ncat) {
      derivative <- derivative + object@slope * (ps[, k] * (1 - ps[, k]) - ps[, k + 1] * (1 - ps[, k + 1]))
    }
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item.pool,numeric-method
#' @examples
#' d.itempool <- calcDerivative(itempool.science, seq(-3, 3, 1))
setMethod(
  f = "calcDerivative",
  signature = c("item.pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        derivative[, i] <- calcDerivative(object@parms[[i]], theta)
      }
    } else {
      stop(msg.missing.theta)
    }
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,pool.cluster,numeric-method
setMethod(
  f = "calcDerivative",
  signature = c("pool.cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        derivative[[i]] <- calcFisher(object@pools[[i]], theta)
      }
    } else {
      stop(msg.missing.theta)
    }
    return(derivative)
  }
)

#' Calculate second derivative
#'
#' An S4 generic and its methods to calculate the second derivative of the probability function.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return Second derivative values.
#'
#' @docType methods
#' @rdname calcDerivative2-methods
#' @export
setGeneric(
  name = "calcDerivative2",
  def = function(object, theta) {
    standardGeneric("calcDerivative2")
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' dd.item.1 = calcDerivative2(item.1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item.1pl", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    derivative2 <- prob[, 1] * prob[, 2] * (prob[, 1] - prob[, 2])
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' dd.item.2 = calcDerivative2(item.2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item.2pl", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    derivative2 <- object@slope^2 * (prob[, 1] * prob[, 2] * (prob[, 1] - prob[, 2]))
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' dd.item.3 = calcDerivative2(item.3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item.3pl", "numeric"),
  definition = function(object, theta) {
    prob        <- calcProb(object, theta)
    derivative2 <- object@slope^2 * prob[, 1] * (prob[, 2] - object@guessing) * (prob[, 1] - prob[, 2] + object@guessing) / (1 - object@guessing)^2
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' dd.item.4 = calcDerivative2(item.4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item.pc", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected.score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative2    <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative2 <- derivative2 + prob[, k] * (k - expected.score) * k
    }
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' dd.item.5 = calcDerivative2(item.5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item.gpc", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected.score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative2    <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative2 <- derivative2 + object@slope * prob[, k] * (k - expected.score) * k
    }
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' dd.item.6 = calcDerivative2(item.6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item.gr", "numeric"),
  definition = function(object, theta) {
    prob        <- calcProb(object, theta)
    derivative2 <- numeric(length(theta))
    ps <- matrix(NA, length(theta), object@ncat + 1)
    ps[, 1] <- 1
    ps[, object@ncat + 1] <- 0
    for (k in 2:(object@ncat)) {
      ps[, k] <- ps[, k - 1] - prob[, k - 1]
    }
    for (k in 1:object@ncat) {
      derivative2 <- derivative2 + object@slope * (ps[, k] * (1 - ps[, k]) - ps[, k + 1] * (1 - ps[, k + 1]))
    }
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item.pool,numeric-method
#' @examples
#' dd.itempool <- calcDerivative2(itempool.science, seq(-3, 3, 1))
setMethod(
  f = "calcDerivative2",
  signature = c("item.pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative2 <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        derivative2[, i] <- calcDerivative(object@parms[[i]], theta)
      }
    } else {
      stop(msg.missing.theta)
    }
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,pool.cluster,numeric-method
setMethod(
  f = "calcDerivative2",
  signature = c("pool.cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative2 <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        derivative2[[i]] <- calcFisher(object@pools[[i]], theta)
      }
    } else {
      stop(msg.missing.theta)
    }
    return(derivative2)
  }
)

#' Calculate first derivative of log-likelihood
#'
#' An S4 generic and its methods to calculate the first derivative of the log-likelihood function.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#' @param resp Response data.
#' @return First derivative values of log-likelihoods.
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
#' @aliases calcJacobian,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' j.item.1 = calcJacobian(item.1, seq(-3, 3, 1), 0)
#' @template 1pl-ref
setMethod(
  f = "calcJacobian",
  signature = c("item.1pl", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(resp - calcEscore(object, theta))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' j.item.2 = calcJacobian(item.2, seq(-3, 3, 1), 0)
#' @template 2pl-ref
setMethod(
  f = "calcJacobian",
  signature = c("item.2pl", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(object@slope * (resp - calcEscore(object, theta)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' j.item.3 = calcJacobian(item.3, seq(-3, 3, 1), 0)
#' @template 3pl-ref
setMethod(
  f = "calcJacobian",
  signature = c("item.3pl", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    expected.score <- calcEscore(object, theta)
    return(object@slope * (resp - expected.score) * (expected.score - object@guessing) / (expected.score * (1.0 - object@guessing)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' j.item.4 = calcJacobian(item.4, seq(-3, 3, 1), 0)
#' @template pc-ref
setMethod(
  f = "calcJacobian",
  signature = c("item.pc", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(resp - calcEscore(object, theta))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' j.item.5 = calcJacobian(item.5, seq(-3, 3, 1), 0)
#' @template gpc-ref
setMethod(
  f = "calcJacobian",
  signature = c("item.gpc", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(object@slope * (resp - calcEscore(object, theta)))
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' j.item.6 = calcJacobian(item.6, seq(-3, 3, 1), 0)
#' @template gr-ref
setMethod(
  f = "calcJacobian",
  signature = c("item.gr", "numeric", "numeric"),
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
#' @aliases calcJacobian,item.pool,numeric-method
#' @examples
#' j.itempool <- calcJacobian(itempool.science, seq(-3, 3, 1), 0)
setMethod(
  f = "calcJacobian",
  signature = c("item.pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      jacobian <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        jacobian[, i] <- calcJacobian(object@parms[[i]], theta, resp[i])
      }
    } else {
      stop(msg.missing.theta)
    }
    return(jacobian)
  }
)

#' @rdname calcJacobian-methods
#' @aliases calcJacobian,pool.cluster,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("pool.cluster", "numeric", "list"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      jacobian <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        jacobian[[i]] <- calcJacobian(object@pools[[i]], theta, resp[[i]])
      }
    } else {
      stop(msg.missing.theta)
    }
    return(jacobian)
  }
)

#' Calculate second derivative of log-likelihood
#'
#' An S4 generic and its methods to calculate the second derivative of the log-likelihood function.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#' @param resp Response data.
#' @return Second derivative values of log-likelihoods.
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
#' @aliases calcHessian,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' h.item.1 = calcHessian(item.1, seq(-3, 3, 1), 0)
#' @template 1pl-ref
setMethod(
  f = "calcHessian",
  signature = c("item.1pl", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' h.item.2 = calcHessian(item.2, seq(-3, 3, 1), 0)
#' @template 2pl-ref
setMethod(
  f = "calcHessian",
  signature = c("item.2pl", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' h.item.3 = calcHessian(item.3, seq(-3, 3, 1), 0)
#' @template 3pl-ref
setMethod(
  f = "calcHessian",
  signature = c("item.3pl", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% c(0, 1))) {
      return(NA)
    }
    prob <- calcProb(object, theta)
    return(object@slope^2 * prob[, 1] * (prob[, 2] - object@guessing) * (object@guessing * resp - prob[, 2]^2) / (prob[, 2]^2 * (1 - object@guessing)^2))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' h.item.4 = calcHessian(item.4, seq(-3, 3, 1), 0)
#' @template pc-ref
setMethod(
  f = "calcHessian",
  signature = c("item.pc", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' h.item.5 = calcHessian(item.5, seq(-3, 3, 1), 0)
#' @template gpc-ref
setMethod(
  f = "calcHessian",
  signature = c("item.gpc", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (!(resp %in% 0:(object@ncat - 1))) {
      return(NA)
    }
    return(-calcFisher(object, theta))
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' h.item.6 = calcHessian(item.6, seq(-3, 3, 1), 0)
#' @template gr-ref
setMethod(
  f = "calcHessian",
  signature = c("item.gr", "numeric", "numeric"),
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
    Hessian <- object@slope^2 * ((ps[, resp] * (1 - ps[, resp]) * ((1 - ps[, resp]) - ps[, resp]) - ps[, resp + 1] * (1 - ps[, resp + 1]) * ((1 - ps[, resp + 1]) - ps[, resp + 1])) / prob[, resp]
      - (ps[, resp] * (1 - ps[, resp]) - ps[, resp + 1] * (1 - ps[, resp + 1]))^2 / prob[, resp]^2)
    return(Hessian)
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,item.pool,numeric-method
#' @examples
#' h.itempool <- calcHessian(itempool.science, seq(-3, 3, 1), 0)
setMethod(
  f = "calcHessian",
  signature = c("item.pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      calcProb(object, theta)
      hessian <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        hessian[, i] <- calcHessian(object@parms[[i]], theta, resp[i])
      }
    } else {
      stop(msg.missing.theta)
    }
    return(hessian)
  }
)

#' @rdname calcHessian-methods
#' @aliases calcHessian,pool.cluster,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("pool.cluster", "numeric", "list"),
  definition = function(object, theta, resp) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      hessian <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        hessian[[i]] <- calcHessian(object@pools[[i]], theta, resp[[i]])
      }
    } else {
      stop(msg.missing.theta)
    }
    return(hessian)
  }
)

#' Simulate item responses
#'
#' An S4 generic and its methods to simulate responses.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return Simulated responses.
#'
#' @export
#' @docType methods
#' @rdname simResp-methods
setGeneric(
  name = "simResp",
  def = function(object, theta) {
    standardGeneric("simResp")
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item.1pl,numeric-method
#' @examples
#' item.1 = new("item.1pl", difficulty = 0.5)
#' sim.item.1 = simResp(item.1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "simResp",
  signature = c("item.1pl", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    random <- runif(length(theta))
    resp <- numeric(length(theta))
    resp[prob[, 2] > random] <- 1
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item.2pl,numeric-method
#' @examples
#' item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
#' sim.item.2 = simResp(item.2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "simResp",
  signature = c("item.2pl", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    random <- runif(length(theta))
    resp <- numeric(length(theta))
    resp[prob[, 2] > random] <- 1
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item.3pl,numeric-method
#' @examples
#' item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' sim.item.3 = simResp(item.3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "simResp",
  signature = c("item.3pl", "numeric"),
  definition = function(object, theta) {
    prob   <- calcProb(object, theta)
    random <- runif(length(theta))
    resp   <- numeric(length(theta))
    resp[prob[, 2] > random] <- 1
    return(resp)
  }
)

#' @rdname simResp-methods
#' @aliases simResp,item.pc,numeric-method
#' @examples
#' item.4 = new("item.pc", threshold = c(-1, 0, 1), ncat = 4)
#' sim.item.4 = simResp(item.4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "simResp",
  signature = c("item.pc", "numeric"),
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
#' @aliases simResp,item.gpc,numeric-method
#' @examples
#' item.5 = new("item.gpc", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' sim.item.5 = simResp(item.5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "simResp",
  signature = c("item.gpc", "numeric"),
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
#' @aliases simResp,item.gr,numeric-method
#' @examples
#' item.6 = new("item.gr", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' sim.item.6 = simResp(item.6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "simResp",
  signature = c("item.gr", "numeric"),
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
#' @aliases simResp,item.pool,numeric-method
#' @examples
#' sim.itempool <- simResp(itempool.science, seq(-3, 3, 1))
setMethod(
  f = "simResp",
  signature = c("item.pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      data <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        data[, i] <- simResp(object@parms[[i]], theta)
      }
      return(data)
    } else {
      stop("invalid values in theta")
    }
  }
)

#' @rdname simResp-methods
#' @aliases simResp,pool.cluster,numeric-method
setMethod(
  f = "simResp",
  signature = c("pool.cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      data <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        data[[i]] <- simResp(object@pools[[i]], theta)
      }
      return(data)
    } else {
      stop("invalid values in theta")
    }
  }
)

#' @rdname simResp-methods
#' @aliases simResp,pool.cluster,list-method
setMethod(
  f = "simResp",
  signature = c("pool.cluster", "list"),
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
