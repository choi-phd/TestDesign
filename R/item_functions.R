#' @include item_class.R
NULL

#' Calculate item response probabilities
#'
#' An S4 generic and its methods to calculate item response probabilities for different item classes
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return A matrix of probability values with a dimension (nq, ncat) for a single item or a list of matrices for an instance of "item_pool".
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
#' @aliases calcProb,item_1PL,numeric-method
#' @examples
#' item_1      <- new("item_1PL", difficulty = 0.5)
#' prob_item_1 <- calcProb(item_1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcProb",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    prob <- matrix(NA, length(theta), 2)
    prob[, 2] <- array_p_1pl(theta, object@difficulty)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_2PL,numeric-method
#' @examples
#' item_2      <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' prob_item_2 <- calcProb(item_2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcProb",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    prob <- matrix(NA, length(theta), 2)
    prob[, 2] <- array_p_2pl(theta, object@slope, object@difficulty)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_3PL,numeric-method
#' @examples
#' item_3      <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' prob_item_3 <- calcProb(item_3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcProb",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    prob <- matrix(NA, length(theta), 2)
    prob[, 2] <- array_p_3pl(theta, object@slope, object@difficulty, object@guessing)
    prob[, 1] <- 1 - prob[, 2]
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_PC,numeric-method
#' @examples
#' item_4      <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' prob_item_4 <- calcProb(item_4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcProb",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    prob <- array_p_pc(theta, object@threshold)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_GPC,numeric-method
#' @examples
#' item_5      <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' prob_item_5 <- calcProb(item_5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcProb",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    prob <- array_p_gpc(theta, object@slope, object@threshold)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_GR,numeric-method
#' @examples
#' item_6      <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' prob_item_6 <- calcProb(item_6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcProb",
  signature = c("item_GR", "numeric"),
  definition = function(object, theta) {
    prob <- array_p_gr(theta, object@slope, object@category)
    return(prob)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,item_pool,numeric-method
#' @examples
#' prob_itempool <- calcProb(itempool_science, seq(-3, 3, 1))
setMethod(
  f = "calcProb",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      prob <- lapply(object@parms, calcProb, theta)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(prob)
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
#' @aliases calcEscore,item_1PL,numeric-method
#' @examples
#' item_1     <- new("item_1PL", difficulty = 0.5)
#' ICC_item_1 <- calcEscore(item_1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcEscore",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    return(calcProb(object, theta)[, 2])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_2PL,numeric-method
#' @examples
#' item_2     <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' ICC_item_2 <- calcEscore(item_2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcEscore",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    return(calcProb(object, theta)[, 2])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_3PL,numeric-method
#' @examples
#' item_3     <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' ICC_item_3 <- calcEscore(item_3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcEscore",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    return(calcProb(object, theta)[, 2])
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_PC,numeric-method
#' @examples
#' item_4     <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' ICC_item_4 <- calcEscore(item_4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcEscore",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected_score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    return(expected_score)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_GPC,numeric-method
#' @examples
#' item_5     <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' ICC_item_5 <- calcEscore(item_5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcEscore",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected_score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    return(expected_score)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_GR,numeric-method
#' @examples
#' item_6     <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' ICC_item_6 <- calcEscore(item_6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcEscore",
  signature = c("item_GR", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected_score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    return(expected_score)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,item_pool,numeric-method
#' @examples
#' TCC_itempool <- calcEscore(itempool_science, seq(-3, 3, 1))
setMethod(
  f = "calcEscore",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      expected_score <- as.vector(Reduce("+", lapply(object@parms, calcEscore, theta)))
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(expected_score)
  }
)

#' Calculate Fisher information
#'
#' An S4 generic and its methods to calculate Fisher information given a vector of thetas for different item classes.
#'
#' @param object An instance of an item class.
#' @param theta A vector of theta values.
#'
#' @return A vector of Fisher information values over theta (nq values) for a single item or a matrix of dimension (nq, ni) for an "item_pool".
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
#' @aliases calcFisher,item_1PL,numeric-method
#' @examples
#' item_1      <- new("item_1PL", difficulty = 0.5)
#' info_item_1 <- calcFisher(item_1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcFisher",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    info_Fisher <- array_info_1pl(theta, object@difficulty)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_2PL,numeric-method
#' @examples
#' item_2      <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' info_item_2 <- calcFisher(item_2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcFisher",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    info_Fisher <- array_info_2pl(theta, object@slope, object@difficulty)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_3PL,numeric-method
#' @examples
#' item_3      <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' info_item_3 <- calcFisher(item_3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcFisher",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    info_Fisher <- array_info_3pl(theta, object@slope, object@difficulty, object@guessing)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_PC,numeric-method
#' item_4      <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' info_item_4 <- calcFisher(item_4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcFisher",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    info_Fisher <- array_info_pc(theta, object@threshold)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_GPC,numeric-method
#' @examples
#' item_5      <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' info_item_5 <- calcFisher(item_5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcFisher",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    info_Fisher <- array_info_gpc(theta, object@slope, object@threshold)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_GR,numeric-method
#' @examples
#' item_6      <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' info_item_6 <- calcFisher(item_6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcFisher",
  signature = c("item_GR", "numeric"),
  definition = function(object, theta) {
    info_Fisher <- array_info_gr(theta, object@slope, object@category)
    return(info_Fisher)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,item_pool,numeric-method
#' @examples
#' info_itempool <- calcFisher(itempool_science, seq(-3, 3, 1))
setMethod(
  f = "calcFisher",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      info_Fisher <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        info_Fisher[, i] <- calcFisher(object@parms[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(info_Fisher)
  }
)

#' @rdname calcProb-methods
#' @aliases calcProb,pool_cluster,numeric-method
setMethod(
  f = "calcProb",
  signature = c("pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      prob <- lapply(object@pools, calcProb, theta)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(prob)
  }
)

#' @rdname calcEscore-methods
#' @aliases calcEscore,pool_cluster,numeric-method
setMethod(
  f = "calcEscore",
  signature = c("pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      expected_score <- lapply(object@pools, calcEscore, theta)
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(expected_score)
  }
)

#' @rdname calcFisher-methods
#' @aliases calcFisher,pool_cluster,numeric-method
#' @export
setMethod(
  f = "calcFisher",
  signature = c("pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      info_Fisher <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        info_Fisher[[i]] <- calcFisher(object@pools[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(info_Fisher)
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
#' @aliases calcLocation,item_1PL,numeric-method
#' @examples
#' item_1       <- new("item_1PL", difficulty = 0.5)
#' theta_item_1 <- calcLocation(item_1)
#' @template 1pl-ref
setMethod(
  f = "calcLocation",
  signature = c("item_1PL"),
  definition = function(object) {
    return(object@difficulty)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_2PL,numeric-method
#' @examples
#' item_2       <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' theta_item_2 <- calcLocation(item_2)
#' @template 2pl-ref
setMethod(
  f = "calcLocation",
  signature = c("item_2PL"),
  definition = function(object) {
    return(object@difficulty)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_3PL,numeric-method
#' @examples
#' item_3       <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' theta_item_3 <- calcLocation(item_3)
#' @template 3pl-ref
setMethod(
  f = "calcLocation",
  signature = c("item_3PL"),
  definition = function(object) {
    location <- object@difficulty + (log(0.5 + sqrt(1 + 8 * object@guessing) / 2) / object@slope) # Birnbaum 1968, p. 464
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_PC,numeric-method
#' @examples
#' item_4       <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' theta_item_4 <- calcLocation(item_4)
#' @template pc-ref
setMethod(
  f = "calcLocation",
  signature = c("item_PC"),
  definition = function(object) {
    return(mean(object@threshold))
  }
)


#' @rdname calcLocation-methods
#' @aliases calcLocation,item_GPC,numeric-method
#' @examples
#' item_5       <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' theta_item_5 <- calcLocation(item_5)
#' @template gpc-ref
setMethod(
  f = "calcLocation",
  signature = c("item_GPC"),
  definition = function(object) {
    return(mean(object@threshold))
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_GR,numeric-method
#' @examples
#' item_6       <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' theta_item_6 <- calcLocation(item_6)
#' @template gr-ref
setMethod(
  f = "calcLocation",
  signature = c("item_GR"),
  definition = function(object) {
    return(mean(object@category))
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_pool,numeric-method
#' @examples
#' theta_itempool <- calcLocation(itempool_science)
setMethod(
  f = "calcLocation",
  signature = c("item_pool"),
  definition = function(object) {
    location <- lapply(object@parms, calcLocation)
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,pool_cluster,numeric-method
setMethod(
  f = "calcLocation",
  signature = c("pool_cluster"),
  definition = function(object) {
    location <- lapply(object@pools, calcLocation)
    return(location)
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
#' @aliases calcDerivative,item_1PL,numeric-method
#' @examples
#' item_1   <- new("item_1PL", difficulty = 0.5)
#' d.item_1 <- calcDerivative(item_1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcDerivative",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    prob       <- calcProb(object, theta)
    derivative <- prob[, 1] * prob[, 2]
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item_2PL,numeric-method
#' @examples
#' item_2   <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' d.item_2 <- calcDerivative(item_2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcDerivative",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    prob       <- calcProb(object, theta)
    derivative <- object@slope * prob[, 1] * prob[, 2]
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item_3PL,numeric-method
#' @examples
#' item_3   <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' d.item_3 <- calcDerivative(item_3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcDerivative",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    prob       <- calcProb(object, theta)
    derivative <- object@slope * prob[, 1] * (prob[, 2] - object@guessing) / (1 - object@guessing)
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item_PC,numeric-method
#' @examples
#' item_4   <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' d.item_4 <- calcDerivative(item_4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcDerivative",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected_score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative     <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative <- derivative + prob[, k] * (k - expected_score) * k
    }
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item_GPC,numeric-method
#' @examples
#' item_5   <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' d.item_5 <- calcDerivative(item_5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcDerivative",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected_score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative     <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative <- derivative + object@slope * prob[, k] * (k - expected_score) * k
    }
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,item_GR,numeric-method
#' @examples
#' item_6   <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' d.item_6 <- calcDerivative(item_6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcDerivative",
  signature = c("item_GR", "numeric"),
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
#' @aliases calcDerivative,item_pool,numeric-method
#' @examples
#' d_itempool <- calcDerivative(itempool_science, seq(-3, 3, 1))
setMethod(
  f = "calcDerivative",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        derivative[, i] <- calcDerivative(object@parms[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(derivative)
  }
)

#' @rdname calcDerivative-methods
#' @aliases calcDerivative,pool_cluster,numeric-method
setMethod(
  f = "calcDerivative",
  signature = c("pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        derivative[[i]] <- calcFisher(object@pools[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
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
#' @aliases calcDerivative2,item_1PL,numeric-method
#' @examples
#' item_1    <- new("item_1PL", difficulty = 0.5)
#' dd_item_1 <- calcDerivative2(item_1, seq(-3, 3, 1))
#' @template 1pl-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item_1PL", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    derivative2 <- prob[, 1] * prob[, 2] * (prob[, 1] - prob[, 2])
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item_2PL,numeric-method
#' @examples
#' item_2    <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' dd_item_2 <- calcDerivative2(item_2, seq(-3, 3, 1))
#' @template 2pl-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item_2PL", "numeric"),
  definition = function(object, theta) {
    prob <- calcProb(object, theta)
    derivative2 <- object@slope^2 * (prob[, 1] * prob[, 2] * (prob[, 1] - prob[, 2]))
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item_3PL,numeric-method
#' @examples
#' item_3    <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' dd_item_3 <- calcDerivative2(item_3, seq(-3, 3, 1))
#' @template 3pl-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item_3PL", "numeric"),
  definition = function(object, theta) {
    prob        <- calcProb(object, theta)
    derivative2 <- object@slope^2 * prob[, 1] * (prob[, 2] - object@guessing) * (prob[, 1] - prob[, 2] + object@guessing) / (1 - object@guessing)^2
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item_PC,numeric-method
#' @examples
#' item_4    <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' dd_item_4 <- calcDerivative2(item_4, seq(-3, 3, 1))
#' @template pc-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item_PC", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected_score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative2    <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative2 <- derivative2 + prob[, k] * (k - expected_score) * k
    }
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item_GPC,numeric-method
#' @examples
#' item_5    <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' dd_item_5 <- calcDerivative2(item_5, seq(-3, 3, 1))
#' @template gpc-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item_GPC", "numeric"),
  definition = function(object, theta) {
    prob           <- calcProb(object, theta)
    expected_score <- as.vector(prob %*% t(matrix(0:(object@ncat - 1), 1)))
    derivative2    <- numeric(length(theta))
    for (k in 1:object@ncat) {
      derivative2 <- derivative2 + object@slope * prob[, k] * (k - expected_score) * k
    }
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,item_GR,numeric-method
#' @examples
#' item_6    <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' dd_item_6 <- calcDerivative2(item_6, seq(-3, 3, 1))
#' @template gr-ref
setMethod(
  f = "calcDerivative2",
  signature = c("item_GR", "numeric"),
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
#' @aliases calcDerivative2,item_pool,numeric-method
#' @examples
#' dd_itempool <- calcDerivative2(itempool_science, seq(-3, 3, 1))
setMethod(
  f = "calcDerivative2",
  signature = c("item_pool", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative2 <- matrix(NA, length(theta), object@ni)
      for (i in 1:object@ni) {
        derivative2[, i] <- calcDerivative(object@parms[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
    }
    return(derivative2)
  }
)

#' @rdname calcDerivative2-methods
#' @aliases calcDerivative2,pool_cluster,numeric-method
setMethod(
  f = "calcDerivative2",
  signature = c("pool_cluster", "numeric"),
  definition = function(object, theta) {
    if (length(theta) > 0 && all(!is.na(theta))) {
      derivative2 <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        derivative2[[i]] <- calcFisher(object@pools[[i]], theta)
      }
    } else {
      stop("'theta' is empty, or contains missing values.")
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
#' @aliases calcJacobian,item_1PL,numeric-method
#' @examples
#' item_1   <- new("item_1PL", difficulty = 0.5)
#' j_item_1 <- calcJacobian(item_1, seq(-3, 3, 1), 0)
#' @template 1pl-ref
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
#' @examples
#' item_2   <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' j_item_2 <- calcJacobian(item_2, seq(-3, 3, 1), 0)
#' @template 2pl-ref
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
#' @examples
#' item_3   <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' j_item_3 <- calcJacobian(item_3, seq(-3, 3, 1), 0)
#' @template 3pl-ref
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
#' @examples
#' item_4   <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' j_item_4 <- calcJacobian(item_4, seq(-3, 3, 1), 0)
#' @template pc-ref
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
#' @examples
#' item_5   <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' j_item_5 <- calcJacobian(item_5, seq(-3, 3, 1), 0)
#' @template gpc-ref
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
#' @examples
#' item_6   <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' j_item_6 <- calcJacobian(item_6, seq(-3, 3, 1), 0)
#' @template gr-ref
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
#' @examples
#' j_itempool <- calcJacobian(itempool_science, seq(-3, 3, 1), 0)
setMethod(
  f = "calcJacobian",
  signature = c("item_pool", "numeric", "numeric"),
  definition = function(object, theta, resp) {
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
#' @aliases calcJacobian,pool_cluster,numeric-method
setMethod(
  f = "calcJacobian",
  signature = c("pool_cluster", "numeric", "list"),
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
#' @aliases calcHessian,item_1PL,numeric-method
#' @examples
#' item_1   <- new("item_1PL", difficulty = 0.5)
#' h_item_1 <- calcHessian(item_1, seq(-3, 3, 1), 0)
#' @template 1pl-ref
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
#' @examples
#' item_2   <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' h_item_2 <- calcHessian(item_2, seq(-3, 3, 1), 0)
#' @template 2pl-ref
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
#' @examples
#' item_3   <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' h_item_3 <- calcHessian(item_3, seq(-3, 3, 1), 0)
#' @template 3pl-ref
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
#' @examples
#' item_4   <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' h_item_4 <- calcHessian(item_4, seq(-3, 3, 1), 0)
#' @template pc-ref
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
#' @examples
#' item_5   <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' h_item_5 <- calcHessian(item_5, seq(-3, 3, 1), 0)
#' @template gpc-ref
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
#' @examples
#' item_6   <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' h_item_6 <- calcHessian(item_6, seq(-3, 3, 1), 0)
#' @template gr-ref
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
#' @examples
#' h_itempool <- calcHessian(itempool_science, seq(-3, 3, 1), 0)
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
#' @aliases calcHessian,pool_cluster,numeric-method
setMethod(
  f = "calcHessian",
  signature = c("pool_cluster", "numeric", "list"),
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
#' @aliases simResp,item_1PL,numeric-method
#' @examples
#' item_1     <- new("item_1PL", difficulty = 0.5)
#' sim_item_1 <- simResp(item_1, seq(-3, 3, 1))
#' @template 1pl-ref
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
#' @examples
#' item_2     <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' sim_item_2 <- simResp(item_2, seq(-3, 3, 1))
#' @template 2pl-ref
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
#' @examples
#' item_3     <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' sim_item_3 <- simResp(item_3, seq(-3, 3, 1))
#' @template 3pl-ref
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
#' @examples
#' item_4     <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' sim_item_4 <- simResp(item_4, seq(-3, 3, 1))
#' @template pc-ref
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
#' @examples
#' item_5     <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' sim_item_5 <- simResp(item_5, seq(-3, 3, 1))
#' @template gpc-ref
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
#' @examples
#' item_6     <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#' sim_item_6 <- simResp(item_6, seq(-3, 3, 1))
#' @template gr-ref
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
#' @examples
#' sim_itempool <- simResp(itempool_science, seq(-3, 3, 1))
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
#' @aliases simResp,pool_cluster,numeric-method
setMethod(
  f = "simResp",
  signature = c("pool_cluster", "numeric"),
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
#' @aliases simResp,pool_cluster,list-method
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
          stop(sprintf("invalid values in theta[[%i]]", i))
        }
      }
      return(data)
    } else {
      stop("length of 'theta' must match object@np.")
    }
  }
)
