#' @include calc_escore_functions.r
NULL

#' Calculate central location (overall difficulty)
#'
#' \code{\link{calcLocation}} is a function to calculate the central location (overall difficulty) of items.
#'
#' @param object an \code{\link{item}} or an \code{\linkS4class{item_pool}} object.
#'
#' @return
#' \describe{
#'   \item{\code{\link{item}} object:}{\code{\link{calcLocation}} returns a theta value representing the central location.}
#'   \item{\code{\linkS4class{item_pool}} object:}{\code{\link{calcProb}} returns a length \emph{ni} list, each containing the central location of the item.}
#' }
#' \describe{
#'   \item{\emph{notations}}{\itemize{
#'     \item{\emph{ni} denotes the number of items in the \code{\linkS4class{item_pool}} object.}
#'   }}
#' }
#'
#' @examples
#' item_1      <- new("item_1PL", difficulty = 0.5)
#' item_2      <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3      <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4      <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
#' item_5      <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
#' item_6      <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)
#'
#' loc_item_1 <- calcLocation(item_1)
#' loc_item_2 <- calcLocation(item_2)
#' loc_item_3 <- calcLocation(item_3)
#' loc_item_4 <- calcLocation(item_4)
#' loc_item_5 <- calcLocation(item_5)
#' loc_item_6 <- calcLocation(item_6)
#' loc_pool   <- calcLocation(itempool_science)
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @docType methods
#' @name calcLocation-methods
#' @aliases calcLocation
#' @export
setGeneric(
  name = "calcLocation",
  def = function(object) {
    standardGeneric("calcLocation")
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_1PL-method
setMethod(
  f = "calcLocation",
  signature = c("item_1PL"),
  definition = function(object) {
    location <- object@difficulty
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_2PL-method
setMethod(
  f = "calcLocation",
  signature = c("item_2PL"),
  definition = function(object) {
    location <- object@difficulty
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_3PL-method
setMethod(
  f = "calcLocation",
  signature = c("item_3PL"),
  definition = function(object) {
    # Birnbaum 1968, p. 464
    location <- object@difficulty +
      (log(0.5 + sqrt(1 + 8 * object@guessing) / 2)) / object@slope
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_PC-method
setMethod(
  f = "calcLocation",
  signature = c("item_PC"),
  definition = function(object) {
    location <- mean(object@threshold)
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_GPC-method
setMethod(
  f = "calcLocation",
  signature = c("item_GPC"),
  definition = function(object) {
    location <- mean(object@threshold)
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_GR-method
setMethod(
  f = "calcLocation",
  signature = c("item_GR"),
  definition = function(object) {
    location <- mean(object@category)
    return(location)
  }
)

#' @rdname calcLocation-methods
#' @aliases calcLocation,item_pool-method
setMethod(
  f = "calcLocation",
  signature = c("item_pool"),
  definition = function(object) {
    location <- lapply(object@parms, calcLocation)
    return(location)
  }
)
