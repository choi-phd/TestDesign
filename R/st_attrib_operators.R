#' @include item_attrib_operators.R
NULL

#' Basic functions for stimulus attribute objects
#'
#' @param x a \code{\linkS4class{st_attrib}} object.
#' @param i,j indices to use in subsetting.
#' @param row.names not used, exists for compatibility.
#' @param optional not used, exists for compatibility.
#' @param drop not used, exists for compatibility.
#' @param ... not used, exists for compatibility.
#'
#' @examples
#' x <- stimattrib_reading
#' x[1:10]
#' dim(x)
#' ncol(x)
#' nrow(x)
#' colnames(x)
#' rownames(x)
#' names(x)
#' as.data.frame(x)
#'
#' @name st_attrib-operators
NULL

#' @aliases [,st_attrib,numeric,ANY,ANY-method
#' @docType methods
#' @rdname st_attrib-operators
setMethod(
  f = "[",
  signature = c("st_attrib", "numeric"),
  definition = function(x, i, j, ...) {
    return(x@data[i, j])
  }
)

#' @aliases dim,st_attrib-method
#' @docType methods
#' @rdname st_attrib-operators
setMethod(
  f = "dim",
  signature = c("st_attrib"),
  definition = function(x) {
    return(dim(x@data))
  }
)

#' @aliases colnames,st_attrib-method
#' @docType methods
#' @rdname st_attrib-operators
setMethod(
  f = "colnames",
  signature = c("st_attrib"),
  definition = function(x) {
    return(colnames(x@data))
  }
)

#' @aliases rownames,st_attrib-method
#' @docType methods
#' @rdname st_attrib-operators
setMethod(
  f = "rownames",
  signature = c("st_attrib"),
  definition = function(x) {
    return(rownames(x@data))
  }
)

#' @aliases names,st_attrib-method
#' @docType methods
#' @rdname st_attrib-operators
setMethod(
  f = "names",
  signature = c("st_attrib"),
  definition = function(x) {
    return(names(x@data))
  }
)

#' @aliases as.data.frame,st_attrib-method
#' @docType methods
#' @rdname st_attrib-operators
#' @export
setMethod(
  f = "as.data.frame",
  signature = c(x = "st_attrib"),
  definition = function(x, ...) {
    return(as.data.frame(x@data, ...))
  }
)
