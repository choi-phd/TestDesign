#' @include item_pool_operators.R
NULL

#' Basic functions for item attribute objects
#'
#' @param x an \code{\linkS4class{item_attrib}} object.
#' @param i,j indices to use in subsetting.
#' @param row.names not used, exists for compatibility.
#' @param optional not used, exists for compatibility.
#' @param drop not used, exists for compatibility.
#' @param ... not used, exists for compatibility.
#'
#' @examples
#' x <- itemattrib_science
#' x[1:10]
#' dim(x)
#' ncol(x)
#' nrow(x)
#' colnames(x)
#' rownames(x)
#' names(x)
#' as.data.frame(x)
#'
#' @name item_attrib-operators
NULL

#' @aliases [,item_attrib,numeric,ANY,ANY-method
#' @docType methods
#' @rdname item_attrib-operators
setMethod(
  f = "[",
  signature = c("item_attrib", "numeric"),
  definition = function(x, i, j, ...) {
    return(x@data[i, j])
  }
)

#' @aliases dim,item_attrib-method
#' @docType methods
#' @rdname item_attrib-operators
setMethod(
  f = "dim",
  signature = c("item_attrib"),
  definition = function(x) {
    return(dim(x@data))
  }
)

#' @aliases colnames,item_attrib-method
#' @docType methods
#' @rdname item_attrib-operators
setMethod(
  f = "colnames",
  signature = c("item_attrib"),
  definition = function(x) {
    return(colnames(x@data))
  }
)

#' @aliases rownames,item_attrib-method
#' @docType methods
#' @rdname item_attrib-operators
setMethod(
  f = "rownames",
  signature = c("item_attrib"),
  definition = function(x) {
    return(rownames(x@data))
  }
)

#' @aliases names,item_attrib-method
#' @docType methods
#' @rdname item_attrib-operators
setMethod(
  f = "names",
  signature = c("item_attrib"),
  definition = function(x) {
    return(names(x@data))
  }
)

#' @aliases as.data.frame,item_attrib-method
#' @docType methods
#' @rdname item_attrib-operators
#' @export
setMethod(
  f = "as.data.frame",
  signature = c(x = "item_attrib"),
  definition = function(x, ...) {
    return(as.data.frame(x@data, ...))
  }
)
