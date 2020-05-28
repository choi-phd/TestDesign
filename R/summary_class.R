#' @include helper_functions.R
NULL

#' @rdname summary-classes
setClass(
  "summary_item_pool",
  slots = c(
    ni           = "numeric",
    ni_per_model = "dataframe_or_null",
    has_se       = "logical"
  ),
  prototype = list(
    ni           = numeric(0),
    ni_per_model = NULL,
    has_se       = logical(0)
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' @rdname summary-classes
setClass(
  "summary_item_attrib",
  slots = c(
    attribs = "character",
    levels  = "list"
  ),
  prototype = list(
    attribs = character(0),
    levels  = list()
  ),
  validity = function(object) {
    return(TRUE)
  }
)
