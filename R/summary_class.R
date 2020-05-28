#' @include helper_functions.R
NULL

#' @rdname summary-classes
setClass(
  "summary_item_pool",
  slots = c(
    ni           = "numeric",
    ni_per_model = "dataframe_or_null"
  ),
  prototype = list(
    ni           = numeric(0),
    ni_per_model = NULL
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

#' @rdname summary-classes
setClass(
  "summary_output_Static",
  slots = c(
   n_targets       = "numeric",
   obj_type        = "character",
   target_location = "numeric",
   selected_items  = "numeric",
   set_based       = "logical",
   n_selected_sets = "numeric_or_null",
   info            = "numeric",
   score           = "numeric"
  ),
  prototype = list(
   n_targets       = numeric(0),
   obj_type        = character(0),
   target_location = numeric(0),
   selected_items  = numeric(0),
   set_based       = logical(0),
   n_selected_sets = numeric(0),
   info            = numeric(0),
   score           = numeric(0)
  ),
  validity = function(object) {
   return(TRUE)
  }
)
