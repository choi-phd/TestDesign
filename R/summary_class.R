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

#' @rdname summary-classes
setClass(
  "summary_constraints",
  slots = c(
    n_constraints     = "numeric",
    n_mip_constraints = "numeric",
    test_length       = "numeric",
    set_based         = "logical"
  ),
  prototype = list(
    n_constraints     = numeric(0),
    n_mip_constraints = numeric(0),
    test_length       = numeric(0),
    set_based         = logical(0)
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
   score           = "numeric",
   achieved        = "dataframe_or_null"
  ),
  prototype = list(
   n_targets       = numeric(0),
   obj_type        = character(0),
   target_location = numeric(0),
   selected_items  = numeric(0),
   set_based       = logical(0),
   n_selected_sets = numeric(0),
   info            = numeric(0),
   score           = numeric(0),
   achieved        = NULL
  ),
  validity = function(object) {
   return(TRUE)
  }
)

#' @rdname summary-classes
setClass(
  "summary_output_Shadow_all",
  slots = c(
    n_simulee       = "numeric",
    test_length     = "numeric",
    true_theta      = "numeric_or_null",
    est_theta       = "numeric_or_null",
    est_theta_se    = "numeric_or_null",
    diff            = "numeric_or_null",
    mse             = "numeric_or_null",
    bias            = "numeric_or_null",
    corr            = "numeric_or_null",
    average_se      = "numeric",
    achieved        = "dataframe_or_null"
  ),
  prototype = list(
    n_simulee       = numeric(0),
    test_length     = numeric(0),
    true_theta      = numeric(0),
    est_theta       = numeric(0),
    est_theta_se    = numeric(0),
    diff            = numeric(0),
    mse             = numeric(0),
    bias            = numeric(0),
    corr            = numeric(0),
    average_se      = numeric(0),
    achieved        = NULL
  ),
  validity = function(object) {
    return(TRUE)
  }
)
