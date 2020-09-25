#' @include summary_functions.R
# Show functions should return NULL invisibly.
# Always call print() internally.
NULL

#' @aliases show,item_1PL-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_1PL", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,item_2PL-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_2PL", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,item_3PL-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_3PL", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,item_PC-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_PC", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,item_GPC-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_GPC", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,item_GR-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_GR", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,item_pool-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_pool", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,pool_cluster-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_pool_cluster", function(object) {
  if (length(object@np) > 0) {
    cat("@np    :", object@np, "\n")
    cat("@names :", paste0(object@names, collapse = ", "), "\n\n")
    for (i in 1:object@np) {
      cat("pool   :", object@names[i], "\n")
      show(object@pools[[i]])
    }
  } else {
    cat("The 'item_pool_cluster' object is empty.")
  }
})

#' @aliases show,item_attrib-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "item_attrib", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,st_attrib-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "st_attrib", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,constraints-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "constraints", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,summary_item_pool-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "summary_item_pool", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,summary_item_attrib-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "summary_item_attrib", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,summary_constraints-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "summary_constraints", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,config_Static-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "config_Static", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,config_Shadow-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "config_Shadow", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,output_Static-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "output_Static", function(object) {
  print(object, index_only = FALSE)
  return(invisible(NULL))
})

#' @aliases show,output_Shadow-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "output_Shadow", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,output_Shadow_all-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "output_Shadow_all", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,summary_output_Static-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "summary_output_Static", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,summary_output_Shadow_all-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "summary_output_Shadow_all", function(object) {
  print(object)
  return(invisible(NULL))
})

#' @aliases show,exposure_rate_plot-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "exposure_rate_plot", function(object) {
  print(object)
  return(invisible(NULL))
})
