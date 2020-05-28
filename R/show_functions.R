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
