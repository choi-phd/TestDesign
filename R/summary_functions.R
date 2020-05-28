#' @include helper_functions.R
NULL

#' @aliases summary,item_pool-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "item_pool", function(object) {
  out <- new("summary_item_pool")
  out@ni           <- object@ni
  tmp              <- as.data.frame(table(object@model))
  colnames(tmp)    <- c("model", "items")
  out@ni_per_model <- tmp
  out@has_se       <- any(na.omit(object@se) > 0)
  return(out)
})

#' @aliases summary,item_attrib-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "item_attrib", function(object) {
  out <- new("summary_item_attrib")
  out@attribs <- names(object@data)
  out@levels  <- list()
  for (a in out@attribs) {
    out@levels[[a]] <- sort(unique(object@data[[a]]))
  }
  return(out)
})

#' @aliases summary,constraints-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "constraints", function(object) {
  out <- new("summary_constraints")
  out@n_constraints     <- dim(object@constraints)[1]
  out@n_mip_constraints <- sum(unlist(lapply(object@list_constraints, function(x) x@nc)))
  out@test_length       <- object@test_length
  out@set_based         <- object@set_based
  return(out)
})
