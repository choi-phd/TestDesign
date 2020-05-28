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

#' @aliases summary,output_Static-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "output_Static", function(object) {
  out <- new("summary_output_Static")
  out@n_targets        <- length(object@config@item_selection$target_location)
  out@obj_type         <- object@config@item_selection$method
  out@target_location  <- object@config@item_selection$target_location
  out@selected_items   <- object@selected[['INDEX']]
  out@set_based        <- object@constraints@set_based
  out@n_selected_sets  <- NULL
  if (out@set_based) {
    out@n_selected_sets <- length(unique(object@selected[['STID']]))
  }
  subpool   <- subsetItemPool(object@pool, select = out@selected_items)
  info      <- calcFisher(subpool, out@target_location)
  out@info  <- rowSums(info)
  out@score <- calcEscore(subpool, out@target_location)
  return(out)
})
