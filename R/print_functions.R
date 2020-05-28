#' @include summary_class.R
# Print functions should return the argument 'x' invisibly.
NULL

#' @aliases print,item_1PL-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_1PL", function(x) {
  cat("One-parameter logistic or Rasch model (item_1PL)\n")
  cat("  Difficulty     :", x@difficulty, "\n")
  return(invisible(x))
})

#' @aliases print,item_2PL-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_2PL", function(x) {
  cat("Two-parameter logistic model (item_2PL) \n")
  cat("  Slope          :", x@slope, "\n")
  cat("  Difficulty     :", x@difficulty, "\n")
  return(invisible(x))
})

#' @aliases print,item_3PL-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_3PL", function(x) {
  cat("Three-parameter logistic model (item_3PL)\n")
  cat("  Slope          :", x@slope, "\n")
  cat("  Difficulty     :", x@difficulty, "\n")
  cat("  Guessing       :", x@guessing, "\n")
  return(invisible(x))
})

#' @aliases print,item_PC-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_PC", function(x) {
  cat("Partial credit model (item_PC)\n")
  cat("  Threshold     :", x@threshold, "\n")
  cat("  N categories  :", x@ncat, "\n")
  return(invisible(x))
})

#' @aliases print,item_GPC-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_GPC", function(x) {
  cat("Generalized partial credit model (item_GPC)\n")
  cat("  Slope         :", x@slope, "\n")
  cat("  Threshold     :", x@threshold, "\n")
  cat("  N categories  :", x@ncat, "\n")
  return(invisible(x))
})

#' @aliases print,item_GR-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_GR", function(x) {
  cat("Graded response model (item_GR)\n")
  cat("  Slope         :", x@slope, "\n")
  cat("  Category b    :", x@category, "\n")
  cat("  N categories  :", x@ncat, "\n")
  return(invisible(x))
})

#' @aliases print,item_pool-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_pool", function(x) {
  if (length(x@ni) > 0) {
    cat("@ni      :", x@ni, "\n")
    cat("@max_cat :", x@max_cat, "\n\n")
    print(data.frame(index = x@index, id = x@id, model = x@model, NCAT = x@NCAT))
    for (i in 1:x@ni) {
      cat("\n", paste0(x@index[i], ". "))
      print(x@parms[[i]])
    }
    cat("\n")
  } else {
    cat("'item_pool' object with 0 items")
  }
  return(invisible(x))
})
