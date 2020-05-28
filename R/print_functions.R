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

#' @aliases print,item_attrib-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_attrib", function(x) {
  print(x@data)
  return(invisible(x@data))
})

#' @aliases print,st_attrib-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "st_attrib", function(x) {
  print(x@data)
  return(invisible(x@data))
})
  
#' @aliases print,constraints-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "constraints", function(x) {
  print(x@constraints)
  return(invisible(x@constraints))
})

#' @aliases print,config_Static-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "config_Static", function(x) {
  cat("Static assembly configurations \n\n")
  cat("  Item selection criterion \n")
  cat("    Method         :", x@item_selection$method, "\n")
  cat("    Info type      :", x@item_selection$info_type, "\n")
  cat("    Theta location :", x@item_selection$target_location, "\n")
  cat("    Target value   :", x@item_selection$target_value, "\n")
  cat("    Target weight  :", x@item_selection$target_weight, "\n")
  cat("\n")
  cat("  MIP \n")
  cat("    Solver         :", x@MIP$solver, "\n")
  cat("    Verbosity      :", x@MIP$verbosity, "\n")
  cat("    Time limit     :", x@MIP$time_limit, "\n")
  cat("    Gap limit \n")
  cat("      Relative     :", x@MIP$gap_limit, "\n")
  cat("      Absolute     :", x@MIP$gap_limit_abs, "\n")
  cat("    Obj. tolerance :", x@MIP$obj_tol, "\n")
  cat("\n")
  return(invisible(x))
})
