#' @include solver_functions.R
NULL

#' Print solution items
#'
#' @param object an \code{\linkS4class{output_Static}} object or an \code{\linkS4class{output_Shadow}} object.
#' @param examinee (optional) the examinee index to display the solution. Used when the 'object' argument is an \code{\linkS4class{output_Shadow}} object.
#' @param position (optional) if supplied, display the item attributes of the assembled test at that item position. If not supplied, display the item attributes of the administered items. Used when the 'object' argument is an \code{\linkS4class{output_Shadow}} object.
#' @param index_only if \code{TRUE}, only print item indices. if \code{FALSE}, print all item attributes. (default = \code{TRUE})
#'
#' @return Item attributes of solution items.
#'
#' @docType methods
#' @rdname getSolution-methods
#' @export
setGeneric(
  name = "getSolution",
  def = function(object, examinee = NA, position = NA, index_only = TRUE) {
    standardGeneric("getSolution")
  }
)

#' @docType methods
#' @rdname getSolution-methods
setMethod(
  f = "getSolution",
  signature = "list",
  definition = function(object, examinee = NA, position = NA, index_only = TRUE) {

    if (inherits(object$config, "config_Shadow")) {
      if (!is.na(examinee)) {
        if (is.na(position)) {
          cat(sprintf("Shadow Assembly : Administered items for examinee %i \n\n", examinee))
          position    <- length(object$output[[examinee]]@administered_item_index)
          shadow_test <- object$output[[examinee]]@shadow_test[[position]]
          tmp         <- object$constraints@item_attrib@data[shadow_test, ]
          if (index_only) {
            tmp <- tmp[['INDEX']]
          }
          return(tmp)
        } else {
          cat(sprintf("Shadow Assembly : Shadow test for examinee %i at item position %i\n\n", examinee, position))
          shadow_test <- object$output[[examinee]]@shadow_test[[position]]
          tmp         <- object$constraints@item_attrib@data[shadow_test, ]
          if (index_only) {
            tmp <- tmp[['INDEX']]
          }
          return(tmp)
        }
      } else {
        stop("Shadow() output was supplied but 'examinee' was not supplied")
      }
    }

    stop(sprintf("Unrecognized object type in object$config: %s (must be 'config_Static' or 'config_Shadow')", class(object$config)))
  }
)

#' @docType methods
#' @rdname getSolution-methods
setMethod(
  f = "getSolution",
  signature = "output_Static",
  definition = function(object, examinee = NA, position = NA, index_only = TRUE) {
    .Deprecated("print", msg = "'getSolution' function is deprecated. Use 'print' function instead.")
    print(object, index_only = index_only)
  }
)

#' @noRd
countConstraints <- function(constraints, item_idx) {

  # this is an orphaned function; this is not being used anywhere.

  o <- getSolutionAttributes(constraints, item_idx, TRUE)

  return(o)

}
