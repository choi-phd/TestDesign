#' @include solver_functions.R
NULL

#' Print solution items
#'
#' @param object Output from \code{\link{Static}} or \code{\link{Shadow}}.
#' @param examinee Examinee id to display the solution. Used when 'object' is from \code{\link{Shadow}}.
#' @param position If supplied, display the item attributes of the shadow test at that item position. If not supplied, display the item attributes of the administered items. Applied when 'object' is from \code{\link{Shadow}}.
#' @param index_only If \code{TRUE} (default), print the item indexes only. Otherwise, print all item attributes.
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

    if (class(object$config) == 'config_Static') {
      cat("Static Assembly : Selected items\n\n")
      tmp <- object$selected
      if (index_only) {
        tmp <- tmp[['INDEX']]
      }
      return(tmp)
    }
    if (class(object$config) == 'config_Shadow') {
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

#' Show constraints
#'
#' Show constraints. This function is a shortcut to access \code{''} slot.
#'
#' @param constraints Output from \code{\link{loadConstraints}}.
#'
#' @docType methods
#' @export

showConstraints <- function(constraints) {
  return(constraints@constraints)
}

