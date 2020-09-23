#' @include solver_functions.R
NULL

#' Print solution items
#'
#' @param object an \code{\linkS4class{output_Static}} object or an \code{\linkS4class{output_Shadow}} object.
#' @param examinee (optional) the examinee index to display the solution. Used when the 'object' argument is an \code{\linkS4class{output_Shadow}} object.
#' @param position (optional) if supplied, display the item attributes of the assembled test at that item position. If not supplied, display the item attributes of the administered items. Used when the 'object' argument is an \code{\linkS4class{output_Shadow}} object.
#' @param index_only if \code{TRUE}, only print item indices. if \code{FALSE}, print all item attributes. (default = {TRUE})
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

#' (deprecated) Show constraints
#'
#' (deprecated) Use \code{\link[TestDesign:print-methods]{print}} instead.
#'
#' @param constraints a \code{\linkS4class{constraints}} object.
#'
#' @docType methods
#' @export
showConstraints <- function(constraints) {
  .Deprecated("print", msg = "'showConstraints' function is deprecated. Use 'print' function instead.")
  print(constraints)
}

#' @noRd
countConstraints <- function(constraints, item_idx) {

  if (!inherits(constraints, "constraints")) {
    stop("'constraints' must be a 'constraints' class object")
  }

  set_based   <- constraints@set_based
  item_attrib <- constraints@item_attrib
  constraints <- constraints@constraints

  nc <- nrow(constraints)
  list_constraints <- vector(mode = "list", length = nc)
  item_constraints <- which(constraints[["WHAT"]] == "ITEM")
  stim_constraints <- which(constraints[["WHAT"]] %in% c("STIMULUS", "PASSAGE", "SET", "TESTLET"))

  count <- vector('list', nc)

  for (index in item_constraints) {
    if (constraints[["TYPE"]][index] %in% c("NUMBER", "COUNT")) {
      if (toupper(constraints[["CONDITION"]][index]) %in% c("", " ", "PER TEST", "TEST")) {
        count[[index]] <- length(item_idx)
      } else if (toupper(constraints[["CONDITION"]][index]) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {
        tmp            <- item_attrib@data[item_idx, ]
        count[[index]] <- aggregate(tmp[["ID"]], by = list(tmp[["STID"]]), function(x) length(x))[, -1]
      } else if (constraints[["CONDITION"]][index] %in% names(item_attrib@data)) {
      } else {
        match_vec      <- with(item_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
        count[[index]] <- sum(item_idx %in% which(match_vec))
      }

    }
  }

  if (set_based) {
    for (index in stim_constraints) {
      if (constraints[["TYPE"]][index] %in% c("NUMBER", "COUNT")) {
        tmp <- item_attrib@data[item_idx, ]
        count[[index]] <- length(na.omit(unique(tmp[["STID"]])))
      }
    }
  }

  return(count)

}
