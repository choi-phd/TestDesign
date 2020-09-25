#' @include st_attrib_operators.R
NULL

#' @title Basic operators for constraints objects
#'
#' @description
#'
#' Create a subset of a \code{\linkS4class{constraints}} object:
#'
#' \itemize{
#'   \item{\code{constraints[i]}}
#'   \item{\code{subsetConstraints(constraints, 1:10)}}
#' }
#'
#' Combine two \code{\linkS4class{constraints}} objects:
#'
#' \itemize{
#'   \item{\code{c(constraints1, constraints2)}}
#'   \item{\code{combineConstraints(constraints1, constraints2)}}
#' }
#'
#' @param x,x1,x2 a \code{\linkS4class{constraints}} object.
#' @param i,j indices to use in subsetting.
#' @param drop not used, exists for compatibility.
#' @param ... not used, exists for compatibility.
#'
#' @examples
#' c1 <- constraints_science
#' c2 <- c1[1:10]
#' c3 <- c1[c(1, 11:36)] # keep constraint 1 for test length
#' c4 <- c(c2, c3)
#'
#' @name constraints-operators
NULL

#' @rdname constraints-operators
#' @export
subsetConstraints <- function(x, i = NULL) {
  if (!inherits(x, "constraints")) {
    stop("'x' must be a 'constraints' object")
  }
  if (!validObject(x)) {
    stop("'x' is not a valid 'constraints' object")
  }

  if (is.null(i)) {
    return(x)
  }

  if (!all(i %in% 1:nrow(x@constraints))) {
    stop("'i' contains constraint indices not defined in @constraints")
  }

  tmp <- x@constraints[i, ]
  o   <- loadConstraints(
    tmp,
    x@pool,
    x@item_attrib,
    x@st_attrib
  )
  return(o)

}

#' @noRd
combineConstraintsData <- function(raw1, raw2) {

  tmp       <- setdiff(names(raw1), names(raw2))
  raw2[tmp] <- NA
  tmp       <- setdiff(names(raw2), names(raw1))
  raw1[tmp] <- NA

  raw       <- rbind(raw1, raw2)

  idx       <- which(!duplicated(raw$CONSTRAINT))
  raw       <- raw[idx, ]

  return(raw)

}

#' @rdname constraints-operators
#' @export
combineConstraints <- function(x1, x2) {

  if (!inherits(x1, "constraints") || !inherits(x2, "constraints")) {
    stop("operands must be 'constraints' objects")
  }
  if (!validObject(x1)) {
    stop("'x1' is not a valid 'constraints' object")
  }
  if (!validObject(x2)) {
    stop("'x2' is not a valid 'constraints' object")
  }

  if (!identical(x1@pool, x2@pool)) {
    stop("@pool does not match")
  }
  if (!identical(x1@item_attrib, x2@item_attrib)) {
    stop("@item_attrib does not match")
  }
  if (!identical(x1@st_attrib, x2@st_attrib)) {
    stop("@st_attrib does not match")
  }

  raw <- combineConstraintsData(x1@constraints, x2@constraints)
  o   <- loadConstraints(raw, x1@pool, x1@item_attrib, x1@st_attrib)

  id     <- c(x1@constraints$CONSTRAINT, x1@constraints$CONSTRAINT)
  if (sum(duplicated(id)) > 0) {
    warning(sprintf("duplicate constraint IDs were removed: %s", paste0(id[duplicated(id)], collapse = ", ")))
  }

  return(o)

}

#' @aliases [,constraints,numeric,ANY,ANY-method
#' @docType methods
#' @rdname constraints-operators
setMethod(
  f = "[",
  signature = c("constraints", "numeric"),
  definition = function(x, i, j, ...) {
    return(subsetConstraints(x, i))
  }
)

#' @aliases c,constraints-method
#' @docType methods
#' @rdname constraints-operators
setMethod(
  f = "c",
  signature = "constraints",
  definition = function(x, ...) {
    arg  <- list(...)
    o <- x
    for (tmp in arg) {
      o <- combineConstraints(o, tmp)
    }
    return(o)
  }
)
