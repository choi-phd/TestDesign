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
  idx_to_drop <- which(toupper(names(x@constraints)) == "CONSTRAINT")
  tmp <- tmp[, -idx_to_drop]
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

#' Toggle constraints
#'
#' \code{\link{toggleConstraints}} is a function to toggle individual constraints in a \code{\linkS4class{constraints}} object.
#'
#' @param object a \code{\linkS4class{constraints}} object from \code{\link{loadConstraints}}.
#' @param on constraint indices to mark as active. Also accepts character IDs.
#' @param off constraint indices to mark as inactive. Also accepts character IDs.
#'
#' @return \code{\link{toggleConstraints}} returns the updated \code{\linkS4class{constraints}} object.
#'
#' @examples
#' constraints_science2 <- toggleConstraints(constraints_science, off = 32:36)
#' constraints_science3 <- toggleConstraints(constraints_science2, on = 32:36)
#' constraints_science4 <- toggleConstraints(constraints_science, off = "C32")
#'
#' @export
toggleConstraints <- function(object, on = NULL, off = NULL) {

  nc <- nrow(object@constraints)
  if (length(intersect(on, off)) > 0) {
    stop("toggleConstraints: 'on' and 'off' must have no values in common")
  }
  if (!"ONOFF" %in% names(object@constraints)) {
    object@constraints[["ONOFF"]] <- ""
  }
  if (inherits(on, "character")) {
    on <- sapply(
      on,
      function(x) {
        which(object@constraints[["CONSTRAINT_ID"]] == x)
      }
    )
  }
  if (inherits(off, "character")) {
    off <- sapply(
      off,
      function(x) {
        which(object@constraints[["CONSTRAINT_ID"]] == x)
      }
    )
  }

  if (!is.null(on)) {
    if (any(!is.element(on, 1:nc))) {
      stop(sprintf("toggleConstraints: 'on' should be within c(1:nc), nc = %s", nc))
    }
    for (index in on) {
      object@list_constraints[[index]]@suspend <- FALSE
      object@constraints[index, "ONOFF"] <- ""
    }
  }
  if (!is.null(off)) {
    if (any(!is.element(off, 1:nc))) {
      stop(sprintf("toggleConstraints: 'off' should be within c(1:nc), nc = %s", nc))
    }
    for (index in off) {
      object@list_constraints[[index]]@suspend <- TRUE
      object@constraints[index, "ONOFF"] <- "OFF"
    }
  }

  index <- NULL
  mat   <- NULL
  dir   <- NULL
  rhs   <- NULL

  for (i in 1:nc) {
    if (object@constraints[["TYPE"]][i] != "ORDER" && !object@list_constraints[[i]]@suspend) {
      object@list_constraints[[i]]@nc <- nrow(object@list_constraints[[i]]@mat)
      mat   <- rbind(mat, object@list_constraints[[i]]@mat)
      dir   <- c(dir, object@list_constraints[[i]]@dir)
      rhs   <- c(rhs, object@list_constraints[[i]]@rhs)
      index <- c(index, rep(object@list_constraints[[i]]@constraint, object@list_constraints[[i]]@nc))
    }
  }

  object@index <- index
  object@mat   <- mat
  object@dir   <- dir
  object@rhs   <- rhs

  return(object)
}
