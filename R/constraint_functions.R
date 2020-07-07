
#' @noRd
normalizeConstraintData <- function(x) {

  names(x) <- toupper(names(x))
  x$TYPE   <- toupper(x$TYPE)
  x$WHAT   <- toupper(x$WHAT)
  x$COUNT  <- NA
  x$ONOFF  <- toupper(x$ONOFF)
  x$ONOFF[is.na(x$ONOFF)] <- ""

  return(x)

}

#' @noRd
validateConstraintData <- function(x) {

  if (x$TYPE %in% c("NUMBER", "COUNT")) {

    if (any(c(x$LB, x$UB) < 0)) {
      stop(sprintf("constraint %s: LB and UB must be >= 0", x$CONSTRAINT))
    }
    if (x$LB > x$UB) {
      stop(sprintf("constraint %s: LB <= UB must be TRUE", x$CONSTRAINT))
    }

  }

}
