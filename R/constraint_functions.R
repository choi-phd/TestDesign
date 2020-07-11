
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
