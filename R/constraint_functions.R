
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
validateLBUB <- function(x) {
  if (any(c(x$LB, x$UB) < 0)) {
      stop(sprintf("constraint %s: LB and UB must be >= 0", x$CONSTRAINT))
    }
  if (x$LB > x$UB) {
    stop(sprintf("constraint %s: LB <= UB must be TRUE", x$CONSTRAINT))
  }
}

#' @noRd
validateExpression <- function(x, attrib, unit_name, use_lt) {

  try_parse <- try(parse(text = x$CONDITION))
  if (inherits(try_parse, "try-error")) {
    stop(sprintf("constraint %s: '%s' is not a valid expression", x$CONSTRAINT, x$CONDITION))
  }

  idx <- with(attrib@data, eval(try_parse))
  if (use_lt & length(which(idx)) < 2) {
    stop(sprintf("constraint %s: '%s' has < 2 %s", x$CONSTRAINT, x$CONDITION, unit_name))
  }
  if (!use_lt & length(which(idx)) == 0) {
    stop(sprintf("constraint %s: '%s' does not match any %s", x$CONSTRAINT, x$CONDITION, unit_name))
  }

}

#' @noRd
validateConstraintData <- function(x, attrib) {

  if (inherits(attrib, "item_attrib")) {
    unit_name  <- "items"
    class_name <- "item_attrib"
  }
  if (inherits(attrib, "st_attrib")) {
    unit_name  <- "stimuli"
    class_name <- "st_attrib"
  }

  if (x$TYPE %in% c("NUMBER", "COUNT")) {

    validateLBUB(x)

    if (
      toupper(x$CONDITION) %in%
      c("", " ", "PER TEST", "TEST")) {
      return()
    }

    if (inherits(attrib, "item_attrib")) {
      if (
        toupper(x$CONDITION) %in%
        c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {
        return()
      }
    }

    if (
      toupper(x$CONDITION) %in%
      names(attrib@data)) {
      return()
    }

    validateExpression(x, attrib, unit_name, FALSE)

    return()

  }

  if (x$TYPE %in% c("SUM", "AVERAGE", "MEAN")) {

    validateLBUB(x)

    if (!(x$CONDITION %in% names(attrib@data))) {
      stop(sprintf("constraint %s: column '%s' not found in %s", x$CONSTRAINT, x$CONDITION, class_name))
    }

    if (any(is.na(attrib@data[[x$CONDITION]]))) {
      stop(sprintf("constraint %s: %s '%s' must not have any missing values", x$CONSTRAINT, class_name, x$CONDITION))
    }

    return()

  }

  if (x$TYPE == "INCLUDE") {

    validateExpression(x, attrib, unit_name, FALSE)
    return()

  }

  if (x$TYPE %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {

    validateExpression(x, attrib, unit_name, FALSE)
    return()

  }

  if (x$TYPE %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {

    validateExpression(x, attrib, unit_name, TRUE)
    return()

  }

  if (x$TYPE %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {

    validateExpression(x, attrib, unit_name, TRUE)
    return()

  }

  if (x$TYPE == "ORDER") {

    if (!(x$CONDITION %in% names(attrib@data))) {
      stop(sprintf("constraint %s: column '%s' not found in %s", x$CONSTRAINT, x$CONDITION, class_name))
    }

    if (any(is.na(attrib@data[[x$CONDITION]]))) {
      stop(sprintf("constraint %s: %s '%s' must not have any missing values", x$CONSTRAINT, class_name, x$CONDITION))
    }

    return()

  }

  stop(sprintf("constraint %s: unrecognized type '%s'", x$CONSTRAINT, x$TYPE))

}
