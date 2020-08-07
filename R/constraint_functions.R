
#' @noRd
normalizeConstraintData <- function(x) {

  names(x)    <- toupper(names(x))

  validCONSTRAINT <- as.character(1:dim(x)[1])
  if (is.null(x$CONSTRAINT)) {
    CONSTRAINT <- validCONSTRAINT
    x <- cbind(CONSTRAINT, x)
  }
  if (
    any(x$CONSTRAINT != validCONSTRAINT) |
    !inherits(x$CONSTRAINT, "character")) {
    x$CONSTRAINT <- validCONSTRAINT
    warning("the 'CONSTRAINT' column was ignored and replaced with valid indices")
  }

  x$TYPE      <- toupper(x$TYPE)
  x$WHAT      <- toupper(x$WHAT)
  x$COUNT     <- NA
  if (is.null(x$ONOFF)) {
    x$ONOFF <- NA
  }
  x$ONOFF     <- toupper(x$ONOFF)
  x$ONOFF[is.na(x$ONOFF)] <- ""
  x$CONDITION <- trimws(x$CONDITION)

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
validateFullColumn <- function(x, attrib, class_name) {

  if (!(x$CONDITION %in% names(attrib@data))) {
    stop(sprintf("constraint %s: column '%s' not found in %s", x$CONSTRAINT, x$CONDITION, class_name))
  }

  if (any(is.na(attrib@data[[x$CONDITION]]))) {
    stop(sprintf("constraint %s: %s '%s' must not have any missing values", x$CONSTRAINT, class_name, x$CONDITION))
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

    if (
      toupper(x$CONDITION) %in%
      names(attrib@data)) {
      return()
    }

    if (
      grepl("\\[", x$CONDITION) |
      grepl(",", x$CONDITION)
    ) {
      return()
    }

    validateFullColumn(x, attrib, class_name)

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

    validateFullColumn(x, attrib, class_name)
    return()

  }

  stop(sprintf("constraint %s: unrecognized type '%s'", x$CONSTRAINT, x$TYPE))

}

#' @noRd
getLBUBInConstraintData <- function(o, x, item_constraints, stim_constraints) {

  for (i in item_constraints) {
    if (x$TYPE[i] %in% c("NUMBER", "COUNT")) {
      if (toupper(x$CONDITION[i]) %in% c("", " ", "PER TEST", "TEST")) {
        o$i_count$LB <- round(x$LB[i])
        o$i_count$UB <- round(x$UB[i])
      }
    }
  }

  for (s in stim_constraints) {
    if (x$TYPE[s] %in% c("NUMBER", "COUNT")) {
      if (toupper(x$CONDITION[s]) %in% c("", " ", "PER TEST", "TEST")) {
        o$s_count$LB <- round(x$LB[s])
        o$s_count$UB <- round(x$UB[s])
      }
    }
  }

  return(o)

}

#' @noRd
parseConstraintData <- function(x, attrib, constants) {

  if (inherits(attrib, "item_attrib")) {
    nx_pad <- 0
    nx     <- constants$ni
  }
  if (inherits(attrib, "st_attrib")) {
    nx_pad <- constants$ni
    nx     <- constants$ns
  }

  ni        <- constants$ni
  ns        <- constants$ns
  nv        <- constants$nv
  set_based <- constants$set_based
  i_by_s    <- constants$i_by_s
  s_by_i    <- constants$s_by_i
  i_count   <- constants$i_count
  s_count   <- constants$s_count

  o <- new("constraint")
  o@constraint <- x$CONSTRAINT
  o@suspend    <- x$ONOFF == "OFF"

  if (x$TYPE %in% c("NUMBER", "COUNT")) {

    if (toupper(x$CONDITION) %in% c("", " ", "PER TEST", "TEST")) {

      if (nx == ni) {

        LB <- round(x$LB)
        o@mat <- matrix(0, nrow = 1, ncol = nv)
        o@mat[1, 1:ni] <- 1
        o@dir <- "=="
        o@rhs <- LB

      }

      if (nx == ns) {

        LB <- round(x$LB)
        UB <- round(x$UB)
        if (LB == UB) {
          o@mat <- matrix(0, nrow = 1, ncol = nv)
          o@mat[1, ni + (1:ns)] <- 1
          o@dir <- "=="
          o@rhs <- LB
        } else {
          o@mat <- matrix(0, nrow = 2, ncol = nv)
          o@mat[, ni + (1:ns)] <- 1
          o@dir <- c(">=", "<=")
          o@rhs <- c(LB, UB)
        }

      }

      return(o)

    }

    if (nx == ni & toupper(x$CONDITION) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {

      LB <- round(x$LB)
      UB <- round(x$UB)

      if (LB == UB) {
        o@mat <- matrix(0, nrow = ns, ncol = nv)
        o@dir <- rep("==", ns)
        o@rhs <- rep(0   , ns)
        for (s in 1:ns) {
          o@mat[s, i_by_s[[s]]] <- 1
          o@mat[s, ni + s] <- -LB
        }
      } else {
        o@mat <- matrix(0, nrow = ns * 2, ncol = nv)
        o@dir <- rep(c(">=", "<="), ns)
        o@rhs <- rep(0            , ns * 2)
        for (s in 1:ns) {
          o@mat[c(s * 2 - 1, s * 2), i_by_s[[s]]] <- 1
          o@mat[c(s * 2 - 1), ni + s] <- -LB
          o@mat[c(s * 2)    , ni + s] <- -UB
        }
      }

      return(o)

    }

    if (x$CONDITION %in% names(attrib@data)) {

      levels <- attrib@data[, x$CONDITION]
      levels <- na.omit(unique(levels))

      if (x$LB == x$UB) {
        o@mat <- matrix(0, nrow = length(levels), ncol = nv)
        o@dir <- rep("==", length(levels))
        o@rhs <- rep(x$LB, length(levels))
        for (m in 1:length(levels)) {
          idx_match <- which(attrib@data[x$CONDITION] == levels[m])
          o@mat[m, nx_pad + idx_match] <- 1
        }
      } else {
        o@mat <- matrix(0, nrow = 2 * length(levels), ncol = nv)
        o@dir <- rep(c(">=", "<="), length(levels))
        o@rhs <- rep(c(x$LB, x$UB), length(levels))
        for (m in 1:length(levels)) {
          idx_match <- which(attrib@data[x$CONDITION] == levels[m])
          o@mat[c(m * 2 - 1, m * 2), nx_pad + idx_match] <- 1
        }
      }

      return(o)

    }

    if (TRUE) {

      flag <- with(attrib@data, eval(parse(text = x$CONDITION)))
      idx  <- which(flag)

      if (x$LB == x$UB) {
        o@mat <- matrix(0, nrow = 1, ncol = nv)
        o@mat[1, nx_pad + idx] <- 1
        o@dir <- "=="
        o@rhs <- x$UB
      } else {
        o@mat <- matrix(0, nrow = 2, ncol = nv)
        o@mat[, nx_pad + idx] <- 1
        o@dir <- c(">=", "<=")
        o@rhs <- c(x$LB, x$UB)
      }

      return(o)

    }

  }

  if (x$TYPE %in% c("SUM", "AVERAGE", "MEAN")) {

    if (nx == ni) {
      denom_LB <- i_count$LB
      denom_UB <- i_count$UB
    }
    if (nx == ns) {
      denom_LB <- s_count$LB
      denom_UB <- s_count$UB
    }

    if (x$CONDITION %in% names(attrib@data)) {

      if (x$LB == x$UB) {
        o@mat <- matrix(0, nrow = 1, ncol = nv)
        o@dir <- "=="
        o@rhs <- x$LB
        if (x$TYPE == "SUM") {
          o@mat[1, nx_pad + (1:nx)] <- attrib@data[[x$CONDITION]]
        } else if (x$TYPE %in% c("AVERAGE", "MEAN")) {
          o@mat[1, nx_pad + (1:nx)] <- attrib@data[[x$CONDITION]] / denom_LB
        }
      } else {
        o@mat <- matrix(0, nrow = 2, ncol = nv)
        o@dir <- c(">=", "<=")
        o@rhs <- c(x$LB, x$UB)
        if (x$TYPE == "SUM") {
          o@mat[1, nx_pad + (1:nx)] <- attrib@data[[x$CONDITION]]
          o@mat[2, nx_pad + (1:nx)] <- attrib@data[[x$CONDITION]]
        } else if (x$TYPE %in% c("AVERAGE", "MEAN")) {
          o@mat[1, nx_pad + (1:nx)] <- attrib@data[[x$CONDITION]] / denom_UB
          o@mat[2, nx_pad + (1:nx)] <- attrib@data[[x$CONDITION]] / denom_LB
        }
      }

      return(o)

    }

    if (
      grepl(",", x$CONDITION) |
      grepl("\\[", x$CONDITION)) {

      if (grepl(",", x$CONDITION)) {
        selector_pos <- regexpr(",", x$CONDITION)
        variable     <- substr(x$CONDITION, 1, selector_pos - 1)
        condition    <- substr(x$CONDITION, selector_pos + 1, nchar(x$CONDITION))
      }
      if (grepl("\\[", x$CONDITION)) {
        selector_pos <- regexpr("\\[", x$CONDITION)
        variable     <- substr(x$CONDITION, 1, selector_pos - 1)
        condition    <- substr(x$CONDITION, selector_pos + 1, nchar(x$CONDITION) - 1)
      }

      flag <- with(attrib@data, eval(parse(text = condition)))
      idx  <- which(flag)
      tmp  <- attrib@data[[variable]]
      tmp[!flag] <- 0

      if (x$LB == x$UB) {
        o@mat <- matrix(0, nrow = 1, ncol = nv)
        o@dir <- "=="
        o@rhs <- x$LB
        if (x$TYPE == "SUM") {
          o@mat[1, nx_pad + (1:nx)] <- tmp
        } else if (x$TYPE %in% c("AVERAGE", "MEAN")) {
          o@mat[1, nx_pad + (1:nx)] <- tmp / denom_LB
        }
      } else {
        o@mat <- matrix(0, nrow = 2, ncol = nv)
        o@dir <- c(">=", "<=")
        o@rhs <- c(x$LB, x$UB)
        if (x$TYPE == "SUM") {
          o@mat[1, nx_pad + (1:nx)] <- tmp
          o@mat[2, nx_pad + (1:nx)] <- tmp
        } else if (x$TYPE %in% c("AVERAGE", "MEAN")) {
          o@mat[1, nx_pad + (1:nx)] <- tmp / denom_UB
          o@mat[2, nx_pad + (1:nx)] <- tmp / denom_LB
        }
      }

    }

  }

  if (x$TYPE == "INCLUDE") {

    flag <- with(attrib@data, eval(parse(text = x$CONDITION)))
    idx  <- which(flag)

    o@mat <- matrix(0, nrow = 1, ncol = nv)
    o@mat[1, nx_pad + idx] <- 1
    o@dir <- "=="
    o@rhs <- length(idx)

    if (nx == ni && set_based) {
      s <- na.omit(unique(s_by_i[idx]))
      o@mat[1, nx_pad + s] <- 1
      o@rhs <- o@rhs + length(s)
    }

    return(o)

  }

  if (x$TYPE %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {

    flag <- with(attrib@data, eval(parse(text = x$CONDITION)))
    idx  <- which(flag)

    o@mat <- matrix(0, nrow = 1, ncol = nv)
    o@mat[1, nx_pad + idx] <- 1
    o@dir <- "=="
    o@rhs <- 0

    if (nx == ns) {
      for (s in idx) {
        o@mat[1, i_by_s[[s]]] <- 1
      }
    }

    return(o)

  }

  if (x$TYPE %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {

    flag  <- with(attrib@data, eval(parse(text = x$CONDITION)))
    idx   <- which(flag)
    n_idx <- sum(flag)

    o@mat <- matrix(0, nrow = (n_idx * (n_idx - 1)) / 2, ncol = nv)
    o@dir <- rep("==", (n_idx * (n_idx - 1)) / 2)
    o@rhs <- rep(0, (n_idx * (n_idx - 1)) / 2)

    tmp_idx <- 0
    for (i in idx) {
      for (j in idx) {
        if (i < j) {
          tmp_idx <- tmp_idx + 1
          o@mat[tmp_idx, nx_pad + c(i, j)] <- c(1, -1)
        }
      }
    }

    return(o)

  }


  if (x$TYPE %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {

    flag <- with(attrib@data, eval(parse(text = x$CONDITION)))
    idx  <- which(flag)

    o@mat <- matrix(0, nrow = 1, ncol = nv)
    o@mat[1, nx_pad + idx] <- 1
    o@dir <- "<="
    o@rhs <- 1

    return(o)

  }

  return(o)

}

#' @noRd
addCountsToConstraintData <- function(x, attrib) {

  if (inherits(attrib, "item_attrib")) {
    count_name <- "COUNT"
  }
  if (inherits(attrib, "st_attrib")) {
    count_name <- "ST_COUNT"
  }

  if (x$TYPE %in% c("NUMBER", "COUNT")) {

    if (toupper(x$CONDITION) %in% c("", " ", "PER TEST", "TEST")) {
      x[[count_name]] <- dim(attrib@data)[1]
      return(x)
    }

    if (toupper(x$CONDITION) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {
      return(x)
    }

    if (x$CONDITION %in% names(attrib@data)) {
      condition <- attrib@data[x$CONDITION]
      x[[count_name]] <- sum(!is.na(condition))
      return(x)
    }

    if (TRUE) {
      match_vec       <- with(attrib@data, eval(parse(text = x$CONDITION)))
      x[[count_name]] <- sum(match_vec)
      return(x)
    }

  }

  if (x$TYPE == "INCLUDE") {
    match_vec       <- with(attrib@data, eval(parse(text = x$CONDITION)))
    x[[count_name]] <- sum(match_vec)
    return(x)
  }

  if (x$TYPE %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {
    match_vec       <- with(attrib@data, eval(parse(text = x$CONDITION)))
    x[[count_name]] <- sum(match_vec)
    return(x)
  }

  if (x$TYPE %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {
    match_vec       <- with(attrib@data, eval(parse(text = x$CONDITION)))
    x[[count_name]] <- sum(match_vec)
    return(x)
  }

  if (x$TYPE %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {
    match_vec       <- with(attrib@data, eval(parse(text = x$CONDITION)))
    x[[count_name]] <- sum(match_vec)
    return(x)
  }

  return(x)

}

#' @noRd
addSolutionToConstraintData <- function(x, attrib, item_idx, all_values) {

  # attrib must be item_attrib

  solution_name <- "solution"

  if (x$TYPE %in% c("NUMBER", "COUNT")) {

    if (toupper(x$CONDITION) %in% c("", " ", "PER TEST", "TEST")) {

      if (x$WHAT == "ITEM") {
        x[[solution_name]] <- length(item_idx)
      }
      if (x$WHAT == "STIMULUS") {
        x[[solution_name]] <- length(unique(attrib@data$STID[item_idx]))
      }

      if (all_values) {
        return(x[[solution_name]])
      }
      return(x)

    }

    if (toupper(x$CONDITION) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {

      tmp     <- attrib@data[item_idx, ]
      i_per_s <- aggregate(tmp$INDEX, by = list(tmp$STID), function(x) length(x))$x

      if (all_values) {
        return(i_per_s)
      }

      x$mean  <- mean(i_per_s)
      x$sd    <- sd(i_per_s)
      x$min   <- min(i_per_s)
      x$max   <- max(i_per_s)
      return(x)

    }

    if (x$CONDITION %in% names(attrib@data)) {

      if (all_values) {
        return(NA)
      }
      return(x)

    }

    if (TRUE) {

      match_vec          <- with(attrib@data, eval(parse(text = x$CONDITION)))
      x[[solution_name]] <- sum(item_idx %in% which(match_vec))

      if (all_values) {
        return(x[[solution_name]])
      }
      return(x)

    }

  }

  if (x$TYPE == "SUM") {

    if (x$CONDITION %in% names(attrib@data)) {

      values <- with(attrib@data, eval(parse(text = x$CONDITION)))
      x[[solution_name]] <- sum(values[item_idx])

      if (all_values) {
        return(x[[solution_name]])
      }
      return(x)

    }

    if (
      grepl(",", x$CONDITION) |
      grepl("\\[", x$CONDITION)) {

      if (grepl(",", x$CONDITION)) {
        selector_pos <- regexpr(",", x$CONDITION)
        variable     <- substr(x$CONDITION, 1, selector_pos - 1)
        condition    <- substr(x$CONDITION, selector_pos + 1, nchar(x$CONDITION))
      }
      if (grepl("\\[", x$CONDITION)) {
        selector_pos <- regexpr("\\[", x$CONDITION)
        variable     <- substr(x$CONDITION, 1, selector_pos - 1)
        condition    <- substr(x$CONDITION, selector_pos + 1, nchar(x$CONDITION) - 1)
      }

      flag <- with(attrib@data, eval(parse(text = condition)))
      idx  <- which(flag)
      tmp  <- attrib@data[[variable]]
      tmp[!flag] <- 0

      x[[solution_name]] <- sum(tmp[item_idx])

      if (all_values) {
        return(x[[solution_name]])
      }
      return(x)

    }

  }

  if (all_values) {
    return(NA)
  }
  return(x)

}

#' Retrieve constraints-related attributes from solution
#'
#' \code{\link{getSolutionAttributes}} is a helper function to retrieve constraints-related attributes from a solution.
#'
#' @param constraints a \code{\linkS4class{constraints}} object.
#' @param item_idx item indices from a solution.
#' @param all_values if \code{TRUE}, return all values as-is without taking the mean when there are multiple values. If \code{FALSE}, return the mean when there are multiple values.
#' This has an effect when there is a constraint on items per stimulus, where there are multiple values of number of items per stimulus.
#' In this case, if \code{TRUE}, the number of items for every stimuli are returned as-is. If \code{FALSE}, the average number of items across stimuli is returned.
#' (default = \code{FALSE})
#'
#' @return
#' \itemize{
#'   \item{If \code{all_values == FALSE}, \code{\link{getSolutionAttributes}} returns a \code{\link{data.frame}} containing constraints data and their associated attributes.}
#'   \item{If \code{all_values == TRUE}, \code{\link{getSolutionAttributes}} returns a \code{\link{list}} containing attributes associated to each constraint.}
#' }
#'
#' @examples
#' item_idx <-
#'   c( 29,  33,  26,  36,  34,
#'     295, 289, 296, 291, 126,
#'     133, 124, 134, 129,  38,
#'      47,  39,  41,  46,  45,
#'     167, 166, 170, 168, 113,
#'     116, 119, 117, 118, 114)
#'
#' getSolutionAttributes(constraints_reading, item_idx, FALSE)
#' getSolutionAttributes(constraints_reading, item_idx, TRUE)
#'
#' @export
getSolutionAttributes <- function(constraints, item_idx, all_values = FALSE) {

  nc  <- length(constraints@list_constraints)

  if (!all_values) {

    o <- constraints@constraints
    o$solution <- NA
    o$mean     <- NA
    o$sd       <- NA
    o$min      <- NA
    o$max      <- NA

    for (i in 1:nc) {
      o[i, ] <-
        addSolutionToConstraintData(
          o[i, ],
          constraints@item_attrib,
          item_idx,
          FALSE
        )
    }

    return(o)

  }

  if (all_values) {

    o <- list()

    for (i in 1:nc) {
      o[[i]] <- addSolutionToConstraintData(
        constraints@constraints[i, ],
        constraints@item_attrib,
        item_idx,
        TRUE
      )
    }

    return(o)

  }

}
