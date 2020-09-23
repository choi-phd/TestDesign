# Program: TestDesign
# Original Author: Seung Choi
# Assumptions: D = 1.0; the minimum score is 0 for all items

# Naming conventions
# Use lowerCamelCase for S3 and S4 function names.
# Use snake_case for arguments.
# Use snake_case for class names.
# Use snake_case for local objects inside functions.
# Use snake_case for global objects (example datasets).
# Use snake_case for S3 list slot names and S4 class slot names.

# Function documentation conventions
# Use lower case for the first letter and end with a period.

# Error message conventions
# Use lower case for the first letter and end with a period.
# Use single quotes for all non-slot references.
# Retain original capitalization of references (e.g. (x) "'Theta'", (o) "'theta'").
# # Prioritize this when beginning a sentence with reference.
# Use appropriate slot operators (@, $) for slot references. Do not encapsulate with quotes.
# Do not disambiguate references. (e.g. (x) "Argument 'x' must be blah.", (o) "'x' must be blah.")
# Always give full names for slot references. (e.g. "@foo$bar")

#' @import lpSolve
#' @import Rcpp methods
#' @import foreach
#' @import crayon
#' @importFrom methods new show validObject
#' @importFrom logitnorm logit rlogitnorm
#' @importFrom grDevices col2rgb dev.control dev.new dev.off pdf recordPlot dev.cur
#' @importFrom stats runif dnorm rlnorm rnorm sd cor na.omit aggregate
#' @importFrom utils capture.output read.csv setTxtProgressBar txtProgressBar write.table packageVersion packageDescription menu
#' @importFrom graphics abline lines axis grid layout legend mtext par plot.new points rect text strheight box
#' @useDynLib TestDesign
NULL

#' @noRd
.onAttach <- function(libname, pkgname) {
  solver_names <- c("lpSolve", "Rsymphony", "lpsymphony", "gurobi", "Rglpk")

  for (s in solver_names) {
    x <- find.package(s, quiet = TRUE)
    if (length(x) > 0) {
      v <- packageVersion(s)
      e <- testSolver(s)
      if (e == "") {
        status <- green("v")
      } else {
        status <- yellow("?")
      }
    } else {
      v <- ""
      status <- red("x")
    }
    msg <- sprintf("%s %-10s %s %s", status, s, white(sprintf("%-7s", v)), white(e))
    packageStartupMessage(msg)
  }

  s      <- "TestDesign"
  v      <- packageVersion(s)
  status <- ">"
  msg    <- sprintf("%s %-10s %s", status, s, sprintf("%-7s", v))
  packageStartupMessage(cyan(msg))

}

#' @noRd
testSolver <- function(solver) {

  obj   <- seq(.1, .5, .1)
  mat   <- matrix(
    c(1, 1, 1, 1, 1,
      0, 0, 0, 1, 0),
    2, 5,
    byrow = TRUE)
  dir   <- rep("==", 2)
  rhs   <- c(2, 0)
  types <- "B"

  solver <- toupper(solver)
  o <- try(
    runMIP(
      solver,
      obj, mat, dir, rhs,
      TRUE, types,
      verbosity = -2,
      time_limit = 5,
      gap_limit_abs = 0.05,
      gap_limit = 0.05
    ),
    silent = TRUE
  )

  if (inherits(o, "try-error")) {
    return(trimws(as.character(o)))
  }

  return("")

}

setClassUnion("dataframe_or_null"   , c("data.frame"  , "NULL"))
setClassUnion("character_or_null"   , c("character"   , "NULL"))
setClassUnion("numeric_or_null"     , c("numeric"     , "NULL"))
setClassUnion("matrix_or_null"      , c("matrix"      , "NULL"))
setClassUnion("list_or_null"        , c("list"        , "NULL"))
setClassUnion("recordedplot_or_null", c("recordedplot", "NULL"))
setClassUnion("matrix_or_numeric_or_null", c("matrix", "numeric", "NULL"))
