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
#' @importFrom utils capture.output read.csv setTxtProgressBar txtProgressBar write.table packageVersion packageDescription menu combn
#' @importFrom graphics abline lines axis grid layout legend mtext par plot.new points rect text strheight box
#' @useDynLib TestDesign
NULL

#' @noRd
.onAttach <- function(libname, pkgname) {

  skip_solver_test <- FALSE
  has_pkgload <- length(find.package("pkgload", quiet = TRUE)) > 0
  if (has_pkgload) {
    if (pkgload::is_dev_package("TestDesign")) {
      # skip if is being loaded by roxygen2
      # roxygen2 conflicts with gurobi in when running roxygen2::roxygenize
      skip_solver_test <- TRUE
    }
  }

  if (!skip_solver_test) {
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
  }

  s      <- "TestDesign"
  v      <- packageVersion(s)
  status <- ">"
  msg    <- sprintf("%s %-10s %s", status, s, sprintf("%-7s", v))
  packageStartupMessage(cyan(msg))

}

setClassUnion("dataframe_or_null"   , c("data.frame"  , "NULL"))
setClassUnion("character_or_null"   , c("character"   , "NULL"))
setClassUnion("numeric_or_null"     , c("numeric"     , "NULL"))
setClassUnion("matrix_or_null"      , c("matrix"      , "NULL"))
setClassUnion("list_or_null"        , c("list"        , "NULL"))
setClassUnion("recordedplot_or_null", c("recordedplot", "NULL"))
setClassUnion("matrix_or_numeric"   , c("matrix", "numeric"))
setClassUnion("matrix_or_numeric_or_null", c("matrix", "numeric", "NULL"))
