# Program: TestDesign
# Original author, repo maintainer, package maintainer: Seung W. Choi
# Joined author, codebase maintainer: Sangdon Lim
# Assumptions: D = 1.0; the minimum score is 0 for all items.

# Naming conventions -----------------------------------------------------------
# - Use one-word UpperCamelCase for major function names (Static(), Shadow(), Split()).
# - Use lowerCamelCase for S3 and S4 function names.
# - Use snake_case for arguments.
# - Use snake_case for class names.
# - Use snake_case for local objects inside functions.
# - Use snake_case for global objects (example datasets).
# - Use snake_case for S3 list slot names and S4 class slot names.

# Documentation conventions: exported functions --------------------------------
# Introducing lines:
# - Introducing lines should be in the below format:
# - \code{\link{functionName}} is a function for *ing ...
# Argument descriptions:
# - Use lower case for the first letter and end with a period.
# - Optional argument descriptions should start with "(optional)"
# - Arguments with default values should be stated as "(default = \code{value})".
# Returned object descriptions:
# - Returned objects should be documented.
# - This should be in the below format:
# - \code{\link{functionName}} returns ...

# Documentation conventions: internal functions --------------------------------
# - Fully document all internal functions.
# - in progress (11 to go)

# Error/validation message conventions -----------------------------------------
# - Use lower case for the first letter and end with a period.
# - Use single quotes for all non-slot references.
# - Retain original capitalization of references (e.g. (o) "'theta'", (x) "'Theta'").
#   - Prioritize this when beginning a sentence with reference.
# - Use appropriate slot operators (@, $) for slot references.
#   - This is meant to help users who may not be familiar with the S4 accessor @.
#   - Always encapsulate with quotes. (e.g. (o) "slot "@theta" is malformed" (x) "slot @theta is malformed")
# - Always disambiguate the type of the referenced object. (e.g. (o) "argument 'x' must be blah.", (x) "'x' must be blah.")
# - Explicitly state what is observed and what is expected.
#   - (e.g. (o) "the value is not positive; it must be positive.")
#   - (e.g. (x) "the value must be positive.")

# Code conventions -------------------------------------------------------------
#
# Promoting debuggable code:
# - Left-to-right assignments/piping of any type are not allowed in codebase: %>%, |>, ->
# - Do not chain multiple functions. It kills debuggability.
# - In short, no tidyverse.
#
# Promoting readable code:
# - Spell out variable names. Abbreviations are not allowed.
# - Do not comment out old code.

#' @import lpSolve
#' @import Rcpp methods
#' @import foreach
#' @import crayon
#' @importFrom methods new show validObject
#' @importFrom logitnorm logit rlogitnorm
#' @importFrom grDevices col2rgb dev.control dev.new dev.off pdf recordPlot dev.cur
#' @importFrom stats rnorm runif dnorm dunif rlnorm sd cor na.omit aggregate var
#' @importFrom utils capture.output read.csv setTxtProgressBar txtProgressBar write.table packageVersion packageDescription menu combn
#' @importFrom graphics abline lines axis grid layout legend mtext par plot.new points rect text strheight box boxplot barplot
#' @useDynLib TestDesign
NULL

#' (Internal) Package startup functions
#'
#' \code{\link{.onAttach}} is an internal function called when the package is first loaded.
#'
#' @param libname,pkgname not used; only defined for compatibility.
#'
#' @returns \code{\link{.onAttach}} does not return anything.
#'
#' @keywords internal
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
    solver_names <- c("lpSolve", "Rsymphony", "highs", "gurobi", "Rglpk")
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
setClassUnion("list_or_matrix_or_numeric", c("list", "matrix", "numeric"))
