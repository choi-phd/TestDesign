# Program: Shadow
# Author: Seung Choi
# Assumptions: D = 1.0; the minimum score is 0 for all items

# Naming conventions
# Use lowerCamelCase for S3 and S4 function names.
# Use snake_case for arguments.
# Use snake_case for class names.
# Use snake_case for local objects inside functions.
# Use snake_case for global objects (example datasets).
# Use snake_case for S3 list slot names and S4 class slot names.

# Error message conventions
# Use sentence case. (capitalize the first letter and end with a period.)
# Use single quotes for all non-slot references.
# Retain original capitalizations of references (e.g. (x) "'Theta'", (o) "'theta'").
# # Prioritize this when beginning a sentence with reference.
# Use appropriate slot operators (@, $) for slot references. Do not encapsulate with quotes.
# Do not disambiguate references. (e.g. (x) "Argument 'x' must be blah.", (o) "'x' must be blah.")
# Always give full names for slot references. (e.g. "@foo$bar")

#' @import Matrix
#' @import lpsymphony
#' @import Rcpp methods
#' @import foreach
#' @import crayon
#' @importFrom Rdpack reprompt
#' @importFrom methods new show validObject
#' @importFrom logitnorm logit rlogitnorm
#' @importFrom grDevices col2rgb dev.control dev.new dev.off pdf recordPlot
#' @importFrom stats runif dnorm rlnorm rnorm sd na.omit
#' @importFrom utils capture.output read.csv setTxtProgressBar txtProgressBar write.table packageVersion packageDescription
#' @importFrom graphics plot abline lines axis grid layout legend mtext par plot.new points rect text strheight box
#' @useDynLib TestDesign
NULL

#' @noRd
.onAttach <- function(libname, pkgname) {
  solver_names <- c("lpsymphony", "Rsymphony", "gurobi", "lpSolve", "Rglpk")

  for (s in solver_names) {
    x <- find.package(s, quiet = TRUE)
    if (length(x) > 0) {
      status <- green("v")
      v <- packageVersion(s)
    } else {
      status <- red("x")
      v <- ""
    }
    packageStartupMessage(status, " ", s, paste0(rep(" ", 11 - nchar(s)), collapse = ""), white(v))
  }

  s <- "TestDesign"
  v <- packageVersion(s)

  packageStartupMessage(cyan(">"), " ", cyan(s), paste0(rep(" ", 11 - nchar(s)), collapse = ""), cyan(packageVersion('TestDesign')))

  packageStartupMessage(cyan("  Please report any issues to:"))
  packageStartupMessage(cyan(paste0("  ", packageDescription('TestDesign')$BugReports)))
}

setClassUnion("dataframe_or_null", c("data.frame", "NULL"))
setClassUnion("character_or_null", c("character",  "NULL"))
setClassUnion("numeric_or_null",   c("numeric",    "NULL"))
setClassUnion("matrix_or_null",    c("matrix",     "NULL"))
setClassUnion("list_or_null",      c("list",       "NULL"))

`%not in%` <- Negate(`%in%`)
