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

#' @import Matrix
#' @import lpSolve
#' @import Rcpp methods
#' @import foreach
#' @import crayon
#' @importFrom Rdpack reprompt
#' @importFrom methods new show validObject
#' @importFrom logitnorm logit rlogitnorm
#' @importFrom grDevices col2rgb dev.control dev.new dev.off pdf recordPlot
#' @importFrom stats runif dnorm rlnorm rnorm sd
#' @importFrom utils capture.output read.csv setTxtProgressBar txtProgressBar write.table packageVersion
#' @importFrom graphics plot abline lines axis grid layout legend mtext par plot.new points rect text strheight
#' @useDynLib TestDesign
NULL

#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(white(bold("  Solver packages:")))
  packageStartupMessage(" ")

  solver_names <- c("lpSolve", "Rsymphony", "gurobi", "Rglpk")

  for (s in solver_names) {
    x <- find.package(s, quiet = TRUE)
    if (length(x) > 0) {
      status <- green("v")
      v <- packageVersion(s)
    } else {
      status <- red("x")
      v <- ""
    }
    packageStartupMessage(status, " ", s, paste0(rep(" ", 10 - nchar(s)), collapse = ""), white(v))
  }
}


