#' @import Rglpk
#' @import Matrix
#' @import lpSolve
#' @import Rcpp methods
#' @import foreach
#' @import crayon
#' @importFrom Rcpp evalCpp
#' @importFrom Rdpack reprompt
#' @importFrom methods new show validObject
#' @importFrom logitnorm logit rlogitnorm
#' @importFrom grDevices col2rgb dev.control dev.new dev.off pdf recordPlot
#' @importFrom stats runif dnorm rlnorm rnorm sd
#' @importFrom utils capture.output read.csv setTxtProgressBar txtProgressBar write.table packageVersion
#' @importFrom graphics plot abline lines axis grid layout legend mtext par plot.new points rect text strheight
#' @importFrom lattice xyplot
#' @useDynLib oat
NULL

#' @noRd
.onAttach = function(libname, pkgname){

  packageStartupMessage(white(bold("  Solver packages:")))
  packageStartupMessage(" ")

  solvernames = c("lpSolve", "Rsymphony", "gurobi", "Rglpk")

  for (s in solvernames){
    x = find.package(s, quiet = T)
    if (length(x) > 0){
      status = green("v")
      v = packageVersion(s)

    } else {
      status = red("x")
      v = ""
    }

    packageStartupMessage(status, " ", s, paste0(rep(" ", 10 - nchar(s)), collapse = ""), white(v))
  }
}
