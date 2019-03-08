# Documentation progress
# Phase 2. Add simple descriptions: COMPLETE

#' ATA.config
#'
#' An S4 object to store configurations for Automated Test Assembly (ATA).
#'
#' @slot itemSelection A list containing item selection criteria. This should have the following entries:
#' \itemize{
#'   \item{\code{method}} The type of criteria. Accepts \code{MAXINFO}, \code{TIF}, or \code{TCC}.
#'   \item{\code{infoType}} The type of information. Accepts \code{FISHER}.
#'   \item{\code{targetLocation}} A numeric vector containing the locations of target points. (e.g. \code{c(-1, 0, 1)})
#'   \item{\code{targetValue}} A numeric vector containing the target values at each location. This should have the same length with \code{targetLocation}.
#'   \item{\code{targetWeight}} A numeric vector containing the weights for each location.
#' }
#' @slot MIP A list containing solver options. This should have the following entries:
#' \itemize{
#'   \item{\code{solver}} The type of solver. Accepts \code{SYMPHONY}, \code{GUROBI}, \code{GLPK}, or \code{LPSOLVE}.
#'   \item{\code{verbosity}} Verbosity level.
#'   \item{\code{timeLimit}} Time limit to be passed onto solver. Used in solvers \code{SYMPHONY}, \code{GUROBI}, and \code{GLPK}.
#'   \item{\code{gapLimit}} Gap limit to be passed onto solver. Used in solvers  \code{SYMPHONY} and \code{GUROBI}.
#'   \item{\code{gapLimitAbs}} Absolute gap limit to be passed onto optimizers \code{GUROBI}.
#' }
#'
#' @export

setClass("ATA.config",
         slots = c(itemSelection = "list",
                   MIP = "list"),
         prototype = list(itemSelection = list(method = "MaxInfo",
                                               infoType = "FISHER",
                                               targetLocation = c(-1.2, 0, 1.2),
                                               targetValue = NULL,
                                               targetWeight = c(1, 1, 1)),
                          MIP = list(solver = "SYMPHONY",
                                     verbosity = -2,
                                     timeLimit = 60,
                                     gapLimit = 0.05,
                                     gapLimitAbs = 1)),
         validity = function(object) {
           if (!toupper(object@itemSelection$method) %in% c("MAXINFO", "TIF", "TCC")) stop("invalid option for itemSelection$method: accepts MaxInfo, TIF, TCC")
           if (toupper(object@itemSelection$method) == "MAXINFO" &
               !is.null(object@itemSelection$targetValue)) warning("MaxInfo method was specified: ignoring targetValue")
           if (toupper(object@itemSelection$infoType) != "FISHER") stop("invalid option for itemSelection$infoType: accepts Fisher")
           if (!all(length(object@itemSelection$targetLocation), length(object@itemSelection$targetLocation), length(object@itemSelection$targetWeight))) stop("itemSelection$targetLocation, itemSelection$targetValue, itemSelection$targetWeight are of different lengths")
           if (!toupper(object@MIP$solver) %in% c("SYMPHONY", "GUROBI", "GLPK", "LPSOLVE")) stop("invalid option for MIP$solver: accepts Symphony, Gurobi, GLPK, LpSolve")
           return(TRUE)
         }
)

#' @inherit methods::show
#'
#' @name show
#'
#' @aliases show,ATA.config-method
#'
#' @docType methods
#' @rdname show-methods
#' @export
setMethod("show", "ATA.config", function(object) {
  cat("ATA Configuration Settings \n\n")
  cat("  Item selection criterion \n")
  cat("    Method         :", object@itemSelection$method, "\n") #c("MAXINFO", "TIF", "TCC")
  cat("    Info type      :", object@itemSelection$infoType, "\n")
  cat("    Theta Location :", object@itemSelection$targetLocation, "\n")
  cat("    Target Value   :", object@itemSelection$targetValue, "\n\n")
  cat("  MIP \n")
  cat("    Solver         :", object@MIP$solver, "\n")
  cat("    Verbosity      :", object@MIP$verbosity, "\n")
  cat("    Time limit     :", object@MIP$timeLimit, "\n")
  cat("    Gap limit      :", object@MIP$gapLimit, "\n\n")
})
