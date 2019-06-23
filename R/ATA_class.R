#' config.ATA
#' 
#' @rdname config.ATA

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
           if (toupper(object@itemSelection$method) == "MAXINFO"){
             if (!is.null(object@itemSelection$targetValue)) warning("MaxInfo method was specified: ignoring targetValue")
             target_lengths = unique(c(
               length(object@itemSelection$targetLocation),
               length(object@itemSelection$targetWeight)
             ))
             if (length(target_lengths) != 1){
               stop("itemSelection$targetLocation, itemSelection$targetWeight have different lengths: should have same lengths")
             }
           }
           if (toupper(object@itemSelection$method) != "MAXINFO"){
             target_lengths = unique(c(
               length(object@itemSelection$targetLocation),
               length(object@itemSelection$targetValue),
               length(object@itemSelection$targetWeight)
             ))
             if (length(target_lengths) != 1){
               stop("itemSelection$targetLocation, itemSelection$targetValue, itemSelection$targetWeight have different lengths: should have same lengths")
             }
           }
           if (toupper(object@itemSelection$infoType) != "FISHER") stop("invalid option for itemSelection$infoType: accepts Fisher")
           
           if (!toupper(object@MIP$solver) %in% c("SYMPHONY", "GUROBI", "GLPK", "LPSOLVE")) stop("invalid option for MIP$solver: accepts Symphony, Gurobi, GLPK, LpSolve")
           return(TRUE)
         }
)

#' config.ATA
#' 
#' Create an \code{\linkS4class{ATA.config}} object for Automated Test Assembly (ATA).
#' 
#' @param itemSelection A list containing item selection criteria. This should have the following entries:
#' \itemize{
#'   \item{\code{method}} The type of criteria. Accepts \code{MAXINFO, TIF, TCC}.
#'   \item{\code{infoType}} The type of information. Accepts \code{FISHER}.
#'   \item{\code{targetLocation}} A numeric vector containing the locations of target theta points. (e.g. \code{c(-1, 0, 1)})
#'   \item{\code{targetValue}} A numeric vector containing the target values at each theta location. This should have the same length with \code{targetLocation}. Ignored if method is \code{MAXINFO}.
#'   \item{\code{targetWeight}} A numeric vector containing the weights for each theta location. This should have the same length with \code{targetlocation}. Defaults to a vector of 1s.
#' }
#' @param MIP A list containing solver options. This should have the following entries:
#' \itemize{
#'   \item{\code{solver}} The type of solver. Accepts \code{SYMPHONY, GUROBI, GLPK, LPSOLVE}.
#'   \item{\code{verbosity}} Verbosity level of the solver. Defaults to -2.
#'   \item{\code{timeLimit}} Time limit in seconds passed onto the solver. Defaults to 60. Used in solvers \code{SYMPHONY, GUROBI, GLPK}.
#'   \item{\code{gapLimit}} Termination criteria in relative scale passed onto the solver. Defaults to .05. Used in solvers  \code{SYMPHONY, GUROBI}.
#'   \item{\code{gapLimitAbs}} Termination criteria in absolute scale passed onto the solver. Defaults to 1. Used in solver \code{GUROBI}.
#' }
#' 
#' @examples
#' conf.1 = config.ATA(list(method = "MAXINFO",
#'                          infoType = "FISHER",
#'                          targetLocation = c(-1, 0, 1),
#'                          targetWeight   = c( 1, 1, 1)))
#'                          
#' conf.2 = config.ATA(list(method = "TIF",
#'                          infoType = "FISHER",
#'                          targetLocation = c(-1, 0, 1),
#'                          targetWeight   = c( 1, 1, 1),
#'                          targetValue    = c( 8,10,12)))
#'                          
#' conf.3 = config.ATA(list(method = "TCC",
#'                          infoType = "FISHER",
#'                          targetLocation = c(-1, 0, 1),
#'                          targetWeight   = c( 1, 1, 1),
#'                          targetValue    = c(10,15,20)))
#' @rdname config.ATA
#' 
#' @export
config.ATA = function(itemSelection = NULL, MIP = NULL){
  conf = new("ATA.config")
  arg.names = c("itemSelection", "MIP")
  obj.names = c()
  for (arg in arg.names){
    if (!is.null(eval(parse(text = arg)))){
      eval(parse(text = paste0("obj.names = names(conf@", arg, ")")))
      for (entry in obj.names){
        entry.l = paste0("conf@", arg, "$", entry)
        entry.r = paste0(arg, "$", entry)
        tmp = eval(parse(text = entry.r))
        if (!is.null(tmp)){
          eval(parse(text = paste0(entry.l, " = ", entry.r)))
        }
      }
    }
  }
  v = validObject(conf)
  if (v) return(conf)
}

#' @name show-method
#' @aliases show,ATA.config-method
#' @docType methods
#' @noRd
setMethod("show", "ATA.config", function(object) {
  cat("ATA Configuration Settings \n\n")
  cat("  Item selection criterion \n")
  cat("    Method         :", object@itemSelection$method, "\n") #c("MAXINFO", "TIF", "TCC")
  cat("    Info type      :", object@itemSelection$infoType, "\n")
  cat("    Theta Location :", object@itemSelection$targetLocation, "\n")
  cat("    Target Value  :", object@itemSelection$targetValue, "\n")
  cat("    Target Weight   :", object@itemSelection$targetWeight, "\n\n")
  cat("  MIP \n")
  cat("    Solver         :", object@MIP$solver, "\n")
  cat("    Verbosity      :", object@MIP$verbosity, "\n")
  cat("    Time limit     :", object@MIP$timeLimit, "\n")
  cat("    Gap limit      :", object@MIP$gapLimit, "\n\n")
})
