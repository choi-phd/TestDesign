# Documentation progress
# Phase 1: Create a skeleton structure    -- complete

#' constraint
#'
#' @slot CONSTRAINT Character.
#' @slot mat A matrix.
#' @slot dir Character.
#' @slot rhs Numeric.
#' @slot nc Numeric.
#'
#' @export

setClass("constraint",
         slots = c(CONSTRAINT = "character",
                   mat = "matrix",
                   dir = "character",
                   rhs = "numeric",
                   nc = "numeric"),
         prototype = list(CONSTRAINT = character(0),
                          mat = matrix(NA, 0, 0),
                          dir = character(0),
                          rhs = numeric(0),
                          nc = 0),
         validity = function(object) {
           # add validity checks
           return(TRUE)
         }
)

setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))

#' test
#'
#' @slot pool An item.pool object.
#' @slot theta Numeric.
#' @slot Prob A list.
#' @slot Info A matrix.
#' @slot trueTheta Numeric or Null.
#' @slot Data A matrix or Null.
#'
#' @export

#define class for test - should I call this something else?
setClass("test",
         slots = c(pool = "item.pool",
                   theta = "numeric",
                   Prob = "list",
                   Info = "matrix",
                   trueTheta = "numericOrNULL",
                   Data = "matrixOrNULL"),
         prototype = list(pool = new("item.pool"),
                          theta = numeric(0),
                          Prob = list(0),
                          Info = matrix(0),
                          trueTheta = numeric(0),
                          Data = matrix(NA, 0, 0)),
         validity = function(object) {
           if (length(object@Prob) != object@pool@ni) stop("length(Prob) is not equal to pool@ni")
           if (ncol(object@Info) != object@pool@ni) stop("ncol(Info) is not equal to pool@ni")
           if (nrow(object@Info) != length(object@theta)) stop("nrow(Info) is not equal to length(theta)")
           # if (!is.null(trueTheta) && !is.null(Data)) {
           #   if (length(trueTheta) != nrow(Data)) {
           #     stop("length(trueTheta) and nrow(Data) do not match")
           #   }
           # }
           return(TRUE)
         }
)

#' test.cluster
#'
#' @slot nt Numeric.
#' @slot tests A list.
#' @slot names Character.
#'
#' @export

setClass("test.cluster",
         slots = c(nt = "numeric",
                   tests = "list",
                   names = "character"),
         prototype = list(nt = numeric(0),
                          tests = list(0),
                          names = character(0)),
         validity = function(object) {
           if (length(object@tests) != object@nt) stop("length(tests) is not equal to nt")
           if (length(object@names) != object@nt) stop("length(names) is not equal to nt")
           return(TRUE)
         }
)

#' Shadow.config
#'
#' An S4 object to store configurations for Shadow Test Assembly (STA).
#'
#' @slot itemSelection A list containing item selection criteria.
#' \itemize{
#'   \item{\code{method}} The type of criteria. Accepts one of \code{MAXINFO, TIF, TCC}
#'   \item{\code{infoType}} The type of information. Accepts \code{FISHER}.
#'   \item{\code{initialTheta}}
#'   \item{\code{fixedTheta}}
#' }
#' @slot contentBalancing A list containing content balancing options.
#' \itemize{
#'   \item{\code{method}} The type of balancing method. Accepts \code{NONE}, or \code{STA}.
#' }
#' @slot MIP A list containing solver options.
#' \itemize{
#'   \item{\code{solver}} The type of solver. Accepts one of \code{SYMPHONY, GUROBI, GLPK, LPSOLVE}.
#'   \item{\code{verbosity}} Verbosity level.
#'   \item{\code{timeLimit}} Time limit to be passed onto solver. Used in solvers \code{SYMPHONY, GUROBI, GLPK}.
#'   \item{\code{gapLimit}} Gap limit to be passed onto solver. Used in solvers  \code{SYMPHONY, GUROBI}.
#' }
#' @slot MCMC A list containing Markov-chain Monte Carlo configurations.
#' \itemize{
#'   \item{\code{burnIn}} Numeric. The number of chains from the start to discard.
#'   \item{\code{postBurnIn}}  Numeric. The number of chains to use after discarding the first \code{burnIn} chains.
#'   \item{\code{thin}} Thin.
#'   \item{\code{jumpfactor}} JumpFactor.
#' }
#' @slot refreshPolicy A list containing refresh policy for obtaining a new shadow test.
#' \itemize{
#'   \item{\code{method}} The type of policy. Accepts one of \code{ALWAYS, POSITION, INTERVAL, THRESHOLD, INTERVAL-THRESHOLD, STIMULUS, SET, PASSAGE}.
#'   \item{\code{interval}}
#'   \item{\code{threshold}}
#'   \item{\code{position}}
#' }
#' @slot exposureControl A list.
#' @slot stoppingCriterion A list.
#' @slot interimTheta A list containing interim theta estimation options.
#' \itemize{
#'   \item{\code{method}} The type of estimation. Accepts one of \code{EAP, EB, FB}.
#'   \item{\code{priorDist}} The type of prior distribution. Accepts one of \code{NORMAL, UNIF}.
#'   \item{\code{priorPar}} Distributional parameters for the prior.
#'   \item{\code{boundML}}
#'   \item{\code{maxIter}}
#'   \item{\code{crit}}
#' }
#' @slot finalTheta A list containing final theta estimation options.
#' \itemize{
#'   \item{\code{method}} The type of estimation. Accepts one of \code{EAP, EB, FB}.
#'   \item{\code{priorDist}} The type of prior distribution. Accepts one of \code{NORMAL, UNIF}.
#'   \item{\code{priorPar}} Distributional parameters for the prior.
#'   \item{\code{boundML}}
#'   \item{\code{maxIter}}
#'   \item{\code{crit}}
#' }
#' @slot thetaGrid Numeric. A vector of theta values to represent the continuum.
#' @slot auditTrail Logical.
#'
#' @export

setClass("Shadow.config",
         slots = c(itemSelection = "list",
                   contentBalancing = "list",
                   MIP = "list",
                   MCMC = "list",
                   refreshPolicy = "list",
                   exposureControl = "list",
                   stoppingCriterion = "list",
                   interimTheta = "list",
                   finalTheta = "list",
                   thetaGrid = "numeric",
                   auditTrail = "logical"),
         prototype = list(itemSelection = list(method = "MFI",
                                               infoType = "FISHER",
                                               initialTheta = NULL,
                                               fixedTheta = NULL),
                          contentBalancing = list(method = "STA"),
                          MIP = list(solver = "SYMPHONY",
                                     verbosity = -2,
                                     timeLimit = -1,
                                     gapLimit = -1),
                          MCMC = list(burnIn = 100,
                                      postBurnIn = 500,
                                      thin = 1,
                                      jumpFactor = 1),
                          refreshPolicy = list(method = "ALWAYS",
                                               interval = 1,
                                               threshold = 0.1,
                                               position = 1),
                          exposureControl = list(method = "ELIGIBILITY",
                                                 M = NULL,
                                                 maxExposureRate = 0.25,
                                                 accelerationFactor = 1.0,
                                                 nSegment = 7,
                                                 firstSegment = NULL,
                                                 segmentCut = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf),
                                                 initialEligibilityStats = NULL,
                                                 fadingFactor = 0.999,
                                                 diagnosticStats = FALSE),
                          stoppingCriterion = list(method = "FIXED",
                                                   testLength = NULL,
                                                   minNI = NULL,
                                                   maxNI = NULL,
                                                   SeThreshold = NULL),
                          interimTheta = list(method = "EAP",
                                              shrinkageCorrection = FALSE,
                                              priorDist = "NORMAL",
                                              priorPar = c(0, 1),
                                              boundML = c(-4, 4),
                                              maxIter = 100,
                                              crit = 0.001),
                          finalTheta = list(method = "EAP",
                                            shrinkageCorrection = FALSE,
                                            priorDist = "NORMAL",
                                            priorPar = c(0, 1),
                                            boundML = c(-4, 4),
                                            maxIter = 100,
                                            crit = 0.001),
                          thetaGrid = seq(-4, 4, .1),
                          auditTrail = FALSE),
         validity = function(object) {
           if (!toupper(object@MIP$solver) %in% c("SYMPHONY", "GUROBI", "GLPK", "LPSOLVE")) stop("invalid option for MIP$solver")
           if (toupper(object@MIP$solver) == "GUROBI" &
               !requireNamespace("gurobi", quietly = TRUE)) stop("GUROBI was specified but is not installed")
           if (!object@itemSelection$method %in% c("MFI", "MPWI", "EB", "FB")) stop("invalid option for selectionCriterion")
           if (!object@contentBalancing$method %in% c("NONE", "STA")) stop("invalid option for contentBalancing")
           if (!object@refreshPolicy$method %in% c("ALWAYS", "POSITION", "INTERVAL", "THRESHOLD", "INTERVAL-THRESHOLD", "STIMULUS", "SET", "PASSAGE")) stop("invalid option for refreshPolicy")
           if (!object@exposureControl$method %in% c("NONE", "ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) stop("invalid option for exposureControl")
           if (!object@stoppingCriterion$method %in% c("FIXED")) stop("invalid option for stoppingCriterion")
           if (!object@interimTheta$method %in% c("EAP", "EB", "FB")) stop("invalid option for interimTheta")
           if (!object@finalTheta$method %in% c("EAP", "EB", "FB")) stop("invalid option for finalTheta")
           if (object@exposureControl$method %in% c("BIGM-BAYESIAN") && !object@interimTheta$method %in% c("EB", "FB")) stop("for exposureControl BIGM-BAYESIAN you must use interimTheta of EB of FB")
           return(TRUE)
         }
)

#' @docType methods
#' @rdname show-methods
#' @export
setMethod("show", "Shadow.config", function(object) {
  cat("Shadow Configuration Settings \n\n")
  cat("  Item selection criterion \n")
  cat("    Method         :", object@itemSelection$method, "\n")
  cat("    Info type      :", object@itemSelection$infoType, "\n")
  cat("    Initial theta  :", object@itemSelection$initialTheta, "\n\n")
  cat("  Content balancing \n")
  cat("    Method         :", object@contentBalancing$method, "\n\n")
  cat("  MIP \n")
  cat("    Solver         :", object@MIP$solver, "\n")
  cat("    Verbosity      :", object@MIP$verbosity, "\n")
  cat("    Time limit     :", object@MIP$timeLimit, "\n")
  cat("    Gap limit      :", object@MIP$gapLimit, "\n\n")
  cat("  MCMC \n")
  cat("    Burn in        :", object@MCMC$burnIn, "\n")
  cat("    Post burn in   :", object@MCMC$postBurnIn, "\n")
  cat("    Thin           :", object@MCMC$thin, "\n")
  cat("    Jump factor    :", object@MCMC$jumpFactor, "\n\n")
  cat("  Refresh policy \n")
  cat("    Method         :", object@refreshPolicy$method, "\n")
  cat("    Interval       :", object@refreshPolicy$interval, "\n")
  cat("    Threshold      :", object@refreshPolicy$threshold, "\n")
  cat("    Position       :", object@refreshPolicy$position, "\n\n")
  cat("  Exposure control \n")
  cat("    Method         :", object@exposureControl$method, "\n")
  cat("    Big M          :", object@exposureControl$M, "\n")
  cat("    Max Exposure   :", object@exposureControl$maxExposureRate, "\n")
  cat("    N Segment      :", object@exposureControl$nSegment, "\n")
  cat("    Segment cut    :", object@exposureControl$segmentCut, "\n")
  cat("    Fading factor  :", object@exposureControl$fadingFactor, "\n")
  cat("    Acceleration   :", object@exposureControl$accelerationFactor, "\n")
  cat("    Diagnostics    :", object@exposureControl$diagnosticStats, "\n\n")
  cat("  Stopping criterion \n")
  cat("    Method         :", object@stoppingCriterion$method, "\n")
  if (toupper(object@stoppingCriterion$method) == "FIXED") {
    cat("    Test Length    :", object@stoppingCriterion$testLength, "\n")
  } else {
    cat("    Min # of items :", object@stoppingCriterion$minNI, "\n")
    cat("    Max # of items :", object@stoppingCriterion$maxNI, "\n")
    cat("    SE threshold   :", object@stoppingCriterion$SeThreshold, "\n")
  }
  cat("    Min # of items :", object@stoppingCriterion$minNI, "\n")
  cat("    Max # of items :", object@stoppingCriterion$maxNI, "\n")
  cat("    SE threshold   :", ifelse(toupper(object@stoppingCriterion$method) == "VARIABLE", object@stoppingCriterion$SeThreshold, NA), "\n\n")
  cat("  Interim theta estimator \n")
  cat("    Method         :", object@interimTheta$method, "\n")
  cat("    Prior dist     :", ifelse(toupper(object@interimTheta$method == "EAP"), object@interimTheta$priorDist, NA), "\n")
  cat("    Prior parm     :", ifelse(toupper(object@interimTheta$method == "EAP"), sprintf(ifelse(toupper(object@interimTheta$priorDist) == "NORMAL", "Mean = %5.3f, SD = %5.3f", "Min = %5.3f, Max = %5.3f"), object@interimTheta$priorPar[1], object@interimTheta$priorPar[2]), NA), "\n\n")
  cat("  Final theta estimator \n")
  cat("    Method         :", object@finalTheta$method, "\n")
  cat("    Prior dist     :", ifelse(toupper(object@finalTheta$method == "EAP"), object@finalTheta$priorDist, NA), "\n")
  cat("    Prior parm     :", ifelse(toupper(object@finalTheta$method == "EAP"), sprintf(ifelse(toupper(object@finalTheta$priorDist) == "NORMAL", "Mean = %5.3f, SD = %5.3f", "Min = %5.3f, Max = %5.3f"), object@finalTheta$priorPar[1], object@finalTheta$priorPar[2]), NA), "\n\n")
  cat("  Theta Grid \n")
  print(object@thetaGrid)
  cat("\n  Plot Audit Trail: ", object@auditTrail, "\n")
})

#' Shadow.output
#'
#' @slot simuleeIndex Numeric.
#' @slot trueTheta Numeric or NULL.
#' @slot trueThetaSegment Numeric or NULL.
#' @slot finalThetaEst Numeric.
#' @slot finalSeEst Numeric.
#' @slot administeredItemIndex Numeric.
#' @slot administeredItemResp Numeric.
#' @slot administeredStimulusIndex Numeric.
#' @slot shadowTestRefreshed Logical.
#' @slot shadowTestFeasible Logical.
#' @slot solveTime Numeric.
#' @slot interimThetaEst Numeric.
#' @slot interimSeEst Numeric.
#' @slot thetaSegmentIndex Numeric.
#' @slot prior Numeric.
#' @slot priorPar Numeric.
#' @slot posterior Numeric.
#' @slot posteriorSample Numeric.
#' @slot likelihood Numeric.
#' @slot shadowTest A list.
#'
#' @export

#define class for Shadow.output per examinee
setClass("Shadow.output",
         slots = c(simuleeIndex = "numeric",
                   trueTheta = "numericOrNULL",
                   trueThetaSegment = "numericOrNULL",
                   finalThetaEst = "numeric",
                   finalSeEst = "numeric",
                   administeredItemIndex = "numeric",
                   administeredItemResp = "numeric",
                   administeredStimulusIndex = "numeric",
                   shadowTestRefreshed = "logical",
                   shadowTestFeasible = "logical",
                   solveTime = "numeric",
                   interimThetaEst = "numeric",
                   interimSeEst = "numeric",
                   thetaSegmentIndex = "numeric",
                   #thetaGrid = "numeric",
                   prior = "numeric",
                   priorPar = "numeric",
                   posterior = "numeric",
                   posteriorSample = "numeric",
                   likelihood = "numeric",
                   shadowTest = "list"),
         prototype = list(simuleeIndex = numeric(0),
                          trueTheta = numeric(0),
                          trueThetaSegment = numeric(0),
                          finalThetaEst = numeric(0),
                          finalSeEst = numeric(0),
                          administeredItemIndex = numeric(0),
                          administeredItemResp = numeric(0),
                          administeredStimulusIndex = numeric(0),
                          shadowTestRefreshed = logical(0),
                          shadowTestFeasible = logical(0),
                          solveTime = numeric(0),
                          interimThetaEst = numeric(0),
                          interimSeEst = numeric(0),
                          thetaSegmentIndex = numeric(0),
                          #thetaGrid = numeric(0),
                          prior = numeric(0),
                          priorPar = numeric(0),
                          posterior = numeric(0),
                          posteriorSample = numeric(0),
                          likelihood = numeric(0),
                          shadowTest = list(0)),
         validity = function(object) {
           return(TRUE)
         }
)

#' @docType methods
#' @rdname show-methods
#' @export
setMethod("show", "Shadow.output", function(object) {
  if (length(object@administeredItemIndex) > 0) {
    cat("Simulee Index          :", object@simuleeIndex, "\n")
    cat("  True Theta           :", object@trueTheta, "\n")
    cat("  Final Theta Estimate :", object@finalThetaEst, "\n")
    cat("  Final SE Estimate    :", object@finalSeEst, "\n")
    output = data.frame(Stage = 1:length(object@administeredItemIndex),
                        StimulusIndex = ifelse(is.nan(object@administeredStimulusIndex), rep(NA, length(object@administeredItemIndex)), object@administeredStimulusIndex),
                        ItemIndex = object@administeredItemIndex,
                        ItemResp = object@administeredItemResp,
                        InterimTheta = object@interimThetaEst,
                        InterimSE = object@interimSeEst,
                        ThetaSegment = object@thetaSegmentIndex)
    print(output)
  } else {
    cat("empty object of class Shadow.output\n")
  }
  cat("\n")
})
