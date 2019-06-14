#' constraint
#'
#' Represents a set of constriants.
#' 
#' @slot CONSTRAINT Character. The index of the constraint set.
#' @slot mat A matrix representing the left-hand side weights. Has nc rows.
#' @slot dir A vector of length nc. Each entry represents a logical operator relating the left-hand side to the right-hand side.
#' @slot rhs A vector of length nc. Each entry represents the right-hand side of the constraint.
#' @slot nc Numeric. The number of constraints represented in this object.
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
#' Create a \code{\linkS4class{test}} object.
#'
#' @slot pool An \code{\linkS4class{item.pool}} object.
#' @slot theta A theta grid.
#' @slot Prob A list of item response probabilities.
#' @slot Info A matrix of item information values.
#' @slot trueTheta An optional vector of true theta values.
#' @slot Data An optional matrix of item responses.
#' 
#' @export

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
#' Create a \code{\linkS4class{test.cluster}} object from a list of \code{\linkS4class{test}} objects.
#'
#' @slot nt Numeric. A scalar to indicate the number of \code{\linkS4class{test}} objects to be clustered.
#' @slot tests A list \code{\linkS4class{test}} objects.
#' @slot names Character. A vector of names correspondign to the \code{\linkS4class{test}} objects.
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

#' config.Shadow
#' 
#' @rdname config.Shadow

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
           if (object@exposureControl$nSegment != length(object@exposureControl$segmentCut) - 1) stop("nSegment and segmentCut are incongruent")
           if (!object@stoppingCriterion$method %in% c("FIXED")) stop("invalid option for stoppingCriterion")
           if (!object@interimTheta$method %in% c("EAP", "EB", "FB")) stop("invalid option for interimTheta")
           if (!object@finalTheta$method %in% c("EAP", "EB", "FB")) stop("invalid option for finalTheta")
           if (object@exposureControl$method %in% c("BIGM-BAYESIAN") && !object@interimTheta$method %in% c("EB", "FB")) stop("for exposureControl BIGM-BAYESIAN you must use interimTheta of EB of FB")
           return(TRUE)
         }
)

#' config.Shadow
#' 
#' Create a \code{\linkS4class{Shadow.config}} object for Shadow Test Assembly (STA).
#' 
#' @param itemSelection A list containing item selection criteria.
#' \itemize{
#'   \item{\code{method}} The type of criteria. Accepts one of \code{MFI, MPWI, FB, EB}.
#'   \item{\code{infoType}} The type of information. Accepts \code{FISHER}.
#'   \item{\code{initialTheta}} 
#'   \item{\code{fixedTheta}} 
#' }
#' @param contentBalancing A list containing content balancing options.
#' \itemize{
#'   \item{\code{method}} The type of balancing method. Accepts one of \code{NONE, STA}.
#' }
#' @param MIP A list containing solver options.
#' \itemize{
#'   \item{\code{solver}} The type of solver. Accepts one of \code{SYMPHONY, GUROBI, GLPK, LPSOLVE}.
#'   \item{\code{verbosity}} Verbosity level.
#'   \item{\code{timeLimit}} Time limit to be passed onto solver. Used in solvers \code{SYMPHONY, GUROBI, GLPK}.
#'   \item{\code{gapLimit}} Gap limit to be passed onto solver. Used in solvers  \code{SYMPHONY, GUROBI}.
#' }
#' @param MCMC A list containing Markov-chain Monte Carlo configurations.
#' \itemize{
#'   \item{\code{burnIn}} Numeric. The number of chains from the start to discard.
#'   \item{\code{postBurnIn}}  Numeric. The number of chains to use after discarding the first \code{burnIn} chains.
#'   \item{\code{thin}} Thin.
#'   \item{\code{jumpfactor}} JumpFactor.
#' }
#' @param refreshPolicy A list containing refresh policy for obtaining a new shadow test.
#' \itemize{
#'   \item{\code{method}} The type of policy. Accepts one of \code{ALWAYS, POSITION, INTERVAL, THRESHOLD, INTERVAL-THRESHOLD, STIMULUS, SET, PASSAGE}.
#'   \item{\code{interval}} Integer. Set to 1 to refresh at each position, 2 to refresh at every two positions, and so on.
#'   \item{\code{threshold}} Numeric. The shadow test is refreshed when the absolute change in theta estimate is greater than this value.
#'   \item{\code{position}}
#' }
#' @param exposureControl A list containing exposure control settings.
#' \itemize{
#'   \item{\code{method}} Accepts one of \code{"NONE", "ELIGIBILITY", "BIGM", "BIGM-BAYESIAN"}.
#'   \item{\code{M}} 
#'   \item{\code{maxExposureRate}} 
#'   \item{\code{accelerationFactor}}
#'   \item{\code{nSegment}}
#'   \item{\code{firstSegment}}
#'   \item{\code{segmentCut}}
#'   \item{\code{initialEligibilityStats}}
#'   \item{\code{fadingFactor}}
#'   \item{\code{diagnosticStats}}
#' }
#' @param stoppingCriterion A list containing stopping criterion.
#' \itemize{
#'   \item{\code{method}} Accepts one of \code{"FIXED"}.
#'   \item{\code{testLength}} 
#'   \item{\code{minNI}}
#'   \item{\code{maxNI}}
#'   \item{\code{SeThreshold}}
#' }
#' @param interimTheta A list containing interim theta estimation options.
#' \itemize{
#'   \item{\code{method}} The type of estimation. Accepts one of \code{EAP, EB, FB}.
#'   \item{\code{shrinkageCorrection}}
#'   \item{\code{priorDist}} The type of prior distribution. Accepts one of \code{NORMAL, UNIF}.
#'   \item{\code{priorPar}} Distributional parameters for the prior.
#'   \item{\code{boundML}}
#'   \item{\code{maxIter}}
#'   \item{\code{crit}}
#' }
#' @param finalTheta A list containing final theta estimation options.
#' \itemize{
#'   \item{\code{method}} The type of estimation. Accepts one of \code{EAP, EB, FB}.
#'   \item{\code{shrinkageCorrection}}
#'   \item{\code{priorDist}} The type of prior distribution. Accepts one of \code{NORMAL, UNIF}.
#'   \item{\code{priorPar}} Distributional parameters for the prior.
#'   \item{\code{boundML}}
#'   \item{\code{maxIter}}
#'   \item{\code{crit}}
#' }
#' @param thetaGrid A numeric vector. Theta values to represent the continuum.
#' @param auditTrail Logical.
#' 
#' @rdname config.Shadow
#' @export
config.Shadow = function(itemSelection = NULL, contentBalancing = NULL, MIP = NULL, MCMC = NULL,
                         refreshPolicy = NULL, exposureControl = NULL, stoppingCriterion = NULL,
                         interimTheta = NULL, finalTheta = NULL, thetaGrid = seq(-4, 4, .1), auditTrail = F){
  conf = new("Shadow.config")
  
  arg.names = c("itemSelection", "contentBalancing", "MIP", "MCMC",
                "refreshPolicy", "exposureControl", "stoppingCriterion",
                "interimTheta", "finalTheta")
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
  if (!is.null(thetaGrid))  conf@thetaGrid  = thetaGrid
  if (!is.null(auditTrail)) conf@auditTrail = auditTrail
  v = validObject(conf)
  if (v) return(conf)
}

#' @docType methods
#' @rdname show-methods
#' @export
setMethod("show", "Shadow.config", function(object) {
  cat("Shadow Configuration Settings \n\n")
  cat("  itemSelection \n")
  cat("    method         :", object@itemSelection$method, "\n")
  cat("    infoType       :", object@itemSelection$infoType, "\n")
  cat("    initialTheta   :", object@itemSelection$initialTheta, "\n")
  cat("    fixedTheta     :", object@itemSelection$fixedTheta, "\n\n")
  cat("  contentBalancing \n")
  cat("    method         :", object@contentBalancing$method, "\n\n")
  cat("  MIP \n")
  cat("    solver         :", object@MIP$solver, "\n")
  cat("    verbosity      :", object@MIP$verbosity, "\n")
  cat("    timeLimit      :", object@MIP$timeLimit, "\n")
  cat("    gapLimit       :", object@MIP$gapLimit, "\n\n")
  cat("  MCMC \n")
  cat("    burnIn         :", object@MCMC$burnIn, "\n")
  cat("    postBurnIn     :", object@MCMC$postBurnIn, "\n")
  cat("    thin           :", object@MCMC$thin, "\n")
  cat("    jumpFactor     :", object@MCMC$jumpFactor, "\n\n")
  cat("  refreshPolicy \n")
  cat("    method         :", object@refreshPolicy$method, "\n")
  cat("    interval       :", object@refreshPolicy$interval, "\n")
  cat("    threshold      :", object@refreshPolicy$threshold, "\n")
  cat("    position       :", object@refreshPolicy$position, "\n\n")
  cat("  exposureControl \n")
  cat("    method                  :", object@exposureControl$method, "\n")
  cat("    M                       :", object@exposureControl$M, "\n")
  cat("    maxExposureRate         :", object@exposureControl$maxExposureRate, "\n")
  cat("    accelerationFactor      :", object@exposureControl$accelerationFactor, "\n")
  cat("    nSegment                :", object@exposureControl$nSegment, "\n")
  cat("    firstSegment            :", object@exposureControl$firstSegment, "\n")
  cat("    segmentCut              :", object@exposureControl$segmentCut, "\n")
  cat("    initialEligibilityStats :", !is.null(object@exposureControl$initialEligibilityStats), "\n")
  cat("    fadingFactor            :", object@exposureControl$fadingFactor, "\n")
  cat("    diagnosticsStats        :", object@exposureControl$diagnosticStats, "\n\n")
  cat("  stoppingCriterion \n")
  cat("    method         :", object@stoppingCriterion$method, "\n")
  cat("    testLength     :", object@stoppingCriterion$testLength, "\n")
  cat("    minNI          :", object@stoppingCriterion$minNI, "\n")
  cat("    maxNI          :", object@stoppingCriterion$maxNI, "\n")
  cat("    SeThreshold    :", ifelse(toupper(object@stoppingCriterion$method) == "VARIABLE", object@stoppingCriterion$SeThreshold, NA), "\n\n")
  cat("  interimTheta \n")
  cat("    method              :", object@interimTheta$method, "\n")
  cat("    shrinkageCorrection :", object@interimTheta$shrinkageCorrection, "\n")
  cat("    priorDist           :", ifelse(toupper(object@interimTheta$method == "EAP"), object@interimTheta$priorDist, NA), "\n")
  cat("    priorPar            :", ifelse(toupper(object@interimTheta$method == "EAP"), sprintf(ifelse(toupper(object@interimTheta$priorDist) == "NORMAL", "Mean = %5.3f, SD = %5.3f", "Min = %5.3f, Max = %5.3f"), object@interimTheta$priorPar[1], object@interimTheta$priorPar[2]), NA), "\n\n")
  cat("  finalTheta \n")
  cat("    method              :", object@finalTheta$method, "\n")
  cat("    shrinkageCorrection :", object@finalTheta$shrinkageCorrection, "\n")
  cat("    priorDist           :", ifelse(toupper(object@finalTheta$method == "EAP"), object@finalTheta$priorDist, NA), "\n")
  cat("    priorPar            :", ifelse(toupper(object@finalTheta$method == "EAP"), sprintf(ifelse(toupper(object@finalTheta$priorDist) == "NORMAL", "Mean = %5.3f, SD = %5.3f", "Min = %5.3f, Max = %5.3f"), object@finalTheta$priorPar[1], object@finalTheta$priorPar[2]), NA), "\n\n")
  cat("  thetaGrid \n")
  print(object@thetaGrid)
  cat("\n  auditTrail: ", object@auditTrail, "\n")
})

#' Shadow.output
#'
#' @slot simuleeIndex Numeric. The index of the simulee.
#' @slot trueTheta Numeric or NULL. True theta value of the simulee if supplied in advance.
#' @slot trueThetaSegment Numeric or NULL. Which segment the true theta value is in.
#' @slot finalThetaEst Numeric. The estimated theta after the last administered item.
#' @slot finalSeEst Numeric. The standard error of estimation after the last administered item.
#' @slot administeredItemIndex Numeric. A vector of item indices administered at each position.
#' @slot administeredItemResp Numeric. A vector of responses at each position.
#' @slot administeredStimulusIndex Numeric. A vector of stimulus indices administered at each position.
#' @slot shadowTestRefreshed Logical. A vector of logical values indicating whether the shadow test was refreshed before administering an item at each position.
#' @slot shadowTestFeasible Logical. A vector of logical values indicating whether a feasible solution to the shadow test was available in each position.
#' @slot solveTime Numeric. A vector of values indicating the time taken in obtaining a shadow test. 
#' @slot interimThetaEst Numeric. A vector containing estimated thetas at each position.
#' @slot interimSeEst Numeric. A vector containing standard errors at each position.
#' @slot thetaSegmentIndex Numeric. A vector containing which segments the estimated thetas were in at each position.
#' @slot prior Numeric.
#' @slot priorPar Numeric.
#' @slot posterior Numeric.
#' @slot posteriorSample Numeric.
#' @slot likelihood Numeric.
#' @slot shadowTest A list of vectors containing item indices of the shadow test at each position.
#' 
#' @export

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
