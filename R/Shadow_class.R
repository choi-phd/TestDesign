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
#' @slot itemSelection A list.
#' @slot contentBalancing A list.
#' @slot MIP A list.
#' @slot MCMC A list.
#' @slot refreshPolicy A list.
#' @slot exposureControl A list.
#' @slot stoppingCriterion A list.
#' @slot interimTheta A list.
#' @slot finalTheta A list.
#' @slot thetaGrid Numeric.
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
