#! /usr/bin/Rscript

# V47a - switching to Bayesian per Wim's memo dated 5/1/18
# gstar in version 44d is determined by final theta estimate not true theta; results should look better for the outer theta segments



if (FALSE){
  require(Rcpp)
  require(Rsymphony)
  require(gurobi)
  require(lattice)
  require(logitnorm)
  library(IRTclass)
  library(Shadow)
  
  load("/Users/sl47276/Box\ Sync/Shadow/47h/data_Pool_B.RData")
  
  seed = 12321
  
  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(pool, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data
  
  nSegment = 7
  plotConfig = c(2, 4)
  
  ################ BIGM-BAYESIAN
  config.1 = new("Shadow.config")
  config.1@exposureControl$fadingFactor = .999
  config.1@exposureControl$accelerationFactor = 2
  config.1@exposureControl$method = "BIGM-BAYESIAN" 
  config.1@exposureControl$diagnosticStats = TRUE
  config.1@interimTheta$method = "FB"
  config.1@interimTheta$priorDist = "NORMAL"
  config.1@interimTheta$priorPar = c(0, 2)
  config.1@finalTheta$method = "FB"
  config.1@finalTheta$priorDist = "NORMAL"
  config.1@finalTheta$priorPar = c(0, 2)
  
  #config@exposureControl$segmentCut = c(-Inf, seq(-2, 2, length = nSegment - 1), Inf)
  #config@exposureControl$nSegment = nSegment
  
  
  CAT.1.out <- Shadow(pool, config.1, trueTheta, Constraints = constraints.0, prior = NULL, priorPar = c(0, 1), Data = respData)
  #saveOutput(CAT.1.out$output, file = paste0("log_checkEligibilityStats_1.csv"))
  #write.table(CAT.1.out$checkEligibilityStats, file = paste0("checkEligibilityStats_1.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
  #write.table(CAT.1.out$noFadingEligibilityStats, file = paste0("nonFadingEligibilityStats_1.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
  
  plotShadow(CAT.1.out$output[[1]], constraints.0, PDF = "shadow.pdf")
  
  CAT.1.out$output[[1]]
  plotCAT(CAT.1.out$output[[1]])
  
  
  
  ER.bySegment.1 = plotExposureRateBySegment(CAT.1.out, config.1, maxRate = .25, pdfFile = "ER_bySegment_1.pdf", mfrow = plotConfig)
  ER.trueTheta.1 = plotExposureRateFinal(CAT.1.out, config.1, maxRate = .25, theta = "TRUE", color = "blue", pdfFile = "ER_trueTheta_1.pdf", mfrow = plotConfig)
  ER.estTheta.1 = plotExposureRateFinal(CAT.1.out, config.1, maxRate = .25, theta = "ESTIMATED", color = "blue", pdfFile = "ER_estTheta_1.pdf", mfrow = plotConfig)
  
  ################ BIGM
  config.2 = new("Shadow.config")
  config.2@exposureControl$fadingFactor = .999
  config.2@exposureControl$accelerationFactor = 2
  config.2@exposureControl$method = "BIGM" 
  config.2@exposureControl$diagnosticStats = TRUE
  config.2@interimTheta$method = "EAP"
  config.2@interimTheta$priorDist = "NORMAL"
  config.2@interimTheta$priorPar = c(0, 2)
  config.2@finalTheta$method = "EAP"
  config.2@finalTheta$priorDist = "NORMAL"
  config.2@finalTheta$priorPar = c(0, 2)
  
  #config@exposureControl$segmentCut = c(-Inf, seq(-2, 2, length = nSegment - 1), Inf)
  #config@exposureControl$nSegment = nSegment
  
  CAT.2.out <- Shadow(pool, config.2, trueTheta, Constraints = constraints.0, prior = NULL, priorPar = c(0, 1), Data = respData)
  #saveOutput(CAT.2.out$output, file = paste0("log_checkEligibilityStats_2.csv"))
  #write.table(CAT.2.out$checkEligibilityStats, file = paste0("checkEligibilityStats_2.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
  #write.table(CAT.2.out$noFadingEligibilityStats, file = paste0("nonFadingEligibilityStats_2.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
  
  CAT.2.out$output[[1]]      # THETA BEING UPDATED NORMALLY
  
  ER.bySegment.2 = plotExposureRateBySegment(CAT.2.out, config.2, maxRate = .25, pdfFile = "ER_bySegment_2.pdf", mfrow = plotConfig)
  ER.trueTheta.2 = plotExposureRateFinal(CAT.2.out, config.2, maxRate = .25, theta = "TRUE", color = "blue", pdfFile = "ER_trueTheta_2.pdf", mfrow = plotConfig)
  ER.estTheta.2 = plotExposureRateFinal(CAT.2.out, config.2, maxRate = .25, theta = "ESTIMATED", color = "blue", pdfFile = "ER_estTheta_2.pdf", mfrow = plotConfig)
  
  
  ################ Eligibility
  config.3 = new("Shadow.config")
  if (!toupper(object@MIP$solver) %in% c("SYMPHONY", "GUROBI", "GLPK", "LPSOLVE")) stop("invalid option for MIP$solver")
  if (!object@itemSelection$method %in% c("MFI", "MPWI", "EB", "FB")) stop("invalid option for selectionCriterion")
  if (!object@contentBalancing$method %in% c("NONE", "STA")) stop("invalid option for contentBalancing")
  if (!object@refreshPolicy$method %in% c("ALWAYS", "POSITION", "INTERVAL", "THRESHOLD", "INTERVAL-THRESHOLD", "STIMULUS", "SET", "PASSAGE")) stop("invalid option for refreshPolicy")
  if (!object@exposureControl$method %in% c("NONE", "ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) stop("invalid option for exposureControl")
  if (!object@stoppingCriterion$method %in% c("FIXED")) stop("invalid option for stoppingCriterion")
  if (!object@interimTheta$method %in% c("EAP", "EB", "FB")) stop("invalid option for interimTheta")
  if (!object@finalTheta$method %in% c("EAP", "EB", "FB")) stop("invalid option for finalTheta")
  
  config.3@exposureControl$fadingFactor = .999         # .9 to 1.0 (def 1)
  config.3@exposureControl$accelerationFactor = 2      # positive, real, 1 or higher
  config.3@exposureControl$method = "ELIGIBILITY"      # 
  config.3@exposureControl$diagnosticStats = TRUE      #
  config.3@interimTheta$method = "EAP"                 # EAP, FB, EB
  config.3@interimTheta$priorDist = "NORMAL"           # NORMAL, UNIF
  config.3@interimTheta$priorPar = c(0, 2)             # 0, 1
  config.3@finalTheta$method = "EAP"                   #
  config.3@finalTheta$priorDist = "NORMAL"             #
  config.3@finalTheta$priorPar = c(0, 2)               #
  
  #config@exposureControl$segmentCut = c(-Inf, seq(-2, 2, length = nSegment - 1), Inf)
  #config@exposureControl$nSegment = nSegment
  
  CAT.3.out <- Shadow(pool, config.3, trueTheta, Constraints = constraints.0, prior = NULL, priorPar = c(0, 1), Data = respData)
  #saveOutput(CAT.3.out$output, file = paste0("log_checkEligibilityStats_3.csv"))
  #write.table(CAT.3.out$checkEligibilityStats, file = paste0("checkEligibilityStats_3.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
  #write.table(CAT.3.out$noFadingEligibilityStats, file = paste0("nonFadingEligibilityStats_3.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
  
  CAT.3.out$output[[1]]      # THETA BEING UPDATED NORMALLY
  
  p = plotCAT(CAT.3.out$output)
  plotShadow(CAT.3.out$output[[1]], constraints.0)
  
  ER.bySegment.3 = plotExposureRateBySegment(CAT.3.out, config.3, maxRate = .25, pdfFile = "ER_bySegment_3.pdf", mfrow = plotConfig)
  ER.trueTheta.3 = plotExposureRateFinal(CAT.3.out, config.3, maxRate = .25, theta = "TRUE", color = "blue", pdfFile = "ER_trueTheta_3.pdf", mfrow = plotConfig)
  ER.estTheta.3 = plotExposureRateFinal(CAT.3.out, config.3, maxRate = .25, theta = "ESTIMATED", color = "blue", pdfFile = "ER_estTheta_3.pdf", mfrow = plotConfig)
  
}

