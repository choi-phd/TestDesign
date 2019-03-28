#Program: Shadow classes
#Author: Seung Choi
#Version: 0.1
#Assumptions: D = 1.0; the minimum score is 0 for all items

# Documentation progress
# Phase 1: Create a skeleton structure    -- complete

#' @import foreach
#' @importFrom logitnorm logit rlogitnorm
#' @importFrom grDevices col2rgb dev.control dev.new dev.off pdf recordPlot
#' @importFrom stats dnorm rlnorm rnorm sd
#' @importFrom utils setTxtProgressBar txtProgressBar write.table
#' @importFrom graphics axis grid layout legend mtext par plot.new points rect text
#' @importFrom lattice xyplot
#' @useDynLib Shadow
NULL

#' STA
#'
#' Shadow Test Approach to CAT.
#'
#' @param Constraints A list representing optimization constraints. Use \code{\link{LoadConstraints}} for this.
#' @param objective A vector of objective values.
#' @param solver The type of solver. Accepts \code{SYMPHONY, GUROBI, GLPK, LPSOLVE}.
#' @param xmat A nice parameter
#' @param xdir A nice parameter
#' @param xrhs A vector of right-side values.
#' @param maximize If \code{TRUE}, treat as a maximization problem.
#' @param mps Only used when \code{solver} is \code{SYMPHONY}. If \code{TRUE}, print an MPS representation of the problem for debugging purposes.
#' @param lp Only used when \code{solver} is \code{SYMPHONY}. If \code{TRUE}, print an LP representation of the problem for debugging purposes.
#' @param verbosity Verbosity level.
#' @param time_limit Time limit to be passed onto solver.
#' @param gap_limit Gap limit to be passed onto solver.
#' @param ... Only used when \code{solver} is \code{SYMPHONY}. Additional parameters to be passed onto \code{\link[Rsymphony]{Rsymphony_solve_LP}}.
#' 
#' @return An output
#'
#' @export

STA = function(Constraints, objective, solver = "Symphony", xmat = NULL, xdir = NULL, xrhs = NULL, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = -2, time_limit = 5, gap_limit = -1, ...) {
  if (length(objective) == Constraints$nv) {
    obj = objective
  } else if (length(objective) == Constraints$ni) {
    obj = numeric(Constraints$nv)
    obj[1:Constraints$ni] = objective
  } else {
    stop(sprintf("length of objective must be %s", Constraints$nv))
  }
  if (!is.null(xmat) && !is.null(xdir) && !is.null(xrhs)) {
    MAT = rbind(Constraints$MAT, xmat)
    DIR = c(Constraints$DIR, xdir)
    RHS = c(Constraints$RHS, xrhs)
  } else {
    MAT = Constraints$MAT
    DIR = Constraints$DIR
    RHS = Constraints$RHS
  }
  solve.time = proc.time()
  if (toupper(solver) == "SYMPHONY") {
    MIP = Rsymphony_solve_LP(obj, MAT, DIR, RHS, max = maximize, types = "B", write_mps = mps, write_lp = lp, verbosity = verbosity, time_limit = time_limit, gap_limit = gap_limit, ...)
    status = MIP$status
    if (!names(status) %in% c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND")) {
      warning(sprintf("MIP solver returned non-zero status: %s", names(MIP$status)))
      return(list(status =status, MIP = NULL, Selected = NULL))
    }
  } else if (toupper(solver) == "GUROBI") {
    DIR[DIR == "=="] = "="
    invisible(capture.output(MIP <- gurobi::gurobi(list(obj = obj, modelsense = "max", rhs = RHS, sense = DIR, vtype = "B", A = MAT), params = NULL, env = NULL)))
    status = MIP$status
    if (MIP$status != "OPTIMAL") {
      warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
      return(list(status = status, MIP = NULL, Selected = NULL))
    }
    MIP[["solution"]] = MIP$x
  } else if (toupper(solver) == "GLPK") {
    MIP = Rglpk_solve_LP(obj, MAT, DIR, RHS, max = maximize, types = "B", control = list(verbose = ifelse(verbosity != -2, TRUE, FALSE), presolve = TRUE, tm_limit = time_limit))
    status = MIP$status
    if (MIP$status != 0) {
      warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
      return(list(status = status, MIP = NULL, Selected = NULL))
    }
  } else if (toupper(solver) == "LPSOLVE") {
    MIP = lp(direction = ifelse(maximize, "max", "min"), obj, MAT, DIR, RHS, all.bin = TRUE, presolve = TRUE)
    status = MIP$status
    if (MIP$status != 0) {
      warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
      return(list(status = status, MIP = NULL, Selected = NULL))
    }
  } else {
    stop("solver must be Symphony, Gurobi, glpk, lpSolve")
  }
  solve.time = (proc.time() - solve.time)["elapsed"]
  if (!is.null(Constraints$StimulusOrder)) {
    Constraints$ItemAttrib = merge(Constraints$ItemAttrib, Constraints$StAttrib[c("STINDEX", "STID", Constraints$StimulusOrderBy)], by = "STID", all.x = TRUE, sort = FALSE)
  } else if (!is.null(Constraints$StAttrib)) {
    Constraints$ItemAttrib = merge(Constraints$ItemAttrib, Constraints$StAttrib[c("STINDEX", "STID")], by = "STID", all.x = TRUE, sort = FALSE)
  }
  index.solution = which(MIP$solution[1:Constraints$ni] == 1)
  Info = obj[index.solution]
  shadowTest = data.frame(cbind(Constraints$ItemAttrib[index.solution, ], Info))
  if (Constraints$setBased) {
    #for items associated with passages, avgInfo = mean info for STID
    #discrete items have NA in ItemAttrib$STID
    if (any(is.na(shadowTest$STID))) {
      #if discrete items present in shadowTest
      shadowTest = data.frame(cbind(.sequence = 1:nrow(shadowTest), shadowTest))
      shadowTestDiscrete = shadowTest[is.na(shadowTest$STID), ]
      shadowTestDiscrete = data.frame(cbind(shadowTestDiscrete, meanInfo = shadowTestDiscrete$Info))
      shadowTestStimulus = shadowTest[!is.na(shadowTest$STID), ]
      meanInfo = tapply(shadowTestStimulus$Info, shadowTestStimulus$STID, mean)
      meanInfo = data.frame(STID = names(meanInfo), meanInfo = meanInfo)
      shadowTestStimulus = merge(shadowTestStimulus, meanInfo, by = "STID", all.x = TRUE, sort = FALSE)
      shadowTest = rbind(shadowTestDiscrete, shadowTestStimulus)
      shadowTest = shadowTest[order(shadowTest$.sequence),]
      shadowTest = shadowTest[-1]
    } else {
      #setBased with no discrete items present in shadowTest
      meanInfo = tapply(shadowTest$Info, shadowTest$STID, mean)
      meanInfo = data.frame(STID = names(meanInfo), meanInfo = meanInfo)
      shadowTest = merge(shadowTest, meanInfo, by = "STID", all.x = TRUE, sort = FALSE)
    }
    if (!is.null(Constraints$StimulusOrderBy) && !is.null(Constraints$ItemOrderBy)) {
      #if both stimuli and items should be ordered (besides information)
      #stimuli ties?
      shadowTest = shadowTest[order(shadowTest[[Constraints$StimulusOrderBy]], shadowTest[["meanInfo"]], shadowTest[["STID"]], shadowTest[[Constraints$ItemOrderBy]], shadowTest[["Info"]], decreasing = c(FALSE, TRUE, FALSE, FALSE, TRUE), method = "radix"), ]
    } else if (!is.null(Constraints$StimulusOrderBy)) {
      #if stimuli should be ordered but items within stimuli should be ordered by meanInfo
      #the problem arises when stimuli and discrete items are mixed in
      shadowTest = shadowTest[order(shadowTest[[Constraints$StimulusOrderBy]], shadowTest[["meanInfo"]], shadowTest[["STID"]], shadowTest[["Info"]], decreasing = c(FALSE, TRUE, FALSE, TRUE), method = "radix"), ]
    } else if (!is.null(Constraints$ItemOrderBy)) {
      #if items within stimuli should be ordered by ItemOrderBy; stimuli should be order by meanInfo
      shadowTest = shadowTest[order(shadowTest[["meanInfo"]], shadowTest[["STID"]], shadowTest[[Constraints$ItemOrderBy]], shadowTest[["Info"]], decreasing = c(TRUE, FALSE, FALSE, TRUE), method = "radix"), ]
    } else {
      #stimuli and items should be ordered by meanInfo
      shadowTest = shadowTest[order(shadowTest[["meanInfo"]], shadowTest[["STID"]], shadowTest[["Info"]], decreasing = c(TRUE, FALSE, TRUE), method = "radix"), ]
    }
  } else {
    #not setBased
    if (!is.null(Constraints$ItemOrderBy)) {
      #items should be ordered by ItemOrderBy
      shadowTest = shadowTest[order(shadowTest[[Constraints$ItemOrderBy]], shadowTest[["Info"]], decreasing = c(FALSE, TRUE), method = "radix"), ]
    } else {
      #items should be ordered by Info
      shadowTest = shadowTest[order(shadowTest[["Info"]], decreasing = TRUE), ]
    }
  }
  obj.value = sum(Info)
  row.names(shadowTest) = 1:nrow(shadowTest)
  return(list(status = status, MIP = MIP, shadowTest = shadowTest, obj.value = obj.value, solve.time = solve.time))
}

# setMethod(f = "[<-", signature = "Shadow.config",
#            definition = function(x, i, j, value) {
#              x@i$j = value
#              validObject(x)
#              return(x)
#            }
# )

#' saveOutput
#'
#' Do fancy stuff.
#'
#' @param objectList A nice parameter
#' @param file A nice parameter
#' 
#' @return An output
#'
#' @export
saveOutput = function(objectList, file = NULL) {
  nj = length(objectList)
  for (j in 1:nj) {
    object = objectList[[j]] 
    output = data.frame(Simulee = object@simuleeIndex,
                        TrueTheta = object@trueTheta,
                        TrueThetaSegment = object@trueThetaSegment,
                        Stage = 1:length(object@administeredItemIndex),
                        StimulusIndex = ifelse(is.nan(object@administeredStimulusIndex), rep(NA, length(object@administeredItemIndex)), object@administeredStimulusIndex),
                        ItemIndex = object@administeredItemIndex, 
                        ItemResp = object@administeredItemResp,
                        InterimTheta = object@interimThetaEst,
                        InterimSE = object@interimSeEst,
                        InterimThetaSegment = object@thetaSegmentIndex)
    if (!is.null(file)) {
      write.table(output, file = file, append = j>1, row.names = FALSE, col.names = j==1, sep = ",")
    } else {
      print(output)
    }
  }
}

#' An S4 generic
#' 
#' @param object A nice parameter
#' @param Constraints A nice parameter
#' @param sortByDifficulty A nice parameter
#' @param PDF A nice parameter
#' 
#' @docType methods
#' @rdname plotShadow-methods
#' @export
setGeneric(name = "plotShadow",
           def = function(object, Constraints, sortByDifficulty = FALSE, PDF = NULL) {
             standardGeneric("plotShadow")
           }
)

#' plotShadow
#' 
#' plotShadow
#' 
#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(f = "plotShadow",
          signature = "Shadow.output",
          definition = function(object, Constraints, sortByDifficulty = FALSE, PDF = NULL) {
            if (!is.null(PDF)) {
              pdf(file = PDF)
            } else {
              pdf(NULL, bg = 'white')
              dev.control(displaylist="enable")
            }
            
            maxNI = Constraints$testLength
            ni = Constraints$ni            
            par(mar = c(2, 3, 1, 1) + 0.1, mfrow = c(1, 1))           
            n.points = sum(!is.na(object@administeredItemResp)) #this should be equal to Constraints$testLength
            item.id = Constraints$ItemAttrib$ID[object@administeredItemIndex]
            item.sequence = object@administeredItemIndex
            responses = object@administeredItemResp            
            plot(c(0.5, maxNI + 0.5), c(0.5, ni + 0.5), type = "n", las = 1, xlim = c(0, maxNI), xaxt = "n", yaxt = "n", ylab = "")
            usr = par("usr")
            text(maxNI / 2, usr[3] / 2, "Position", adj = c(0.5, 0), cex = 1.0)            
            if (sortByDifficulty) {
              axis(2, at = ni / 2, labels = "Easier <-  Items  -> Harder", cex.axis = 1.5, tick = FALSE, line = 0)
            } else {
              axis(2, at = ni / 2, labels = "Items", cex.axis = 1.5, tick = FALSE, line = 0)
            }            
            text(maxNI / 2, mean(c(usr[4], ni)), paste0("Examinee ID: ", object@simuleeIndex), adj = c(0.5, 0.5), cex = 1)
            axis(1, at = 0:maxNI, tick = TRUE, labels = 0:maxNI, cex.axis = 0.7)           
            text(0, seq(10, ni, 10), seq(10, ni, 10), adj = c(0.5, 0.5), cex = 0.7)           
            for (i in 1:n.points) {
              for (j in 1:ni) {
                rect(i - 0.25, j - 0.25, i + 0.25, j + 0.25, border = "gray88", lwd = 0.3)
              }
              if (object@shadowTestRefreshed[i]) {
                text(i, usr[3], "S", col = "red", cex = 0.7, adj = c(0.5, 0))
              }
            }           
            if (Constraints$setBased) {             
              for (p in 1:Constraints$ns) {
                for (i in 1:n.points) {
                  rect(i - 0.35, min(Constraints$ItemIndexByStimulus[[p]]) - 0.5, i + 0.35, max(Constraints$ItemIndexByStimulus[[p]]) + 0.5, border = "gray88", lwd = 0.5)
                }
              }
            }            
            shadow.tests = object@shadowTest
            if (Constraints$setBased) {    
              item.table = merge(Constraints$ItemAttrib, Constraints$StAttrib[c("STID", "STINDEX", "NITEM")], by = "STID", all.x = TRUE, sort = FALSE)
              for (k in 1:n.points) {                
                items = shadow.tests[[k]]
                current.item = object@administeredItemIndex[k]
                passages = unique(item.table$STINDEX[items])
                current.passage = item.table$STINDEX[current.item]                
                for (p in 1:length(passages)) {
                  sub.items = Constraints$ItemIndexByStimulus[[passages[p]]]                 
                  if (passages[p] == current.passage) {
                    rect(k - 0.35, min(sub.items) - 0.5, k + 0.35, max(sub.items) + 0.5, border = "blue", col = "khaki", lwd = 0.5)
                    for (i in 1:length(sub.items)) {
                      if (sub.items[i] == current.item) {
                        if (responses[k] >= 1) {
                          rect(k - 0.25, sub.items[i] - 0.25, k + 0.25, sub.items[i] + 0.25, border = "lime green", col = "lime green", lwd = 0.3)
                        } else if (responses[k] == 0) {
                          rect(k - 0.25, sub.items[i] - 0.25, k + 0.25, sub.items[i] + 0.25, border = "red", col = "red", lwd = 0.3)
                        }
                      } else if (sub.items[i] %in% items) {
                        rect(k - 0.25, sub.items[i] - 0.25, k + 0.25, sub.items[i] + 0.25, border = "black", lwd = 0.3)
                      } else {
                        rect(k - 0.25, sub.items[i] - 0.25, k + 0.25, sub.items[i] + 0.25, border = "white", lwd = 0.3)
                      }
                    }
                  } else {
                    rect(k - 0.35, min(sub.items) - 0.5, k + 0.35, max(sub.items) + 0.5, border = "blue", col = "gray50", lwd = 0.5)
                    for (i in 1:length(sub.items)) {
                      if (sub.items[i] %in% items) {
                        rect(k - 0.25, sub.items[i] - 0.25, k + 0.25, sub.items[i] + 0.25, border = "black", lwd = 0.3)
                      } else {
                        rect(k - 0.25, sub.items[i] - 0.25, k + 0.25, sub.items[i] + 0.25, border = "gray88", lwd = 0.3)
                      }
                    }
                  }
                }
              }
            } else { #discrete items only; need to handle a mixed-type case
              for (k in 1:n.points) {                
                items = shadow.tests[[k]]
                current.item = object@administeredItemIndex[k]
                for (i in 1:length(items)) {
                  if (items[i] != current.item) {
                    rect(k - 0.25, items[i] - 0.25, k + 0.25, items[i] + 0.25, border = "black", lwd = 0.3)
                  }
                }
              }
              for (k in 1:n.points) {
                items = shadow.tests[[k]]
                current.item = object@administeredItemIndex[k]
                for (i in 1:length(items)) {
                  if (items[i] == current.item) {
                    for (kk in k:n.points){
                      rect(kk - 0.25, items[i] - 0.25, kk + 0.25, items[i] + 0.25, border = "gray33", col = "gray33", lwd = 0.3)
                    }
                    if (responses[k] >= 1) {
                      rect(k - 0.25, items[i] - 0.25, k + 0.25, items[i] + 0.25, border = "lime green", col = "lime green", lwd = 0.3)
                    } else if (responses[k] == 0) {
                      rect(k - 0.25, items[i] - 0.25, k + 0.25, items[i] + 0.25, border = "red", col = "red", lwd = 0.3)
                    }
                  }
                }
              }
            }
            if (!is.null(PDF)) {
              dev.off()
            } else {
              p = recordPlot()
              plot.new()
              dev.off()
              
              return(p)
            }
          }
)

#' plotShadow
#' 
#' plotShadow
#' 
#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(f = "plotShadow",
          signature = "list",
          definition = function(object, Constraints, sortByDifficulty = FALSE, PDF = NULL) {
            if (!is.null(PDF)) {
              pdf(file = PDF)
            }
            for (i in 1:length(object)) {
              plotShadow(object[[i]], Constraints)
            }
            if (!is.null(PDF)) {
              dev.off()
            }
          }
)

#' plotCAT
#' 
#' @param object A nice parameter
#' @param minTheta A nice parameter
#' @param maxTheta A nice parameter
#' @param minScore A nice parameter
#' @param maxScore A nice parameter
#' @param zCI A nice parameter
#' @param PDF A nice parameter
#' 
#' @docType methods
#' @rdname plotCAT-methods
#' @export
setGeneric(name = "plotCAT",
           def = function(object, minTheta = -5, maxTheta = 5, minScore = 0, maxScore = 1, zCI = 1.96, PDF = NULL) {
             standardGeneric("plotCAT")
           }
)

#' plotCAT
#' 
#' plotCAT
#' 
#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(f = "plotCAT",
          signature = "Shadow.output",
          definition = function(object, minTheta = -5, maxTheta = 5, minScore = 0, maxScore = 1, zCI = 1.96, PDF = NULL) {
            nItems = length(object@administeredItemIndex) #number of items administered
            
            if (!is.null(PDF)) {
              pdf(file = PDF, bg = 'white')
            } else {
              pdf(NULL, bg = 'white')
              dev.control(displaylist="enable")
            }
            
            if (nItems > 0) {
              par(mar = c(2, 3, 1, 1) + 0.1)
              layout(rbind(c(1, 1), c(1, 1), c(1, 1), c(1, 1), c(2, 2)))
              
              plot(1:nItems, seq(minTheta, maxTheta, length = nItems), ylab = "Theta", type = "n", las = 1, xlim = c(0, nItems), xaxt = "n", yaxt = "n")
              grid()
              text(nItems/2, maxTheta, paste0("Examinee ID: ", object@simuleeIndex), adj = c(0.5, 0.5), cex = 2)
              axis(1, at = 0:nItems, tick = TRUE, labels = 0:nItems, cex.axis = 1.5)
              axis(2, at = minTheta:maxTheta, labels = minTheta:maxTheta, cex.axis = 1.5)
              text(0.5, minTheta + 1.0, paste("Theta: ", round(object@finalThetaEst, digits = 2)," SE: ", round(object@finalSeEst, digits = 2)), cex = 1.5, adj = 0)
              for (i in 1:nItems) {
                lines(rep(i ,2), c(object@interimThetaEst[i] - zCI * object@interimSeEst[i], object@interimThetaEst[i] + zCI * object@interimSeEst[i]), col = "purple4")
                lines(c(i - 0.25, i + 0.25), c(object@interimThetaEst[i] - zCI * object@interimSeEst[i], object@interimThetaEst[i] - zCI * object@interimSeEst[i]), col = "purple4")
                lines(c(i - 0.25, i + 0.25), c(object@interimThetaEst[i] + zCI * object@interimSeEst[i], object@interimThetaEst[i] + zCI * object@interimSeEst[i]), col = "purple4")
              }
              lines(1:nItems, object@interimThetaEst, lty = 3, col = "blue", lwd = 1.5)
              points(1:nItems, object@interimThetaEst, pch = 16, cex = 2.5, col = "blue")
              points(1:nItems, object@interimThetaEst, pch = 1, cex = 2.5, col = "purple4")
              
              if (!is.null(object@trueTheta)) {
                abline(h = object@trueTheta, lty = 1, col = "red")
              } else {
                abline(h = object@finalThetaEst, lty = 1, col = "red")
              }
              
              for (i in 1:nItems) {
                if (object@shadowTestRefreshed[i]) {
                  text(i, minTheta, "S", col = "red", cex = 1.5)
                }
              }
              
              plot(1:nItems, seq(minScore, maxScore, length.out = nItems), type = "n", xaxt = "n", ylim = c(minScore - 1, maxScore + 1), xlim = c(0, nItems), yaxt = "n", ylab = "")
              mtext("Position", side = 1, line = 1, outer = FALSE, cex = 1.5)
              axis(2, at = (minScore + maxScore)/2, labels = "Response", cex.axis = 2, tick = FALSE)
              for (i in 1:nItems) {
                x = i
                y = object@administeredItemResp[i]
                if (!is.na(y)) {
                  if (object@administeredItemResp[i] == minScore) {
                    rect(x - 0.25, minScore - 1, x + 0.25, y, col = "red", border = "black")
                  } else {
                    rect(x - 0.25, minScore - 1, x + 0.25, y, col = "lime green", border = "black")
                  }
                }
              }
            } else {
              cat("Shadow.output is empty\n")
            }
            
            if (!is.null(PDF)) {
              dev.off()
            } else {
              p = recordPlot()
              plot.new()
              dev.off()
              
              return(p)
            }
          }
)

#' plotCAT
#' 
#' plotCAT
#' 
#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(f = "plotCAT",
          signature = "list",
          definition = function(object, minTheta = -5, maxTheta = 5, minScore = 0, maxScore = 1, zCI = 1.96, PDF = NULL) {
            if (!is.null(PDF)) {
              pdf(file = PDF)
            }
            for (i in 1:length(object)) {
              plotCAT(object[[i]], minTheta = minTheta, maxTheta = maxTheta, minScore = minScore, maxScore = maxScore, zCI = zCI)
            }
            if (!is.null(PDF)) {
              dev.off()
            }
          }
)

#' calcRP
#' 
#' calcRP
#' 
#' @param object A nice parameter
#' @param rp A nice parameter
#' @param maxIter A nice parameter
#' @param conv A nice parameter
#' @param startTheta A nice parameter
#' 
#' @export
calcRP = function(object, rp = .50, maxIter = 100, conv = 0.0001, startTheta = 0) {
  RP = numeric(object@ni)
  for (i in 1:object@ni) {
    maxScore = object@NCAT[i] - 1
    theta = startTheta
    ep = as.vector(calcEscore(object@parms[[i]], theta)) / maxScore
    gap = abs(rp - ep)
    done = gap < conv
    iter = 0
    while (!done && iter < maxIter) {
      iter = iter + 1
      h = gap / -calcFisher(object@parms[[i]], theta)
      theta = theta - h
      ep = as.vector(calcEscore(object@parms[[i]], theta)) / maxScore
      gap = abs(rp - ep)
      done = gap < conv
    }
    RP[i] = theta
  }
  return(RP)
}

#' @rdname simResp-methods
#' @aliases simResp,pool.cluster,numeric-method
setMethod(f = "simResp",
          signature = c("pool.cluster", "list"),
          definition = function(object, theta) {
            if (length(theta) != length(object@np)) {
              DataL = vector(mode = "list", length = object@np)
              for (i in 1:object@np) {
                if (all(!is.na(theta[[i]]))) {
                  DataL[[i]] = simResp(object@pools[[i]], theta[[i]])
                } else {
                  stop(paste0("invalid values in thetaL","[[",i,"]]"))
                }
              }
              return (DataL)
            } else {
              stop("length of thetaL not equal to np")
            }
          }
)

#' +.item.pool
#' 
#' +.item.pool
#' 
#' @param pool1 A nice parameter
#' @param pool2 A nice parameter
#' 
#' @export
"+.item.pool" = function(pool1, pool2) {
  if(class(pool1) != "item.pool" || class(pool2) != "item.pool") stop("operarands must be of class \"item.pool\" ")
  
  if (validObject(pool1) && validObject(pool2)) {
    combined.pool = new("item.pool")
    
    ID = c(pool1@ID, pool2@ID)
    model = c(pool1@model, pool2@model)
    NCAT = c(pool1@NCAT, pool2@NCAT)
    parms = c(pool1@parms, pool2@parms)
    ipar = cbind(pool1@ipar, pool2@ipar)
    SEs = cbind(pool1@ipar, pool2@ipar)
    
    is.unique = which(!duplicated(ID))
    
    combined.pool@ni = length(is.unique)
    combined.pool@maxCat = max(NCAT[is.unique])
    combined.pool@index = 1:combined.pool@ni
    combined.pool@ID = ID[is.unique]
    combined.pool@model = model[is.unique]
    combined.pool@NCAT = NCAT[is.unique]
    combined.pool@parms = parms[is.unique]
    
    if (sum(duplicated(ID)) > 0) {
      warning("duplicate items were found and removed")
      cat("duplicate ID:", ID[duplicated(ID)], "\n")
    }
    
    return(combined.pool)
  } else {
    stop("invlid pool(s) submitted")
  }
}

#' -.item.pool
#' 
#' -.item.pool
#' 
#' @param pool1 A nice parameter
#' @param pool2 A nice parameter
#' 
#' @export
"-.item.pool" = function(pool1, pool2) {
  if(class(pool1) != "item.pool" || class(pool2) != "item.pool") stop("operarands must be of class \"item.pool\" ")
  
  if(any(pool2@ID %in% pool1@ID)) {
    left = which(!(pool1@ID %in% pool2@ID))
    if (length(left) > 0) {
      pool1@ni = length(left)
      pool1@maxCat = max(pool1@NCAT[left])
      pool1@index = 1:length(left)
      pool1@ID = pool1@ID[left]
      pool1@model = pool1@model[left]
      pool1@NCAT = pool1@NCAT[left] 
      pool1@parms = pool1@parms[left]
      pool1@ipar = pool1@ipar[left, ]
      pool1@SEs = pool1@SEs[left, ]
    } else {
      return("item pool is empty")
    }
  } 
  return(pool1)
}

#' ==.item.pool
#' 
#' ==.item.pool
#' 
#' @param pool1 A nice parameter
#' @param pool2 A nice parameter
#' 
#' @export
"==.item.pool" = function(pool1, pool2) {
  if(class(pool1) != "item.pool" || class(pool2) != "item.pool") stop("operarands must be of class \"item.pool\" ")
  
  return(identical(pool1, pool2))
}

#' ==.pool.cluster
#' 
#' ==.pool.cluster
#' 
#' @param pool.cluster1 A nice parameter
#' @param pool.cluster2 A nice parameter
#' 
#' @export
"==.pool.cluster" = function(pool.cluster1, pool.cluster2) {
  if(class(pool.cluster1) != "pool.cluster" || class(pool.cluster2) != "pool.cluster") stop("operarands must be of class \"pool.cluster\" ")
  
  return(identical(pool.cluster1, pool.cluster2))
}

#' Left bracket
#' 
#' @param x x
#' @param i i
#' @param j j
#' @param ... ...
#' @param drop drop
#' 
#' @name [-method
#' @aliases [,test,ANY,ANY,ANY-method
#' @docType methods
setMethod(
  f = "[",
  signature = "test",
  definition = function(x, i, j, ...) {
    if (i == "pool") return(x@pool)
    if (i == "theta") return(x@theta)
    if (i == "Prob") return(x@Prob)
    if (i == "Info") return(x@Info)
    if (i == "trueTheta") return(x@trueTheta)
    if (i == "Data") return(x@Data)
  }
)

#' Left bracket
#' 
#' @name [-method
#' @aliases [,item.pool,ANY,ANY,ANY-method
#' @docType methods
setMethod(
  f = "[",
  signature = "item.pool",
  definition = function(x, i, j, ...) {
    if (i == "ni") return(x@ni)
    if (i == "maxCat") return(x@maxCat)
    if (i == "index") return(x@index)
    if (i == "ID") return(x@ID)
    if (i == "model") return(x@model)
    if (i == "NCAT") return(x@NCAT)
    if (i == "parms") return(x@parms)
    if (i == "ipar") return(x@ipar)
    if (i == "SEs") return(x@SEs)
  }
)

#' subsetPool
#' 
#' subsetPool
#' 
#' @param pool A nice parameter
#' @param select A nice parameter
#' 
#' @export
subsetPool = function(pool, select = NULL) {
  if (class(pool) != "item.pool") {
    stop("test must be of class \"item.pool\"")
  }
  if (is.null(select)) {
    return(pool)
  } else if (all(select %in% 1:pool@ni) && anyDuplicated(select) == 0) {
    n.select = length(select)
    sub.pool = new("item.pool")
    sub.pool@ni = n.select
    sub.pool@maxCat = max(pool@NCAT[select])
    sub.pool@index = 1:n.select
    sub.pool@ID = pool@ID[select]
    sub.pool@model = pool@model[select]
    sub.pool@NCAT = pool@NCAT[select]
    sub.pool@parms = pool@parms[select]
    sub.pool@ipar = pool@ipar[select, ]
    sub.pool@SEs = pool@SEs[select, ]
  } else {
    stop("select contains invalid values")
  }
  return(sub.pool)
}

#' subsetTest
#' 
#' subsetTest
#' 
#' @param test A nice parameter
#' @param select A nice parameter
#' 
#' @export
subsetTest = function(test, select = NULL) {
  if (class(test) != "test") {
    stop("test must be of class \"test\"")
  }
  if (is.null(select)) {
    return(test)
  } else if (all(select %in% 1:test@pool@ni) && anyDuplicated(select) == 0) {
    n.select = length(select)
    sub.test = new("test")
    sub.test@pool = subsetItemPool(test@pool, select = select)
    sub.test@theta = test@theta
    sub.test@Prob = test@Prob[select]
    sub.test@Info = test@Info[, select, drop = FALSE]
    sub.test@trueTheta = test@trueTheta
    sub.test@Data = test@Data[, select, drop = FALSE]
    return(sub.test)
  } else {
    stop("select contains invalid values")
  }
}

#' An S4 Generic
#' 
#' @param object A nice parameter
#' @param theta A nice parameter
#' @param infoType A nice parameter
#' @param trueTheta A nice parameter
#' 
#' @docType methods
#' @rdname MakeTest-methods
#' @export
setGeneric(name = "MakeTest",
           def = function(object, theta, infoType = "FISHER", trueTheta = NULL) {
             standardGeneric("MakeTest")
           }
)

#' MakeTest
#' 
#' MakeTest
#' 
#' @docType methods
#' @rdname MakeTest-methods
#' @export
setMethod(f = "MakeTest",
          signature = "item.pool",
          definition = function(object, theta, infoType = "FISHER", trueTheta = NULL) {
            Prob = calcProb(object, theta)
            if (toupper(infoType) == "FISHER") {
              Info = calcFisher(object, theta)
            } else {
              stop("Invalid infoType specified")
            }
            if (!is.null(trueTheta)) {
              Data = simResp(object, trueTheta)
            } else {
              Data = NULL ##this is provision for cases where data is imported from elsewhere
            }
            return(new("test", pool = object, theta = theta, Prob = Prob, Info = Info, trueTheta = trueTheta, Data = Data))
          }
)

#' An S4 Generic
#' 
#' @param object A nice parameter
#' @param theta A nice parameter
#' @param trueTheta A nice parameter
#' 
#' @docType methods
#' @rdname MakeTestCluster-methods
#' @export
setGeneric(name = "MakeTestCluster",
           def = function(object, theta, trueTheta) {
             standardGeneric("MakeTestCluster")
           }
)

#' MakeTestCluster
#' 
#' MakeTestCluster
#' 
#' @docType methods
#' @rdname MakeTestCluster-methods
#' @export
setMethod(f = "MakeTestCluster",
          signature = c("pool.cluster", "numeric", "numeric"),
          definition = function(object, theta, trueTheta) {
            tests = vector(mode = "list", length = object@np)
            for (p in 1:object@np) {
              tests[[p]] = MakeTest(object@pools[[p]], theta, trueTheta)
            }
            return(new("test.cluster", nt = object@np, names = object@names))
          }
)

#' MakeTestCluster
#' 
#' MakeTestCluster
#' 
#' @docType methods
#' @rdname MakeTestCluster-methods
#' @export
setMethod(f = "MakeTestCluster",
          signature = c("pool.cluster", "numeric", "list"),
          definition = function(object, theta, trueTheta) {
            tests = vector(mode = "list", length = object@np)
            for (p in 1:object@np) {
              tests[[p]] = MakeTest(object@pools[[p]], theta, trueTheta[[p]])
            }
            return(new("test.cluster", nt = object@np, names = object@names))
          }
)

#' An S4 Generic
#' 
#' @param object A nice parameter
#' @param startTheta A nice parameter
#' @param rangeTheta A nice parameter
#' @param maxIter A nice parameter
#' @param crit A nice parameter
#' @param select A nice parameter
#' 
#' @docType methods
#' @rdname MLE-methods
#' @export
# TODO: define methods to score test data using MLE
setGeneric(name = "MLE",
           def = function(object, startTheta = NULL, rangeTheta = c(-4, 4), maxIter = 100, crit = 0.001, select = NULL) {
             standardGeneric("MLE")
           }
)

#' MLE
#' 
#' MLE
#' 
#' @docType methods
#' @rdname MLE-methods
#' @export
setMethod(f = "MLE",
          signature = "test",
          definition = function(object, startTheta = NULL, rangeTheta = c(-4, 4), maxIter = 100, crit = 0.001, select = NULL) {
            ni = ncol(object@Data)
            nj = nrow(object@Data)
            nq = length(object@theta)
            
            if (is.null(select)) {
              select = 1:object@pool@ni
              Resp = object@Data
            } else {
              if (!all(select %in% 1:object@pool@ni)) {
                stop("select contains invalid values")
              }
              Resp = object@Data[, unique(select)]
            }
            
            for (j in 1:nj) {
              theta_0 = 0
              converged = FALSE
              iteration = 0
              while (!converged) {
                iteration = iteration + 1
                
              }
            }
          
        }   
)

#' eap
#' 
#' @param object A nice parameter
#' @param theta A nice parameter
#' @param prior A nice parameter
#' @param resp A nice parameter
#' @param select A nice parameter
#' 
#' @docType methods
#' @rdname eap-methods
#' @export
setGeneric(name = "eap",
           def = function(object, theta, prior, resp, select = NULL) {
             standardGeneric("eap")
           }
)

#' eap
#' 
#' eap
#' 
#' @docType methods
#' @rdname eap-methods
#' @export
setMethod(f = "eap",
          signature = "item.pool",
          definition = function(object, theta, prior, resp, select = NULL) {
            ni = object@ni
            nq = length(theta)
            
            Prob = calcProb(object, theta)
            
            if (is.vector(resp)) {
              nj = 1
            } else if (is.matrix(resp)) {
              nj = nrow(resp)
            } else if (is.data.frame(resp)) {
              nj = nrow(resp)
              resp = as.matrix(resp)
            } else {
              stop("resp must be of class either vector or matrix")
            }
            
            posterior = matrix(rep(prior, nj), nj, nq, byrow = TRUE)
            
            if (length(prior) != nq) {
              stop("theta and prior must be equal in length")
            }
            
            if (!is.null(select)) {
              if (length(resp) != length(select)) {
                stop("resp and select must be equal in length when select is not NULL")
              }
              if (anyDuplicated(select) > 0) {
                warning("select contains duplicated entries")
                select = select[-duplicated(select)]
                response = response[-duplicated(select)]
              }
              if (!all(select %in% 1:ni)) {
                stop("select contains invalid entries")
              }
              items = select
            } else {
              items = 1:ni
            }
            
            if (nj == 1) {
              for (i in 1:length(items)) {
                if (resp[i] >= 0 && resp[i] < object@maxCat) {
                  posterior = posterior * Prob[[items[i]]][, resp[i] + 1]
                }
              }
              TH = sum(posterior * theta) / sum(posterior)
              SE = sqrt(sum(posterior * (theta - TH)^2) / sum(posterior))
            } else {
              for (i in items) {
                response = matrix(resp[, i] + 1, nj, 1)
                if (!all(is.na(response))) {
                  prob = t(Prob[[items[i]]][, response])
                  prob[is.na(prob)] = 1
                  posterior = posterior * prob
                }
              }
              TH = as.vector(posterior %*% theta / rowSums(posterior))
              SE = as.vector(sqrt(rowSums(posterior * (matrix(theta, nj, nq, byrow = TRUE) - matrix(TH, nj, nq))^2) / rowSums(posterior)))
            }
            return (list(TH = TH, SE = SE))
          }
)

#' EAP
#' 
#' @param object A nice parameter
#' @param prior A nice parameter
#' @param select A nice parameter
#' @param resetPrior A nice parameter
#' 
#' @docType methods
#' @rdname eaparray-methods
#' @export
# define methods to score test data using EAP
setGeneric(name = "EAP",
           def = function(object, prior, select = NULL, resetPrior = FALSE) {
             standardGeneric("EAP")
           }
)

#' EAP
#' 
#' EAP
#' 
#' @docType methods
#' @rdname eaparray-methods
#' @export
setMethod(f = "EAP",
          signature = "test",
          definition = function(object, prior, select = NULL, resetPrior = FALSE) {
            nj = nrow(object@Data)
            if (is.matrix(prior)) {
              nq = ncol(prior)
              if (nj != nrow(prior)) stop("nrow(prior) is not equal to nrow(Data)")
              posterior = prior
            } else {
              nq = length(prior)
              posterior = matrix(rep(prior, nj), nj, nq, byrow = TRUE)
            }
            if (is.null(select)) {
              select = 1:object@pool@ni
            } else {
              if (!all(select %in% 1:object@pool@ni)) {
                stop("select contains invalid values")
              }
            }
            for (i in unique(select)) {
              resp = matrix(object@Data[, i] + 1, nj, 1)
              if (!all(is.na(resp))) {
                prob = t(object@Prob[[i]][, resp])
                prob[is.na(prob)] = 1
                posterior = posterior * prob
              }
            }
            TH = as.vector(posterior %*% object@theta / rowSums(posterior))
            SE = as.vector(sqrt(rowSums(posterior * (matrix(object@theta, nj, nq, byrow = TRUE) - matrix(TH, nj, nq))^2) / rowSums(posterior)))
            if (is.null(object@trueTheta)) {
              RMSE = NULL
            } else {
              RMSE = sqrt(mean((TH - object@trueTheta)^2))
            }
            return(list(TH = TH, SE = SE, prior = prior, posterior = posterior, RMSE = RMSE))
          }
)

#' EAP
#' 
#' EAP
#' 
#' @docType methods
#' @rdname eaparray-methods
#' @export
setMethod(f = "EAP",
          signature = "test.cluster",
          definition = function(object, prior, select = NULL, resetPrior = FALSE) {
            EAP.cluster = vector(mode = "list", length = object@nt)
            EAP.cluster[[1]] = EAP(object@tests[[1]], prior, select)
            if (resetPrior) {
              for (t in 2:object@nt) {
                EAP.cluster[[t]] = EAP(object@tests[[t]], prior, select)
              }
            } else {
              for (t in 2:object@nt) {
                EAP.cluster[[t]] = EAP(object@tests[[t]], EAP.cluster[[t - 1]]@posterior, select)
              }
            }
            return(EAP.cluster)
          }
)

#' subsetItemPool
#' 
#' subsetItemPool
#' 
#' @param pool A nice parameter
#' @param select A nice parameter
#' 
#' @export
subsetItemPool = function(pool, select = NULL) {
  if (class(pool) != "item.pool") {
    stop("pool must be of class \"item.pool\"")
  }
  if (is.null(select)) {
    return(pool)
  } else if (all(select %in% 1:pool@ni)) {
    select = unique(select)
    n.select = length(select)
    sub.pool = new("item.pool")
    sub.pool@ni = n.select
    sub.pool@index = 1:n.select
    sub.pool@ID = pool@ID[select]
    sub.pool@model = pool@model[select]
    sub.pool@NCAT = pool@NCAT[select]
    sub.pool@parms = pool@parms[select]
    sub.pool@maxCat = max(sub.pool@NCAT)
    if (pool@SEs != 0) {
      sub.pool@SEs = pool@SEs[select]
    }
    return(sub.pool)
  } else {
    stop("select contains invalid values")
  }
}

#' MakeItemPoolCluster
#' 
#' MakeItemPoolCluster
#' 
#' @param pools A nice parameter
#' @param names A nice parameter
#' 
#' @export
MakeItemPoolCluster = function(pools, names = NULL) {
  np = length(pools)
  if (np == 0) {
    stop("pools is empty")
  } else if (np == 1) {
    stop("only one pool found in pools - expecting 2 or more")
  }
  
  if (is.null(names)) {
    names = paste0("Pool_", 1:np)
  } else {
    if (length(names) != np) stop("pools and names are of different lengths")
  }
  
  PoolCluster = new("pool.cluster")
  
  PoolCluster@np = np
  PoolCluster@pools = vector(mode = "list", length = np)
  PoolCluster@names = names
  
  for (i in 1:np) {
    if (class(pools[[i]]) != "item.pool") stop(paste0("pool.list[[", i, "]] is not of class \"item.pool\""))
    PoolCluster@pools[[i]] = pools[[i]]
  }
  
  if (validObject(PoolCluster)) return(PoolCluster)
}


#' Shadow
#' 
#' @param object An \code{\linkS4class{item.pool}} object. Use \code{\link[IRTclass]{LoadItemPool}} for this.
#' @param config A \code{\linkS4class{Shadow.config}} object.
#' @param trueTheta Numeric. A vector of true theta values to be used in simulation.
#' @param Constraints A list representing optimization constraints. Use \code{\link{LoadConstraints}} for this.
#' @param prior Numeric. A matrix or a vector containing priors.
#' @param priorPar Numeric. A vector of parameters for prior distribution.
#' @param Data Numeric. A matrix containing item response data.
#' @param session Used to communicate with a Shiny session.
#' 
#' @rdname Shadow-methods
#' @export
setGeneric(name = "Shadow",
           def = function(object, config, trueTheta = NULL, Constraints = NULL, prior = NULL, priorPar = NULL, Data = NULL, session = NULL) {
             standardGeneric("Shadow")
           }
)

#' Shadow
#' 
#' Shadow
#' 
#' @rdname Shadow-methods
#' @export
setMethod(f = "Shadow",
          signature = "item.pool",
          definition = function(object, config, trueTheta, Constraints, prior, priorPar, Data, session) {
            if (!validObject(config)) {
              stop("invalid configuration options specified")
            }
            
            if (!is.null(Constraints)) {
              ni = Constraints$ni            # number of items
              ns = Constraints$ns            # number of stimuli (if Constraints$setBased = TRUE, 0 otherwise)
              nv = Constraints$nv            # number of decision variables
            } else {
              ni = object@ni
            }
            
            model = object@model
            model[which(model == "item.1pl")] = 1
            model[which(model == "item.2pl")] = 2
            model[which(model == "item.3pl")] = 3
            model[which(model == "item.pc")]  = 4
            model[which(model == "item.gpc")] = 5
            model[which(model == "item.gr")]  = 6
            model = as.numeric(model)
            
            if (!is.null(trueTheta)) {
              nj = length(trueTheta)       # number of simulees
            } else if (!is.null(Data)) {
              nj = nrow(Data)
            } else {
              stop("either trueTheta or Data should be provided at a minimum")
            }
            
            nq = length(config@thetaGrid)  # number of theta quadrature points
            
            minTheta = min(config@thetaGrid)
            maxTheta = max(config@thetaGrid)
            
            exposureControl = toupper(config@exposureControl$method)
            refreshPolicy = toupper(config@refreshPolicy$method)
            
            if (toupper(config@contentBalancing$method %in% c("STA", "SHADOW", "SHADOWTEST", "SHADOW TEST"))) {
              if (is.null(Constraints)) {
                stop("Constraints must not be NULL for STA")
              } else {
                sta = TRUE
                setBased = Constraints$setBased
                testLength = Constraints$testLength
                minNI = Constraints$testLength
                maxNI = Constraints$testLength
                
                refreshShadow = rep(FALSE, testLength)
                
                if (refreshPolicy %in% c("ALWAYS", "THRESHOLD")) {
                  refreshShadow = rep(TRUE, testLength)
                } else if (refreshPolicy == "POSITION") {
                  if (all(config@refreshPolicy$position %in% 1:testLength)) {
                    refreshShadow[config@refreshPolicy$position] = TRUE
                  } else {
                    stop("invalid entries in config@refreshPolicy$position")
                  }
                } else if (refreshPolicy %in% c("INTERNVAL", "INTERVAL-THRESHOLD")) {
                  if (config@refreshPolicy$interval >= 1 && config@refreshPolicy$interval <= testLength) {
                    refreshShadow[seq(1, testLength, config@refreshPolicy$interval)] = TRUE
                  } else {
                    stop("invalid entry in config@refreshPolicy$interval")
                  }
                } else if (refreshPolicy %in% c("STIMULUS", "SET", "PASSAGE")) {
                  #refresh only at the beginning of each new stimulus
                  if (!setBased) {
                    stop("setBased must be TRUE when config@refreshPolicy$method equals \"STIMULUS\"")
                  } 
                }
                refreshShadow[1] = TRUE
              }
            } else {
              sta = FALSE
              setBased = FALSE
              minNI = config@stoppingCriterion$minNI
              maxNI = config@stoppingCriterion$maxNI
              maxSE = config@stoppingCriterion$SeThreshold
            } 
            
            if (!is.null(Data)) {
              #Test = MakeTest(object, config@thetaGrid, infoType = config@itemSelection$infoType, trueTheta = NULL)
              Test = MakeTest(object, config@thetaGrid, infoType = "FISHER", trueTheta = NULL)
              Test@Data = as.matrix(Data)
            } else if (!is.null(trueTheta)) {
              #Test = MakeTest(object, config@thetaGrid, infoType = config@itemSelection$infoType, trueTheta)
              Test = MakeTest(object, config@thetaGrid, infoType = "FISHER", trueTheta)
            } else {
              stop("both Data and trueTheta cannot be NULL")
            }
            
            maxInfo = max(Test@Info)
            
            if (is.null(prior)) {
              #if no priors specified
              if (!is.null(priorPar)) {
                if (is.vector(priorPar) && length(priorPar) == 2) {
                  #if prior is ~N and defined as c(mu, sigma)
                  posterior = matrix(dnorm(config@thetaGrid, mean = priorPar[1], sd = priorPar[2]), nj, nq, byrow = TRUE)
                } else if (is.matrix(priorPar) && all(dim(priorPar) == c(nj, 2))) {
                  #if prior is ~N and defined as c(mu, sigma) for each examinee separately
                  posterior = matrix(NA, nj, nq) 
                  for (j in 1:nj) {
                    posterior[j, ] = dnorm(config@thetaGrid, mean = priorPar[j, 1], sd = priorPar[j, 2])
                  }
                } else {
                  stop("priorPar must be a vector of length 2, c(mean, sd), or a matrix of dim c(nj x 2)")
                }
              } else if (toupper(config@interimTheta$priorDist) == "NORMAL") {
                posterior = matrix(dnorm(config@thetaGrid, mean = config@interimTheta$priorPar[1], sd = config@interimTheta$priorPar[2]), nj, nq, byrow = TRUE)
              } else if (toupper(config@interimTheta$priorDist) == "UNIFORM") {
                posterior = matrix(1, nj, nq)
              } else stop("invalid configuration option for interimTheta$priorDist")
            } else if (is.vector(prior) && length(prior) == nq) {
              posterior = matrix(prior, nj, nq, byrow = TRUE)
            } else if (is.matrix(prior) && all(dim(prior) == c(nj, nq))) {
              posterior = prior
            } else {
              stop("misspecification for prior or priorPar")
            }
            
            if (toupper(config@interimTheta$method) %in% c("EB", "FB")) {
              nSample = config@MCMC$burnIn + config@MCMC$postBurnIn
              if (toupper(config@interimTheta$method) == "FB") {
                #this was insde the for loop over j; moved to here for speed
                #iparList = iparPosteriorSample_foreach(object, nSample = nSample) #nSample = burnIn + postBurnIn
                #the following is a serial version, whereas the above runs parallel
                iparList = iparPosteriorSample(object, nSample = nSample) #nSample = burnIn + postBurnIn
              }
            }
            
            if (!is.null(config@itemSelection$initialTheta)) {
              initialTheta = rep(config@itemSelection$initialTheta, nj)
            } else {
              initialTheta = as.vector(posterior %*% matrix(config@thetaGrid, ncol = 1))
            }
            
            itemsAdministered = matrix(FALSE, nj, ni)
            outputList = vector(mode = "list", length = nj)
            
            if (exposureControl %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
              itemEligibilityControl = TRUE
              maxExposureRate = config@exposureControl$maxExposureRate
              fadingFactor = config@exposureControl$fadingFactor
              accelerationFactor = config@exposureControl$accelerationFactor
              nSegment = config@exposureControl$nSegment
              if (!length(maxExposureRate) %in% c(1, nSegment)) {
                stop("length(maxExposureRate) must be 1 or nSegment")
              }
              trueSegmentFreq = numeric(nSegment)
              estSegmentFreq = numeric(nSegment)
              trueSegmentCount = numeric(nj)
              estSegmentCount = numeric(nj)
              
              segmentCut = config@exposureControl$segmentCut
              cutLower = segmentCut[1:nSegment]
              cutUpper = segmentCut[2:(nSegment + 1)]
              
              pe_i = matrix(1, nSegment, ni) #initializing eligibility probabilities for items by segment
              
              if (setBased) {
                pe_s = matrix(1, nSegment, ns) #initializing eligibility probabilities for stimuli by segment
              } else {
                pe_s = NULL
                alpha_sjk = NULL
                rho_sjk = NULL
              }
              
              if (config@exposureControl$diagnosticStats) {
                alpha_g_i = matrix(0, nrow = nj, ncol = nSegment * ni)
                epsilon_g_i = matrix(0, nrow = nj, ncol = nSegment * ni)
                
                if (setBased) {
                  alpha_g_s = matrix(0, nrow = nj, ncol = nSegment * ns)
                  epsilon_g_s = matrix(0, nrow = nj, ncol = nSegment * ns)
                }
                
                if (fadingFactor != 1) {
                  noFading_alpha_g_i = matrix(0, nrow = nj, ncol = nSegment * ni)
                  noFading_epsilon_g_i = matrix(0, nrow = nj, ncol = nSegment * ni)
                  
                  if (setBased) {
                    noFading_alpha_g_s = matrix(0, nrow = nj, ncol = nSegment * ns)
                    noFading_epsilon_g_s = matrix(0, nrow = nj, ncol = nSegment * ns)
                  }
                }
              }
              
              if (!is.null(config@exposureControl$initialEligibilityStats)) {
                #if initial usage stats are available
                n_jk = config@exposureControl$initialEligibilityStats$n_jk #number of test takers through j who visited item pool k
                alpha_ijk = config@exposureControl$initialEligibilityStats$alpha_ijk #number of test takers through j who took item i while visiting item pool k
                phi_jk = config@exposureControl$initialEligibilityStats$phi_jk #number of test takers through j who visited item pool k when the shadow test was feasible
                rho_ijk = config@exposureControl$initialEligibilityStats$rho_ijk #number of test takers through j who visited item pool k when item i was eligible or the test was infeasible
                if (setBased) {
                  alpha_sjk = config@exposureControl$initialEligibilityStats$alpha_sjk #number of test takers through j who visited item pool k and took stimulus s
                  rho_sjk = config@exposureControl$initialEligibilityStats$rho_sjk #number of test takers through j who visited item pool k when stimulus s was eligible or the test was infeasible
                } 
              } else {
                #if no initial usage stats are available
                n_jk = numeric(nSegment) #number of test takers through j who visited item pool k
                alpha_ijk = matrix(0, nSegment, ni) #number of test takers through j who took item i while visiting item pool k
                phi_jk = numeric(nSegment) #number of test takers through j who visited item pool k when the shadow test was feasible
                rho_ijk = matrix(0, nSegment, ni) #number of test takers through j who visited item pool k when item i was eligible or the test was infeasible
                if (setBased) {
                  alpha_sjk = matrix(0, nSegment, ns) #number of test takers through j who visited item pool k and took stimulus s
                  rho_sjk = matrix(0, nSegment, ns) #number of test takers through j who visited item pool k when stimulus s was eligible or the test was infeasible
                }
              }
              if (fadingFactor != 1) {
                noFading_n_jk = n_jk
                noFading_alpha_ijk = alpha_ijk
                noFading_rho_ijk = rho_ijk
                if (setBased) {
                  noFading_alpha_sjk = alpha_sjk
                  noFading_rho_sjk = rho_sjk
                }
              }
            } else {
              itemEligibilityControl = FALSE
              trueSegmentCount = NULL
              estSegmentCount = NULL
            } 
            
            #select all items to optimize fixedTheta
            if (!is.null(config@itemSelection$fixedTheta)) {
              if (length(config@itemSelection$fixedTheta) == 1) {
                infoFixedTheta = vector(mode = "list", length = nj)
                infoFixedTheta[1:nj] = Test@Info[which.min(abs(Test@theta - config@itemSelection$fixedTheta)), ]
                config@itemSelection$fixedTheta = rep(config@itemSelection$fixedTheta, nj)
                selectAtFixedTheta = TRUE
              } else if (length(config@itemSelection$fixedTheta) == nj) {
                infoFixedTheta = lapply(seq_len(nj), function(j) calc_info(config@itemSelection$fixedTheta[j], object@ipar, object@NCAT, model))
                selectAtFixedTheta = TRUE
              } else {
                stop("length of config@itemSelection$fixedTheta must be either 1 or nj")
              }
            } else {
              selectAtFixedTheta = FALSE
            }
            
            if (setBased) {
              usageMatrix = matrix(FALSE, nrow = nj, ncol = nv)
            } else {
              usageMatrix = matrix(FALSE, nrow = nj, ncol = ni)
            }
            
            getInfo = function() {
              if (selectAtFixedTheta) {
                info = infoFixedTheta[[j]] #* itemsAvailable[j, ]
              } else if (config@itemSelection$method == "MPWI") { 
                info = as.vector(matrix(posterior[j, ], nrow = 1) %*% Test@Info)
              } else if (config@itemSelection$method == "MFI") {
                info = calc_info(currentTheta, object@ipar, object@NCAT, model)
              } else if (config@itemSelection$method == "EB") {
                info = calc_info_EB(output@posteriorSample, object@ipar, object@NCAT, model)  
              } else if (config@itemSelection$method == "FB") {
                if (config@itemSelection$infoType == "FISHER") {
                  info = calc_info_FB(output@posteriorSample, iparList, object@NCAT, model)
                } else if (config@itemSelection$infoType %in% c("MI", "MUTUAL")) {
                  info = calc_MI_FB(output@posteriorSample, iparList, object@NCAT, model)
                }
              } else {
                stop("invalid option for config@itemSelection$method")
              }
              
              return(info)
            }
            
            selectItem = function() {
              #for non-sta
              if (position > 1) {
                info[output@administeredItemIndex[1:(position - 1)]] = -1 #previously administered items will have a value of -1
              }
              infoIndex = order(info, decreasing = TRUE)
              itemSelected = infoIndex[1]
              
              if (itemSelected %in% output@administeredItemIndex[1:(position - 1)]) {
                stop(sprintf("the selected item %i has been already administered", itemSelected))
              }
              return(itemSelected)
            }
            
            selectItemShadowTest = function() {
              nRemaining = testLength - position
              newStimulusSelected = FALSE
              lastStimulusIndex = 0
              
              if (!setBased) {
                stimulusSelected = NA
                stimulusFinished = FALSE
              }
              if (position == 1) {
                #items in shadowTest are already sorted 
                selected = 1
                if (setBased) {
                  stimulusSelected = optimal$shadowTest$STINDEX[1]
                  newStimulusSelected = TRUE
                  if (sum(optimal$shadowTest$STINDEX == stimulusSelected) == 1) {
                    stimulusFinished = TRUE
                  } else {
                    stimulusFinished = FALSE
                  }
                }
              } else {
                #posotion > 1
                #items in the shadow test which have not been administered previously
                remaining = which(!optimal$shadowTest$INDEX %in% output@administeredItemIndex[1:(position - 1)]) #is this safe? assumes subsetting won't mess up the order
                
                if (!setBased) {
                  selected = remaining[1]
                } else {
                  #if setBased
                  #are there any remaining items associated with the unfinished stimulus 
                  lastStimulusIndex = output@administeredStimulusIndex[position - 1]
                  if (any(optimal$shadowTest$STINDEX[remaining] == lastStimulusIndex)) {
                    #if the stimulus administered in the last stage has one or more items in the shadow test (endSet == FALSE)
                    remainingInStimulus = remaining[which(optimal$shadowTest$STINDEX[remaining] == lastStimulusIndex)]
                    selected = remainingInStimulus[1]
                  } else {
                    selected = remaining[1]
                  }
                  
                  stimulusSelected = optimal$shadowTest$STINDEX[selected]
                  
                  if (lastStimulusIndex != stimulusSelected) {
                    newStimulusSelected = TRUE
                  }
                  
                  if (nRemaining == 0) {
                    stimulusFinished = TRUE
                  } else {
                    #nRemaining > 0
                    if (sum(optimal$shadowTest$STINDEX[remaining] == stimulusSelected) == 1) {
                      stimulusFinished = TRUE
                      #selected is the last item associated with the stimulus
                    } else {
                      stimulusFinished = FALSE
                    }
                  }
                }
              }
              itemSelected = optimal$shadowTest$INDEX[selected]
              return(list(itemSelected = itemSelected, stimulusSelected = stimulusSelected, stimulusFinished = stimulusFinished, lastStimulusIndex = lastStimulusIndex, newStimulusSelected = newStimulusSelected, nRemaining = nRemaining))
            }
            
            plotAuditTrail = function() {
              par(mfrow = c(2, 1)) 
              plot(1:maxNI, seq(minTheta, maxTheta, length = maxNI), main = paste0("Examinee ", j), xlab = "Items Administered", ylab = "Theta", type = "n", las = 1)
              points(1:maxNI, output@interimThetaEst, type="b", pch = 9, col = "blue")
              if (!is.null(trueTheta)) {
                abline(h = output@trueTheta, lty = 2, col = "red")
              } else {
                abline(h = output@finalThetaEst, lty = 2, col = "red")
              }
              
              item.string = paste(output@administeredItemIndex, collapse = ",")
              text(1, maxTheta, paste0("Items: ", item.string), cex = 0.7, adj = 0)
              text(1, minTheta + 0.3, paste("Theta: ", round(output@finalThetaEst, digits = 2)," SE: ", round(output@finalSeEst, digits = 2)), cex = 0.8, adj = 0)
              for (i in 1:maxNI) {
                lines(rep(i ,2), c(output@interimThetaEst[i] - 1.96 * output@interimSeEst[i], output@interimThetaEst[i] + 1.96 * output@interimSeEst[i]))
                if (sta) {
                  if (output@shadowTestRefreshed[i]) {
                    #text(i, minTheta, "S", col = "red")
                    points(i, output@interimThetaEst[i],  pch = 18, col = "red")
                  }
                }
              }
              resp.string = paste(output@administeredItemResp, collapse = ",")
              plot(config@thetaGrid, output@posterior, main = "Final Posterior Distribution", xlab = "Theta", ylab = "Posterior", type = "l", col = "blue", yaxt = "n")
              text(minTheta, max(output@posterior), paste0("Responses: ", resp.string), cex = 0.7, adj = 0)
            }
            
            pb = txtProgressBar(0, nj, char = "|", style = 3)
            ###########################################################################################
            ############## FOR LOOP OVER SIMULEES #####################################################
            ###########################################################################################
            
            for (j in 1:nj) {
              output = new("Shadow.output")
              output@simuleeIndex = j
              
              if (!is.null(trueTheta)) {
                output@trueTheta = trueTheta[j]
              } else {
                output@trueTheta = NULL
              }
              
              output@prior = posterior[j, ]
              output@administeredItemIndex = numeric(maxNI)
              output@administeredItemResp = numeric(maxNI)
              output@thetaSegmentIndex = numeric(maxNI)
              output@interimThetaEst = numeric(maxNI)
              output@interimSeEst = numeric(maxNI)
              output@administeredStimulusIndex = NaN
              output@shadowTest = vector(mode = "list", length = maxNI)
              
              if (config@interimTheta$method %in% c("EAP")) {
                currentTheta = initialTheta[j]
              } else if (toupper(config@interimTheta$method) %in% c("EB", "FB")) {
                #list of length ni, each element is a matrix of ipar for one item sampled
                if (is.vector(priorPar) && length(priorPar) == 2) {
                  output@priorPar = priorPar
                } else if (is.matrix(priorPar) && all(dim(priorPar) == c(nj, 2))) {
                  output@priorPar = priorPar[j, ]
                } else {
                  output@priorPar = config@interimTheta$priorPar
                }
                
                output@posteriorSample = rnorm(nSample, mean = output@priorPar[1], sd = output@priorPar[2])
                output@posteriorSample = output@posteriorSample[seq(from = config@MCMC$burnIn + 1, to = nSample, by = config@MCMC$thin)]
                currentTheta = mean(output@posteriorSample)
                currentSE = sd(output@posteriorSample) * config@MCMC$jumpFactor
              }
              
              if (setBased) {
                output@administeredStimulusIndex = numeric(maxNI)
                endSet = TRUE
                finishedStimulusIndex = NULL
                finishedStimulusItemCount = NULL
              }
              
              if (sta) {
                output@shadowTestFeasible = logical(testLength) 
                output@shadowTestRefreshed = logical(testLength)
                imat = NULL
                idir = NULL
                irhs = NULL
                if (setBased) {
                  smat = NULL
                  sdir = NULL
                  srhs = NULL
                }
              }
              
              likelihood = rep(1, nq)
              thetaChange = 10000
              done = FALSE
              position = 0
              
              if (exposureControl %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
                #run probability experiment to determine eligibility for Examinee j
                ineligible_i = matrix(0, nSegment, ni) # 0 = eligible
                prob_random = matrix(runif(nSegment * ni), nSegment, ni)
                ineligible_i[prob_random >= pe_i] = 1 # 1 = ineligible
                
                if (setBased) {
                  ineligible_s = matrix(0, nSegment, ns)
                  prob_random = matrix(runif(nSegment * ns), nSegment, ns)
                  ineligible_s[prob_random >= pe_s] = 1 # 1 = ineligible
                  
                  for (k in 1:nSegment) {
                    for (s in which(ineligible_s[k, ] == 1)) {
                      #make sure all items associated with ineligible stimuli are also ineligible
                      ineligible_i[k, Constraints$ItemIndexByStimulus[[s]]] = 1
                    }
                    for (s in which(ineligible_s[k, ] == 0)) {
                      #make sure all items associated with eligible stimuli are also eligible
                      ineligible_i[k, Constraints$ItemIndexByStimulus[[s]]] = 0
                    }
                  }
                }
                
                if (exposureControl %in% c("ELIGIBILITY")) {
                  xmat = NULL
                  xdir = NULL
                  xrhs = NULL
                }
              }
              
              while (!done) {
                position = position + 1
                info = getInfo()
                
                if (sta) {
                  if (exposureControl %in% c("ELIGIBILITY", "BIGM")) {
                    if (!is.null(config@exposureControl$firstSegment) && length(config@exposureControl$firstSegment) >= position && all(config@exposureControl$firstSegment >= 1) && all(config@exposureControl$firstSegment <= nSegment)) {
                      output@thetaSegmentIndex[position] = config@exposureControl$firstSegment[position]
                    } else {
                      output@thetaSegmentIndex[position] = findSegment(segmentCut, currentTheta)
                    }
                  } else if (exposureControl %in% c("BIGM-BAYESIAN")) {
                    sampleSegment = findSegment(segmentCut, output@posteriorSample)
                    segmentDistribution = table(sampleSegment) / length(sampleSegment) #proportion of segments classified
                    segmentClassified = as.numeric(names(segmentDistribution)) #segments classified (not necessarily visited)
                    segmentProb = numeric(nSegment) #initialized to 0
                    segmentProb[segmentClassified] = segmentDistribution
                    #output@thetaSegmentIndex[position] = findSegment(segmentCut, output@posteriorSample[length(output@posteriorSample)])
                    output@thetaSegmentIndex[position] = which.max(segmentProb)
                  }
                  
                  if (position == 1 ||
                      (refreshPolicy == "ALWAYS") ||
                      (refreshPolicy %in% c("POSITION", "INTERVAL") && refreshShadow[position]) || 
                      (refreshPolicy == "THRESHOLD" && abs(thetaChange) > config@refreshPolicy$threshold) ||
                      (refreshPolicy == "INTERVAL-THRESHOLD" && refreshShadow[position] && abs(thetaChange) > config@refreshPolicy$threshold) || 
                      (refreshPolicy %in% c("STIMULUS", "SET", "PASSAGE") && setBased && endSet)) {
                    
                    output@shadowTestRefreshed[position] = TRUE
                    
                    if (position > 1) {
                      imat = matrix(0, nrow = position - 1, ncol = nv)
                      #imat[cbind(1:(position - 1), output@administeredItemIndex[1:(position - 1)])] = 1
                      for (p in 1:(position - 1)) {
                        imat[p, output@administeredItemIndex[p]] = 1
                      }
                      idir = rep("==", position - 1)
                      irhs = rep(1, position - 1)
                      
                      if (setBased) {
                        if (sum(output@administeredStimulusIndex[1:(position - 1)] > 0) > 0) {
                          #output@administeredStimulusIndex initialized to 0
                          administeredStimulusIndex = unique(output@administeredStimulusIndex[1:(position - 1)])
                          administeredStimulusIndex = administeredStimulusIndex[administeredStimulusIndex > 0]
                          smat = matrix(0, nrow = length(administeredStimulusIndex), ncol = nv)
                          for (s in 1:length(administeredStimulusIndex)) {
                            smat[s, ni + administeredStimulusIndex[s]] = 1
                          }
                          sdir = rep("==", length(administeredStimulusIndex))
                          srhs = rep(1, length(administeredStimulusIndex))
                          
                          imat = rbind(imat, smat)
                          idir = c(idir, sdir)
                          irhs = c(irhs, srhs)
                          
                          #make sure no additional items will be selected associated with previously administered stimuli upon moving on to a different stimulus
                          if (refreshPolicy %in% c("STIMULUS", "SET", "PASSAGE") && setBased && endSet) {
                            nAdministeredStimulus = length(administeredStimulusIndex)
                            if (nAdministeredStimulus > 0) {
                              smat = matrix(0, nrow = nAdministeredStimulus, ncol = nv)
                              sdir = rep("==", nAdministeredStimulus)
                              srhs = numeric(nAdministeredStimulus)
                              for (s in 1:nAdministeredStimulus) {
                                smat[s, Constraints$ItemIndexByStimulus[[administeredStimulusIndex[s]]]] = 1
                                srhs[s] = sum(output@administeredStimulusIndex[1:(position - 1)] == administeredStimulusIndex[s])
                              }
                              imat = rbind(imat, smat)
                              idir = c(idir, sdir)
                              irhs = c(irhs, srhs)
                            }
                          } else {
                            nFinishedStimulus = length(finishedStimulusIndex)
                            if (nFinishedStimulus > 0) {
                              smat = matrix(0, nrow = nFinishedStimulus, ncol = nv)
                              sdir = rep("==", nFinishedStimulus)
                              srhs = finishedStimulusItemCount
                              for (s in 1:nFinishedStimulus) {
                                smat[s, Constraints$ItemIndexByStimulus[[finishedStimulusIndex[s]]]] = 1 
                              }
                              imat = rbind(imat, smat)
                              idir = c(idir, sdir)
                              irhs = c(irhs, srhs)
                            }
                          }
                        }
                      }
                    } # if (position > 1)
                    
                    if (itemEligibilityControl) {
                      itemIneligible = ineligible_i[output@thetaSegmentIndex[position], ] #1 = ineligible
                      
                      if (setBased) {
                        stimulusIneligible = ineligible_s[output@thetaSegmentIndex[position], ]
                      }
                      
                      if (position > 1) {
                        itemIneligible[output@administeredItemIndex[1:(position - 1)]] = 0 #make all previously administered items eligible in the current segment temporarily
                        
                        if (setBased) {
                          stimulusIneligible[unique(output@administeredStimulusIndex[1:(position - 1)])] = 0
                        }
                      }
                      
                      if (exposureControl %in% c("ELIGIBILITY")) {
                        if (any(itemIneligible == 1)) {
                          xmat = numeric(nv)
                          xmat[1:ni] = itemIneligible
                          xdir = "=="
                          xrhs = 0
                          
                          if (setBased) {
                            if (any(stimulusIneligible == 1)) {
                              xmat[(ni + 1):nv] = stimulusIneligible
                              #if stimulus s is ineligible make sure all items that belong to the stimulus also ineligible
                              for (s in which(stimulusIneligible == 1)) {
                                xmat[Constraints$ItemIndexByStimulus[[s]]] = 1
                              }
                              #if stimulus s is eligible make sure all items that belong to the stimulus also eligible
                              for (s in which(stimulusIneligible == 0)) {
                                xmat[Constraints$ItemIndexByStimulus[[s]]] = 0
                              }
                            }
                          }
                        }
                        
                        optimal = STA(Constraints, info, xmat = rbind(xmat, imat), xdir = c(xdir, idir), xrhs = c(xrhs, irhs), maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$timeLimit, gap_limit = config@MIP$gapLimit, solver = config@MIP$solver)
                        
                        #nrow(optimal$shadowTest) can be less than testLength when names(optimal$status) == "PREP_OPTIMAL_SOLUTION_FOUND" - this causes the program to stop
                        
                        if (toupper(config@MIP$solver) == "SYMPHONY"){
                          is_optimal = names(optimal$status) %in% c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND")
                        }
                        if (toupper(config@MIP$solver) == "GUROBI"){
                          is_optimal = optimal$status %in% c("OPTIMAL")
                        }
                        if (toupper(config@MIP$solver) == "LPSOLVE"){
                          is_optimal = optimal$status == 0
                        }
                        
                        if (!is_optimal){
                          #if (optimal$status != 0) {
                          #if infeasible - "PREP_NO_SOLUTION"
                          output@shadowTestFeasible[position] = FALSE #superflous; initialized to FALSE
                          #remove all ineligibility constraints
                          optimal = STA(Constraints, info, xmat = imat, xdir = idir, xrhs = irhs, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$timeLimit, gap_limit = config@MIP$gapLimit, solver = config@MIP$solver)
                          #optimal = STA(Constraints, info, xmat = NULL, xdir = NULL, xrhs = NULL, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$timeLimit, gap_limit = config@MIP$gapLimit)
                        } else {
                          output@shadowTestFeasible[position] = TRUE
                        }
                        
                        #end exposureControl == "ELIGIBILITY"
                      } else if (exposureControl %in% c("BIGM", "BIGM-BAYESIAN")){
                        if (!is.null(config@exposureControl$M)) {
                          info[itemIneligible == 1] = info[itemIneligible == 1] - config@exposureControl$M
                        } else {
                          info[itemIneligible == 1] = -1 * maxInfo - 1
                        }
                        
                        optimal = STA(Constraints, info, xmat = imat, xdir = idir, xrhs = irhs, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$timeLimit, gap_limit = config@MIP$gapLimit, solver = config@MIP$solver)
                        output@shadowTestFeasible[position] = TRUE
                      } 
                      #end itemIneligibilityControl
                    } else {
                      #if not itemEligibilityControl
                      optimal = STA(Constraints, info, xmat = imat, xdir = idir, xrhs = irhs, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$timeLimit, gap_limit = config@MIP$gapLimit, solver = config@MIP$solver)
                      output@shadowTestFeasible[position] = TRUE #safe assumption
                    }
                    
                    if (toupper(config@MIP$solver) == "SYMPHONY"){
                      is_optimal = names(optimal$status) %in% c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND")
                    }
                    if (toupper(config@MIP$solver) == "GUROBI"){
                      is_optimal = optimal$status %in% c("OPTIMAL")
                    }
                    if (toupper(config@MIP$solver) == "LPSOLVE"){
                      is_optimal = optimal$status == 0
                    }
                    
                    if (!is_optimal) {
                      #if (optimal$status != 0) {
                      stop(sprintf("MIP returned non-zero status: Examinee %i at position %i", j, position))
                    }
                    
                    output@solveTime[position] = optimal$solve.time
                    #end refreshShadow[position]
                  } else {
                    #if shadowTest not refreshed
                    output@shadowTestRefreshed[position] = FALSE #superflous; already intializaed to FALSE
                    output@shadowTestFeasible[position] = TRUE
                  }
                  #select item here
                  #shadowTest is already sorted - descending order of info after any ordering specified by 
                  selection = selectItemShadowTest()
                  output@administeredItemIndex[position] = selection$itemSelected
                  output@shadowTest[[position]] = optimal$shadowTest$INDEX
                } else {
                  #if not sta
                  output@administeredItemIndex[position] = selectItem()
                }
                
                if (setBased) {
                  output@administeredStimulusIndex[position] = selection$stimulusSelected
                  #are there any remaining items associated with the current stimulus
                  #check if the next record in shadowTest has the same STID
                  
                  if (selection$stimulusFinished) {
                    endSet = TRUE
                    #make sure no additional items from the stimulus will be selected
                  } else {
                    endSet = FALSE
                  }
                  
                  if (selection$newStimulusSelected && selection$lastStimulusIndex != 0) {
                    finishedStimulusIndex = c(finishedStimulusIndex, selection$lastStimulusIndex) 
                    finishedStimulusItemCount = c(finishedStimulusItemCount, sum(output@administeredStimulusIndex[1:(position - 1)] == selection$lastStimulusIndex))
                  }
                }
                
                output@administeredItemResp[position] = Test@Data[j, output@administeredItemIndex[position]]
                itemsAdministered[j, output@administeredItemIndex[position]] = TRUE
                
                probResp = Test@Prob[[output@administeredItemIndex[position]]][, output@administeredItemResp[position] + 1]
                posterior[j, ] = posterior[j, ] * probResp
                likelihood = likelihood * probResp
                
                if (toupper(config@interimTheta$method) == "EAP") {
                  output@interimThetaEst[position] = sum(posterior[j, ] * Test@theta) / sum(posterior[j, ])
                  output@interimSeEst[position] = sqrt(sum(posterior[j, ] * (Test@theta - output@interimThetaEst[position])^2) / sum(posterior[j, ]))
                } else if (toupper(config@interimTheta$method) %in% c("EB", "FB")) {
                  currentItem = output@administeredItemIndex[position]
                  if (toupper(config@interimTheta$method == "EB")) {
                    output@posteriorSample = theta_EB_single(nSample, currentTheta, currentSE,
                                                             object@ipar[currentItem, ],
                                                             output@administeredItemResp[position], object@NCAT[currentItem],
                                                             model[currentItem], 1, c(currentTheta, currentSE))
                  } else {
                    output@posteriorSample = theta_FB_single(nSample, currentTheta, currentSE, iparList[[currentItem]],
                                                             object@ipar[currentItem, ],
                                                             output@administeredItemResp[position], object@NCAT[currentItem],
                                                             model[currentItem], 1, c(currentTheta, currentSE))
                  }
                  output@posteriorSample = output@posteriorSample[seq(from = config@MCMC$burnIn + 1, to = nSample, by = config@MCMC$thin)]
                  output@interimThetaEst[position] = mean(output@posteriorSample) 
                  output@interimSeEst[position] = sd(output@posteriorSample)
                } else {
                  stop("invalid interimTheta@method specified")
                }
                
                thetaChange = output@interimThetaEst[position] - currentTheta
                currentTheta = output@interimThetaEst[position]
                currentSE = output@interimSeEst[position]
                
                if (refreshPolicy == "THRESHOLD") {
                  if ((abs(thetaChange) > config@refreshPolicy$threshold) && (position < testLength)) {
                    refreshShadow[position + 1] = TRUE
                  }
                }
                
                if (position == maxNI) {
                  done = TRUE
                  output@likelihood = likelihood
                  output@posterior = posterior[j, ]
                }
              } #end done
              
              if (all.equal(config@finalTheta, config@interimTheta) == TRUE) {
                output@finalThetaEst = output@interimThetaEst[position]
                output@finalSeEst = output@interimSeEst[position]
              } else if (toupper(config@finalTheta$method == "EAP")) {
                if (toupper(config@finalTheta$priorDist) == "NORMAL") {
                  finalPrior = dnorm(config@thetaGrid, mean = config@finalTheta$priorPar[1], sd = config@finalTheta$priorPar[2])
                } else if (toupper(config@finalTheta$priorDist) == "UNIFORM") {
                  finalPrior = rep(1, nq)
                } else stop("invalid configuration option for finalTheta$priorDist")
                output@posterior = output@likelihood * finalPrior
                output@finalThetaEst = sum(output@posterior * config@thetaGrid) / sum(output@posterior)
                output@finalSeEst = sqrt(sum(output@posterior * (config@thetaGrid - output@finalThetaEst)^2)/sum(output@posterior))
              }
              
              usageMatrix[j, output@administeredItemIndex] = TRUE
              
              if (setBased) {
                usageMatrix[j, ni + unique(output@administeredStimulusIndex)] = TRUE
              }
              
              outputList[[j]] = output
              
              if (itemEligibilityControl) {
                if (!is.null(trueTheta)) {
                  segmentTrue = findSegment(segmentCut, output@trueTheta)
                  outputList[[j]]@trueThetaSegment = segmentTrue
                  trueSegmentFreq[segmentTrue] = trueSegmentFreq[segmentTrue] + 1
                  trueSegmentCount[j] = trueSegmentFreq[segmentTrue]
                }
                segmentFinal = findSegment(segmentCut, output@finalThetaEst)
                eligibleInFinalSegment = ineligible_i[segmentFinal, ] == 0
                estSegmentFreq[segmentFinal] = estSegmentFreq[segmentFinal] + 1
                estSegmentCount[j] = estSegmentFreq[segmentFinal]
                segmentVisited = sort(unique(output@thetaSegmentIndex))
                segmentOther = segmentVisited[segmentVisited != segmentFinal]
                
                if (exposureControl %in% c("ELIGIBILITY")) {
                  n_jk[segmentFinal] = fadingFactor *  n_jk[segmentFinal] + 1
                  
                  alpha_ijk[segmentFinal, ] = fadingFactor * alpha_ijk[segmentFinal, ]
                  alpha_ijk[segmentFinal, output@administeredItemIndex] = alpha_ijk[segmentFinal, output@administeredItemIndex] + 1
                  # this nested for-loop is experimental 3/21/19
                  if (length(segmentOther) > 0) {
                    if (any(!eligibleInFinalSegment[output@administeredItemIndex])) {
                      for (k in segmentOther) {
                        for (i in output@administeredItemIndex[output@thetaSegmentIndex == k]) {
                          if (!eligibleInFinalSegment[i]) {
                            alpha_ijk[k, i] = alpha_ijk[k, i] + 1
                          }
                        }
                      }
                    }
                  }
                  if (fadingFactor != 1) {
                    noFading_n_jk[segmentFinal] = noFading_n_jk[segmentFinal] + 1
                    noFading_alpha_ijk[segmentFinal, output@administeredItemIndex] = noFading_alpha_ijk[segmentFinal, output@administeredItemIndex] + 1
                  }
                  
                  segmentFeasible = unique(output@thetaSegmentIndex[output@shadowTestFeasible == TRUE])
                  segmentInfeasible = unique(output@thetaSegmentIndex[output@shadowTestFeasible == FALSE])
                  
                  phi_jk[segmentFinal] = fadingFactor * phi_jk[segmentFinal]
                  rho_ijk[segmentFinal, ] = fadingFactor * rho_ijk[segmentFinal, ] #number of test takers through j who visited item pool k when item i was ELIGIBLE or the test was infeasible
                  
                  if (segmentFinal %in% segmentFeasible) {
                    phi_jk[segmentFinal] = phi_jk[segmentFinal] + 1
                    rho_ijk[segmentFinal, eligibleInFinalSegment] = rho_ijk[segmentFinal, eligibleInFinalSegment] + 1
                    if (fadingFactor != 1) {
                      noFading_rho_ijk[segmentFinal, eligibleInFinalSegment] = noFading_rho_ijk[segmentFinal, eligibleInFinalSegment] + 1
                    }
                  } else {
                    rho_ijk[segmentFinal, ] = rho_ijk[segmentFinal, ] + 1
                    if (fadingFactor != 1) {
                      noFading_rho_ijk[segmentFinal, ] = noFading_rho_ijk[segmentFinal, ] + 1
                    }
                  }
                  
                  nf_ijk = matrix(n_jk / phi_jk, nSegment, ni)
                  
                  if (accelerationFactor > 1) {
                    p_alpha_ijk = alpha_ijk / matrix(n_jk, nSegment, ni) #alpha_ijk as a probability
                    p_rho_ijk = rho_ijk / matrix(n_jk, nSegment, ni)     #rho_ijk as a probability
                    p_alpha_ijk[is.na(p_alpha_ijk)] = 0
                    p_rho_ijk[is.na(p_rho_ijk)] = 1
                    flag_alpha_ijk = p_alpha_ijk > maxExposureRate       #flagging items
                    if (length(maxExposureRate) == nSegment) {
                      for (k in 1:nSegment) {
                        pe_i[k, flag_alpha_ijk[k, ]] = 1 - nf_ijk[k, flag_alpha_ijk[k, ]] + (maxExposureRate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^accelerationFactor * nf_ijk[k, flag_alpha_ijk[k, ]] * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                        pe_i[k, !flag_alpha_ijk[k, ]] = 1 - nf_ijk[k, !flag_alpha_ijk[k, ]] + maxExposureRate[k] * nf_ijk[k, !flag_alpha_ijk[k, ]] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
                      }
                    } else {
                      pe_i[flag_alpha_ijk] = 1 - nf_ijk[flag_alpha_ijk] + (maxExposureRate / p_alpha_ijk[flag_alpha_ijk])^accelerationFactor * nf_ijk[flag_alpha_ijk] * p_rho_ijk[flag_alpha_ijk]
                      pe_i[!flag_alpha_ijk] = 1 - nf_ijk[!flag_alpha_ijk] + maxExposureRate * nf_ijk[!flag_alpha_ijk] * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
                    }
                  } else {
                    pe_i = 1 - nf_ijk + maxExposureRate * nf_ijk * rho_ijk / alpha_ijk
                  }
                  
                  pe_i[is.na(pe_i) | alpha_ijk == 0] = 1
                  pe_i[pe_i > 1] = 1
                  
                  if (setBased) {
                    
                    alpha_sjk[segmentFinal, ] = fadingFactor * alpha_sjk[segmentFinal, ]
                    alpha_sjk[segmentFinal, output@administeredStimulusIndex] = alpha_sjk[segmentFinal, output@administeredStimulusIndex] + 1
                    
                    eligibleSetInFinalSegment = ineligible_s[segmentFinal, ] == 0
                    
                    if (fadingFactor != 1) {
                      noFading_alpha_sjk[segmentFinal, output@administeredStimulusIndex] = noFading_alpha_sjk[segmentFinal, output@administeredStimulusIndex] + 1
                    }
                    
                    rho_sjk[segmentFinal, ] = fadingFactor * rho_sjk[segmentFinal, ]
                    
                    if (segmentFinal %in% segmentFeasible) {
                      rho_sjk[segmentFinal, eligibleSetInFinalSegment] = rho_sjk[segmentFinal, eligibleSetInFinalSegment] + 1
                      if (fadingFactor != 1) {
                        noFading_rho_sjk[segmentFinal, eligibleSetInFinalSegment] = noFading_rho_sjk[segmentFinal, eligibleSetInFinalSegment] + 1
                      }
                    } else {
                      rho_sjk[segmentFinal, ] = rho_sjk[segmentFinal, ] + 1
                      if (fadingFactor != 1) {
                        noFading_rho_sjk[segmentFinal, ] = noFading_rho_sjk[segmentFinal, ] + 1
                      }
                    }
                    
                    nf_sjk = matrix(n_jk / phi_jk, nSegment, ns)
                    
                    if (accelerationFactor > 1) {
                      
                      p_alpha_sjk = alpha_sjk / matrix(n_jk, nSegment, ns)     # Note: Error when using ELIGIBILITY + EAP
                      p_rho_sjk = rho_sjk / matrix(n_jk, nSegment, ns)
                      p_alpha_sjk[is.na(p_alpha_sjk)] = 0
                      p_rho_sjk[is.na(p_rho_sjk)] = 1
                      flag_alpha_sjk = p_alpha_sjk > maxExposureRate
                      if (length(maxExposureRate) == nSegment) {
                        for (k in 1:nSegment) {
                          pe_s[k, flag_alpha_sjk[k, ]] = 1 - nf_sjk[k, flag_alpha_sjk[k, ]] + (maxExposureRate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^accelerationFactor * nf_sjk[k, flag_alpha_sjk[k, ]] * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                          pe_s[k, !flag_alpha_sjk[k, ]] = 1 - nf_sjk[k, !flag_alpha_sjk[k, ]] + maxExposureRate[k] * nf_sjk[k, !flag_alpha_sjk[k, ]] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                        }
                      } else {
                        pe_s[flag_alpha_sjk] = 1 - nf_sjk[flag_alpha_sjk] + (maxExposureRate / p_alpha_sjk[flag_alpha_sjk])^accelerationFactor * nf_sjk[flag_alpha_sjk] * p_rho_sjk[flag_alpha_sjk]
                        pe_s[!flag_alpha_sjk] = 1 - nf_sjk[!flag_alpha_sjk] + maxExposureRate * nf_sjk[!flag_alpha_sjk] * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
                      }
                    } else {
                      pe_s = 1 - nf_sjk + maxExposureRate * nf_sjk * rho_sjk / alpha_sjk
                    }
                    pe_s[is.na(pe_s) | alpha_sjk == 0] = 1
                    pe_s[pe_s > 1] = 1
                  }
                } else if (exposureControl %in% c("BIGM")) {
                  n_jk[segmentFinal] = fadingFactor *  n_jk[segmentFinal] + 1 
                  
                  alpha_ijk[segmentFinal, ] = fadingFactor * alpha_ijk[segmentFinal, ]
                  alpha_ijk[segmentFinal, output@administeredItemIndex] = alpha_ijk[segmentFinal, output@administeredItemIndex] + 1
                  # this nested for-loop is experimental 3/21/19
                  if (length(segmentOther) > 0) {
                    if (any(!eligibleInFinalSegment[output@administeredItemIndex])) {
                      for (k in segmentOther) {
                        for (i in output@administeredItemIndex[output@thetaSegmentIndex == k]) {
                          if (!eligibleInFinalSegment[i]) {
                            alpha_ijk[k, i] = alpha_ijk[k, i] + 1
                          }
                        }
                      }
                    }
                  }
                  
                  rho_ijk[segmentFinal, ] = fadingFactor * rho_ijk[segmentFinal, ] #number of test takers through j who visited item pool k when item i was eligible 
                  rho_ijk[segmentFinal, eligibleInFinalSegment] = rho_ijk[segmentFinal, eligibleInFinalSegment] + 1
                  
                  if (fadingFactor != 1) {
                    noFading_n_jk[segmentFinal] = noFading_n_jk[segmentFinal] + 1
                    noFading_alpha_ijk[segmentFinal, output@administeredItemIndex] = noFading_alpha_ijk[segmentFinal, output@administeredItemIndex] + 1
                    noFading_rho_ijk[segmentFinal, eligibleInFinalSegment] = noFading_rho_ijk[segmentFinal, eligibleInFinalSegment] + 1
                  }
                  
                  if (accelerationFactor > 1) {
                    p_alpha_ijk = alpha_ijk / matrix(n_jk, nSegment, ni)
                    p_rho_ijk = rho_ijk / matrix(n_jk, nSegment, ni)
                    p_alpha_ijk[is.na(p_alpha_ijk)] = 0
                    p_rho_ijk[is.na(p_rho_ijk)] = 1
                    flag_alpha_ijk = p_alpha_ijk > maxExposureRate
                    if (length(maxExposureRate) == nSegment) {
                      for (k in 1:nSegment) {
                        pe_i[k, flag_alpha_ijk[k, ]] = (maxExposureRate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^accelerationFactor * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                        pe_i[k, !flag_alpha_ijk[k, ]] = maxExposureRate[k] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
                      }
                    } else {
                      pe_i[flag_alpha_ijk] = (maxExposureRate / p_alpha_ijk[flag_alpha_ijk])^accelerationFactor * p_rho_ijk[flag_alpha_ijk]
                      pe_i[!flag_alpha_ijk] = maxExposureRate * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
                    }
                  } else {
                    pe_i = maxExposureRate * rho_ijk / alpha_ijk
                  }
                  pe_i[is.na(pe_i) | alpha_ijk == 0] = 1
                  pe_i[pe_i > 1] = 1
                  
                  if (setBased) {
                    alpha_sjk[segmentFinal, ] = fadingFactor * alpha_sjk[segmentFinal, ]
                    alpha_sjk[segmentFinal, output@administeredStimulusIndex] = alpha_sjk[segmentFinal, output@administeredStimulusIndex] + 1
                    
                    eligibleSetInFinalSegment = ineligible_s[segmentFinal, ] == 0
                    
                    rho_sjk[segmentFinal, ] = fadingFactor * rho_sjk[segmentFinal ]
                    rho_sjk[segmentFinal, eligibleSetInFinalSegment] = rho_sjk[segmentFinal, eligibleSetInFinalSegment] + 1
                    
                    if (fadingFactor != 1) {
                      noFading_alpha_sjk[segmentFinal, output@administeredStimulusIndex] = noFading_alpha_sjk[segmentFinal, output@administeredStimulusIndex] + 1
                      noFading_rho_sjk[segmentFinal, eligibleSetInFinalSegment] = noFading_rho_sjk[segmentFinal, eligibleSetInFinalSegment] + 1
                    }
                    
                    if (accelerationFactor > 1) {
                      p_alpha_sjk = alpha_sjk / matrix(n_jk, nSegment, ns)
                      p_rho_sjk = rho_sjk / matrix(n_jk, nSegment, ns)
                      p_alpha_sjk[is.na(p_alpha_sjk)] = 0
                      p_rho_sjk[is.na(p_rho_sjk)] = 1
                      flag_alpha_sjk = p_alpha_sjk > maxExposureRate
                      if (length(maxExposureRate) == nSegment) {
                        for (k in 1:nSegment) {
                          pe_s[k, flag_alpha_sjk[k, ]] = (maxExposureRate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^accelerationFactor * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                          pe_s[k, !flag_alpha_sjk[k, ]] = maxExposureRate[k] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                        }
                      } else {
                        pe_s[flag_alpha_sjk] = (maxExposureRate / p_alpha_sjk[flag_alpha_sjk])^accelerationFactor * p_rho_sjk[flag_alpha_sjk]
                        pe_s[!flag_alpha_sjk] = maxExposureRate * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
                      }
                    } else {
                      pe_s = maxExposureRate * rho_sjk / alpha_sjk
                    }
                    
                    pe_s[is.na(pe_s) | alpha_sjk == 0] = 1
                    pe_s[pe_s > 1] = 1
                  }
                } else if (exposureControl %in% c("BIGM-BAYESIAN")) {
                  segmentVisited = sort(unique(output@thetaSegmentIndex)) 
                  sampleSegment = findSegment(segmentCut, output@posteriorSample)
                  segmentDistribution = table(sampleSegment) / length(sampleSegment)
                  segmentClassified = as.numeric(names(segmentDistribution)) 
                  segmentProb = numeric(nSegment)
                  segmentProb[segmentClassified] = segmentDistribution
                  
                  n_jk = fadingFactor * n_jk + segmentProb
                  rho_ijk = fadingFactor * rho_ijk
                  alpha_ijk = fadingFactor * alpha_ijk
                  alpha_ijk[, output@administeredItemIndex] = alpha_ijk[, output@administeredItemIndex] + segmentProb
                  # this nested for-loop is experimental 3/21/19
                  if (length(segmentOther) > 0) {
                    if (any(!eligibleInFinalSegment[output@administeredItemIndex])) {
                      for (k in segmentOther) {
                        for (i in output@administeredItemIndex[output@thetaSegmentIndex == k]) {
                          if (!eligibleInFinalSegment[i]) {
                            alpha_ijk[k, i] = alpha_ijk[k, i] + segmentProb[k]
                          }
                        }
                      }
                    }
                  }
                  
                  for (segment in 1:nSegment) {
                    eligible = ineligible_i[segment, ] == 0
                    #eligible = ineligible_i[segment, ] == 0 & eligibleInFinalSegment
                    rho_ijk[segment, eligible] = rho_ijk[segment, eligible] + segmentProb[segment]
                  }
                  
                  if (fadingFactor != 1) {
                    noFading_n_jk = noFading_n_jk + segmentProb
                    noFading_alpha_ijk[, output@administeredItemIndex] = noFading_alpha_ijk[, output@administeredItemIndex] + segmentProb
                    for (segment in 1:nSegment) {
                      eligible = ineligible_i[segment, ] == 0
                      noFading_rho_ijk[segment, eligible] = noFading_rho_ijk[segment, eligible] + segmentProb[segment]
                    }
                  }
                  
                  if (accelerationFactor > 1) {
                    p_alpha_ijk = alpha_ijk / matrix(n_jk, nSegment, ni)
                    p_rho_ijk = rho_ijk / matrix(n_jk, nSegment, ni)
                    p_alpha_ijk[is.na(p_alpha_ijk)] = 0
                    p_rho_ijk[is.na(p_rho_ijk)] = 1
                    flag_alpha_ijk = p_alpha_ijk > maxExposureRate
                    if (length(maxExposureRate) == nSegment) {
                      for (k in 1:nSegment) {
                        pe_i[k, flag_alpha_ijk[k, ]] = (maxExposureRate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^accelerationFactor * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                        pe_i[k, !flag_alpha_ijk[k, ]] = maxExposureRate[k] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
                      }
                    } else {
                      pe_i[flag_alpha_ijk] = (maxExposureRate / p_alpha_ijk[flag_alpha_ijk])^accelerationFactor * p_rho_ijk[flag_alpha_ijk]
                      pe_i[!flag_alpha_ijk] = maxExposureRate * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
                    }
                  } else {
                    pe_i = maxExposureRate * rho_ijk / alpha_ijk
                  }
                  
                  pe_i[is.na(pe_i) | alpha_ijk == 0] = 1
                  pe_i[pe_i > 1] = 1
                  
                  if (setBased) {
                    alpha_sjk = fadingFactor * alpha_sjk
                    rho_sjk = fadingFactor * rho_sjk
                    alpha_sjk[, output@administeredStimulusIndex] = alpha_sjk[, output@administeredStimulusIndex] + segmentProb
                    
                    for (segment in 1:nSegment) {
                      rho_sjk[segment, ineligible_s[segment, ] == 0] = rho_sjk[segment, ineligible_s[segment, ] == 0] + segmentProb[segment]
                    }
                    
                    if (fadingFactor != 1) {
                      noFading_alpha_sjk[, output@administeredStimulusIndex] = noFading_alpha_sjk[, output@administeredStimulusIndex] + segmentProb
                      for (segment in 1:nSegment) {
                        noFading_rho_sjk[segment, ineligible_s[segment, ] == 0] = noFading_rho_sjk[segment, ineligible_s[segment, ] == 0] + segmentProb[k]
                      }
                    }
                    
                    if (accelerationFactor > 1) {
                      p_alpha_sjk = alpha_sjk / matrix(n_jk, nSegment, ns)
                      p_rho_sjk = rho_sjk / matrix(n_jk, nSegment, ns)
                      p_alpha_sjk[is.na(p_alpha_sjk)] = 0
                      p_rho_sjk[is.na(p_rho_sjk)] = 1
                      flag_alpha_sjk = p_alpha_sjk > maxExposureRate
                      if (length(maxExposureRate) == nSegment) {
                        for (k in 1:nSegment) {
                          pe_s[k, flag_alpha_sjk[k, ]] = (maxExposureRate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^accelerationFactor * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                          pe_s[k, !flag_alpha_sjk[k, ]] = maxExposureRate[k] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                        }
                      } else {
                        pe_s[flag_alpha_sjk] = (maxExposureRate / p_alpha_sjk[flag_alpha_sjk])^accelerationFactor * p_rho_sjk[flag_alpha_sjk]
                        pe_s[!flag_alpha_sjk] = maxExposureRate * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
                      }
                    } else {
                      pe_s = maxExposureRate * rho_sjk / alpha_sjk
                    }
                    pe_s[is.na(pe_s) | alpha_sjk == 0] = 1
                    pe_s[pe_s > 1] = 1
                  }
                }  
                
                if (config@exposureControl$diagnosticStats) {
                  for (g in 1:nSegment) {
                    alpha_g_i[j, (g - 1) * ni + 1:ni] = alpha_ijk[g, ]
                    epsilon_g_i[j, (g - 1) * ni + 1:ni] = rho_ijk[g, ]
                    
                    if (setBased) {
                      alpha_g_s[j, (g - 1) * ns + 1:ns] = alpha_sjk[g, ]
                      epsilon_g_s[j, (g - 1) * ns + 1:ns] = rho_sjk[g, ]
                    }
                  }
                  if (fadingFactor != 1) {
                    for (g in 1:nSegment) {
                      noFading_alpha_g_i[j, (g - 1) * ni + 1:ni] = noFading_alpha_ijk[g, ]
                      noFading_epsilon_g_i[j, (g - 1) * ni + 1:ni] = noFading_rho_ijk[g, ]
                      
                      if (setBased) {
                        noFading_alpha_g_s[j, (g - 1) * ns + 1:ns] = noFading_alpha_sjk[g, ]
                        noFading_epsilon_g_s[j, (g - 1) * ns + 1:ns] = noFading_rho_sjk[g, ]
                      }
                    }
                  }
                }
              } #end itemEligibilityControl
              
              if (config@auditTrail) {
                plotAuditTrail()
              }
              
              if (!is.null(session)) {
                shinyWidgets::updateProgressBar(session = session, id = "pb", value = j, total = nj)
              } else {
                setTxtProgressBar(pb, j)
              }
            } #end j
            
            finalThetaEst = unlist(lapply(1:nj, function(j) outputList[[j]]@finalThetaEst))
            finalSeEst = unlist(lapply(1:nj, function(j) outputList[[j]]@finalSeEst))
            
            exposureRate = colSums(usageMatrix) / nj
            
            eligibilityStats = NULL
            checkEligibilityStats = NULL
            noFadingEligibilityStats = NULL
            
            if (itemEligibilityControl) {
              eligibilityStats = list(pe_i = pe_i, n_jk = n_jk, alpha_ijk = alpha_ijk, phi_jk = phi_jk, rho_ijk = rho_ijk, pe_s = pe_s, alpha_sjk = alpha_sjk, rho_sjk = rho_sjk)
              
              if (config@exposureControl$diagnosticStats) {
                checkEligibilityStats = as.data.frame(cbind(1:nj, trueTheta, findSegment(segmentCut, trueTheta), trueSegmentCount, alpha_g_i, epsilon_g_i), row.names = NULL) 
                names(checkEligibilityStats) = c("Examinee", "TrueTheta", "TrueSegment", "TrueSegmentCount", paste("a", "g", rep(1:nSegment, rep(ni, nSegment)), "i", rep(1:ni, nSegment), sep = "_"), paste("e", "g", rep(1:nSegment, rep(ni, nSegment)), "i", rep(1:ni, nSegment), sep = "_"))
                if (setBased) {
                  checkEligibilityStats_stimulus = as.data.frame(cbind(alpha_g_s, epsilon_g_s), row.names = NULL)
                  names(checkEligibilityStats_stimulus) = c(paste("a", "g", rep(1:nSegment, rep(ns, nSegment)), "s", rep(1:ns, nSegment), sep = "_"), paste("e", "g", rep(1:nSegment, rep(ns, nSegment)), "s", rep(1:ns, nSegment), sep = "_"))
                  checkEligibilityStats = cbind(checkEligibilityStats, checkEligibilityStats_stimulus)
                }
                if (fadingFactor != 1) {
                  noFadingEligibilityStats = as.data.frame(cbind(1:nj, trueTheta, findSegment(segmentCut, trueTheta), trueSegmentCount, noFading_alpha_g_i, noFading_epsilon_g_i), row.names = NULL) 
                  names(noFadingEligibilityStats) = c("Examinee", "TrueTheta", "TrueSegment", "TrueSegmentCount", paste("a", "g", rep(1:nSegment, rep(ni, nSegment)), "i", rep(1:ni, nSegment), sep = "_"), paste("e", "g", rep(1:nSegment, rep(ni, nSegment)), "i", rep(1:ni, nSegment), sep = "_"))
                  if (setBased) {
                    noFadingEligibilityStats_stimulus = as.data.frame(cbind(noFading_alpha_g_s, noFading_epsilon_g_s), row.names = NULL)
                    names(noFadingEligibilityStats_stimulus) = c(paste("a", "g", rep(1:nSegment, rep(ns, nSegment)), "s", rep(1:ns, nSegment), sep = "_"), paste("e", "g", rep(1:nSegment, rep(ns, nSegment)), "s", rep(1:ns, nSegment), sep = "_"))
                    noFadingEligibilityStats = cbind(noFadingEligibilityStats, noFadingEligibilityStats_stimulus)
                  }
                }
              }
            }
            
            if (sta) {
              freqInfeasible = table(unlist(lapply(1:nj, function(j) sum(!outputList[[j]]@shadowTestFeasible))))
            } else {
              freqInfeasible = NULL
            }
            return(list(output = outputList, pool = object, config = config, trueTheta = trueTheta, Constraints = Constraints, prior = prior, priorPar = priorPar, Data = Test@Data, finalThetaEst = finalThetaEst, finalSeEst = finalSeEst, exposureRate = exposureRate, usageMatrix = usageMatrix, trueSegmentCount = trueSegmentCount, estSegmentCount = estSegmentCount, eligibilityStats = eligibilityStats, checkEligibilityStats = checkEligibilityStats, noFadingEligibilityStats = noFadingEligibilityStats, freqInfeasible = freqInfeasible))
          }
)

#' addTrans
#' 
#' addTrans
#' 
#' @param color A nice parameter
#' @param trans A nice parameter
#' 
#' @export
addTrans = function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

#' plotEligibilityStats
#' 
#' plotEligibilityStats
#' 
#' @param config A \code{\linkS4class{Shadow.config}} object.                                            
#' @param object A nice parameter
#' @param objectNoFading A nice parameter
#' @param file A nice parameter
#' @param fileNoFading A nice parameter
#' @param segment A nice parameter
#' @param items A nice parameter
#' @param PDF A nice parameter
#' @param maxRate A nice parameter
#' @param discardFirst A nice parameter
#' 
#' @export
plotEligibilityStats = function(config, object = NULL, objectNoFading = NULL, file = NULL, fileNoFading = NULL, segment = 1, items = c(1), PDF = NULL, maxRate = 0.25, discardFirst = NULL) {
  
  fadingFactor = config@exposureControl$fadingFactor
  
  if (!is.null(PDF)) {
    pdf(file = PDF)
  }
  
  if (is.null(object) && is.null(file)) {
    stop("Both object and file are NULL")
  } else if (!is.null(object)) {
    eligibilityStats = object
  } else if (!is.null(file)) {
    eligibilityStats = read.csv(file, header = TRUE, sep = ",")
  }
  
  eligibilityStatsNoFading = NULL
  
  if (!is.null(objectNoFading)) {
    eligibilityStatsNoFading = objectNoFading
  } else if (!is.null(fileNoFading)) {
    eligibilityStatsNoFading = read.csv(fileNoFading, header = TRUE, sep = ",")
  }
  
  eligibilityStatsSegment = split(eligibilityStats, eligibilityStats$TrueSegment)[[segment[1]]]
  if (!is.null(eligibilityStatsNoFading)) {
    eligibilityStatsSegmentNoFading = split(eligibilityStatsNoFading, eligibilityStatsNoFading$TrueSegment)[[segment[1]]]
  }
  if (!is.null(discardFirst) && discardFirst < nrow(eligibilityStatsSegment)) {
    eligibilityStatsSegment = eligibilityStatsSegment[eligibilityStatsSegment$TrueSegmentCount > discardFirst, ]
    if (!is.null(eligibilityStatsNoFading)) {
      eligibilityStatsSegmentNoFading = eligibilityStatsSegmentNoFading[eligibilityStatsSegmentNoFading$TrueSegmentCount > discardFirst, ]
    }
  }
  
  examinee = eligibilityStatsSegment$TrueSegmentCount
  nExaminee = length(examinee)
  fadingExaminee = examinee
  for (j in 2:length(examinee)) {
    fadingExaminee[j] = fadingExaminee[j - 1] * fadingFactor + 1
  }
  
  for (i in items) {
    
    alpha = eligibilityStatsSegment[[paste("a_g", segment, "i", i, sep = "_")]]
    epsilon = eligibilityStatsSegment[[paste("e_g", segment, "i", i, sep = "_")]]
    p_alpha = alpha / fadingExaminee
    p_epsilon = epsilon / fadingExaminee
    p_epsilon[p_epsilon > 1] = 1
    p_eligibility = rep(1, nExaminee)
    for (j in 2:nExaminee) {
      if (alpha[j - 1] > 0) {
        p_eligibility[j] = min(epsilon[j - 1] * maxRate / alpha[j - 1], 1)
      }
    }
    
    if (!is.null(eligibilityStatsNoFading)) {
      alpha_noFading = eligibilityStatsSegmentNoFading[[paste("a_g", segment, "i", i, sep = "_")]]
      epsilon_noFading = eligibilityStatsSegmentNoFading[[paste("e_g", segment, "i", i, sep = "_")]]
      p_alpha_noFading = alpha_noFading / examinee
      p_epsilon_noFading = epsilon_noFading / examinee
    }
    
    plot(examinee, p_alpha, main = paste("Segment", segment, "- Item", i), type = "n", ylim = c(0, 1), xlab = "Examinees", ylab = "Rate")
    lines(examinee, p_alpha, col = "red", lty = 1, lwd = 3)
    lines(examinee, p_epsilon, col = "blue", lty = 2, lwd = 3)
    lines(examinee, p_eligibility, col = "purple", lty = 3, lwd = 3)
    
    if (is.null(eligibilityStatsNoFading)) {
      legend("topright", c("alpha", "epsilon", "Pr{eligible}"), lty = c(1, 2, 3), col = c("red", "blue", "purple"), lwd = c(2, 2, 2), bg = "white")
    } else {
      lines(examinee, p_epsilon_noFading, col = addTrans("blue", 20), lty = 1, type = "h")
      lines(examinee, p_alpha_noFading, col = addTrans("red", 20), lty = 1, type = "h")
      legend("topright", c("alpha", "epsilon", "Pr{eligible}", "alpha empirical", "epsilon empirical"), lty = c(1, 2, 3, 1, 1), lwd = c(2, 2, 2, 5, 5), col = c("red", "blue", "purple", addTrans("red", 100), addTrans("blue", 100)))
    }
    abline(h = maxRate, col = "gray")
  }
  
  if (!is.null(PDF)) {
    dev.off()
  }
}

#' RMSE
#' 
#' RMSE
#' 
#' @param x A nice parameter
#' @param y A nice parameter
#' @param conditional A nice parameter
#' 
#' @export
RMSE = function(x, y, conditional = TRUE) {
  if (length(x) != length(y)) {
    stop("length(x) and length(y) are not equal")
  }
  if (conditional) {
    MSE = tapply((x - y)^2, x, mean)
  } else {
    MSE = mean((x - y)^2)
  }
  return(sqrt(MSE))
}

#' RE
#' 
#' RE
#' 
#' @param RMSE.foc A nice parameter
#' @param RMSE.ref A nice parameter
#' 
#' @export
RE = function(RMSE.foc, RMSE.ref) {
  if (length(RMSE.foc) != length(RMSE.ref)) {
    stop("length(x) and length(y) are not equal")
  }
  
  RE = RMSE.ref^2 / RMSE.foc^2
  return(RE)
}

# > names(constraints.Fatigue.3)
# [1] "Constraints"         "ListConstraints"     "pool"                "ItemAttrib"          "StAttrib"           
# [6] "testLength"          "nv"                  "ni"                  "ns"                  "ID"                 
# [11] "INDEX"               "MAT"                 "DIR"                 "RHS"                 "setBased"           
# [16] "ItemOrder"           "ItemOrderBy"         "StimulusOrder"       "StimulusOrderBy"     "ItemIndexByStimulus"
# [21] "StimulusIntexByItem"

#' checkConstraints
#' 
#' checkConstraints
#' 
#' @param constraints A nice parameter
#' @param usageMatrix A nice parameter
#' @param trueTheta A nice parameter
#' 
#' @export
checkConstraints = function(constraints, usageMatrix, trueTheta = NULL) {
  Constraints = constraints$Constraints
  ListConstraints = constraints$ListConstraints
  
  nc = nrow(Constraints)
  nj = nrow(usageMatrix)
  ni = ncol(usageMatrix)
  
  MET = matrix(FALSE, nrow = nj, ncol = nc)
  COUNT = matrix(NA, nrow = nj, ncol = nc)
  
  if (ni != constraints$ni) {
    stop("unequal number of items in constraints and usageMatrix ")
  }
  
  byTheta = FALSE
  MEAN = rep(NA, nc)
  SD = rep(NA, nc)
  MIN = rep(NA, nc)
  MAX = rep(NA, nc)
  HIT = rep(NA, nc)
  
  if (!is.null(trueTheta)) {
    if (length(trueTheta) != nj) {
      stop("length of trueTheta is not equal to nrow of usageMatrix")
    }
    byTheta = TRUE
    groupMEAN = matrix(NA, nrow = nc, ncol = length(unique(trueTheta)))
    groupSD = matrix(NA, nrow = nc, ncol = length(unique(trueTheta)))
    groupMIN = matrix(NA, nrow = nc, ncol = length(unique(trueTheta)))
    groupMAX = matrix(NA, nrow = nc, ncol = length(unique(trueTheta)))
    groupHIT = matrix(NA, nrow = nc, ncol = length(unique(trueTheta)))
  } else {
    groupMEAN = NULL
    groupSD = NULL
    groupMIN = NULL
    groupMAX = NULL
    groupHIT = NULL
  }
  
  nEnemy = sum(Constraints$TYPE == "ENEMY") 
  
  if (nEnemy > 0) {
    enemyIndex = which(Constraints$TYPE == "ENEMY")
    Constraints$LB[enemyIndex] = 0
    Constraints$UB[enemyIndex] = 1
  }
  
  numberIndex = which(Constraints$TYPE == "NUMBER")
  
  for (index in 1:nc) {
    if (Constraints$WHAT[index] == "ITEM") {
      if (Constraints$TYPE[index] %in% c("NUMBER", "ENEMY")) {
        items = which(ListConstraints[[index]]@mat[1, ] == 1)
        COUNT[, index] = rowSums(usageMatrix[, items])
        MET[, index] =  COUNT[, index] >= Constraints$LB[index] & COUNT[, index] <= Constraints$UB[index]
        
        if (byTheta) {
          groupMEAN[index, ] = round(tapply(COUNT[, index], trueTheta, mean), 3)
          groupSD[index, ] = round(tapply(COUNT[, index], trueTheta, sd), 3)
          groupMIN[index, ] = tapply(COUNT[, index], trueTheta, min)
          groupMAX[index, ] = tapply(COUNT[, index], trueTheta, max)
          groupHIT[index, ] = round(tapply(MET[, index], trueTheta, mean), 3)
        }
        
        MEAN[index] = round(mean(COUNT[, index]), 2)
        SD[index] = round(sd(COUNT[, index]), 2)
        MIN[index] = min(COUNT[, index])
        MAX[index] = max(COUNT[, index])
        HIT[index] = round(mean(MET[, index]), 3)
      }
    }
  }
 
  LD = NULL
  if (nEnemy > 0) {
    LD = rowSums(COUNT[, enemyIndex] > 1)
  }
  
  Check = data.frame(Constraints, MEAN = MEAN, SD = SD, MIN = MIN, MAX = MAX, HIT = HIT)
  return(list(Check = Check[Constraints$TYPE == "NUMBER", ], LD = LD, groupMEAN = groupMEAN[numberIndex, ], groupSD = groupSD[numberIndex, ], groupMIN = groupMIN[numberIndex, ], groupMAX = groupMAX[numberIndex, ], groupHIT = groupHIT[numberIndex, ]))
}

#' runShadow
#' 
#' runShadow
#' 
#' @param pool A nice parameter
#' @param trueTheta A nice parameter
#' @param constraints A nice parameter
#' @param exposureControl A nice parameter
#' @param maxExposureRate A nice parameter
#' @param nSegment A nice parameter
#' @param segmentCut A nice parameter
#' @param itemSelection A nice parameter
#' @param refreshPolicy A nice parameter
#' @param position A nice parameter
#' @param threshold A nice parameter
#' @param config A nice parameter
#' @param Data A nice parameter
#' @param fixedTheta A nice parameter
#' 
#' @export
runShadow = function(pool, trueTheta, constraints, exposureControl = "ELIGIBILITY", maxExposureRate = 0.2, nSegment = 1, segmentCut = c(-Inf, Inf), itemSelection = "MPWI", refreshPolicy = "ALWAYS", position = 1:12, threshold = 0.1, config = NULL, Data = NULL, fixedTheta = NULL) {
  if (is.null(config)) {
    config = new("Shadow.config")
    config@exposureControl$method = exposureControl
    config@exposureControl$maxExposureRate = maxExposureRate
    config@exposureControl$nSegment = nSegment
    config@exposureControl$segmentCut = segmentCut
    config@itemSelection$method = itemSelection
    config@refreshPolicy$method = refreshPolicy
    if (refreshPolicy == "POSITION") {
      config@refreshPolicy$position = position
    } else if (refreshPolicy == "THRESHOLD") {
      config@refreshPolicy$threshold = threshold
    }
    if (!is.null(fixedTheta)) {
      config@itemSelection$fixedTheta = fixedTheta
    }
  }
  out = Shadow(pool, config, trueTheta, Constraints = constraints, prior = NULL) 
  return(out)
}

#' plotRMSE
#' 
#' plotRMSE
#' 
#' @param ... Some dots
#' @param title A nice parameter
#' @param legendTitle A nice parameter
#' @param legendLabels A nice parameter
#' @param ltySet A nice parameter
#' @param colSet A nice parameter
#' @param theta A nice parameter
#' 
#' @export
plotRMSE = function(..., title = NULL, legendTitle = NULL, legendLabels = NULL, ltySet = NULL, colSet = NULL, theta = seq(-2, 2, 1)) {
  outputList = list(...)
  nOutput = length(outputList)
  if (is.null(ltySet)) {
    ltySet = 1:nOutput
  } else if (length(ltySet) != nOutput) {
    warning("... and ltySet are of different lengths")
    ltySet = 1:nOutput
  }
  if (is.null(colSet)) {
    colSet = 1:nOutput
  } else if (length(colSet) != nOutput) {
    warning("... and colSet are of different lengths")
    colSet = 1:nOutput
  }
  plot(unique(outputList[[1]]$trueTheta), RMSE(outputList[[1]]$trueTheta, outputList[[1]]$finalThetaEst), xlim = range(theta), ylim = c(0, 1), xlab = "Theta", ylab = "RMSE", type = "n", xaxt = "n", yaxt = "n", main = title)
  axis(1, at = theta, labels = theta)
  axis(2, at = seq(0, 1.0, .2), labels = format(seq(0, 1.0, .2), digits = 1), las = 2)
  grid()
  for (i in 1:nOutput) {
    lines(unique(outputList[[i]]$trueTheta), RMSE(outputList[[i]]$trueTheta, outputList[[i]]$finalThetaEst), lty = ltySet[i], col = colSet[i], lwd = 2)
  }
  if (!is.null(legendLabels)) {
    if (length(legendLabels) != nOutput) {
      warning("... and legendLabels are of different lengths")
      legendLabels = 1:nOutput
    }
    legend("top", labels, lty = ltySet, col = colSet, title = legendTitle, bg = "white")
  }
}

# plotRMSE.3 = function(CAT1, CAT2, CAT3, legend = TRUE, labels = c("CAT", "MST", "LOFT"), legend.title = NULL, title = NULL, theta = seq(-2, 2, 1), PDF = NULL) {
#   if (!is.null(PDF)) {
#     pdf(file = PDF)
#   }
#   plot(unique(trueTheta), RMSE(CAT1$trueTheta, CAT1$finalThetaEst), xlim = range(theta), ylim = c(0, 1.0), xlab = "Theta", ylab = "RMSE", type = "n", xaxt = "n", yaxt = "n", main = title)
#   #axis(1, at = theta, labels = ifelse(Tscore, round(theta * 10 + 50, digits = 0), theta))
#   axis(1, at = theta, labels = theta)
#   axis(2, at = seq(0, 1.0, .2), labels = format(seq(0, 1.0, .2), digits = 1), las = 2)
#   grid()
#   lines(unique(trueTheta), RMSE(CAT1$trueTheta, CAT1$finalThetaEst), lty = 1, col = "black", lwd = 2)
#   lines(unique(trueTheta), RMSE(CAT2$trueTheta, CAT2$finalThetaEst), lty = 2, col = "blue", lwd = 2)
#   lines(unique(trueTheta), RMSE(CAT3$trueTheta, CAT3$finalThetaEst), lty = 3, col = "red", lwd = 2)
#   if (legend) {
#     legend("top", labels, lty = c(1, 2, 3), lwd = 2, col = c("black", "blue", "red"), title = legend.title, bg = "white")
#   }
#   if (!is.null(PDF)) {
#     dev.off()
#   }
# }

#' plotExposureRateBySegment
#' 
#' plotExposureRateBySegment
#' 
#' @param object A nice parameter
#' @param config A nice parameter
#' @param maxRate A nice parameter
#' @param pdfFile A nice parameter
#' @param width A nice parameter
#' @param height A nice parameter
#' @param mfrow A nice parameter
#' 
#' @export
plotExposureRateBySegment = function(object, config, maxRate = 0.25, pdfFile = NULL, width = 7, height = 6, mfrow = c(2, 4)) {
  trueTheta = object$trueTheta
  estTheta = object$finalThetaEst
  nj = length(trueTheta)
  ni = ncol(object$usageMatrix)
  
  segmentCut = config@exposureControl$segmentCut
  
  nSegment = config@exposureControl$nSegment
  cutLower = segmentCut[1:nSegment]
  cutUpper = segmentCut[2:(nSegment + 1)]
  segmentLabel = character(nSegment)
  
  for (k in 1:nSegment) {
    if (k < nSegment ) {
      segmentLabel[k] = paste0("(", round(cutLower[k], 1), ",", round(cutUpper[k], 1), "]")
    } else {
      segmentLabel[k] = paste0("(", round(cutLower[k], 1), ",", round(cutUpper[k], 1), ")")
    }
  }
  
  exposureRate = colSums(object$usageMatrix) / nj
  exposureRateSegment = vector("list", nSegment)
  names(exposureRateSegment) = segmentLabel
  
  for (k in 1:nSegment) {
    if (object$eligibilityStats$n_jk[k] == 0) {
      exposureRateSegment[[k]] = numeric(ni)
    } else {
      exposureRateSegment[[k]] = object$eligibilityStats$alpha_ijk[k, ] / object$eligibilityStats$n_jk[k]
    }
  }
  
  plotER = function(ni, exposureRate, maxRate = maxRate, title = NULL) {
    plot(1:ni, sort(exposureRate, decreasing = TRUE), type = "n", lwd = 2, ylim = c(0, 1), xlab = "Item", ylab = "Exposure Rate", main = title)
    lines(1:ni, sort(exposureRate, decreasing = TRUE), type = "l", lty = 1, lwd = 2, col = "blue")
    points(1:ni, sort(exposureRate, decreasing = TRUE), type = "h", lwd = 1, col = "blue")
    abline(h = maxRate, col="gray")
  }
  
  if (!is.null(pdfFile)) {
    pdf(file = pdfFile, width = width, height = height)
  }
  
  par(mfrow = mfrow)
  
  #plotER(ni, exposureRate, maxRate = maxRate, title = "Overall")
  plotER(ni, exposureRate, maxRate = maxRate, title = paste0("Overall (N = ", nj, ")"))

  for (k in 1:config@exposureControl$nSegment) {
    #plotER(ni, exposureRateSegment[[k]], maxRate = maxRate, title = segmentLabel[k])
    plotER(ni, exposureRateSegment[[k]], maxRate = maxRate, title = paste0(segmentLabel[k], " (n = ", round(object$eligibilityStats$n_jk[k], 1), ")"))
  }
  
  if (!is.null(pdfFile)) {
    dev.off()
  }
  
  return(exposureRateSegment)
}

#' plotExposureRate
#' 
#' plotExposureRate
#' 
#' @param object A nice parameter
#' @param config A nice parameter
#' @param maxRate A nice parameter
#' @param theta A nice parameter
#' @param segmentCut A nice parameter
#' @param color A nice parameter
#' @param pdfFile A nice parameter
#' @param width A nice parameter
#' @param height A nice parameter
#' @param mfrow A nice parameter
#' 
#' @export
plotExposureRate = function(object, config = NULL, maxRate = 0.25, theta = "Estimated", segmentCut = NULL, color = "red", pdfFile = NULL, width = 7, height = 6, mfrow = c(2, 4)) {
  trueTheta = object$trueTheta
  estTheta = object$finalThetaEst
  nj = length(trueTheta)
  ni = ncol(object$usageMatrix)
  
  if (is.null(config)) {
    config = object$config
  }
  
  if (is.null(segmentCut)) {
    segmentCut = config@exposureControl$segmentCut
  }
  
  nSegment = length(segmentCut) - 1
  cutLower = segmentCut[1:nSegment]
  cutUpper = segmentCut[2:(nSegment + 1)]
  segmentLabel = character(nSegment)
  thetaSegmentIndex = numeric(nj)
  
  if (toupper(theta) == "TRUE") {
    for (k in 1:nSegment) {
      thetaSegmentIndex[trueTheta > cutLower[k] & trueTheta <= cutUpper[k]] = k
    }
  } else {
    for (k in 1:nSegment) {
      thetaSegmentIndex[estTheta > cutLower[k] & estTheta <= cutUpper[k]] = k
    }
  }
  
  for (k in 1:nSegment) {
    if (k < nSegment ) {
      segmentLabel[k] = paste0("(", cutLower[k], ",", cutUpper[k], "]")
    } else {
      segmentLabel[k] = paste0("(", cutLower[k], ",", cutUpper[k], ")")
    }
  }
  
  exposureRate = colSums(object$usageMatrix) / nj
  
  exposureRateSegment = vector("list", nSegment)
  names(exposureRateSegment) = segmentLabel
  
  for (k in 1:nSegment) {
    if (sum(thetaSegmentIndex == k) > 2) {
      exposureRateSegment[[k]] = colMeans(object$usageMatrix[thetaSegmentIndex == k, ])
    }
    if (is.null(exposureRateSegment[[k]])) {
      exposureRateSegment[[k]] = numeric(ni)
    } else if (any(is.nan(exposureRateSegment[[k]]))) {
      exposureRateSegment[[k]][is.nan(exposureRateSegment[[k]])] = 0
    }
  }
  
  plotER = function(ni, exposureRate, maxRate = maxRate, title = NULL) {
    plot(1:ni, sort(exposureRate, decreasing = TRUE), type = "n", lwd = 2, ylim = c(0, 1), xlab = "Item", ylab = "Exposure Rate", main = title)
    lines(1:ni, sort(exposureRate, decreasing = TRUE), type = "l", lty = 1, lwd = 2, col = color)
    points(1:ni, sort(exposureRate, decreasing = TRUE), type = "h", lwd = 1, col = color)
    abline(h = maxRate, col="gray")
  }
  
  if (!is.null(pdfFile)) {
    pdf(file = pdfFile, width = width, height = height)
  }
  
  par(mfrow = mfrow)
  
  plotER(ni, exposureRate, maxRate = maxRate, title = paste0("Overall (N = ", nj, ")"))
 
  for (k in 1:nSegment) {
    plotER(ni, exposureRateSegment[[k]], maxRate = maxRate, title = paste0(segmentLabel[k], " (n = ", sum(thetaSegmentIndex == k), ")"))
  }
  
  if (!is.null(pdfFile)) {
    dev.off()
  }
  
  return(list(exposureRate = exposureRate, exposureRateSegment = exposureRateSegment))
}

#' plotExposureRateFinal
#' 
#' plotExposureRateFinal
#' 
#' @param object A nice parameter
#' @param config A nice parameter
#' @param maxRate A nice parameter
#' @param theta A nice parameter
#' @param segmentCut A nice parameter
#' @param color A nice parameter
#' @param pdfFile A nice parameter
#' @param width A nice parameter
#' @param height A nice parameter
#' @param mfrow A nice parameter
#' @param burnIn A nice parameter
#' @param retain A nice parameter
#' 
#' @export
plotExposureRateFinal = function(object, config =NULL, maxRate = 0.25, theta = "Estimated", segmentCut = NULL, color = "red", pdfFile = NULL, width = 7, height = 6, mfrow = c(2, 4), burnIn = 0, retain = NULL) {
  #object is an instance of the Shadow.output class
  trueTheta = object$trueTheta
  estTheta = object$finalThetaEst
  nj = length(trueTheta)
  
  if (burnIn > 0) {
    if (toupper(theta) == "TRUE") {
      retained = object$trueSegmentCount > burnIn
    } else {
      retained = object$estSegmentCount > burnIn
    }
  } else if (!is.null(retain)) { 
    retained = (1:nj) %in% retain
  } else {
    retained = rep(TRUE, nj)
  }
 
  nRetained = sum(retained)
  ni = ncol(object$usageMatrix)
  
  if (is.null(config)) {
    config = object$config
  }
  
  if (is.null(segmentCut)) {
    segmentCut = config@exposureControl$segmentCut
  }
  
  nSegment = length(segmentCut) - 1
  cutLower = segmentCut[1:nSegment]
  cutUpper = segmentCut[2:(nSegment + 1)]
  segmentLabel = character(nSegment)
  
  thetaSegmentIndex = numeric(sum(retained))
  
  if (toupper(theta) == "TRUE") {
    thetaSegmentIndex = findSegment(segmentCut, trueTheta[retained])
  } else {
    thetaSegmentIndex = findSegment(segmentCut, estTheta[retained])
  }
  
  segmentN = numeric(nSegment) #the number of examinees classified into each segment based on their final posterior distribution
  segmentDist = table(thetaSegmentIndex)
  segmentN[as.numeric(names(segmentDist))] = segmentDist
  segmentIndexTable = matrix(NA, nRetained, object$Constraints$testLength)

  for (k in 1:nSegment) {
    if (k < nSegment ) {
      segmentLabel[k] = paste0("(", cutLower[k], ",", cutUpper[k], "]")
    } else {
      segmentLabel[k] = paste0("(", cutLower[k], ",", cutUpper[k], ")")
    }
  }
  
  usageMatrix = object$usageMatrix[retained, ]
  usageMatrixFinal = object$usageMatrix[retained, ]
  
  idx = 0
  for (j in 1:nj) {
    if (retained[j]) {
      idx = idx + 1
      usageMatrixFinal[idx, object$output[[j]]@administeredItemIndex[object$output[[j]]@thetaSegmentIndex != thetaSegmentIndex[idx]]] = FALSE
      segmentIndexTable[idx, ] = object$output[[j]]@thetaSegmentIndex
    }
  }
  
  segmentFreq = matrix(0, nSegment, nSegment)
  
  for (i in 1:object$Constraints$testLength) {
    segmentTable = tapply(segmentIndexTable[, i], thetaSegmentIndex, table)
    for (s in 1:nSegment) {
      segmentFreq[s, as.numeric(names(segmentTable[[s]]))] = segmentFreq[s, as.numeric(names(segmentTable[[s]]))] + segmentTable[[s]]
    }
  }
  
  segmentRate = segmentFreq / segmentN #eqch column of segmentFreq is divided by segmentN
  
  segmentRateTable = data.frame(segmentClass = factor(rep(segmentLabel, rep(nSegment, nSegment)), levels = segmentLabel), segment = rep(1:nSegment, nSegment), avgVisit = matrix(t(segmentRate), nrow = nSegment^2, ncol = 1))
  
  exposureRate = colSums(usageMatrix) / nRetained
  exposureRateFinal = colSums(usageMatrixFinal) / nRetained
  
  exposureRateSegment = vector("list", nSegment)
  exposureRateSegmentFinal = vector("list", nSegment)
  names(exposureRateSegment) = segmentLabel
  names(exposureRateSegmentFinal) = segmentLabel
  
  for (k in 1:nSegment) {
    if (segmentN[k] > 2) {
      exposureRateSegment[[k]] = colMeans(usageMatrix[thetaSegmentIndex == k, ])
    }
    if (is.null(exposureRateSegment[[k]])) {
      exposureRateSegment[[k]] = numeric(ni)
    } else if (any(is.nan(exposureRateSegment[[k]]))) {
      exposureRateSegment[[k]][is.nan(exposureRateSegment[[k]])] = 0
    }
  }
  
  for (k in 1:nSegment) {
    if (segmentN[k] > 2) {
      exposureRateSegmentFinal[[k]] = colMeans(usageMatrixFinal[thetaSegmentIndex == k, ])
    }
    if (is.null(exposureRateSegmentFinal[[k]])) {
      exposureRateSegmentFinal[[k]] = numeric(ni)
    } else if (any(is.nan(exposureRateSegmentFinal[[k]]))) {
      exposureRateSegmentFinal[[k]][is.nan(exposureRateSegmentFinal[[k]])] = 0
    }
  }
  
  plotER = function(ni, exposureRate, exposureRateFinal, maxRate = maxRate, title = NULL) {
    ER.order = order(exposureRate, decreasing = TRUE)
    exposureRateOrdered = exposureRate[ER.order]
    exposureRateFinalOrdered = exposureRateFinal[ER.order]
    plot(1:ni, exposureRateOrdered, type = "n", lwd = 2, ylim = c(0, 1), xlab = "Item", ylab = "Exposure Rate", main = title)
    lines(1:ni, exposureRateOrdered, type = "l", lty = 1, lwd = 2, col = color)
    points(1:ni, exposureRateOrdered, type = "h", lwd = 1, col = color)
    points(1:ni, exposureRateFinalOrdered, type = "h", lwd = 1, lty = 1, col = "yellow")
    abline(h = maxRate, col="gray")
  }
  
  if (!is.null(pdfFile)) {
    pdf(file = pdfFile, width = width, height = height)
  }
  
  par(mfrow = mfrow)
  
  plotER(ni, exposureRate, exposureRateFinal, maxRate = maxRate, title = paste0("Overall (N = ", nRetained, ")"))
  
  for (k in 1:nSegment) {
    plotER(ni, exposureRateSegment[[k]], exposureRateSegmentFinal[[k]], maxRate = maxRate, title = paste0(segmentLabel[k], " (n = ", segmentN[k], ")"))
  }
  
  if (!is.null(pdfFile)) {
    dev.off()
  }
  
  return(list(exposureRate = exposureRate, exposureRateSegment = exposureRateSegment, exposureRateSegmentFinal = exposureRateSegmentFinal, segmentRateTable = segmentRateTable, nSegment = nSegment, segmentN = segmentN, segmentCut = segmentCut, segmentLabel = segmentLabel))
}

#' plotExposureRateFinalFlag
#' 
#' plotExposureRateFinalFlag
#' 
#' @param object A nice parameter
#' @param pool A nice parameter
#' @param theta A nice parameter
#' @param flagCriterior A nice parameter
#' @param pdfFile A nice parameter
#' @param width A nice parameter
#' @param height A nice parameter
#' @param color A nice parameter
#' @param mfrow A nice parameter
#' 
#' @export
plotExposureRateFinalFlag = function(object, pool, theta = seq(-3, 3, .1), flagCriterior = 0.4, pdfFile = NULL, width = 7, height = 6, color = "red", mfrow = c(2, 4)) {
  #object is an output from plotExposureRateFinal
  #pool is of class item.pool
  
  Info = calcFisher(pool, theta)
  ni = pool@ni
  nSegment = object$nSegment
  segmentCut = object$segmentCut
  segmentCut[1] = min(theta)
  segmentCut[length(segmentCut)] = max(theta)
  segmentLabel = object$segmentLabel
  
  itemsFlaggedSegment = lapply(seq_len(object$nSegment), function(j) which(object$exposureRateSegment[[j]] > flagCriterior))
  
  if (!is.null(pdfFile)) {
    pdf(file = pdfFile, width = width, height = height)
  }
  
  par(mfrow = mfrow)
  
  for (k in 1:nSegment) {
    thetaSegmentRange = which(theta >= segmentCut[k] & theta <= segmentCut[k + 1])
    thetaSegmentRangeOutside = which(theta <= segmentCut[k] | theta >= segmentCut[k + 1])
    
    plot(theta, Info[, 1], xlab = "Theta", ylab = "Info", main = segmentLabel[k], type = "n", ylim = c(0, max(Info)))
    
    for (i in 1:ni) {
      lines(theta, Info[, i], col = "light grey", lwd = 0.5)
      lines(theta[thetaSegmentRange], Info[thetaSegmentRange, i], col = "grey", lwd = 1.0)
    }
    
    itemsFlagged = itemsFlaggedSegment[[k]]
      
    if (length(itemsFlagged) > 0) {
      for (i in itemsFlagged) {
        lines(theta[thetaSegmentRange], Info[thetaSegmentRange, i], col = color, lwd = 2)
        lines(theta[thetaSegmentRangeOutside], Info[thetaSegmentRangeOutside, i], col = color, lwd = 1)
      }
    }
    abline(v = segmentCut[k], col = "dark grey")
    abline(v = segmentCut[k + 1], col = "dark grey")
  }
  
  if (!is.null(pdfFile)) {
    dev.off()
  }
  
  return(itemsFlaggedSegment)
}

#' plotAvgSegmentVisit
#' 
#' plotAvgSegmentVisit
#' 
#' @param object A nice parameter
#' @param pdfFile A nice parameter
#' @param width A nice parameter
#' @param height A nice parameter
#' 
#' @export
plotAvgSegmentVisit = function(object, pdfFile = NULL, width = 7, height = 6) {
  #object is an output from plotExposureRateFinal
  #lattice xyplot is not automatically generated within a function; it needs to be printed using the print() function
  
  if (!is.null(pdfFile)) {
    pdf(file = pdfFile, width = width, height = height)
  } else {
    dev.new()
  }
  
  plotAvgVisit = xyplot(avgVisit ~ segment | segmentClass, object$segmentRateTable, xlab = "Theta Segment", ylab = "Average Number of Visits", type = c("b", "g"))
  print(plotAvgVisit)
  
  if (!is.null(pdfFile)) {
    dev.off()
  } else {
    return(print(plotAvgVisit))
  }
}

#' plotInfo
#' 
#' plotInfo
#' 
#' @param object A nice parameter
#' @param theta A nice parameter
#' @param infoType A nice parameter
#' @param select A nice parameter
#' @param pdfFile A nice parameter
#' @param color A nice parameter
#' @param width A nice parameter
#' @param height A nice parameter
#' @param mfrow A nice parameter
#' 
#' @export
plotInfo = function(object, theta, infoType = "FISHER", select = NULL, pdfFile = NULL, color = "blue", width = 7, height = 6, mfrow = c(2, 4)) {
  #object is of class item.pool
  
  if (toupper(infoType) == "FISHER") {
    Info = calcFisher(object, theta)
  } else {
    #TODO: other info
    stop("Invalid infoType specified")
  }
  
  if (!is.null(pdfFile)) {
    pdf(file = pdfFile, width = width, height = height)
  }
  
  par(mfrow = mfrow)
  
  items = 1:object@ni
  
  if (!is.null(select) && all(select %in% items)) {
    items = select
  }
  
  for (i in items) {
    plot(theta, Info[, i], xlab = "Theta", ylab = "Info", main = object@ID[i], type = "l", col = color, ylim = c(0, max(Info)))
  }
  
  if (!is.null(pdfFile)) {
    dev.off()
  }
}

#' plotInfoOverlay
#' 
#' plotInfoOverlay
#' 
#' @param object A nice parameter
#' @param theta A nice parameter
#' @param infoType A nice parameter
#' @param select A nice parameter
#' @param pdfFile A nice parameter
#' @param color A nice parameter
#' @param width A nice parameter
#' @param height A nice parameter
#' 
#' @export
plotInfoOverlay = function(object, theta, infoType = "FISHER", select = NULL, pdfFile = NULL, color = "red", width = 7, height = 6) {
  if (toupper(infoType) == "FISHER") {
    Info = calcFisher(object, theta)
  } else {
    #TODO: other info
    stop("Invalid infoType specified")
  }
  
  if (!is.null(pdfFile)) {
    pdf(file = pdfFile, width = width, height = height)
  }
  
  items = 1:object@ni
  
  if (!is.null(select) && all(select %in% items)) {
    items = select
  }
  
  plot(theta, Info[, 1], xlab = "Theta", ylab = "Info", main = "", type = "n", ylim = c(0, max(Info)))
  
  for (i in 1:object@ni) {
    lines(theta, Info[, i], col = "light grey", lwd = 0.5)
  }
    
  for (i in items) {
    lines(theta, Info[, i], col = color, lwd = 2)
  }
  
  if (!is.null(pdfFile)) {
    dev.off()
  }
}

#' checkExposureRate
#' 
#' checkExposureRate
#' 
#' @param object A nice parameter
#' @param config A nice parameter
#' 
#' @export
checkExposureRate = function(object, config) {
  par(mfrow = c(2, 4), oma = c(3, 3, 0, 0), mar = c(3, 2, 2, 1) + 0.1)
  
  plotExposureRateBySegment(object, config, maxRate = 0.2)
  mtext("Item", side = 1, line = 1, outer = TRUE, cex = 1.2)
  mtext(expression(paste("Exposure Rate (", alpha[ijk]/n[jk], ")")), side = 2, line = 1, outer = TRUE, cex = 1.2)
  
  plotExposureRate(object, config, theta = "Estimated")
  mtext("Item", side = 1, line = 1, outer = TRUE, cex = 1.2)
  mtext("Exposure Rate", side = 2, line = 1, outer = TRUE, cex = 1.2)
  
  plotExposureRate(object, config, theta = "True")
  mtext("Item", side = 1, line = 1, outer = TRUE, cex = 1.2)
  mtext("Exposure Rate", side = 2, line = 1, outer = TRUE, cex = 1.2)
}

#' lnHyperPars
#' 
#' lnHyperPars
#' 
#' @param mean A nice parameter
#' @param sd A nice parameter
#' 
#' @export
#calculate hyperparameters for lognormal distribution
lnHyperPars = function(mean, sd) {
  location = log(mean^2 / sqrt(sd^2 + mean^2))
  scale = sqrt(log(1 + sd^2 / mean^2))
  return(c(location, scale))
}

#' logitHyperPars
#' 
#' logitHyperPars
#' 
#' @param mean A nice parameter
#' @param sd A nice parameter
#' 
#' @export
#calculate hyperparameters for logit-normal distribution
logitHyperPars = function(mean, sd) {
  logitSamples = numeric(10000)
  counter = 0
  while(counter < 10000) {
    normSample = rnorm(1, mean, sd) 
    if(normSample >= 0 && normSample <= 1) {
      counter = counter + 1
      logitSamples[counter] = logit(normSample)
    }
  }
  return(c(mean(logitSamples), sd(logitSamples)))
}

#' iparPosteriorSample
#' 
#' iparPosteriorSample
#' 
#' @param pool A nice parameter
#' @param nSample A nice parameter
#' 
#' @export
iparPosteriorSample = function(pool, nSample = 500) {
  requireNamespace("logitnorm")
  
  iparList = vector(mode = "list", length = pool@ni)
  
  for (i in 1:pool@ni) {
    if (pool@model[i] == "item.1pl") {
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = 1)
      iparList[[i]][, 1] = rnorm(nSample, pool@ipar[i, 1], pool@SEs[i, 1])
    } else if (pool@model[i] == "item.2pl") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = 2)
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      iparList[[i]][, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
    } else if (pool@model[i] == "item.3pl") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      cHP = logitHyperPars(pool@ipar[i, 3], pool@SEs[i, 3])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = 3)
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      iparList[[i]][, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
      iparList[[i]][, 3] = rlogitnorm(nSample, mu = cHP[1], sigma = cHP[2])
    } else if (pool@model[i] == "item.pc") {
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        iparList[[i]][, k] = rnorm(nSample, pool@ipar[i, k], pool@SEs[i, k])
      }
    } else if (pool@model[i] == "item.gpc") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        iparList[[i]][, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
    } else if (pool@model[i] == "item.gr") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        iparList[[i]][, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
      for (s in 1:nSample) {
        if (is.unsorted(iparList[[i]][s, 2:pool@NCAT[i]])) {
          iparList[[i]][s, 2:pool@NCAT[i]] = sort(iparList[[i]][s, 2:pool@NCAT[i]])
        }
      }
    }
  }
  return(iparList)
}

#' iparPosteriorSample_foreach
#' 
#' iparPosteriorSample_foreach
#' 
#' @param pool A nice parameter
#' @param nSample A nice parameter
#' 
#' @export
iparPosteriorSample_foreach = function(pool, nSample = 500) {
  requireNamespace("logitnorm")
  
  i = 0
  iparList = foreach(i = 1:pool@ni, .export=c("lnHyperPars", "logitHyperPars"), .packages = "logitnorm") %dopar% {
    if (pool@model[i] == "item.1pl") {
      out = matrix(NA, nrow = nSample, ncol = 1)
      out[, 1] = rnorm(nSample, pool@ipar[i, 1], pool@SEs[i, 1])
      return(out)
    } else if (pool@model[i] == "item.2pl") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      out = matrix(NA, nrow = nSample, ncol = 2)
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      out[, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
      return(out)
    } else if (pool@model[i] == "item.3pl") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      cHP = logitHyperPars(pool@ipar[i, 3], pool@SEs[i, 3])
      out = matrix(NA, nrow = nSample, ncol = 3)
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      out[, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
      out[, 3] = rlogitnorm(nSample, mu = cHP[1], sigma = cHP[2])
      return(out)
    } else if (pool@model[i] == "item.pc") {
      out = matrix(NA, nrow = nSample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        out[, k] = rnorm(nSample, pool@ipar[i, k], pool@SEs[i, k])
      }
      return(out)
    } else if (pool@model[i] == "item.gpc") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      out = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        out[, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
      return(out)
    } else if (pool@model[i] == "item.gr") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      out = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        out[, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
      for (s in 1:nSample) {
        if (is.unsorted(out[s, 2:pool@NCAT[i]])) {
          out[s, 2:pool@NCAT[i]] = sort(iparList[[i]][s, 2:pool@NCAT[i]])
        }
      }
      return(out)
    }
  }
  
  return(iparList)
}

#' iparPosteriorSample_lapply
#' 
#' iparPosteriorSample_lapply
#' 
#' @param pool A nice parameter
#' @param nSample A nice parameter
#' 
#' @export
iparPosteriorSample_lapply = function(pool, nSample = 500) {
  requireNamespace("logitnorm")
  
  iparList = lapply(1:pool@ni, function(i) {
    if (pool@model[i] == "item.1pl") {
      out = matrix(NA, nrow = nSample, ncol = 1)
      out[, 1] = rnorm(nSample, pool@ipar[i, 1], pool@SEs[i, 1])
      return(out)
    } else if (pool@model[i] == "item.2pl") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      out = matrix(NA, nrow = nSample, ncol = 2)
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      out[, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
      return(out)
    } else if (pool@model[i] == "item.3pl") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      cHP = logitHyperPars(pool@ipar[i, 3], pool@SEs[i, 3])
      out = matrix(NA, nrow = nSample, ncol = 3)
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      out[, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
      out[, 3] = rlogitnorm(nSample, mu = cHP[1], sigma = cHP[2])
      return(out)
    } else if (pool@model[i] == "item.pc") {
      out = matrix(NA, nrow = nSample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        out[, k] = rnorm(nSample, pool@ipar[i, k], pool@SEs[i, k])
      }
      return(out)
    }else if (pool@model[i] == "item.gpc") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      out = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        out[, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
      return(out)
    }  else if (pool@model[i] == "item.gr") {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      out = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      out[, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        out[, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
      for (s in 1:nSample) {
        if (is.unsorted(out[s, 2:pool@NCAT[i]])) {
          out[s, 2:pool@NCAT[i]] = sort(iparList[[i]][s, 2:pool@NCAT[i]])
        }
      }
      return(out)
    }
  })
  
  return(iparList)
}

#' iparPosteriorSample_which
#' 
#' iparPosteriorSample_which
#' 
#' @param pool A nice parameter
#' @param nSample A nice parameter
#' 
#' @export
iparPosteriorSample_which = function(pool, nSample = 500) {
  requireNamespace("logitnorm")
  
  iparList = vector(mode = "list", length = pool@ni)
  
  items.1pl = which(pool@model == "item.1pl")
  items.2pl = which(pool@model == "item.1pl")
  items.3pl = which(pool@model == "item.3pl")
  items.pc  = which(pool@model == "item.pc")
  items.gpc = which(pool@model == "item.gpc")
  items.gr  = which(pool@model == "item.gr")
  
  if (length(items.1pl) > 0) {
    for (i in items.1pl) {
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = 1)
      iparList[[i]][, 1] = rnorm(nSample, pool@ipar[i, 1], pool@SEs[i, 1])
    }
  }
  
  if (length(items.2pl) > 0) {
    for (i in items.2pl) {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = 2)
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      iparList[[i]][, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
    }
  }
  
  if (length(items.3pl) > 0) {
    for (i in items.3pl) {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      cHP = logitHyperPars(pool@ipar[i, 3], pool@SEs[i, 3])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = 3)
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      iparList[[i]][, 2] = rnorm(nSample, pool@ipar[i, 2], pool@SEs[i, 2])
      iparList[[i]][, 3] = rlogitnorm(nSample, mu = cHP[1], sigma = cHP[2])
    }
  }
  
  if (length(items.pc) > 0) {
    for (i in items.pc) {
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        iparList[[i]][, k] = rnorm(nSample, pool@ipar[i, k], pool@SEs[i, k])
      }
    }
  }
  
  if (length(items.gpc) > 0) {
    for (i in items.gpc)  {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        iparList[[i]][, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
    }
  }
  
  if (length(items.gr) > 0) {
    for (i in items.gr) {
      aHP = lnHyperPars(pool@ipar[i, 1], pool@SEs[i, 1])
      iparList[[i]] = matrix(NA, nrow = nSample, ncol = pool@NCAT[i])
      iparList[[i]][, 1] = rlnorm(nSample, aHP[1], aHP[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        iparList[[i]][, k + 1] = rnorm(nSample, pool@ipar[i, k + 1], pool@SEs[i, k + 1])
      }
      for (s in 1:nSample) {
        if (is.unsorted(iparList[[i]][s, 2:pool@NCAT[i]])) {
          iparList[[i]][s, 2:pool@NCAT[i]] = sort(iparList[[i]][s, 2:pool@NCAT[i]])
        }
      }
    }
  }

  return(iparList)
}

#' maxinfo_plot
#' 
#' maxinfo_plot
#' 
#' @param pool A nice parameter
#' @param const A nice parameter
#' 
#' @export
maxinfo_plot = function(pool, const){
  idx.nitems = which(toupper(const$Constraints[['WHAT']]) == "ITEM" &
                     toupper(const$Constraints[['CONDITION']]) == "")
  n.items = const$Constraints[idx.nitems,]['LB'][1,1]
  
  continuum = seq(-3, 3, .5)
  max.info = min.info = continuum * 0
  
  for(i in 1:length(continuum)){
    max.info[i] = sum(sort(calcFisher(pool, continuum[i]), T)[1:n.items])
    min.info[i] = sum(sort(calcFisher(pool, continuum[i]), F)[1:n.items])
  }
  
  pdf(NULL, bg = 'white')
  dev.control(displaylist="enable")
  
  plot(0, 0, type = 'n', xlim = c(-3, 3), ylim = c(0, max(max.info)),
       xlab = 'theta', ylab = 'Information')
  lines(continuum, max.info, lty = 2, lwd = 2)
  lines(continuum, min.info, lty = 2, lwd = 2)
  grid()
  
  p = recordPlot()
  plot.new()
  dev.off()
  
  return(p)
}


