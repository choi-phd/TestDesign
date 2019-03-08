# Documentation progress
# Phase 2. Add simple descriptions: COMPLETE

#' An S4 generic and its methods for Automated Test Assembly (ATA).
#'
#' @param config An \code{\linkS4class{ATA.config}} object containing configuration options.
#' @param Constraints A list representing optimization constraints. Use \code{\link{LoadConstraints}} for this.
#' @param plot Logical. Draws Fisher information plot from the selected items.
#' @param plotrange Numeric. A vector of length 2 containing the lower and upper bounds of plot range. Default is \code{c(-3, 3)}.
#' @export
#' @docType methods
#'
#' @rdname ATA-methods

setGeneric(name = "ATA",
           def = function(config, Constraints, plot, plotrange = c(-3, 3)) {
             standardGeneric("ATA")
           }
)

#' Run Automated Test Assembly
#'
#' Perform Automated Test Assembly from specified configurations.
#'
#' @param config An \code{\linkS4class{ATA.config}} object containing configuration options.
#' @param Constraints A list representing optimization constraints. Use \code{\link{LoadConstraints}} for this.
#' @param plot Logical. If \code{TRUE}, draws Fisher information plot from the selected items.
#' @param plotrange Numeric. A vector of length 2 containing the lower and upper bounds of plot range. Default is \code{c(-3, 3)}.
#'
#' @return A list containing the following entries:
#' \itemize{
#'   \item{MIP} Foo.
#'   \item{Selected} Foo.
#'   \item{solver} Foo.
#'   \item{obj.value} Foo.
#'   \item{solve.time} Foo.
#' }
#'
#' @export

# list(Constraints = Constraints, ListConstraints = ListConstraints, pool = pool, ItemAttrib = ItemAttrib, StAttrib = StAttrib,
#      testLength = testLength, nv = nv, ni = ni, ns = ns, ID = ID, INDEX = INDEX, MAT = MAT, DIR = DIR, RHS = RHS, setBased = setBased,
#      ItemOrder = ItemOrder, ItemOrderBy = ItemOrderBy, StimulusOrder = StimulusOrder, StimulusOrderBy = StimulusOrderBy,
#      ItemIndexByStimulus = item.index.by.stimulus, StimulusIntexByItem = stimulus.index.by.item)
# Fisher = matrix(NA, length(theta), object@ni)

setMethod(f = "ATA",
          signature = c("ATA.config"),
          definition = function(config, Constraints, plot, plotrange = c(-3, 3)) {
            if (!validObject(config)) {
              stop("invalid configuration options specified")
            }
            nt = length(config@itemSelection$targetLocation) #number of target points
            mat = Constraints$MAT
            dir = Constraints$DIR
            rhs = Constraints$RHS
            nv = Constraints$nv
            ni = Constraints$ni
            types = rep("B", nv)
            obj = numeric(nv)
            if (toupper(config@itemSelection$method) == "MAXINFO") {
              objective = as.vector(config@itemSelection$targetWeight %*% calcFisher(Constraints$pool, config@itemSelection$targetLocation))
              obj[1:ni] = objective
              maximize = TRUE
            } else {
              maximize = FALSE
              mat = cbind(mat, numeric(nrow(mat))) #add a column of 0's to the right of mat
              types = c(types, "C")
              obj = c(rep(0, nv), 1)
              if (toupper(config@itemSelection$method) == "TIF") {
                objective = calcFisher(Constraints$pool, config@itemSelection$targetLocation) #matrix of dim c(nt, ni)
              } else if (toupper(config@itemSelection$method) == "TCC") {
                objective = sapply(Constraints$pool@parms, calcEscore, config@itemSelection$targetLocation) #matrix of dim c(nt, ni)
                if(nt == 1) objective = t(as.matrix(objective))
              }
              for (k in 1:nt) {
                add.mat = matrix(0, nrow = 2, ncol = nv + 1)
                add.mat[1, 1:ni] = objective[k, ]
                add.mat[2, 1:ni] = objective[k, ]
                add.mat[1, nv + 1] = -1
                add.mat[2, nv + 1] = 1
                mat = rbind(mat, add.mat)
                dir = c(dir, c("<=", ">="))
                rhs = c(rhs, rep(config@itemSelection$targetValue[k], 2))
              }
              add.mat = c(rep(0, nv), 1)
              mat = rbind(mat, add.mat)
              dir = c(dir, ">=")
              rhs = c(rhs, 0) #non-negative constraint on the real-valued decision variable
            }

            solve.time = proc.time()
            if (toupper(config@MIP$solver) == "SYMPHONY") {
              if (!maximize){
                len_rhs = length(rhs)
                rhs[len_rhs] = config@MIP$gapLimit
              }
              MIP = Rsymphony_solve_LP(obj, mat, dir, rhs, max = maximize, types = types,
                                       verbosity = config@MIP$verbosity, time_limit = config@MIP$timeLimit, gap_limit = config@MIP$gapLimit)
              status = MIP$status
              if (!names(status) %in% c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND")) {
                warning(sprintf("MIP solver returned non-zero status: %s", names(MIP$status)))
                return(list(status = status, MIP = NULL, Selected = NULL))
              }
            } else if (toupper(config@MIP$solver) == "GUROBI") {
              if (!maximize){
                len_rhs = length(rhs)
                rhs[len_rhs] = config@MIP$gapLimit
              }
              constraints.dir = dir
              constraints.dir[constraints.dir == "=="] = "="
              invisible(capture.output(MIP <- gurobi(list(obj = obj, modelsense = ifelse(maximize, "max", "min"), rhs = rhs, sense = constraints.dir, vtype = types, A = mat), params = list(TimeLimit = config@MIP$timeLimit), env = NULL)))
              status = MIP$status
              if (MIP$status != "OPTIMAL") {
                warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
                return(list(status = status, MIP = NULL, Selected = NULL))
              }
              MIP[["solution"]] = MIP$x
            } else if (toupper(config@MIP$solver) == "GLPK") {
              len_rhs = length(rhs)
              rhs[len_rhs] = config@MIP$gapLimit
              MIP = Rglpk_solve_LP(obj, mat, dir, rhs, max = maximize, types = types, control = list(verbose = ifelse(config@MIP$verbosity != -2, TRUE, FALSE), presolve = FALSE, tm_limit = config@MIP$timeLimit * 1000))
              status = MIP$status
              if (MIP$status != 0) {
                warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
                return(list(status = status, MIP = NULL, Selected = NULL))
              }
            } else if (toupper(config@MIP$solver) == "LPSOLVE") {
              if (!maximize){
                len_rhs = length(rhs)
                rhs[len_rhs] = config@MIP$gapLimit
              }
              MIP = lpSolve::lp(direction = ifelse(maximize, "max", "min"), obj, mat, dir, rhs, binary.vec = 1:nv, presolve = TRUE)
              status = MIP$status
              if (MIP$status != 0) {
                warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
                return(list(status = status, MIP = NULL, Selected = NULL))
              }
            } else {
              stop("solver must be Symphony, Gurobi, glpk, or lpSolve")
            }
            solve.time = proc.time() - solve.time
            if (!is.null(Constraints$StimulusOrder)) {
              Constraints$ItemAttrib = merge(Constraints$ItemAttrib, Constraints$StAttrib[c("STINDEX", "STID", Constraints$StimulusOrderBy)], by = "STID", all.x = TRUE, sort = FALSE)
            } else if (!is.null(Constraints$StAttrib)) {
              Constraints$ItemAttrib = merge(Constraints$ItemAttrib, Constraints$StAttrib[c("STINDEX", "STID")], by = "STID", all.x = TRUE, sort = FALSE)
            }
            Selected = Constraints$ItemAttrib[which(MIP$solution[1:Constraints$ni] == 1), ]
            obj.value = sum(obj[which(MIP$solution[1:Constraints$ni] == 1)])
            if (!is.null(Constraints$ItemOrderBy) && !is.null(Constraints$StimulusOrderBy)) {
              Selected = Selected[order(Selected[[Constraints$StimulusOrderBy]], Selected[["STID"]], Selected[[Constraints$ItemOrderBy]]), ]
            } else if (!is.null(Constraints$ItemOrderBy)) {
              if (Constraints$setBased) {
                Selected = Selected[order(Selected[["STID"]], Selected[[Constraints$ItemOrderBy]]), ]
              } else {
                Selected = Selected[order(Selected[[Constraints$ItemOrderBy]]), ]
              }
            } else if (!is.null(Constraints$StimulusOrderBy)) {
              Selected = Selected[order(Selected[[Constraints$StimulusOrderBy]], Selected[["STID"]]), ]
            }
            row.names(Selected) = 1:nrow(Selected)

            if (plot){
              continuum = seq(plotrange[1], plotrange[2], .1)
              continuum = sort(c(continuum, config@itemSelection$targetLocation))

              idx = which(MIP$solution[1:ni] == 1)
              if (toupper(config@itemSelection$method) == "TIF"){
                mat.sub = calcFisher(Constraints$pool, continuum)[,idx]
                vec.sub = apply(mat.sub, 1, sum)
                ylab = "Information"
                title = "Test Information Function based on the selected items"
              }
              if (toupper(config@itemSelection$method) == "TCC"){
                l = calcProb(Constraints$pool, continuum)[idx]
                for(i in 1:length(l)){
                  prob.mat = l[[i]]
                  max.score = dim(prob.mat)[2] - 1
                  prob.mat = prob.mat * matrix(c(0:max.score), dim(prob.mat)[1], dim(prob.mat)[2], byrow = T)
                  l[[i]] = apply(prob.mat, 1, sum)
                }
                vec.sub = Reduce('+', l)
                ylab = "Expected Score"
                title = "Test Characteristic Function based on the selected items"
              }
              if (toupper(config@itemSelection$method) == "MAXINFO"){
                mat.sub = calcFisher(Constraints$pool, continuum)[,idx]
                vec.sub = apply(mat.sub, 1, sum)
                ylab = "Information"
                title = "Test Information Function based on the selected items"
              }

              ymax = max(vec.sub, config@itemSelection$targetValue)

              pdf(NULL, bg = 'white')
              dev.control(displaylist="enable")
              plot(continuum, vec.sub, xlim = c(min(continuum), max(continuum)), ylim = c(0, ymax),
                   main = title, xlab = "Theta", ylab = ylab, type = 'n')
              lines(continuum, vec.sub, lty = 1, lwd = 3)
              if (!maximize) abline(h = config@itemSelection$targetValue, lty = 3, lwd = 2)
              abline(v = config@itemSelection$targetLocation, lty = 3, lwd = 2)
              p = recordPlot()
              plot.new()
              dev.off()
            }

            return(list(MIP = MIP, Selected = Selected, solver = config@MIP$solver, obj.value = obj.value, solve.time = solve.time, plot = p))
          }
)
