#' Run Automated Test Assembly
#'
#' Perform Automated Test Assembly with specified configurations.
#'
#' @param config An \code{\linkS4class{ATA.config}} object containing configuration options. Use \code{\link{CreateStaticTestConfig}} for this.
#' @param constraints A list representing optimization constraints. Use \code{\link{LoadConstraints}} for this.
#' @param plot Logical. If \code{TRUE}, draw Fisher information plot from the selected items.
#' @param plotrange Numeric. A vector of length 2 containing the lower and upper bounds of plot range. The default is \code{c(-3, 3)}.
#'
#' @return A list containing the following entries:
#' \itemize{
#'   \item{\code{MIP}} A list containing the result from MIP solver.
#'   \itemize{
#'     \item{\code{solution}} Solution vector. Each value represents an item. A value of 1 indicates the item was selected.
#'     \item{\code{objval}} Objective value of the solution.
#'     \item{\code{status}} Status value indicating whether an optimal solution was found.
#'   }
#'   \item{\code{Selected}} The attributes of the selected items.
#'   \item{\code{solver}} The name of the MIP solver used in the assembly.
#'   \item{\code{obj.value}} Objective value of the solution. Identical to the one above.
#'   \item{\code{solve.time}} The elapsed time in running the solver.
#' }
#'
#' @docType methods
#' @rdname ATA-methods
#' @export

setGeneric(
  name = "ATA",
  def = function(config, constraints, plot = FALSE, plotrange = c(-3, 3)) {
    standardGeneric("ATA")
  }
)

#' @docType methods
#' @rdname ATA-methods
#' @export

setMethod(
  f = "ATA",
  signature = c("ATA.config"),
  definition = function(config, constraints, plot = FALSE, plotrange = c(-3, 3)) {
    if (!validObject(config)) {
      stop("invalid configuration options.")
    }
    nt <- length(config@itemSelection$targetLocation)
    mat <- constraints$MAT
    dir <- constraints$DIR
    rhs <- constraints$RHS
    nv <- constraints$nv
    ni <- constraints$ni
    types <- rep("B", nv)
    obj <- numeric(nv)
    if (toupper(config@itemSelection$method) == "MAXINFO") {
      objective <- as.vector(config@itemSelection$targetWeight %*% calcFisher(constraints$pool, config@itemSelection$targetLocation))
      obj[1:ni] <- objective
      maximize <- TRUE
    } else {
      maximize <- FALSE
      mat <- cbind(mat, numeric(nrow(mat)))
      types <- c(types, "C")
      obj <- c(rep(0, nv), 1)
      if (toupper(config@itemSelection$method) == "TIF") {
        objective <- calcFisher(
          constraints$pool,
          config@itemSelection$targetLocation
        )
      } else if (toupper(config@itemSelection$method) == "TCC") {
        objective <- sapply(
          constraints$pool@parms, calcEscore,
          config@itemSelection$targetLocation
        )
        if (nt == 1) {
          objective <- t(as.matrix(objective))
        }
      }
      for (k in 1:nt) {
        add.mat            <- matrix(0, nrow = 2, ncol = nv + 1)
        add.mat[1, 1:ni]   <- objective[k, ]
        add.mat[2, 1:ni]   <- objective[k, ]
        add.mat[1, nv + 1] <- -1
        add.mat[2, nv + 1] <- 1
        mat <- rbind(mat, add.mat)
        dir <- c(dir, c("<=", ">="))
        rhs <- c(rhs, rep(config@itemSelection$targetValue[k], 2))
      }
      add.mat <- c(rep(0, nv), 1)
      mat <- rbind(mat, add.mat)
      dir <- c(dir, ">=")
      rhs <- c(rhs, 0)
    }
    solve.time <- proc.time()
    if (toupper(config@MIP$solver) == "SYMPHONY") {
      if (!maximize) {
        len_rhs      <- length(rhs)
        rhs[len_rhs] <- config@MIP$gapLimit
      }
      MIP <- Rsymphony::Rsymphony_solve_LP(obj, mat, dir, rhs,
        max = maximize, types = types,
        verbosity = config@MIP$verbosity,
        time_limit = config@MIP$timeLimit,
        gap_limit = config@MIP$gapLimit
      )
      status <- MIP$status
      if (!names(status) %in% c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND")) {
        warning(sprintf("MIP solver returned non-zero status: %s", names(MIP$status)))
        return(list(status = status, MIP = NULL, Selected = NULL))
      }
    } else if (toupper(config@MIP$solver) == "GUROBI") {
      if (!maximize) {
        len_rhs      <- length(rhs)
        rhs[len_rhs] <- config@MIP$gapLimit
      }
      constraints.dir <- dir
      constraints.dir[constraints.dir == "=="] <- "="
      invisible(capture.output(MIP <- gurobi::gurobi(list(obj = obj, modelsense = ifelse(maximize, "max", "min"), rhs = rhs, sense = constraints.dir, vtype = types, A = mat), params = list(TimeLimit = config@MIP$timeLimit), env = NULL)))
      status <- MIP$status
      if (MIP$status != "OPTIMAL") {
        warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
        return(list(status = status, MIP = NULL, Selected = NULL))
      }
      MIP[["solution"]] <- MIP$x
    } else if (toupper(config@MIP$solver) == "GLPK") {
      len_rhs      <- length(rhs)
      rhs[len_rhs] <- config@MIP$gapLimit
      MIP <- Rglpk_solve_LP(obj, mat, dir, rhs, max = maximize, types = types,
                            control = list(verbose = ifelse(config@MIP$verbosity != -2, TRUE, FALSE), presolve = FALSE, tm_limit = config@MIP$timeLimit * 1000))
      status <- MIP$status
      if (MIP$status != 0) {
        warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
        return(list(status = status, MIP = NULL, Selected = NULL))
      }
    } else if (toupper(config@MIP$solver) == "LPSOLVE") {
      if (!maximize) {
        len_rhs      <- length(rhs)
        rhs[len_rhs] <- config@MIP$gapLimit
      }
      MIP <- lpSolve::lp(direction = ifelse(maximize, "max", "min"), obj, mat, dir, rhs, binary.vec = 1:nv, presolve = TRUE)
      status <- MIP$status
      if (MIP$status != 0) {
        warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
        return(list(status = status, MIP = NULL, Selected = NULL))
      }
    }

    solve.time <- proc.time() - solve.time
    if (!is.null(constraints$StimulusOrder)) {
      constraints$ItemAttrib <- merge(constraints$ItemAttrib,
        constraints$StAttrib[c("STINDEX", "STID", constraints$StimulusOrderBy)],
        by = "STID", all.x = TRUE, sort = FALSE
      )
    } else if (!is.null(constraints$StAttrib)) {
      constraints$ItemAttrib <- merge(constraints$ItemAttrib,
        constraints$StAttrib[c("STINDEX", "STID")],
        by = "STID", all.x = TRUE, sort = FALSE
      )
    }

    selected <- constraints$ItemAttrib[which(MIP$solution[1:constraints$ni] == 1), ]
    obj.value <- sum(obj[which(MIP$solution[1:constraints$ni] == 1)])

    if (!is.null(constraints$ItemOrderBy) && !is.null(constraints$StimulusOrderBy)) {
      selected <- selected[order(selected[[constraints$StimulusOrderBy]], selected[["STID"]], selected[[constraints$ItemOrderBy]]), ]
    } else if (!is.null(constraints$ItemOrderBy)) {
      if (constraints$setBased) {
        selected <- selected[order(selected[["STID"]], selected[[constraints$ItemOrderBy]]), ]
      } else {
        selected <- selected[order(selected[[constraints$ItemOrderBy]]), ]
      }
    } else if (!is.null(constraints$StimulusOrderBy)) {
      selected <- selected[order(selected[[constraints$StimulusOrderBy]], selected[["STID"]]), ]
    }
    row.names(selected) <- 1:nrow(selected)

    if (plot) {
      continuum <- seq(plotrange[1], plotrange[2], .1)
      continuum <- sort(c(continuum, config@itemSelection$targetLocation))
      idx <- which(MIP$solution[1:ni] == 1)

      if (toupper(config@itemSelection$method) == "TIF") {
        mat.sub <- calcFisher(constraints$pool, continuum)[, idx]
        vec.sub <- apply(mat.sub, 1, sum)
        ylab <- "Information"
        title <- "Test Information Function based on the selected items"
      }
      if (toupper(config@itemSelection$method) == "TCC") {
        l <- calcProb(constraints$pool, continuum)[idx]
        for (i in 1:length(l)) {
          prob.mat <- l[[i]]
          max.score <- dim(prob.mat)[2] - 1
          prob.mat <- prob.mat * matrix(c(0:max.score), dim(prob.mat)[1], dim(prob.mat)[2], byrow = T)
          l[[i]] <- apply(prob.mat, 1, sum)
        }
        vec.sub <- Reduce("+", l)
        ylab <- "Expected Score"
        title <- "Test Characteristic Curve based on the selected items"
      }
      if (toupper(config@itemSelection$method) == "MAXINFO") {
        mat.sub <- calcFisher(constraints$pool, continuum)[, idx]
        vec.sub <- apply(mat.sub, 1, sum)
        ylab <- "Information"
        title <- "Test Information Function based on the selected items"
      }

      ymax <- max(vec.sub, config@itemSelection$targetValue)

      pdf(NULL, bg = "white")
      dev.control(displaylist = "enable")

      plot(continuum, vec.sub,
        xlim = c(min(continuum), max(continuum)), ylim = c(0, ymax),
        main = title, xlab = "Theta", ylab = ylab, type = "n"
      )
      lines(continuum, vec.sub, lty = 1, lwd = 3)
      if (!maximize) {
        abline(h = config@itemSelection$targetValue, lty = 3, lwd = 2)
      }
      abline(v = config@itemSelection$targetLocation, lty = 3, lwd = 2)

      p <- recordPlot()
      plot.new()
      dev.off()
    } else {
      p <- NULL
    }

    return(list(
      MIP = MIP, selected = selected,
      solver = config@MIP$solver, obj.value = obj.value,
      solve.time = solve.time, plot = p
    ))
  }
)
