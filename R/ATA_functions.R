#' @include ATA_class.R
NULL

#' Run Automated Test Assembly
#'
#' Perform Automated Test Assembly with specified configurations.
#'
#' @param config An \code{\linkS4class{config_ATA}} object containing configuration options. Use \code{\link{createStaticTestConfig}} for this.
#' @param constraints A list representing optimization constraints. Use \code{\link{loadConstraints}} for this.
#' @param plot Logical. If \code{TRUE}, draw Fisher information plot from the selected items.
#' @param plot_range Numeric. A vector of length 2 containing the lower and upper bounds of plot range. The default is \code{c(-3, 3)}.
#'
#' @return A list containing the following entries:
#' \itemize{
#'   \item{\code{MIP}} A list containing the result from MIP solver.
#'   \itemize{
#'     \item{\code{solution}} Solution vector. Each value represents an item. A value of 1 indicates the item was selected.
#'     \item{\code{objval}} Objective value of the solution.
#'     \item{\code{status}} Status value indicating whether an optimal solution was found.
#'   }
#'   \item{\code{selected}} The attributes of the selected items.
#'   \item{\code{solver}} The name of the MIP solver used in the assembly.
#'   \item{\code{obj_value}} Objective value of the solution. Identical to the one above.
#'   \item{\code{solve_time}} The elapsed time in running the solver.
#' }
#' 
#' @references
#' \insertRef{van_der_linden_linear_2005}{TestDesign}
#'
#' @examples
#' config_science <- createStaticTestConfig(
#'   list(
#'     method = "MAXINFO",
#'     target_location = c(-1, 1)
#'   )
#' )
#' solution <- ATA(config_science, constraints_science, plot = TRUE)
#'
#' @docType methods
#' @rdname ATA-methods
#' @export

setGeneric(
  name = "ATA",
  def = function(config, constraints, plot = FALSE, plot_range = c(-3, 3)) {
    standardGeneric("ATA")
  }
)

#' @docType methods
#' @rdname ATA-methods
#' @export

setMethod(
  f = "ATA",
  signature = c("config_ATA"),
  definition = function(config, constraints, plot = FALSE, plot_range = c(-3, 3)) {

    if (!validObject(config)) {
      stop("invalid configuration options.")
    }

    nt    <- length(config@item_selection$target_location)
    mat   <- constraints$mat
    dir   <- constraints$dir
    rhs   <- constraints$rhs
    nv    <- constraints$nv
    ni    <- constraints$ni
    types <- rep("B", nv)
    obj   <- numeric(nv)

    if (toupper(config@item_selection$method) == "MAXINFO") {

      maximize  <- TRUE
      objective <- as.vector(config@item_selection$target_weight %*% calcFisher(constraints$pool, config@item_selection$target_location))
      obj[1:ni] <- objective

    } else {

      maximize  <- FALSE
      mat       <- cbind(mat, numeric(nrow(mat)))
      types     <- c(types, "C")
      obj       <- c(obj, 1)

      if (toupper(config@item_selection$method) == "TIF") {
        objective <- calcFisher(
          constraints$pool,
          config@item_selection$target_location
        )
      } else if (toupper(config@item_selection$method) == "TCC") {
        objective <- sapply(
          constraints$pool@parms, calcEscore,
          config@item_selection$target_location
        )
        if (nt == 1) {
          objective <- t(as.matrix(objective))
        }
      }

      for (k in 1:nt) {
        add_mat            <- matrix(0, nrow = 2, ncol = nv + 1)
        add_mat[1, 1:ni]   <- objective[k, ]
        add_mat[2, 1:ni]   <- objective[k, ]
        add_mat[1, nv + 1] <- -1
        add_mat[2, nv + 1] <- 1
        mat <- rbind(mat, add_mat)
        dir <- c(dir, c("<=", ">="))
        rhs <- c(rhs, rep(config@item_selection$target_value[k], 2))
      }

      add_mat <- c(rep(0, nv), 1)
      mat <- rbind(mat, add_mat)
      dir <- c(dir, ">=")
      rhs <- c(rhs, 0)

    }

    # Run Solver --------------------------------------------------------------------------------

    solve_time <- proc.time()

    if (toupper(config@MIP$solver) == "SYMPHONY") {

      if (!maximize) {
        len_rhs      <- length(rhs)
        rhs[len_rhs] <- config@MIP$gap_limit
      }

      MIP <- Rsymphony::Rsymphony_solve_LP(obj, mat, dir, rhs,
        max = maximize, types = types,
        verbosity = config@MIP$verbosity,
        time_limit = config@MIP$time_limit,
        gap_limit = config@MIP$gap_limit
      )

    } else if (toupper(config@MIP$solver) == "GUROBI") {

      if (!maximize) {
        len_rhs      <- length(rhs)
        rhs[len_rhs] <- config@MIP$gap_limit
      }
      constraints_dir <- dir
      constraints_dir[constraints_dir == "=="] <- "="

      invisible(capture.output(MIP <- gurobi::gurobi(list(obj = obj, modelsense = ifelse(maximize, "max", "min"), rhs = rhs, sense = constraints_dir, vtype = types, A = mat), params = list(TimeLimit = config@MIP$time_limit), env = NULL)))

      if (isOptimal(MIP$status, config@MIP$solver)) {
        MIP[["solution"]] <- MIP$x
      }

    } else if (toupper(config@MIP$solver) == "GLPK") {

      len_rhs      <- length(rhs)
      rhs[len_rhs] <- config@MIP$gap_limit

      MIP <- Rglpk::Rglpk_solve_LP(obj, mat, dir, rhs, max = maximize, types = types,
                            control = list(verbose = ifelse(config@MIP$verbosity != -2, TRUE, FALSE), presolve = FALSE, tm_limit = config@MIP$time_limit * 1000))

    } else if (toupper(config@MIP$solver) == "LPSOLVE") {

      if (!maximize) {
        len_rhs      <- length(rhs)
        rhs[len_rhs] <- config@MIP$gap_limit
      }

      MIP <- lpSolve::lp(direction = ifelse(maximize, "max", "min"), obj, mat, dir, rhs, binary.vec = 1:nv, presolve = TRUE)

    }

    is_optimal <- isOptimal(MIP$status, config@MIP$solver)
    if (!is_optimal) {
      warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
      return(list(status = MIP$status, MIP = NULL, selected = NULL))
    }

    solve_time <- proc.time() - solve_time

    if (!is.null(constraints$stimulus_order)) {
      constraints$item_attrib <- merge(constraints$item_attrib,
        constraints$st_attrib[c("STINDEX", "STID", constraints$stimulus_order_by)],
        by = "STID", all.x = TRUE, sort = FALSE
      )
    } else if (!is.null(constraints$st_attrib)) {
      constraints$item_attrib <- merge(constraints$item_attrib,
        constraints$st_attrib[c("STINDEX", "STID")],
        by = "STID", all.x = TRUE, sort = FALSE
      )
    }

    selected  <- constraints$item_attrib[which(MIP$solution[1:constraints$ni] == 1), ]
    obj_value <- sum(obj[which(MIP$solution[1:constraints$ni] == 1)])

    if (!is.null(constraints$item_order_by) && !is.null(constraints$stimulus_order_by)) {
      selected <- selected[order(selected[[constraints$stimulus_order_by]], selected[["STID"]], selected[[constraints$item_order_by]]), ]
    } else if (!is.null(constraints$item_order_by)) {
      if (constraints$set_based) {
        selected <- selected[order(selected[["STID"]], selected[[constraints$item_order_by]]), ]
      } else {
        selected <- selected[order(selected[[constraints$item_order_by]]), ]
      }
    } else if (!is.null(constraints$stimulus_order_by)) {
      selected <- selected[order(selected[[constraints$stimulus_order_by]], selected[["STID"]]), ]
    }
    row.names(selected) <- 1:nrow(selected)

    if (plot) {
      continuum <- seq(plot_range[1], plot_range[2], .1)
      continuum <- sort(c(continuum, config@item_selection$target_location))
      idx <- which(MIP$solution[1:ni] == 1)

      if (toupper(config@item_selection$method) == "TIF") {
        mat_sub <- calcFisher(constraints$pool, continuum)[, idx]
        vec_sub <- apply(mat_sub, 1, sum)
        ylab    <- "Information"
        title   <- "Test Information Function based on the selected items"
      }
      if (toupper(config@item_selection$method) == "TCC") {
        l <- calcProb(constraints$pool, continuum)[idx]
        for (i in 1:length(l)) {
          prob_mat  <- l[[i]]
          max_score <- dim(prob_mat)[2] - 1
          prob_mat  <- prob_mat * matrix(c(0:max_score), dim(prob_mat)[1], dim(prob_mat)[2], byrow = T)
          l[[i]]    <- apply(prob_mat, 1, sum)
        }
        vec_sub <- Reduce("+", l)
        ylab    <- "Expected Score"
        title   <- "Test Characteristic Curve based on the selected items"
      }
      if (toupper(config@item_selection$method) == "MAXINFO") {
        mat_sub <- calcFisher(constraints$pool, continuum)[, idx]
        vec_sub <- apply(mat_sub, 1, sum)
        ylab    <- "Information"
        title   <- "Test Information Function based on the selected items"
      }

      ymax <- max(vec_sub, config@item_selection$target_value)

      pdf(NULL, bg = "white")
      dev.control(displaylist = "enable")

      plot(continuum, vec_sub,
        xlim = c(min(continuum), max(continuum)), ylim = c(0, ymax),
        main = title, xlab = "Theta", ylab = ylab, type = "n"
      )
      lines(continuum, vec_sub, lty = 1, lwd = 3)
      if (!maximize) {
        abline(h = config@item_selection$target_value, lty = 3, lwd = 2)
      }
      abline(v = config@item_selection$target_location, lty = 3, lwd = 2)

      p <- recordPlot()
      plot.new()
      dev.off()
    } else {
      p <- NULL
    }

    return(list(
      MIP = MIP, selected = selected,
      solver = config@MIP$solver, obj_value = obj_value,
      solve_time = solve_time, plot = p
    ))
  }
)
