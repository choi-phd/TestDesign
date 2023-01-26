#' @include static_functions.R
NULL

#' Run Test Assembly
#'
#' \code{\link{runAssembly}} is a function to perform test assembly. This function is used internally in \code{\link{Static}} and \code{\link{Shadow}}.
#'
#' @param config a \code{\linkS4class{config_Static}} or a \code{\linkS4class{config_Shadow}} object containing configuration options. Use \code{\link{createStaticTestConfig}} and \code{\link{createShadowTestConfig}} for this.
#' @param constraints a \code{\linkS4class{constraints}} object. Use \code{\link{loadConstraints}} for this.
#' @param xdata a list containing extra constraints in MIP form, to force-include previously administered items.
#' @param objective the information value for each item in the pool.
#'
#' @return a list containing the following entries:
#' \itemize{
#'   \item{\code{MIP}} a list containing the result from MIP solver.
#'   \item{\code{status}} the MIP status value, indicating whether an optimal solution was found.
#'   \item{\code{shadow_test}} the attributes of the selected items.
#'   \item{\code{obj_value}} the objective value of the solution.
#'   \item{\code{solve_time}} the elapsed time in running the solver.
#' }
#'
#' @template mipbook-ref
runAssembly <- function(config, constraints, xdata = NULL, objective = NULL) {

  ni    <- constraints@ni
  nv    <- constraints@nv
  mat   <- constraints@mat
  dir   <- constraints@dir
  rhs   <- constraints@rhs

  solver        <- toupper(config@MIP$solver)
  verbosity     <- config@MIP$verbosity
  time_limit    <- config@MIP$time_limit
  gap_limit     <- config@MIP$gap_limit
  gap_limit_abs <- config@MIP$gap_limit_abs

  # setup MIP matrices for problem type

  obj   <- numeric(nv)
  types <- rep("B", nv)

  if (inherits(config, "config_Static")) {

    sort_by_info <- FALSE

    if (toupper(config@item_selection$method) == "MAXINFO") {

      maximize  <- TRUE
      obj[1:ni] <- objective

    } else {

      maximize  <- FALSE

      # add one variable (minimax deviation)

      mat       <- cbind(mat, 0)
      obj       <- c(obj, 1)
      types     <- c(types, "C")

      # add two constraints (minimax)

      for (k in 1:nrow(objective)) {
        add_mat            <- matrix(0, nrow = 2, ncol = nv + 1)
        add_mat[1, 1:ni]   <- objective[k, ]
        add_mat[2, 1:ni]   <- objective[k, ]
        add_mat[1, nv + 1] <- -1
        add_mat[2, nv + 1] <- 1
        mat <- rbind(mat, add_mat)
        dir <- c(dir, c("<=", ">="))
        rhs <- c(rhs, rep(config@item_selection$target_value[k], 2))
      }

      # add one constraint (enforce lowerbound on minimax deviation)

      add_mat <- c(rep(0, nv), 1)
      mat <- rbind(mat, add_mat)
      dir <- c(dir, ">=")
      rhs <- c(rhs, config@MIP$obj_tol)

    }

  }

  if (inherits(config, "config_Shadow")) {

    if (all(length(objective) != nv, length(objective) != ni)) {
      stop(sprintf("length of 'objective' must be %s or %s", nv, ni))
    }

    sort_by_info <- TRUE

    # add x for previous items

    xmat          <- xdata[["xmat"]]
    xdir          <- xdata[["xdir"]]
    xrhs          <- xdata[["xrhs"]]

    if (!is.null(xmat) && !is.null(xdir) && !is.null(xrhs)) {
      mat <- rbind(mat, xmat)
      dir <- c(dir, xdir)
      rhs <- c(rhs, xrhs)
    }

    if (config@item_selection$method == "GFI") {

      maximize     <- FALSE

      # add one variable (minimax deviation)

      mat       <- cbind(mat, 0)
      obj       <- c(obj, 1)
      types     <- c(types, "C")

      # add two constraints (minimax)

      for (k in 1) {
        add_mat            <- matrix(0, nrow = 2, ncol = nv + 1)
        add_mat[1, 1:ni]   <- objective[, k]
        add_mat[2, 1:ni]   <- objective[, k]
        add_mat[1, nv + 1] <- -1
        add_mat[2, nv + 1] <- 1
        mat <- rbind(mat, add_mat)
        dir <- c(dir, c("<=", ">="))
        rhs <- c(rhs, rep(config@item_selection$target_value, 2))
      }

      # add one constraint (enforce lowerbound on minimax deviation)

      add_mat <- c(rep(0, nv), 1)
      mat <- rbind(mat, add_mat)
      dir <- c(dir, ">=")
      rhs <- c(rhs, config@MIP$obj_tol)

    } else {

      maximize     <- TRUE
      obj[1:length(objective)] <- objective

    }

  }


  # RUN SOLVER ---------------------------------------------------

  solve_time <- proc.time()

  MIP <- runMIP(
    solver, obj, mat, dir, rhs,
    maximize, types,
    verbosity, time_limit,
    gap_limit_abs, gap_limit
  )

  if (config@MIP$retry > 0 & !isOptimal(MIP$status, solver)) {
    # if errors, run again to check if it is indeed an error
    # some solvers error even when a solution exists
    n_retry <- 0
    while (TRUE) {
      n_retry <- n_retry + 1
      MIP <- runMIP(
        solver, obj, mat, dir, rhs,
        maximize, types,
        verbosity, time_limit,
        gap_limit_abs, gap_limit
      )
      if (isOptimal(MIP$status, solver) | n_retry == config@MIP$retry) {
        break
      }
    }
  }

  if (!isOptimal(MIP$status, solver)) {
    return(list(solver = solver, status = MIP$status, MIP = MIP, selected = NULL))
  }

  MIP$solution[types == "B"] <- round(MIP$solution[types == "B"], 0)

  solve_time <- (proc.time() - solve_time)[["elapsed"]]

  # STIMULUS-LEVEL SORT IF NEEDED --------------------------------------------------------------

  if (!is.null(constraints@stim_order)) {
    constraints@item_attrib@data$tmpsort <- 1:constraints@ni
    constraints@item_attrib@data <- merge(
      constraints@item_attrib@data,
      constraints@st_attrib@data[c("STINDEX", "STID", constraints@stim_order_by)],
      by = "STID", all.x = TRUE, sort = FALSE
    )
    constraints@item_attrib@data <- constraints@item_attrib@data[order(constraints@item_attrib@data$tmpsort), ]
    constraints@item_attrib@data <- constraints@item_attrib@data[, !(colnames(constraints@item_attrib@data) %in% "tmpsort")]
  } else if (!is.null(constraints@st_attrib)) {
    constraints@item_attrib@data$tmpsort <- 1:constraints@ni
    constraints@item_attrib@data <- merge(
      constraints@item_attrib@data,
      constraints@st_attrib@data[c("STINDEX", "STID")],
      by = "STID", all.x = TRUE, sort = FALSE
    )
    constraints@item_attrib@data <- constraints@item_attrib@data[order(constraints@item_attrib@data$tmpsort), ]
    constraints@item_attrib@data <- constraints@item_attrib@data[, !(colnames(constraints@item_attrib@data) %in% "tmpsort")]
  }

  # Common logic for attribute parsing

  index_solution <- which(MIP$solution[1:constraints@ni] == 1)
  shadow_test    <- constraints@item_attrib@data[index_solution, ]
  info           <- objective[index_solution]
  obj_value      <- sum(info)

  if (sort_by_info) {

    # Append info columns

    shadow_test    <- data.frame(cbind(shadow_test, info))
    if (constraints@set_based) {
      if (any(is.na(shadow_test[["STID"]]))) {

        # if is a mixed test

        shadow_test_discrete <- shadow_test[is.na(shadow_test$STID), ]
        shadow_test_discrete <- data.frame(cbind(shadow_test_discrete, set_info = shadow_test_discrete$info))

        shadow_test_setbased <- shadow_test[!is.na(shadow_test$STID), ]
        shadow_test_setbased <- appendMeanInfo(shadow_test_setbased, "STID", "set_info")

        shadow_test <- rbind(shadow_test_discrete, shadow_test_setbased)

      } else {

        shadow_test <- appendMeanInfo(shadow_test, "STID", "set_info")

      }
    }
  }

  if (sort_by_info) {
    shadow_test <- shadow_test[order(shadow_test$info, decreasing = TRUE), ]
  }
  if (constraints@set_based) {
    shadow_test <- shadow_test[order(shadow_test$STID), ]
  }
  if (constraints@set_based & sort_by_info) {
    shadow_test <- shadow_test[order(shadow_test$set_info, decreasing = TRUE), ]
  }
  if (!is.null(constraints@item_order_by)) {
    shadow_test <- shadow_test[order(shadow_test[[constraints@item_order_by]]), ]
  }
  if (!is.null(constraints@stim_order_by)) {
    shadow_test <- shadow_test[order(shadow_test[[constraints@stim_order_by]]), ]
  }

  return(list(
    MIP = MIP, solver = solver, status = MIP$status,
    shadow_test = shadow_test, obj_value = obj_value,
    solve_time = solve_time
  ))

}

#' @noRd
runMIP <- function(solver, obj, mat, dir, rhs, maximize, types,
                   verbosity, time_limit, gap_limit_abs, gap_limit) {

  if (solver == "RSYMPHONY") {

    if (!is.null(gap_limit_abs)) {
      MIP <- Rsymphony::Rsymphony_solve_LP(
        obj, mat, dir, rhs,
        max = maximize, types = types,
        verbosity = verbosity,
        time_limit = time_limit,
        gap_limit = gap_limit_abs
      )
    } else {
      MIP <- Rsymphony::Rsymphony_solve_LP(
        obj, mat, dir, rhs,
        max = maximize, types = types,
        verbosity = verbosity,
        time_limit = time_limit
      )
    }

  } else if (solver == "GUROBI") {

    constraints_dir <- dir
    constraints_dir[constraints_dir == "=="] <- "="

    if (verbosity <= 0) verbosity <- 0
    if (verbosity >= 1) verbosity <- 1

    if (!is.null(gap_limit)) {
      MIP <- gurobi::gurobi(
        model = list(obj = obj, modelsense = ifelse(maximize, "max", "min"), rhs = rhs, sense = constraints_dir, vtype = types, A = mat),
        params = list(MIPGap = gap_limit, TimeLimit = time_limit, LogToConsole = verbosity)
      )
    } else {
      MIP <- gurobi::gurobi(
        model = list(obj = obj, modelsense = ifelse(maximize, "max", "min"), rhs = rhs, sense = constraints_dir, vtype = types, A = mat),
        params = list(TimeLimit = time_limit, LogToConsole = verbosity)
      )
    }

    MIP[["solution"]] <- MIP$x

  } else if (solver == "LPSOLVE") {

    binary_vec <- which(types == "B")

    MIP <- lpSolve::lp(direction = ifelse(maximize, "max", "min"), obj, mat, dir, rhs, binary.vec = binary_vec, presolve = TRUE)

  } else if (solver == "RGLPK") {

    MIP <- Rglpk::Rglpk_solve_LP(obj, mat, dir, rhs, max = maximize, types = types,
      control = list(verbose = ifelse(verbosity != -2, TRUE, FALSE), presolve = FALSE, tm_limit = time_limit * 1000))

  }

  return(MIP)

}

#' @noRd
isOptimal <- function(status, solver) {
  is_optimal <- FALSE
  if (toupper(solver) == "RSYMPHONY") {
    is_optimal <- names(status) %in% c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND", "TM_TARGET_GAP_ACHIEVED")
  } else if (toupper(solver) == "GUROBI") {
    is_optimal <- status %in% c("OPTIMAL")
  } else if (toupper(solver) == "LPSOLVE") {
    is_optimal <- status == 0
  } else if (toupper(solver) == "RGLPK") {
    is_optimal <- status == 0
  }
  return(is_optimal)
}

#' @noRd
getSolverStatusMessage <- function(status, solver) {
  if (toupper(solver) == "RSYMPHONY") {
    tmp <- sprintf("MIP solver returned non-zero status: %s", names(status))
  } else if (toupper(solver) == "GUROBI") {
    tmp <- sprintf("MIP solver returned non-zero status: %s", status)
  } else if (toupper(solver) == "LPSOLVE") {
    tmp <- sprintf("MIP solver returned non-zero status: %s", status)
  } else if (toupper(solver) == "RGLPK") {
    tmp <- sprintf("MIP solver returned non-zero status: %s", status)
  }
  return(tmp)
}

#' @noRd
printSolverNewline <- function(solver) {
  if (toupper(solver) == "RSYMPHONY") {
    cat("\n")
  }
}

#' @noRd
validateSolver <- function(config, constraints, purpose = NULL) {

  if (constraints@set_based) {
    if (!toupper(config@MIP$solver) %in%  c("RSYMPHONY", "GUROBI")) {

      if (!interactive()) {
        txt <- paste(
          sprintf("Set-based assembly with %s is not allowed in non-interactive mode", config@MIP$solver),
          "(allowed solvers: RSYMPHONY, or GUROBI)",
          sep = " "
        )
        warning(txt)
        return(FALSE)
      }

      txt <- paste(
        sprintf("Set-based assembly with %s takes a long time.", config@MIP$solver),
        "Recommended solvers: RSYMPHONY, or GUROBI",
        sep = "\n"
      )

      choices <- c(
        sprintf("Proceed with %s", config@MIP$solver),
        "Abort"
      )
      input <- menu(choices, FALSE, txt)

      return(input == 1)

    }
  }

  if (is.null(purpose)) {
    return(TRUE)
  }

  if (purpose == "SPLIT") {
    if (!toupper(config@MIP$solver) %in%  c("RSYMPHONY", "GUROBI")) {

      if (!interactive()) {
        txt <- paste(
          sprintf("Split() with %s is not allowed in non-interactive mode", config@MIP$solver),
          "(allowed solvers: RSYMPHONY, or GUROBI)",
          sep = " "
        )
        warning(txt)
        return(FALSE)
      }

      txt <- paste(
        sprintf("Split() with %s takes a long time.", config@MIP$solver),
        "Recommended solvers: RSYMPHONY, or GUROBI",
        sep = "\n"
      )

      choices <- c(
        sprintf("Proceed with %s", config@MIP$solver),
        "Abort"
      )
      input <- menu(choices, FALSE, txt)

      return(input == 1)

    }
  }

  return(TRUE)

}

#' Test solver
#'
#' @param solver a solver package name. Accepts \code{lpSolve, Rsymphony, gurobi, Rglpk}.
#'
#' @return empty string \code{""} if solver works. A string containing error messages otherwise.
#'
#' @docType methods
#' @export
testSolver <- function(solver) {

  obj   <- seq(.1, .5, .1)
  mat   <- matrix(
    c(1, 1, 1, 1, 1,
      0, 0, 0, 1, 0),
    2, 5,
    byrow = TRUE)
  dir   <- rep("==", 2)
  rhs   <- c(2, 0)
  types <- "B"

  solver <- toupper(solver)
  o <- try(
    runMIP(
      solver,
      obj, mat, dir, rhs,
      TRUE, types,
      verbosity = -2,
      time_limit = 5,
      gap_limit_abs = 0.05,
      gap_limit = 0.05
    ),
    silent = TRUE
  )

  if (inherits(o, "try-error")) {
    return(trimws(as.character(o)))
  }

  return("")

}

#' Detect best solver
#'
#' @return the package name of the best available solver on the system.
#'
#' @examples
#' solver <- detectBestSolver()
#' cfg <- createStaticTestConfig(MIP = list(solver = solver))
#' cfg <- createShadowTestConfig(MIP = list(solver = solver))
#'
#' @docType methods
#' @export
detectBestSolver <- function() {
  solver_list <- c(
    "gurobi", "Rsymphony", "lpSolve"
  )
  for (solver in solver_list) {
    is_solver_available <- suppressWarnings(requireNamespace(solver, quietly = TRUE))
    if (!is_solver_available) {
      next
    }
    is_solver_functional <- testSolver(solver)
    if (is_solver_functional == "") {
      return(solver)
    }
  }
  stop("a functional solver is not available on this system")
}
