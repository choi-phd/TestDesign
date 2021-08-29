#' @include ATA_functions.R
NULL

#' @noRd

isOptimal <- function(status, solver) {
  is_optimal <- FALSE
  if (toupper(solver) == "SYMPHONY") {
    is_optimal <- names(status) %in% c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND")
  } else if (toupper(solver) == "GUROBI") {
    is_optimal <- status %in% c("OPTIMAL")
  } else if (toupper(solver) == "LPSOLVE") {
    is_optimal <- status == 0
  } else if (toupper(solver) == "GLPK") {
    is_optimal <- status == 0
  }
  return(is_optimal)
}

