#' @include loading_functions.R
NULL

#' @rdname createStaticTestConfig
#' @export
setClass("config_Static",
  slots = c(
    item_selection = "list",
    MIP            = "list"
  ),
  prototype = list(
    item_selection = list(
      method          = "MaxInfo",
      info_type       = "FISHER",
      target_location = c(-1.2, 0, 1.2),
      target_value    = NULL,
      target_weight   = c(1, 1, 1)
    ),
    MIP = list(
      solver          = "LPSOLVE",
      verbosity       = -2,
      time_limit      = 60,
      gap_limit       = 0.05,
      gap_limit_abs   = 0.05,
      obj_tol         = 0.05,
      retry           = 5
    )
  ),
  validity = function(object) {
    err <- NULL
    if (!toupper(object@item_selection$method) %in% c("MAXINFO", "TIF", "TCC")) {
      msg <- sprintf("config@item_selection: unexpected $method '%s' (accepts MAXINFO, TIF, or TCC)", toupper(object@item_selection$method))
      err <- c(err, msg)
    }
    if (toupper(object@item_selection$method) == "MAXINFO") {
      if (!is.null(object@item_selection$target_value)) {
        msg <- "config@item_selection: $target_value must be empty when $method is 'MAXINFO'"
        err <- c(err, msg)
      }
      target_lengths <- unique(c(
        length(object@item_selection$target_location),
        length(object@item_selection$target_weight)
      ))
      if (length(target_lengths) != 1) {
        msg <- "config@item_selection: $target_location and $target_weight must have the same length()"
        err <- c(err, msg)
      }
    }
    if (toupper(object@item_selection$method) != "MAXINFO") {
      target_lengths <- unique(c(
        length(object@item_selection$target_location),
        length(object@item_selection$target_value),
        length(object@item_selection$target_weight)
      ))
      if (length(target_lengths) != 1) {
        msg <- "config@item_selection: $target_location, $target_value, and $target_weight must have the same length()"
        err <- c(err, msg)
      }
    }
    if (toupper(object@item_selection$info_type) != "FISHER") {
      msg <- sprintf("config@item_selection: unexpected $info_type '%s' (accepts FISHER)", toupper(object@item_selection$info_type))
      err <- c(err, msg)
    }
    if (!object@MIP$solver %in% c("RSYMPHONY", "HIGHS", "GUROBI", "LPSOLVE", "RGLPK")) {
      # only capitalized names are valid values;
      # the rest of the package assumes this is capitalized
      msg <- sprintf("config@MIP: unrecognized $solver '%s' (accepts RSYMPHONY, HIGHS, GUROBI, LPSOLVE, or RGLPK)", object@MIP$solver)
      err <- c(err, msg)
    }
    if (object@MIP$solver == "HIGHS") {
      warning(sprintf(
        "HIGHS solver is unstable as of 1.10.0.1 (installed version: %s) - it sometimes returns 0.5 as a solution for binary decision variables",
        packageVersion("highs")
      ))
    }

    if (length(err) == 0) {
      return(TRUE)
    } else {
      return(err)
    }
  }
)

#' Create a config_Static object
#'
#' \code{\link{createStaticTestConfig}} is a config function for creating a \code{\linkS4class{config_Static}} object for Static (fixed-form) test assembly.
#' Default values are used for any unspecified parameters/slots.
#'
#' @param item_selection a named list containing item selection criteria.
#' \itemize{
#'   \item{\code{method}} the type of selection criteria. Accepts \code{MAXINFO, TIF, TCC}. (default = \code{MAXINFO})
#'   \item{\code{info_type}} the type of information. Accepts \code{FISHER}. (default = \code{FISHER})
#'   \item{\code{target_location}} a numeric vector containing the locations of target theta points. (e.g. \code{c(-1, 0, 1)}) (default = \code{c(-1.2, 0, 1.2)})
#'   \item{\code{target_value}} a numeric vector containing the target values at each theta location. This should have the same length with \code{target_location}. Ignored if method is \code{MAXINFO}. (default = \code{NULL})
#'   \item{\code{target_weight}} a numeric vector containing the weights for each theta location. This should have the same length with \code{target_location}. (default = \code{rep(1, length(target_location))}
#' }
#'
#' @param MIP a named list containing solver options.
#' \itemize{
#'   \item{\code{solver}} the type of solver. Accepts \code{Rsymphony, highs, gurobi, lpSolve, Rglpk}. (default = \code{LPSOLVE})
#'   \item{\code{verbosity}} verbosity level of the solver. (default = \code{-2})
#'   \item{\code{time_limit}} time limit in seconds. Used in solvers \code{Rsymphony, gurobi, Rglpk}. (default = \code{60})
#'   \item{\code{gap_limit}} search termination criterion. Gap limit in relative scale passed onto the solver. Used in solver \code{gurobi}. (default = \code{.05})
#'   \item{\code{gap_limit_abs}} search termination criterion. Gap limit in absolute scale passed onto the solver. Used in solvers \code{Rsymphony}. (default = \code{0.05})
#'   \item{\code{obj_tol}} search termination criterion. The lower bound to use on the minimax deviation variable. Used when \code{item_selection$method} is \code{TIF} or \code{TCC}. (default = \code{0.05})
#'   \item{\code{retry}} number of times to retry running the solver if the solver returns no solution. Some solvers incorrectly return no solution even when a solution exists. This is the number of attempts to verify that the problem is indeed infeasible in such cases. Set to \code{0} to not retry. (default = \code{5})
#' }
#'
#' @return \code{\link{createStaticTestConfig}} returns a \code{\linkS4class{config_Static}} object. This object is used in \code{\link{Static}}.
#'
#' @examples
#' cfg1 <- createStaticTestConfig(
#'   list(
#'     method = "MAXINFO",
#'     info_type = "FISHER",
#'     target_location = c(-1, 0, 1),
#'     target_weight = c(1, 1, 1)
#'   )
#' )
#'
#' cfg2 <- createStaticTestConfig(
#'   list(
#'     method = "TIF",
#'     info_type = "FISHER",
#'     target_location = c(-1, 0, 1),
#'     target_weight = c(1, 1, 1),
#'     target_value = c(8, 10, 12)
#'   )
#' )
#'
#' cfg3 <- createStaticTestConfig(
#'   list(
#'     method = "TCC",
#'     info_type = "FISHER",
#'     target_location = c(-1, 0, 1),
#'     target_weight = c(1, 1, 1),
#'     target_value = c(10, 15, 20)
#'   )
#' )
#' @rdname createStaticTestConfig
#' @export
createStaticTestConfig <- function(item_selection = NULL, MIP = NULL) {
  cfg <- new("config_Static")
  arg_names <- c("item_selection", "MIP")
  obj_names <- c()
  for (arg in arg_names) {
    if (!is.null(eval(parse(text = arg)))) {
      accepted_slots <-
        eval(parse(text = sprintf("names(cfg@%s)", arg)))
      supplied_slots <-
        eval(parse(text = sprintf("names(%s)", arg)))
      leftovers <- setdiff(supplied_slots, accepted_slots)
      if (length(leftovers) > 0) {
        for (x in leftovers) {
          stop(
            sprintf(
              "cfg@%s: slot '%s' is unused",
              arg, x
            )
          )
        }
      }
      for (entry in accepted_slots) {
        entry_l <- paste0("cfg@", arg, "$", entry)
        entry_r <- paste0(arg, "$", entry)
        tmp <- eval(parse(text = entry_r))
        if (!is.null(tmp)) {
          eval(parse(text = paste0(entry_l, " <- ", entry_r)))
        }
      }
    }
  }

  # ensure the solver name is capitalized here;
  # the rest of the package assumes this is already done
  cfg@MIP$solver <- toupper(cfg@MIP$solver)

  if (is.null(item_selection$target_weight)) {
    cfg@item_selection$target_weight <- rep(1, length(cfg@item_selection$target_location))
  }
  if (toupper(cfg@item_selection$method) == "MAXINFO") {
    cfg@item_selection$target_value <- NULL
  }

  v <- validObject(cfg)
  if (v) {
    return(cfg)
  }
}

#' Class 'output_Static': fixed-form assembly solution
#'
#' \code{\linkS4class{output_Static}} is an S4 class for representing a fixed-form assembly solution.
#'
#' @slot call the function call used for obtaining this object.
#' @slot MIP a list containing the result from MIP solver.
#' @slot selected a \code{\link{data.frame}} containing the selected items and their attributes.
#' @slot obj_value the objective value of the solution.
#' @slot solve_time the elapsed time in running the solver.
#' @slot achieved a \code{\link{data.frame}} containing attributes of the assembled test, by each constraint.
#' @slot pool the \code{\linkS4class{item_pool}} used in the assembly.
#' @slot config the \code{\linkS4class{config_Static}} used in the assembly.
#' @slot constraints the \code{\linkS4class{constraints}} used in the assembly.
#'
#' @export
setClass("output_Static",
  slots = c(
    call        = "call",
    MIP         = "list_or_null",
    selected    = "dataframe_or_null",
    obj_value   = "numeric_or_null",
    solve_time  = "numeric_or_null",
    achieved    = "dataframe_or_null",
    pool        = "item_pool",
    config      = "config_Static",
    constraints = "constraints"
  ),
  prototype = list(
    MIP         = NULL,
    selected    = NULL,
    obj_value   = -1,
    solve_time  = -1,
    achieved    = NULL,
    pool        = new("item_pool"),
    config      = new("config_Static"),
    constraints = new("constraints")
  ),
  validity = function(object) {
    return(TRUE)
  }
)
