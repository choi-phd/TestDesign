#' @include loading_functions.R
NULL

#' createStaticTestConfig
#'
#' @rdname createStaticTestConfig

setClass("config_Static",
  slots = c(
    item_selection = "list",
    MIP = "list"
  ),
  prototype = list(
    item_selection = list(
      method = "MaxInfo",
      info_type = "FISHER",
      target_location = c(-1.2, 0, 1.2),
      target_value = NULL,
      target_weight = c(1, 1, 1)
    ),
    MIP = list(
      solver = "LPSOLVE",
      verbosity = -2,
      time_limit = 60,
      gap_limit = 0.05,
      gap_limit_abs = 0.05,
      obj_tol = 0.05
    )
  ),
  validity = function(object) {
    errors <- NULL
    if (!toupper(object@item_selection$method) %in% c("MAXINFO", "TIF", "TCC")) {
      errors <- c(errors, "@item_selection$method only accepts one of MAXINFO, TIF, or TCC.")
    }
    if (toupper(object@item_selection$method) == "MAXINFO") {
      if (!is.null(object@item_selection$target_value)) {
        errors <- c(errors, "@item_selection$target_value must be empty when @item_selection$method is MAXINFO.")
      }
      target_lengths <- unique(c(
        length(object@item_selection$target_location),
        length(object@item_selection$target_weight)
      ))
      if (length(target_lengths) != 1) {
        errors <- c(errors, "@item_selection$target_location and @item_selection$target_weight must have the same length.")
      }
    }
    if (toupper(object@item_selection$method) != "MAXINFO") {
      target_lengths <- unique(c(
        length(object@item_selection$target_location),
        length(object@item_selection$target_value),
        length(object@item_selection$target_weight)
      ))
      if (length(target_lengths) != 1) {
        errors <- c(errors, "@item_selection$target_location, @item_selection$target_value, and @item_selection$target_weight must have the same length.")
      }
    }
    if (toupper(object@item_selection$info_type) != "FISHER") {
      errors <- c(errors, "@item_selection$info_type only accepts FISHER.")
    }
    if (!toupper(object@MIP$solver) %in% c("LPSYMPHONY", "RSYMPHONY", "GUROBI", "LPSOLVE", "RGLPK")) {
      errors <- c(errors, "@MIP$solver only accepts one of lpsymphony, Rsymphony, gurobi, lpSolve, or Rglpk.")
    }

    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' @noRd
setClassUnion("config_ATA", c("config_Static"))

#' Create a config_Static object
#'
#' Create a \code{\linkS4class{config_Static}} object for Static (fixed-form) test assembly.
#'
#' @param item_selection A list containing item selection criteria. This should have the following entries:
#' \itemize{
#'   \item{\code{method}} The type of criteria. Accepts \code{MAXINFO, TIF, TCC}.
#'   \item{\code{info_type}} The type of information. Accepts \code{FISHER}.
#'   \item{\code{target_location}} A numeric vector containing the locations of target theta points. (e.g. \code{c(-1, 0, 1)})
#'   \item{\code{target_value}} A numeric vector containing the target values at each theta location. This should have the same length with \code{target_location}. Ignored if method is \code{MAXINFO}.
#'   \item{\code{target_weight}} A numeric vector containing the weights for each theta location. This should have the same length with \code{targetlocation}. Defaults to a vector of 1s.
#' }
#' @param MIP A list containing solver options. This should have the following entries:
#' \itemize{
#'   \item{\code{solver}} The type of solver. Accepts \code{lpsymphony, Rsymphony, gurobi, lpSolve, Rglpk}.
#'   \item{\code{verbosity}} Verbosity level of the solver. Defaults to -2.
#'   \item{\code{time_limit}} Time limit in seconds passed onto the solver. Defaults to 60. Used in solvers \code{lpsymphony, Rsymphony, gurobi, Rglpk}.
#'   \item{\code{gap_limit}} Termination criterion. Gap limit in relative scale passed onto the solver. Defaults to .05. Used in solver \code{gurobi}.
#'   \item{\code{gap_limit_abs}} Termination criterion. Gap limit in absolute scale passed onto the solver. Defaults to .05. Used in solver \code{lpsymphony, Rsymphony}.
#'   \item{\code{obj_tol}} Termination criterion. Tolerance on target objective value in absolute difference scale. Defaults to .05. Ignored if method is \code{MAXINFO}.
#' }
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
#'
#' @export
createStaticTestConfig <- function(item_selection = NULL, MIP = NULL) {
  cfg <- new("config_Static")
  arg_names <- c("item_selection", "MIP")
  obj_names <- c()
  for (arg in arg_names) {
    if (!is.null(eval(parse(text = arg)))) {
      eval(parse(text = paste0("obj_names <- names(cfg@", arg, ")")))
      for (entry in obj_names) {
        entry_l <- paste0("cfg@", arg, "$", entry)
        entry_r <- paste0(arg, "$", entry)
        tmp <- eval(parse(text = entry_r))
        if (!is.null(tmp)) {
          eval(parse(text = paste0(entry_l, " <- ", entry_r)))
        }
      }
    }
  }

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

#' @name show-method
#' @aliases show,config_Static-method
#' @docType methods
#' @noRd
setMethod("show", "config_Static", function(object) {
  cat("Static Assembly Configurations \n\n")
  cat("  Item selection criterion \n")
  cat("    Method         :", object@item_selection$method, "\n")
  cat("    Info type      :", object@item_selection$info_type, "\n")
  cat("    Theta location :", object@item_selection$target_location, "\n")
  cat("    Target value   :", object@item_selection$target_value, "\n")
  cat("    Target weight  :", object@item_selection$target_weight, "\n\n")
  cat("  MIP \n")
  cat("    Solver         :", object@MIP$solver, "\n")
  cat("    Verbosity      :", object@MIP$verbosity, "\n")
  cat("    Time limit     :", object@MIP$time_limit, "\n")
  cat("    Gap limit      \n")
  cat("      Relative     :", object@MIP$gap_limit, "\n")
  cat("      Absolute     :", object@MIP$gap_limit_abs, "\n")
  cat("    Obj. tolerance :", object@MIP$obj_tol, "\n\n")
})
