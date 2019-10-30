#' @include shadow_class.R
NULL


#' Run Static Test Assembly
#'
#' Perform static (fixed-form) test assembly with specified configurations.
#'
#' @param config A \code{\linkS4class{config_Static}} object containing configuration options. Use \code{\link{createStaticTestConfig}} for this.
#' @param constraints A list representing optimization constraints. Use \code{\link{loadConstraints}} for this.
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
#' solution <- Static(config_science, constraints_science)
#'
#' @docType methods
#' @rdname Static-methods
#' @export

setGeneric(
  name = "Static",
  def = function(config, constraints) {
    standardGeneric("Static")
  }
)

#' @docType methods
#' @rdname Static-methods
#' @export
setMethod(
  f = "Static",
  signature = c("config_Static"),
  definition = function(config, constraints) {

    if (!validObject(config)) {
      stop("'config' object is not valid.")
    }

    pool <- constraints@pool

    nt <- length(config@item_selection$target_location)

    if (toupper(config@item_selection$method) == "MAXINFO") {

      objective <- as.vector(config@item_selection$target_weight %*% calcFisher(pool, config@item_selection$target_location))

    } else if (toupper(config@item_selection$method) == "TIF") {

      objective <- calcFisher(pool, config@item_selection$target_location)

    } else if (toupper(config@item_selection$method) == "TCC") {

      tmp <- lapply(pool@parms, calcEscore, config@item_selection$target_location)
      objective <- t(do.call(rbind, tmp))

    }

    results <- runAssembly(config, constraints, objective = objective)

    return(list(
      MIP = results$MIP,
      selected = results$shadow_test, obj_value = results$obj_value,
      solve_time = results$solve_time,
      pool = pool, config = config, constraints = constraints
    ))

  }
)
