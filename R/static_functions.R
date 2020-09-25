#' @include constraints_operators.R
NULL

#' Run fixed-form test assembly
#'
#' \code{\link{Static}} is a test assembly function to perform fixed-form test assembly based on the generalized shadow-test framework.
#'
#' @template config_Static-param
#' @template constraints-param
#' @template force_solver_param
#'
#' @return \code{\link{Static}} returns a \code{\linkS4class{output_Static}} object containing the selected items.
#'
#' @template mipbook-ref
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
  def = function(config, constraints, force_solver = FALSE) {
    standardGeneric("Static")
  }
)

#' @docType methods
#' @rdname Static-methods
#' @export
setMethod(
  f = "Static",
  signature = c("config_Static"),
  definition = function(config, constraints, force_solver = FALSE) {

    if (!validObject(config)) {
      stop("'config' object is not valid.")
    }

    if (!force_solver) {
      o <- validateSolver(config, constraints)
      if (!o) {
        return(invisible())
      }
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

    is_optimal <- isOptimal(results$status, config@MIP$solver)
    if (!is_optimal) {
      printSolverNewline(config@MIP$solver)
      msg <- getSolverStatusMessage(results$status, config@MIP$solver)
      warning(msg, immediate. = TRUE)
    }

    tmp <- NULL
    if (is_optimal) {
      tmp <- getSolutionAttributes(
        constraints,
        results$shadow_test$INDEX,
        FALSE
      )
    }

    out             <- new("output_Static")
    out@MIP         <- list(results$MIP)
    out@selected    <- results$shadow_test
    out@obj_value   <- results$obj_value
    out@solve_time  <- results$solve_time
    out@achieved    <- tmp
    out@pool        <- pool
    out@config      <- config
    out@constraints <- constraints

    return(out)

  }
)

#' (deprecated) Plot item/test/pool-level information
#'
#' (deprecated) Use \code{\link[TestDesign:plot-methods]{plot}}.
#'
#' @param object \itemize{
#'   \item{\code{\linkS4class{item_pool}}: plot pool-level or item-level information.}
#'   \item{\code{\linkS4class{output_Static}}: plot test-level information of the assembly solution.}
#'   \item{\code{\linkS4class{constraints}}: plot attainable information range.}
#' }
#' @param theta the theta grid to use on the x-axis. (default = \code{seq(-3, 3, .1)})
#' @param info_type the type of information. Accepts \code{FISHER}. (default = \code{FISHER})
#' @param plot_sum used when the \code{object} argument is an \code{\linkS4class{item_pool}} object.
#'   If \code{TRUE} then draw pool-level information, and if \code{FALSE} draw item-level information for every item in the pool. (default = \code{TRUE})
#' @param select (optional) used when the \code{object} argument is an \code{\linkS4class{item_pool}} object. Items to select from the pool.
#' @param color the color of the curve. (default = \code{blue})
#' @param file_pdf (optional) if supplied a filename, save as a PDF file.
#' @param width the width of the plot. (default = \code{7})
#' @param height the height of the plot. (default = \code{6})
#' @param mfrow multi-panel configurations to use. (default = \code{c(2, 4)})
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:8)
#' plot(subitempool)
#'
#' config <- createStaticTestConfig()
#' solution <- Static(config, constraints_science)
#' plot(solution)
#'
#' @docType methods
#' @rdname plotInfo-methods
#' @export
setGeneric(
  name = "plotInfo",
  def = function(object, theta = seq(-3, 3, .1), info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "blue", file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4)) {
    standardGeneric("plotInfo")
  }
)

#' @docType methods
#' @rdname plotInfo-methods
#' @export
setMethod(
  f = "plotInfo",
  signature = "output_Static",
  definition = function(object, theta = seq(-3, 3, .1), info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "blue", file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4)) {
    .Deprecated("plot", msg = "'plotInfo' function is deprecated. Use 'plot' function instead.")
    p <- plot(x = object,
      theta = theta,
      info_type = info_type,
      plot_sum = plot_sum,
      select = select,
      color = color
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotInfo-methods
#' @export
setMethod(
  f = "plotInfo",
  signature = "item_pool",
  definition = function(object, theta = seq(-3, 3, .1), info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "blue", file_pdf = NULL, width = 7, height = 6, mfrow = c(1, 1)) {
    .Deprecated("plot", msg = "'plotInfo' function is deprecated. Use 'plot' function instead.")
    p <- plot(x = object,
      theta = theta,
      info_type = info_type,
      plot_sum = plot_sum,
      select = select,
      color = color
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotInfo-methods
#' @export
setMethod(
  f = "plotInfo",
  signature = "constraints",
  definition = function(object, theta = seq(-3, 3, .1), info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "black", file_pdf = NULL, width = 7, height = 6, mfrow = c(1, 1)) {
    .Deprecated("plot", msg = "'plotInfo' function is deprecated. Use 'plot' function instead.")
    p <- plot(x = object,
      theta = theta,
      info_type = info_type,
      plot_sum = plot_sum,
      select = select,
      color = color
    )
    return(p)
  }
)
