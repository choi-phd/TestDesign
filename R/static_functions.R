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

    out             <- new("output_Static")
    out@MIP         <- list(results$MIP)
    out@selected    <- results$shadow_test
    out@obj_value   <- results$obj_value
    out@solve_time  <- results$solve_time
    out@pool        <- pool
    out@config      <- config
    out@constraints <- constraints

    return(out)

  }
)

#' Draw item information plots
#'
#' Draw item information plots.
#'
#' @param object An \code{\linkS4class{item_pool}} object to draw pool-level or item-level information, or a list from \code{\link{Static}} to draw test-level information.
#' @param theta Theta values for drawing the curve. Default is \code{seq(-3, 3, .1)}.
#' @param info_type Type of information. Currently only accepts \code{FISHER} (default).
#' @param plot_sum When 'object' is an \code{\linkS4class{item_pool}} object, if \code{TRUE} then draw pool-level information, and if \code{FALSE} draw item-level information for every item in the pool.
#' @param select A vector of indices identifying the items to subset, for when 'object' is an \code{\linkS4class{item_pool}} object.
#' @param color The color of the curve.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of graphics device.
#' @param height Width of graphics device.
#' @param mfrow Multipanel configurations as c(nrow, ncol).
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:8)
#' plotInfo(subitempool)
#'
#' config <- createStaticTestConfig()
#' solution <- Static(config, constraints_science)
#' plotInfo(solution)
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
      color = color,
      file_pdf = file_pdf,
      width = width,
      height = height,
      mfrow = mfrow
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
      color = color,
      file_pdf = file_pdf,
      width = width,
      height = height,
      mfrow = mfrow
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

    idx_n_items <- which(toupper(object@constraints[["WHAT"]]) == "ITEM" &
                           toupper(object@constraints[["CONDITION"]]) == "")
    n_items <- object@constraints[idx_n_items, ]["LB"][1, 1]

    max_info <- numeric(length(theta))

    for (i in 1:length(theta)) {
      max_info[i] <- sum(sort(calcFisher(object@pool, theta[i]), TRUE)[1:n_items])
    }

    mean_info <- calcFisher(object@pool, theta)
    mean_info <- apply(mean_info, 1, mean)
    mean_info <- mean_info * n_items

    pdf(NULL, bg = "white")
    dev.control(displaylist = "enable")

    tmp = sprintf("Maximum attainable test information and the randomly selected information")

    plot(0, 0,
         type = "n", xlim = c(-3, 3), ylim = c(0, max(max_info)),
         xlab = "Theta", ylab = "Information", main = "Test information based on best items vs. random selection"
    )
    grid()

    lines(theta, max_info,  lty = 1, lwd = 1, col = 'blue')
    lines(theta, mean_info, lty = 2, lwd = 1, col = 'blue')

    p <- recordPlot()
    plot.new()
    dev.off()

    return(p)

  }
)
