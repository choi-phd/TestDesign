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
  signature = "list",
  definition = function(object, theta = seq(-3, 3, .1), info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "blue", file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4)) {

    config      <- object$config
    constraints <- object$constraints
    continuum   <- theta
    continuum   <- sort(c(continuum, config@item_selection$target_location))

    idx <- which(object$MIP$solution[1:constraints@ni] == 1)

    if (toupper(config@item_selection$method) == "MAXINFO") {
      mat_sub <- calcFisher(constraints@pool, continuum)[, idx]
      vec_sub <- apply(mat_sub, 1, sum)
      ylab    <- "Information"
      title   <- "Test Information Function based on the assembled test"
    }
    if (toupper(config@item_selection$method) == "TIF") {
      mat_sub <- calcFisher(constraints@pool, continuum)[, idx]
      vec_sub <- apply(mat_sub, 1, sum)
      ylab    <- "Information"
      title   <- "Test Information Function based on the assembled test"
    }
    if (toupper(config@item_selection$method) == "TCC") {
      l <- calcProb(constraints@pool, continuum)[idx]
      for (i in 1:length(l)) {
        prob_mat  <- l[[i]]
        max_score <- dim(prob_mat)[2] - 1
        prob_mat  <- prob_mat * matrix(c(0:max_score), dim(prob_mat)[1], dim(prob_mat)[2], byrow = T)
        l[[i]]    <- apply(prob_mat, 1, sum)
      }
      vec_sub <- Reduce("+", l)
      ylab    <- "Expected Score"
      title   <- "Test Characteristic Curve based on the assembled test"
    }

    ymax <- max(vec_sub, config@item_selection$target_value)

    pdf(NULL, bg = "white")
    dev.control(displaylist = "enable")

    plot(continuum, vec_sub,
         xlim = c(min(continuum), max(continuum)), ylim = c(0, ymax),
         main = title, xlab = "Theta", ylab = ylab, type = "n", bty = "n"
    )
    if (toupper(config@item_selection$method) != "MAXINFO") {
      abline(h = config@item_selection$target_value, lty = 3, lwd = 1)
    }
    abline(v = config@item_selection$target_location, lty = 3, lwd = 1)
    lines(continuum, vec_sub, lty = 1, lwd = 1, col = color)

    legend("topleft",
           "Target locations",
           bty = "o", bg = "white",
           box.lty = 0, box.lwd = 0, box.col = "white",
           lty = 3, lwd = 1, seg.len = 1, inset = c(0, .01))
    box()

    p <- recordPlot()
    plot.new()
    dev.off()

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

    if (toupper(info_type) == "FISHER") {
      info <- calcFisher(object, theta)
      if (plot_sum) {
        info <- rowSums(info)
      }
    } else {
      stop("Invalid info_type specified")
    }

    if (!is.null(file_pdf)) {
      pdf(file = file_pdf, width = width, height = height)
    }

    old_mfrow <- par()$mfrow
    on.exit(par(mfrow = old_mfrow))
    par(mfrow = mfrow)

    items <- 1:object@ni
    if (!is.null(select) && all(select %in% items)) {
      items <- select
    }

    if (plot_sum) {
      plot(theta, info, xlab = "Theta", ylab = "Info", main = sprintf("Information from all %i items", object@ni), type = "l", col = color, ylim = c(0, max(info)))
    } else {
      for (i in items) {
        plot(theta, info[, i], xlab = "Theta", ylab = "Info", main = object@id[i], type = "l", col = color, ylim = c(0, max(info)))
      }
    }

    if (!is.null(file_pdf)) {
      dev.off()
    }

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

    mean_info <- numeric(length(theta))

    n_trials <- 1000
    for (i in 1:length(theta)) {
      info_vec <- sort(calcFisher(object@pool, theta[i]), T)
      tmp_k    <- numeric(n_trials)
      for (k in 1:n_trials) {
        tmp_k[k] <- sum(sample(info_vec, n_items))
      }
      mean_info[i] <- mean(tmp_k)
    }

    pdf(NULL, bg = "white")
    dev.control(displaylist = "enable")

    tmp = sprintf("Maximum attainable test information and the randomly selected information")

    plot(0, 0,
         type = "n", xlim = c(-3, 3), ylim = c(0, max(max_info)),
         xlab = "Theta", ylab = "Information", main = "Test information based on best items vs. random selection"
    )
    grid()

    lines(theta, max_info, lty = 1, lwd = 1)
    lines(theta, mean_info, lty = 2, lwd = 1)

    p <- recordPlot()
    plot.new()
    dev.off()

    return(p)

  }
)
