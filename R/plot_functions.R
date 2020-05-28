#' @include helper_functions.R
NULL

#' Plot from objects
#'
#' Plot from objects in the TestDesign package.
#'
#' @param x An \code{\linkS4class{item_pool}} object to draw pool-level or item-level information, or a list from \code{\link{Static}} to draw test-level information.
#' @param y Unused argument, exists for compatibility with \code{\link{plot}} in the base R package.
#' @param type Type of the plot. \code{info} to plot information, \code{score} to plot expected scores, \code{shadow} to plot shadow test chart, \code{audit} to plot audit trail, and \code{exposure} to plot exposure rates.
#' @param theta Theta values for drawing the curve. Default is \code{seq(-3, 3, .1)}.
#' @param info_type Type of information. Currently only accepts \code{FISHER} (default).
#' @param plot_sum When 'object' is an \code{\linkS4class{item_pool}} object, if \code{TRUE} then draw pool-level information, and if \code{FALSE} draw item-level information for every item in the pool.
#' @param select A vector of indices identifying the items to subset, for when 'object' is an \code{\linkS4class{item_pool}} object.
#' @param color The color of the curve.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of graphics device.
#' @param height Height of graphics device.
#' @param mfrow Multipanel configurations as c(nrow, ncol).
#' @param ... Arguments to pass onto \code{\link{plot}}.
#'
#' @examples
#' ## Plot item information of a pool
#' subitempool <- subsetItemPool(itempool_science, 1:8)
#' plot(subitempool)
#' plot(itempool_science, select = 1:8)
#'
#' ## Plot expected score of a pool
#' plot(subitempool, type = "score")
#' plot(itempool_science, type = "score", select = 1:8)
#'
#' ## Plot assembly results from Static()
#' config <- createStaticTestConfig()
#' solution <- Static(config, constraints_science)
#' plot(solution)
#'
#' ## Plot attainable information range from constraints
#' plot(constraints_science)
#'
#' @docType methods
#' @rdname plot-methods
#' @export
setMethod(
  f = "plot",
  signature = "item_pool",
  definition = function(x, y, theta = seq(-3, 3, .1), type = "info", info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "blue", file_pdf = NULL, width = 7, height = 6, mfrow = c(1, 1), ...) {

    if (!is.null(select)) {
      if (all(select %in% 1:x@ni)) {
        items <- select
      } else {
        stop("'select' is out of bounds")
      }
      txt_s <- "selected"
    } else {
      items <- 1:x@ni
      txt_s <- "all"
    }

    if (type == "info") {
      if (toupper(info_type) == "FISHER") {
        y <- calcFisher(subsetItemPool(x, items), theta)
      } else {
        stop("Invalid info_type specified")
      }
      txt <- "Information"
    } else if (type == "score") {
      y <- calcProb(subsetItemPool(x, items), theta)
      y <- sapply(y, function(l) l %*% ((1:dim(l)[2]) - 1), simplify = TRUE)
      txt <- "Expected score"
    } else {
      stop("'type' should be either 'info' or 'score' for an item_pool object")
    }

    if (plot_sum) {
      y <- rowSums(y)
    }

    if (!is.null(file_pdf)) {
      pdf(file = file_pdf, width = width, height = height)
    }

    old_mfrow <- par()$mfrow
    on.exit(par(mfrow = old_mfrow))
    par(mfrow = mfrow)

    if (plot_sum) {
      plot(theta, y, xlab = "Theta", ylab = txt,
           main = sprintf("%s from %s %i items", txt, txt_s, length(items)),
           type = "l", col = color, ylim = c(0, max(y)), ...)
    } else {
      for (i in 1:length(items)) {
        plot(theta, y[, i], xlab = "Theta", ylab = txt,
             main = x@id[items[i]],
             type = "l", col = color, ylim = c(0, max(y)), ...)
      }
    }

    if (!is.null(file_pdf)) {
      dev.off()
    } else if (plot_sum) {
      p <- recordPlot()
      plot.new()
      dev.off()
      return(p)
    }

  }
)

#' @docType methods
#' @rdname plot-methods
#' @export
setMethod(
  f = "plot",
  signature = "output_Static",
  definition = function(x, y, theta = seq(-3, 3, .1), type = NULL, info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "blue", file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4), ...) {
    config      <- x@config
    constraints <- x@constraints
    continuum   <- theta
    continuum   <- sort(c(continuum, config@item_selection$target_location))
    idx <- which(x@MIP[[1]]$solution[1:constraints@ni] == 1)
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
    # Begin plot
    pdf(NULL, bg = "white")
    dev.control(displaylist = "enable")
    plot(
      continuum, vec_sub,
      xlim = c(min(continuum), max(continuum)), ylim = c(0, ymax),
      main = title, xlab = "Theta", ylab = ylab, type = "n", bty = "n", ...
    )
    if (toupper(config@item_selection$method) != "MAXINFO") {
      abline(h = config@item_selection$target_value, lty = 3, lwd = 1)
    }
    abline(v = config@item_selection$target_location, lty = 3, lwd = 1)
    lines(continuum, vec_sub, lty = 1, lwd = 1, col = color)
    legend(
      "topleft",
      "Target locations",
      bty = "o", bg = "white",
      box.lty = 0, box.lwd = 0, box.col = "white",
      lty = 3, lwd = 1, seg.len = 1, inset = c(0, .01))
    box()
    p <- recordPlot()
    plot.new()
    dev.off()
    # End plot
    return(p)
  }
)

#' @docType methods
#' @rdname plot-methods
#' @export
setMethod(
  f = "plot",
  signature = "constraints",
  definition = function(x, y, theta = seq(-3, 3, .1), type = "info", info_type = "FISHER", plot_sum = TRUE, select = NULL, color = "black", file_pdf = NULL, width = 7, height = 6, mfrow = c(1, 1), ...) {
    if (!type == "info") {
      stop("'type' should be 'info' for constraints")
    }
    idx_n_items <- which(
      toupper(x@constraints[["WHAT"]]) == "ITEM" &
        toupper(x@constraints[["CONDITION"]]) == "")
    n_items <- x@constraints[idx_n_items, ]["LB"][1, 1]
    max_info <- numeric(length(theta))
    for (i in 1:length(theta)) {
      max_info[i] <- sum(sort(calcFisher(x@pool, theta[i]), TRUE)[1:n_items])
    }
    mean_info <- calcFisher(x@pool, theta)
    mean_info <- apply(mean_info, 1, mean)
    mean_info <- mean_info * n_items
    # Begin plot
    pdf(NULL, bg = "white")
    dev.control(displaylist = "enable")
    tmp = sprintf("Maximum attainable test information and the randomly selected information")
    plot(
      0, 0,
      type = "n", xlim = c(-3, 3), ylim = c(0, max(max_info)),
      xlab = "Theta", ylab = "Information", main = "Test information based on best items vs. random selection",
      ...
    )
    grid()
    lines(theta, max_info,  lty = 1, lwd = 1, col = 'blue')
    lines(theta, mean_info, lty = 2, lwd = 1, col = 'blue')
    p <- recordPlot()
    plot.new()
    dev.off()
    # End plot
    return(p)

  }
)
