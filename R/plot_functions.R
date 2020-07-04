#' @include helper_functions.R
NULL

#' Plot from objects
#'
#' Plot from objects in the TestDesign package.
#'
#' @param x accepts the following signatures:
#' \itemize{
#'   \item{\code{\linkS4class{item_pool}}}: plot information and expected scores.
#'   \item{\code{\linkS4class{constraints}}}: plot information range based on the test length constraint.
#'   \item{\code{\linkS4class{output_Static}}}: plot information and expected scores based on the fixed assembly solution.
#'   \item{\code{\linkS4class{output_Shadow_all}}}: plot audit trail, shadow test chart, and exposure rates from the adaptive assembly solution.
#'   \item{\code{\linkS4class{output_Shadow}}}: plot audit trail and shadow test chart from the adaptive assembly solution.
#' }
#' @param y not used, exists for compatibility with \code{\link[base]{plot}} in the base R package.
#' @param type the type of plot.
#' \itemize{
#'    \item{\code{info} plots information from \code{\linkS4class{item_pool}} and \code{\linkS4class{output_Static}}.}
#'    \item{\code{score} plots expected scores from \code{\linkS4class{item_pool}} and \code{\linkS4class{output_Static}}.}
#'    \item{\code{audit} plots audit trail from \code{\linkS4class{output_Shadow_all}} and \code{\linkS4class{output_Shadow}}.}
#'    \item{\code{shadow} plots shadow test chart from \code{\linkS4class{output_Shadow_all}} and \code{\linkS4class{output_Shadow}}.}
#'    \item{\code{exposure} plots exposure rates from \code{\linkS4class{output_Shadow_all}}.}
#' }
#' @param theta the theta grid to use in plotting. (default = \code{seq(-3, 3, .1)})
#' @param info_type the type of information. Currently only accepts \code{FISHER}. (default = \code{FISHER})
#' @param plot_sum used in \code{\linkS4class{item_pool}} objects.
#' \itemize{
#'    \item{if \code{TRUE} then plot pool-level values.}
#'    \item{if \code{FALSE} then plot item-level values, and repeat for all items in the pool.}
#'    \item{(default = \code{TRUE})}
#' }
#' @param select used in \code{\linkS4class{item_pool}} objects. Item indices to subset.
#' @param color the color of the curve.
#' @param examinee_id used in \code{\linkS4class{output_Shadow}} and \code{\linkS4class{output_Shadow_all}} with \code{type = 'audit'} and \code{type = 'shadow'}. The examinee numeric ID to draw the plot.
#' @param theta_range used in \code{\linkS4class{output_Shadow}} and \code{\linkS4class{output_Shadow_all}} with \code{type = 'audit'}. The theta range to plot. (default = \code{c(-5, 5)})
#' @param z_ci used in \code{\linkS4class{output_Shadow}} and \code{\linkS4class{output_Shadow_all}} with \code{type = 'audit'}. The range to use for confidence intervals. (default = \code{1.96})
#' @param simple used in \code{\linkS4class{output_Shadow}} and \code{\linkS4class{output_Shadow_all}} with \code{type = 'shadow'}. If \code{TRUE}, simplify the chart by hiding unused items.
#' @param theta_segment used in \code{\linkS4class{output_Shadow_all}} with \code{type = 'exposure'}. The type of theta to determine exposure segments. Accepts \code{Estimated} or \code{True}. (default = \code{Estimated})
#' @param color_final used in \code{\linkS4class{output_Shadow_all}} with \code{type = 'exposure'}. The color of item-wise exposure rates, only counting the items administered in the final theta segment as exposed.
#' @param ... arguments to pass onto \code{\link{plot}}.
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
#' cfg <- createStaticTestConfig()
#' solution <- Static(cfg, constraints_science)
#' plot(solution)                 # defaults to the objective type
#' plot(solution, type = "score") # plot expected scores
#'
#' ## Plot attainable information range from constraints
#' plot(constraints_science)
#'
#' ## Plot assembly results from Shadow()
#' cfg <- createShadowTestConfig()
#' set.seed(1)
#' solution <- Shadow(cfg, constraints_science, true_theta = rnorm(1))
#' plot(solution, type = 'audit' , examinee_id = 1)
#' plot(solution, type = 'shadow', examinee_id = 1, simple = TRUE)
#'
#' ## plot(solution, type = 'exposure')
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

#' Draw an audit trail plot
#'
#' Draw an audit trail plot.
#'
#' @param x An output object generated by \code{\link{Shadow}}.
#' @param y Unused argument, exists for compatibility with \code{\link{plot}} in the base R package.
#' @param examinee_id Numeric ID of the examinee to draw the plot.
#' @param type If 'audit', draw an audit trail plot. If 'shadow', draw a shadow chart. If 'exposure', draw exposure rate plots.
#' @param min_theta For type 'audit', the lower bound of theta range to plot.
#' @param max_theta For type 'audit', the upper bound of theta range to plot.
#' @param min_score For type 'audit', the minimum item score.
#' @param max_score For type 'audit', the maximum item score.
#' @param z_ci For type 'audit', the quantile of the normal distribution for confidence intervals.
#' @param simple For type 'shadow', if \code{TRUE}, simplity the chart by hiding unused items.
#' @param theta_segment For type 'exposure', True or Estimated theta used to create segments ("Estimated" or "True").
#' @param color For type 'exposure', color of item-wise exposure rates.
#' @param color_final For type 'exposure', color of item-wise exposure rates, only counting the items while in the final theta segment as exposed.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(config, constraints_science, true_theta)
#' plot(solution, type = 'audit' , examinee_id = 1)
#' plot(solution, type = 'shadow', examinee_id = 1)
#' plot(solution, type = 'exposure')
#'
#' @docType methods
#' @rdname plot-output_Shadow
#' @export
setMethod(
  f = "plot",
  signature = "output_Shadow",
  definition = function(x, y, examinee_id = 1, type = "audit",
                        min_theta = -5, max_theta = 5, z_ci = 1.96, ...) {
    if (type == "audit") {
      n_items <- length(x@administered_item_index)
      if (n_items > 0) {
        old_mar <- par()$mar
        on.exit(par(mar = old_mar))
        par(mar = c(2, 3, 1, 1) + 0.1)
        layout(rbind(c(1, 1), c(1, 1), c(1, 1), c(1, 1), c(2, 2)))
        plot(1:n_items, seq(min_theta, max_theta, length = n_items), ylab = "Theta", type = "n", las = 1, xlim = c(0, n_items), xaxt = "n", yaxt = "n")
        grid()
        text(n_items / 2, max_theta, paste0("Examinee ID: ", x@simulee_id), adj = c(0.5, 0.5), cex = 2)
        axis(1, at = 0:n_items, tick = TRUE, labels = 0:n_items, cex.axis = 1.5)
        axis(2, at = min_theta:max_theta, labels = min_theta:max_theta, cex.axis = 1.5)
        text(0.5, min_theta + 1.0, paste("Final Theta: ", round(x@final_theta_est, digits = 2), " SE: ", round(x@final_se_est, digits = 2)), cex = 1.5, adj = 0)
        for (i in 1:n_items) {
          ci_lower = x@interim_theta_est[i] - z_ci * x@interim_se_est[i]
          ci_upper = x@interim_theta_est[i] + z_ci * x@interim_se_est[i]
          lines(rep(i, 2)            , c(ci_lower, ci_upper), col = "purple4")
          lines(c(i - 0.25, i + 0.25), c(ci_lower, ci_lower), col = "purple4")
          lines(c(i - 0.25, i + 0.25), c(ci_upper, ci_upper), col = "purple4")
        }
        lines(1:n_items, x@interim_theta_est, lty = 3, col = "blue", lwd = 1.5)
        points(1:n_items, x@interim_theta_est, pch = 16, cex = 2.5, col = "blue")
        points(1:n_items, x@interim_theta_est, pch = 1, cex = 2.5, col = "purple4")
        if (!is.null(x@true_theta)) {
          abline(h = x@true_theta, lty = 1, col = "red")
        }
        for (i in 1:n_items) {
          if (x@shadow_test_refreshed[i]) {
            text(i, min_theta, "S", col = "red", cex = 1.5)
          }
        }

        min_score <- 0
        max_score <- x@max_cat_pool
        plot(
          1:n_items, seq(min_score, max_score, length.out = n_items),
          type = "n", xaxt = "n", yaxt = "n",
          xlim = c(0, n_items),
          ylim = c(min_score - 1, max_score + 1),
          ylab = "")
        mtext("Position", side = 1, line = 1, outer = FALSE, cex = 1.5)
        axis(2, at = (min_score + max_score) / 2, labels = "Response", cex.axis = 2, tick = FALSE)
        for (i in 1:n_items) {
          rect_x <- i
          rect_y <- x@administered_item_resp[i]
          if (!is.na(rect_y)) {

            if (x@administered_item_ncat[i] == 2) {
              if (x@administered_item_resp[i] == min_score) {
                rect_col = "red"
              } else {
                rect_col = "lime green"
              }
            } else {
              rect_col = "cyan2"
            }
            for (ii in 0:rect_y) {
              rect(rect_x - 0.25, ii - 1, rect_x + 0.25, ii, col = rect_col, border = "black")
            }

          }
        }
      } else {
        cat("output_Shadow is empty\n")
      }
    }
  }
)

#' @docType methods
#' @rdname plot-output_Shadow
#' @export
setMethod(
  f = "plot",
  signature = "output_Shadow_all",
  definition = function(
    x, y, examinee_id = 1, type = "audit",
    min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96,
    simple = FALSE, theta_segment = "Estimated", color = "blue", color_final = "blue",
    file_pdf = NULL, ...) {
    if (!type %in% c("audit", "shadow", "exposure")) {
      stop("'type' must be audit, shadow, or exposure")
    }
    if (type == "audit") {
      if (!all(examinee_id %in% 1:length(x@output))) {
        stop("'examinee_id' out of bounds")
      }
      if (!is.null(file_pdf)) {
        pdf(file = file_pdf, bg = "white")
      }
      for (id in examinee_id) {
        plot(
          x@output[[id]],
          type = "audit",
          examinee_id = examinee_id,
          min_theta = min_theta, max_theta = max_theta,
          min_score = min_score, max_score = max_score,
          z_ci = z_ci,
          ...
        )
      }
      if (!is.null(file_pdf)) {
        dev.off()
      } else {
        p <- recordPlot()
        return(p)
      }
    } else if (type == "shadow") {
      if (!is.null(file_pdf)) {
        pdf(file = file_pdf, bg = "white")
      }
      constraints <- x@constraints
      for (id in examinee_id) {
        examinee_output <- x@output[[id]]
        max_ni <- constraints@test_length
        ni     <- constraints@ni
        old_mar   <- par()$mar
        old_mfrow <- par()$mfrow
        on.exit(par(mar = old_mar, mfrow = old_mfrow))
        par(mar = c(2, 3, 1, 1) + 0.1, mfrow = c(1, 1))
        n_points <- sum(!is.na(examinee_output@administered_item_resp)) # this should be equal to constraints@test_length
        item_id <- constraints@item_attrib@data[["ID"]][examinee_output@administered_item_index]
        item_sequence <- examinee_output@administered_item_index
        responses     <- examinee_output@administered_item_resp
        item_ncat     <- examinee_output@administered_item_ncat
        if (simple) {
          items_used <- sort(unique(do.call(c, examinee_output@shadow_test)))
          new_y <- 1:length(items_used)
          y_map <- rep(1, items_used[1] - 1)
          for (i in 1:(length(items_used) - 1)) {
            y_map <- c(y_map, rep(new_y[i], items_used[i + 1] - items_used[i]))
          }
          y_map <- c(y_map, rep(new_y[i + 1], ni - items_used[i + 1] + 1))
        } else {
          items_used <- 1:ni
          y_map <- 1:ni
        }
        plot(
          c(0.5, max_ni + 0.5),
          c(0.5, y_map[ni] + 0.5),
          type = "n", las = 1, xlim = c(0, max_ni),
          xaxt = "n", yaxt = "n", ylab = "")

        y_adj_3 <- (strheight("S") / 3)
        usr <- par("usr")
        text(max_ni / 2, (usr[3] / 2), "Position", adj = c(0.5, 0), cex = 1.0)
        axis(2, at = y_map[ni] / 2, labels = "Items", cex.axis = 1.5, tick = FALSE, line = 0)
        text(max_ni / 2, mean(c(usr[4], y_map[ni])), paste0("Examinee ID: ", examinee_output@simulee_id), adj = c(0.5, 0.5), cex = 1)
        axis(1, at = 1:max_ni, tick = TRUE, labels = 1:max_ni, cex.axis = 0.7)
        if (!simple) {
          text(0, seq(10, y_map[ni], 10), seq(10, y_map[ni], 10), adj = c(0.5, 0.5), cex = 0.7)
        } else {
          text(0, new_y, items_used, adj = c(0.5, 0.5), cex = 0.7)
        }

        for (i in 1:n_points) {
          y_dupecheck <- numeric(ni)
          for (j in 1:ni) {
            if (y_dupecheck[y_map[j]] == FALSE) {
              y_dupecheck[y_map[j]] <- TRUE
              rect(i - 0.25, y_map[j] - 0.25, i + 0.25, y_map[j] + 0.25, border = "gray88", lwd = 0.3)
            }
          }
          if (examinee_output@shadow_test_refreshed[i]) {
            mtext("S", at = i, side = 1, line = 0.3, col = "red", adj = c(0.5, 0.5), cex = 0.7)
          }
        }
        if (constraints@set_based) {
          for (p in 1:constraints@ns) {
            tmp = constraints@item_index_by_stimulus[[p]]
            if (!is.null(tmp)) {
              tmp = tmp[tmp %in% items_used]
              if (length(tmp) > 0) {
                for (i in 1:n_points) {
                  rect(i - 0.35, y_map[min(tmp)] - 0.5,
                       i + 0.35, y_map[max(tmp)] + 0.5, border = "gray88", lwd = 0.5)
                }
              }
            }
          }
        }
        shadow_tests <- examinee_output@shadow_test
        if (constraints@set_based) {
          item_table <- merge(constraints@item_attrib@data, constraints@st_attrib@data[c("STID", "STINDEX")], by = "STID", all.x = TRUE, sort = FALSE)
          for (k in 1:n_points) {
            items <- shadow_tests[[k]]
            current_item <- examinee_output@administered_item_index[k]
            passages <- unique(item_table[["STINDEX"]][which(item_table[["INDEX"]] %in% items)])
            current_passage <- item_table[["STINDEX"]][which(item_table[["INDEX"]] == current_item)]
            for (p in 1:length(passages)) {
              if (!is.na(passages[p])) {
                sub_items <- constraints@item_index_by_stimulus[[passages[p]]]
                sub_items <- sub_items[sub_items %in% items_used]
                if (!is.na(current_passage)) {
                  if (passages[p] == current_passage) {
                    rect(
                      k - 0.35, y_map[min(sub_items)] - 0.5,
                      k + 0.35, y_map[max(sub_items)] + 0.5, border = "blue", col = "khaki", lwd = 0.5)
                  } else {
                    rect(
                      k - 0.35, y_map[min(sub_items)] - 0.5,
                      k + 0.35, y_map[max(sub_items)] + 0.5, border = "blue", col = "gray50", lwd = 0.5)
                  }
                } else {
                  rect(
                    k - 0.35, y_map[min(sub_items)] - 0.5,
                    k + 0.35, y_map[max(sub_items)] + 0.5, border = "blue", col = "gray50", lwd = 0.5)
                }
              } else {
                sub_items <- item_table[["INDEX"]][which(is.na(item_table[["STINDEX"]]) & item_table[["INDEX"]] %in% items)]
                for (i in 1:length(sub_items)) {
                  if (sub_items[i] == current_item) {
                    rect(
                      k - 0.35, y_map[sub_items[i]] - 0.5,
                      k + 0.35, y_map[sub_items[i]] + 0.5, border = "blue", col = "khaki", lwd = 0.5)
                  } else {
                    rect(
                      k - 0.35, y_map[sub_items[i]] - 0.5,
                      k + 0.35, y_map[sub_items[i]] + 0.5, border = "blue", col = "gray50", lwd = 0.5)
                  }
                }
              }
            }
          }
        }
        for (k in 1:n_points) {
          items <- shadow_tests[[k]]
          current_item <- examinee_output@administered_item_index[k]
          for (i in 1:length(items)) {
            if (items[i] != current_item) {
              rect(
                k - 0.25, y_map[items[i]] - 0.25,
                k + 0.25, y_map[items[i]] + 0.25, border = "black", lwd = 0.3)
            }
          }
        }
        for (k in 1:n_points) {
          items <- shadow_tests[[k]]
          current_item <- examinee_output@administered_item_index[k]
          for (i in 1:length(items)) {
            if (items[i] == current_item) {
              for (kk in k:n_points) {
                rect(
                  kk - 0.25, y_map[items[i]] - 0.25,
                  kk + 0.25, y_map[items[i]] + 0.25, border = "gray33", col = "gray33", lwd = 0.3)
              }
              if (item_ncat[k] == 2) {
                if (responses[k] == 0) {
                  rect_col = "red"
                } else if (responses[k] == 1) {
                  rect_col = "lime green"
                }
              } else {
                rect_col = "cyan2"
              }
              rect(
                k - 0.25, y_map[items[i]] - 0.25,
                k + 0.25, y_map[items[i]] + 0.25, border = rect_col, col = rect_col, lwd = 0.3)
            }
          }
        }
      }

      if (!is.null(file_pdf)) {
        dev.off()
      } else {
        p <- recordPlot()
        return(p)
      }

    } else if (type == "exposure") {

      if (toupper(theta_segment) == "TRUE") {
        theta_value <- x@true_theta
        nj          <- length(theta_value)
      } else if (toupper(theta_segment) == "ESTIMATED") {
        theta_value <- x@final_theta_est
        nj          <- length(theta_value)
      } else {
        stop("'theta_segment' must be 'true' or 'estimated'.")
      }

      ni <- x@pool@ni
      nv <- ncol(x@usage_matrix)
      max_rate      <- x@config@exposure_control$max_exposure_rate
      segment_cut   <- x@config@exposure_control$segment_cut
      n_segment     <- x@config@exposure_control$n_segment
      cut_lower     <- segment_cut[1:n_segment]
      cut_upper     <- segment_cut[2:(n_segment + 1)]
      segment_label <- character(n_segment)
      for (k in 1:n_segment) {
        if (k < n_segment) {
          segment_label[k] <- sprintf("(%s,%s]", cut_lower[k], cut_upper[k])
        } else {
          segment_label[k] <- sprintf("(%s,%s)", cut_lower[k], cut_upper[k])
        }
      }
      theta_segment_index <- numeric(nj)
      theta_segment_index <- find_segment(theta_value, segment_cut)
      segment_n    <- numeric(n_segment)
      segment_dist <- table(theta_segment_index)
      segment_n[as.numeric(names(segment_dist))] <- segment_dist
      segment_index_table <- matrix(NA, nj, x@constraints@test_length)
      usage_matrix       <- x@usage_matrix
      usage_matrix_final <- x@usage_matrix
      for (j in 1:nj) {
        administered_items <- x@output[[j]]@administered_item_index
        pos_item_outside_of_segment <- x@output[[j]]@theta_segment_index != theta_segment_index[j]
        idx_item_outside_of_segment <- administered_items[pos_item_outside_of_segment]
        usage_matrix_final[j, idx_item_outside_of_segment] <- FALSE
        segment_index_table[j, ] <- x@output[[j]]@theta_segment_index
      }
      ## visited segments across item positions and each examinee
      segment_freq <- matrix(0, n_segment, n_segment)
      for (i in 1:x@constraints@test_length) {
        interim_segment_dist <- factor(segment_index_table[, i], levels = 1:n_segment)
        segment_table <- tapply(interim_segment_dist, theta_segment_index, table)
        for (s in 1:length(segment_table)) {
          idx_r <- as.numeric(names(segment_table)[s])
          idx_c <- as.numeric(names(segment_table[[s]]))
          segment_freq[idx_r, idx_c] <- segment_freq[idx_r, idx_c] + segment_table[[s]]
        }
      }
      segment_rate                <- segment_freq / segment_n
      segment_rate_table          <- data.frame(
        segment_class = factor(rep(segment_label, rep(n_segment, n_segment)), levels = segment_label),
        segment = rep(1:n_segment, n_segment),
        avg_visit = matrix(
          t(segment_rate),
          nrow = n_segment^2, ncol = 1)
      )
      exposure_rate               <- colSums(usage_matrix) / nj
      exposure_rate_final         <- colSums(usage_matrix_final) / nj
      item_exposure_rate          <- exposure_rate[1:ni]
      item_exposure_rate_final    <- exposure_rate_final[1:ni]
      if (x@constraints@set_based) {
        stim_exposure_rate        <- exposure_rate[(ni + 1):nv][x@constraints@stimulus_index_by_item]
        stim_exposure_rate_final  <- exposure_rate_final[(ni + 1):nv][x@constraints@stimulus_index_by_item]
      } else {
        stim_exposure_rate        <- NULL
        stim_exposure_rate_final  <- NULL
      }
      exposure_rate_segment       <- vector("list", n_segment)
      exposure_rate_segment_final <- vector("list", n_segment)
      names(exposure_rate_segment)       <- segment_label
      names(exposure_rate_segment_final) <- segment_label
      for (k in 1:n_segment) {
        if (segment_n[k] > 2) {
          exposure_rate_segment[[k]]       <- colMeans(usage_matrix[theta_segment_index == k, ])
          exposure_rate_segment_final[[k]] <- colMeans(usage_matrix_final[theta_segment_index == k, ])
        }
        if (is.null(exposure_rate_segment[[k]])) {
          exposure_rate_segment[[k]] <- numeric(nv)
        } else if (any(is.nan(exposure_rate_segment[[k]]))) {
          exposure_rate_segment[[k]][is.nan(exposure_rate_segment[[k]])] <- 0
        }
        if (is.null(exposure_rate_segment_final[[k]])) {
          exposure_rate_segment_final[[k]] <- numeric(nv)
        } else if (any(is.nan(exposure_rate_segment_final[[k]]))) {
          exposure_rate_segment_final[[k]][is.nan(exposure_rate_segment_final[[k]])] <- 0
        }
      }
      item_exposure_rate_segment       <- exposure_rate_segment
      item_exposure_rate_segment_final <- exposure_rate_segment_final
      for (k in 1:n_segment) {
        item_exposure_rate_segment[[k]]       <- item_exposure_rate_segment[[k]][1:ni]
        item_exposure_rate_segment_final[[k]] <- item_exposure_rate_segment_final[[k]][1:ni]
      }
      if (x@constraints@set_based) {
        stim_exposure_rate_segment       <- exposure_rate_segment
        stim_exposure_rate_segment_final <- exposure_rate_segment_final
        for (k in 1:n_segment) {
          stim_exposure_rate_segment[[k]]       <- stim_exposure_rate_segment[[k]][(ni + 1):nv][x@constraints@stimulus_index_by_item]
          stim_exposure_rate_segment_final[[k]] <- stim_exposure_rate_segment_final[[k]][(ni + 1):nv][x@constraints@stimulus_index_by_item]
        }
      } else {
        stim_exposure_rate_segment       <- NULL
        stim_exposure_rate_segment_final <- NULL
      }
      if (!is.null(file_pdf)) {
        pdf(file = file_pdf, ...)
      }
      old_oma <- par()$oma
      old_mar <- par()$mar
      on.exit(par(oma = old_oma, mar = old_mar))
      par(oma = c(3, 3, 0, 0), mar = c(3, 3, 2, 2))
      plotER(
        item_exposure_rate, item_exposure_rate_final, stim_exposure_rate, x@constraints@stimulus_index_by_item,
        max_rate = max_rate, title = "Overall", color = color, color_final = color_final, simple = TRUE)
      for (k in 1:n_segment) {
        plotER(
          item_exposure_rate_segment[[k]], item_exposure_rate_segment_final[[k]], stim_exposure_rate_segment[[k]], x@constraints@stimulus_index_by_item,
          max_rate = max_rate, title = segment_label[k], color = color, color_final = color_final, simple = TRUE)
      }
      mtext(text = "Item", side = 1, line = 0, outer = T)
      mtext(text = "Exposure Rate", side = 2, line = 0, outer = T)
      if (!is.null(file_pdf)) {
        dev.off()
      } else {
        p <- recordPlot()
      }
      out = new("exposure_rate_plot")
      out@plot = p
      out@item_exposure_rate         = item_exposure_rate
      out@item_exposure_rate_segment = item_exposure_rate_segment
      out@item_exposure_rate_segment_final = item_exposure_rate_segment_final
      out@stim_exposure_rate               = stim_exposure_rate
      out@stim_exposure_rate_segment       = stim_exposure_rate_segment
      out@stim_exposure_rate_segment_final = stim_exposure_rate_segment_final
      out@segment_rate_table = segment_rate_table
      out@n_segment = n_segment
      out@segment_n = segment_n
      out@segment_cut = segment_cut
      out@segment_label = segment_label
      return(out)
    }
  }
)
