#' @include shadow_class.R
NULL

#' Perform shadow test assembly
#'
#' Perform Shadow Test Assembly (STA) for computerized adaptive testing.
#'
#' @param constraints A list representing optimization constraints. Use \code{\link{loadConstraints}} for this.
#' @param objective A vector of objective values.
#' @param solver The type of solver. Accepts \code{SYMPHONY, GUROBI, GLPK, LPSOLVE}.
#' @param xmat A matrix of additional constraint coefficients for any previously administered items.
#' @param xdir A character vector with the directions for the constraints in \code{xmat}.
#' @param xrhs A vector of right-side values for the constraints in \code{xmat}.
#' @param maximize If \code{TRUE}, treat as a maximization problem. Otherwise treat as a minimization problem.
#' @param mps Only used when \code{solver} is \code{SYMPHONY}. If \code{TRUE}, print an MPS representation of the problem for debugging purposes.
#' @param lp Only used when \code{solver} is \code{SYMPHONY}. If \code{TRUE}, print an LP representation of the problem for debugging purposes.
#' @param verbosity Verbosity level.
#' @param time_limit Time limit passed onto the solver.
#' @param gap_limit Gap limit passed onto the solver.
#' @param ... Only used when \code{solver} is \code{SYMPHONY}. Additional parameters to be passed onto the solver.
#'
#' @return A list containing the optimal solution and pertinent diagnostics.
#'
#' @references{
#'   \insertRef{van_der_linden_model_1998}{TestDesign}
#'
#'   \insertRef{van_der_linden_optimal_1998}{TestDesign}
#'
#'   \insertRef{van_der_linden_optimal_2000}{TestDesign}
#'
#'   \insertRef{van_der_linden_linear_2005}{TestDesign}
#' }
#' @export
STA <- function(constraints, objective, solver = "Lpsolve", xmat = NULL, xdir = NULL, xrhs = NULL,
  maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = -2, time_limit = 5, gap_limit = -1, ...) {

  if (length(objective) == constraints$nv) {
    obj <- objective
  } else if (length(objective) == constraints$ni) {
    obj <- numeric(constraints$nv)
    obj[1:constraints$ni] <- objective
  } else {
    stop(sprintf("length of objective must be %s", constraints$nv))
  }
  if (!is.null(xmat) && !is.null(xdir) && !is.null(xrhs)) {
    mat <- rbind(constraints$mat, xmat)
    dir <- c(constraints$dir, xdir)
    rhs <- c(constraints$rhs, xrhs)
  } else {
    mat <- constraints$mat
    dir <- constraints$dir
    rhs <- constraints$rhs
  }
  solve_time <- proc.time()
  if (toupper(solver) == "SYMPHONY") {
    MIP <- Rsymphony::Rsymphony_solve_LP(obj, mat, dir, rhs, max = maximize, types = "B", write_mps = mps, write_lp = lp, verbosity = verbosity, time_limit = time_limit, gap_limit = gap_limit, ...)
    if (!isOptimal(MIP$status, solver)) {
      warning(sprintf("MIP solver returned non-zero status: %s", names(MIP$status)))
      return(list(status = MIP$status, MIP = NULL, selected = NULL))
    }
  } else if (toupper(solver) == "GUROBI") {
    dir[dir == "=="] <- "="
    invisible(capture.output(MIP <- gurobi::gurobi(list(obj = obj, modelsense = "max", rhs = rhs, sense = dir, vtype = "B", A = mat), params = NULL, env = NULL)))
    if (!isOptimal(MIP$status, solver)) {
      warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
      return(list(status = MIP$status, MIP = NULL, selected = NULL))
    }
    MIP[["solution"]] <- MIP$x
  } else if (toupper(solver) == "GLPK") {
    MIP <- Rglpk::Rglpk_solve_LP(obj, mat, dir, rhs, max = maximize, types = "B", control = list(verbose = ifelse(verbosity != -2, TRUE, FALSE), presolve = TRUE, tm_limit = time_limit))
    if (!isOptimal(MIP$status, solver)) {
      warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
      return(list(status = MIP$status, MIP = NULL, selected = NULL))
    }
  } else if (toupper(solver) == "LPSOLVE") {
    MIP <- lp(direction = ifelse(maximize, "max", "min"), obj, mat, dir, rhs, all.bin = TRUE, presolve = TRUE)
    if (!isOptimal(MIP$status, solver)) {
      warning(sprintf("MIP solver returned non-zero status: %s", MIP$status))
      return(list(status = MIP$status, MIP = NULL, selected = NULL))
    }
  } else {
    stop("solver must be Symphony, Gurobi, glpk, lpSolve")
  }
  solve_time <- (proc.time() - solve_time)["elapsed"]
  if (!is.null(constraints$stimulus_order)) {
    constraints$item_attrib <- merge(constraints$item_attrib, constraints$st_attrib[c("STINDEX", "STID", constraints$stim_order_by)], by = "STID", all.x = TRUE, sort = FALSE)
  } else if (!is.null(constraints$st_attrib)) {
    constraints$item_attrib <- merge(constraints$item_attrib, constraints$st_attrib[c("STINDEX", "STID")], by = "STID", all.x = TRUE, sort = FALSE)
  }
  index_solution <- which(MIP$solution[1:constraints$ni] == 1)
  info <- obj[index_solution]
  shadow_test <- data.frame(cbind(constraints$item_attrib[index_solution, ], info))
  if (constraints$set_based) {
    if (any(is.na(shadow_test[["STID"]]))) {
      shadow_test <- data.frame(cbind(.sequence = 1:nrow(shadow_test), shadow_test))
      shadow_test_discrete <- shadow_test[is.na(shadow_test[["STID"]]), ]
      shadow_test_discrete <- data.frame(cbind(shadow_test_discrete, meanInfo = shadow_test_discrete$info))
      shadow_test_stimulus <- shadow_test[!is.na(shadow_test[["STID"]]), ]
      mean_info <- tapply(shadow_test_stimulus$info, shadow_test_stimulus[["STID"]], mean)
      mean_info <- data.frame(STID = names(mean_info), meanInfo = mean_info)
      shadow_test_stimulus <- merge(shadow_test_stimulus, mean_info, by = "STID", all.x = TRUE, sort = FALSE)
      shadow_test <- rbind(shadow_test_discrete, shadow_test_stimulus)
      shadow_test <- shadow_test[order(shadow_test$.sequence), ]
      shadow_test <- shadow_test[-1]
    } else {
      mean_info <- tapply(shadow_test$info, shadow_test[["STID"]], mean)
      mean_info <- data.frame(STID = names(mean_info), meanInfo = mean_info)
      shadow_test <- merge(shadow_test, mean_info, by = "STID", all.x = TRUE, sort = FALSE)
    }
    if (!is.null(constraints$stim_order_by) && !is.null(constraints$item_order_by)) {
      shadow_test <- shadow_test[order(shadow_test[[constraints$stim_order_by]], shadow_test[["meanInfo"]], shadow_test[["STID"]], shadow_test[[constraints$item_order_by]], shadow_test[["info"]], decreasing = c(FALSE, TRUE, FALSE, FALSE, TRUE), method = "radix"), ]
    } else if (!is.null(constraints$stim_order_by)) {
      shadow_test <- shadow_test[order(shadow_test[[constraints$stim_order_by]], shadow_test[["meanInfo"]], shadow_test[["STID"]], shadow_test[["info"]], decreasing = c(FALSE, TRUE, FALSE, TRUE), method = "radix"), ]
    } else if (!is.null(constraints$item_order_by)) {
      shadow_test <- shadow_test[order(shadow_test[["meanInfo"]], shadow_test[["STID"]], shadow_test[[constraints$item_order_by]], shadow_test[["info"]], decreasing = c(TRUE, FALSE, FALSE, TRUE), method = "radix"), ]
    } else {
      shadow_test <- shadow_test[order(shadow_test[["meanInfo"]], shadow_test[["STID"]], shadow_test[["info"]], decreasing = c(TRUE, FALSE, TRUE), method = "radix"), ]
    }
  } else {
    if (!is.null(constraints$item_order_by)) {
      shadow_test <- shadow_test[order(shadow_test[[constraints$item_order_by]], shadow_test[["info"]], decreasing = c(FALSE, TRUE), method = "radix"), ]
    } else {
      shadow_test <- shadow_test[order(shadow_test[["info"]], decreasing = TRUE), ]
    }
  }
  obj_value <- sum(info)
  row.names(shadow_test) <- 1:nrow(shadow_test)
  return(list(status = MIP$status, MIP = MIP, shadow_test = shadow_test, obj_value = obj_value, solve_time = solve_time))
}

#' Save or print audit trails
#'
#' Save or print audit trails for all simulees.
#'
#' @param object_list A list of output objects generated from \code{STA}.
#' @param file An optional file name as a character string to save the output.
#'
#' @return None
saveOutput <- function(object_list, file = NULL) {
  nj <- length(object_list)
  for (j in 1:nj) {
    object <- object_list[[j]]
    output <- data.frame(
      simulee = object@simulee_id,
      true_theta = object@true_theta,
      true_theta_segment = object@true_theta_segment,
      stage = 1:length(object@administered_item_index),
      stimulus_index = ifelse(is.nan(object@administered_stimulus_index), rep(NA, length(object@administered_item_index)), object@administered_stimulus_index),
      item_index = object@administered_item_index,
      item_resp = object@administered_item_resp,
      interim_theta = object@interim_theta_est,
      interim_se = object@interim_se_est,
      interim_theta_segment = object@theta_segment_index
    )
    if (!is.null(file)) {
      write.table(output, file = file, append = j > 1, row.names = FALSE, col.names = j == 1, sep = ",")
    } else {
      print(output)
    }
  }
}

#' Draw a shadow test chart
#'
#' Draw a chart of shadow tests constructed for each simulee. The index of a column represents the position of item administration process, and each column represents the item pool.
#'
#' @param object An output from \code{\link{Shadow}} function.
#' @param constraints The constraint object used in obtaining the output.
#' @param examinee_id Numeric ID of the examinee to draw the plot.
#' @param sort_by_difficulty Sort the items by difficulty.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' object <- itempool_science
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(itempool_science, config, true_theta, constraints_science)
#' plotShadow(solution, constraints_science, 1)
#'
#' @docType methods
#' @rdname plotShadow-methods
#' @export
setGeneric(
  name = "plotShadow",
  def = function(object, constraints, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, ...) {
    standardGeneric("plotShadow")
  }
)

#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(
  f = "plotShadow",
  signature = "list",
  definition = function(object, constraints, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, ...) {
    if (!is.null(file_pdf)) {
      pdf(file = file_pdf, bg = "white")
    }
    for (i in examinee_id) {
      plotShadow(object$output[[i]], constraints, examinee_id, file_pdf = NULL, ...)
    }
    if (!is.null(file_pdf)) {
      dev.off()
    } else {
      p <- recordPlot()
      return(p)
    }
  }
)

#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(
  f = "plotShadow",
  signature = "output_Shadow",
  definition = function(object, constraints, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, ...) {
    max_ni <- constraints$test_length
    ni     <- constraints$ni

    old_mar   <- par()$mar
    old_mfrow <- par()$mfrow
    on.exit(par(mar = old_mar, mfrow = old_mfrow))
    par(mar = c(2, 3, 1, 1) + 0.1, mfrow = c(1, 1))

    n_points <- sum(!is.na(object@administered_item_resp)) # this should be equal to constraints$test_length
    item_id <- constraints$item_attrib[["ID"]][object@administered_item_index]
    item_sequence <- object@administered_item_index
    responses <- object@administered_item_resp
    plot(c(0.5, max_ni + 0.5), c(0.5, ni + 0.5), type = "n", las = 1, xlim = c(0, max_ni), xaxt = "n", yaxt = "n", ylab = "")
    usr <- par("usr")
    text(max_ni / 2, usr[3] / 2, "Position", adj = c(0.5, 0), cex = 1.0)
    if (sort_by_difficulty) {
      axis(2, at = ni / 2, labels = "Easier <-  Items  -> Harder", cex.axis = 1.5, tick = FALSE, line = 0)
    } else {
      axis(2, at = ni / 2, labels = "Items", cex.axis = 1.5, tick = FALSE, line = 0)
    }
    text(max_ni / 2, mean(c(usr[4], ni)), paste0("Examinee ID: ", object@simulee_id), adj = c(0.5, 0.5), cex = 1)
    axis(1, at = 0:max_ni, tick = TRUE, labels = 0:max_ni, cex.axis = 0.7)
    text(0, seq(10, ni, 10), seq(10, ni, 10), adj = c(0.5, 0.5), cex = 0.7)
    for (i in 1:n_points) {
      for (j in 1:ni) {
        rect(i - 0.25, j - 0.25, i + 0.25, j + 0.25, border = "gray88", lwd = 0.3)
      }
      if (object@shadow_test_refreshed[i]) {
        text(i, usr[3] + strheight("S") / 3, "S", col = "red", cex = 0.7, adj = c(0.5, 0))
      }
    }
    if (constraints$set_based) {
      for (p in 1:constraints$ns) {
        for (i in 1:n_points) {
          rect(i - 0.35, min(constraints$item_index_by_stimulus[[p]]) - 0.5, i + 0.35, max(constraints$item_index_by_stimulus[[p]]) + 0.5, border = "gray88", lwd = 0.5)
        }
      }
    }
    shadow_tests <- object@shadow_test
    if (constraints$set_based) {
      item_table <- merge(constraints$item_attrib, constraints$st_attrib[c("STID", "STINDEX", "NITEM")], by = "STID", all.x = TRUE, sort = FALSE)
      for (k in 1:n_points) {
        items <- shadow_tests[[k]]
        current_item <- object@administered_item_index[k]
        passages <- unique(item_table[["STINDEX"]][items])
        current_passage <- item_table[["STINDEX"]][current_item]
        for (p in 1:length(passages)) {
          sub_items <- constraints$item_index_by_stimulus[[passages[p]]]
          if (passages[p] == current_passage) {
            rect(k - 0.35, min(sub_items) - 0.5, k + 0.35, max(sub_items) + 0.5, border = "blue", col = "khaki", lwd = 0.5)
            for (i in 1:length(sub_items)) {
              if (sub_items[i] == current_item) {
                if (responses[k] >= 1) {
                  rect(k - 0.25, sub_items[i] - 0.25, k + 0.25, sub_items[i] + 0.25, border = "lime green", col = "lime green", lwd = 0.3)
                } else if (responses[k] == 0) {
                  rect(k - 0.25, sub_items[i] - 0.25, k + 0.25, sub_items[i] + 0.25, border = "red", col = "red", lwd = 0.3)
                }
              } else if (sub_items[i] %in% items) {
                rect(k - 0.25, sub_items[i] - 0.25, k + 0.25, sub_items[i] + 0.25, border = "black", lwd = 0.3)
              } else {
                rect(k - 0.25, sub_items[i] - 0.25, k + 0.25, sub_items[i] + 0.25, border = "white", lwd = 0.3)
              }
            }
          } else {
            rect(k - 0.35, min(sub_items) - 0.5, k + 0.35, max(sub_items) + 0.5, border = "blue", col = "gray50", lwd = 0.5)
            for (i in 1:length(sub_items)) {
              if (sub_items[i] %in% items) {
                rect(k - 0.25, sub_items[i] - 0.25, k + 0.25, sub_items[i] + 0.25, border = "black", lwd = 0.3)
              } else {
                rect(k - 0.25, sub_items[i] - 0.25, k + 0.25, sub_items[i] + 0.25, border = "gray88", lwd = 0.3)
              }
            }
          }
        }
      }
    } else {
      for (k in 1:n_points) {
        items <- shadow_tests[[k]]
        current_item <- object@administered_item_index[k]
        for (i in 1:length(items)) {
          if (items[i] != current_item) {
            rect(k - 0.25, items[i] - 0.25, k + 0.25, items[i] + 0.25, border = "black", lwd = 0.3)
          }
        }
      }
      for (k in 1:n_points) {
        items <- shadow_tests[[k]]
        current_item <- object@administered_item_index[k]
        for (i in 1:length(items)) {
          if (items[i] == current_item) {
            for (kk in k:n_points) {
              rect(kk - 0.25, items[i] - 0.25, kk + 0.25, items[i] + 0.25, border = "gray33", col = "gray33", lwd = 0.3)
            }
            if (responses[k] >= 1) {
              rect(k - 0.25, items[i] - 0.25, k + 0.25, items[i] + 0.25, border = "lime green", col = "lime green", lwd = 0.3)
            } else if (responses[k] == 0) {
              rect(k - 0.25, items[i] - 0.25, k + 0.25, items[i] + 0.25, border = "red", col = "red", lwd = 0.3)
            }
          }
        }
      }
    }
  }
)

#' Draw an audit trail plot
#'
#' Draw an audit trail plot.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param examinee_id Numeric ID of the examinee to draw the plot.
#' @param min_theta A lower bound of theta.
#' @param max_theta An upper bound of theta.
#' @param min_score A minimum item score.
#' @param max_score A maximum item score.
#' @param z_ci A quantile of the normal distribution for confidence intervals.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' object <- itempool_science
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(itempool_science, config, true_theta, constraints_science)
#' plotCAT(solution, 1)
#'
#' @docType methods
#' @rdname plotCAT-methods
#' @export
setGeneric(
  name = "plotCAT",
  def = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    standardGeneric("plotCAT")
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "list",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    if (!is.null(file_pdf)) {
      pdf(file = file_pdf, bg = "white")
    }
    for (i in examinee_id) {
      plotCAT(object$output[[i]], examinee_id, min_theta = min_theta, max_theta = max_theta, min_score = min_score, max_score = max_score, z_ci = z_ci, file_pdf = NULL, ...)
    }
    if (!is.null(file_pdf)) {
      dev.off()
    } else {
      p <- recordPlot()
      return(p)
    }
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "output_Shadow",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    n_items <- length(object@administered_item_index)
    if (n_items > 0) {

      old_mar <- par()$mar
      on.exit(par(mar = old_mar))
      par(mar = c(2, 3, 1, 1) + 0.1)

      layout(rbind(c(1, 1), c(1, 1), c(1, 1), c(1, 1), c(2, 2)))
      plot(1:n_items, seq(min_theta, max_theta, length = n_items), ylab = "Theta", type = "n", las = 1, xlim = c(0, n_items), xaxt = "n", yaxt = "n")
      grid()
      text(n_items / 2, max_theta, paste0("Examinee ID: ", object@simulee_id), adj = c(0.5, 0.5), cex = 2)
      axis(1, at = 0:n_items, tick = TRUE, labels = 0:n_items, cex.axis = 1.5)
      axis(2, at = min_theta:max_theta, labels = min_theta:max_theta, cex.axis = 1.5)
      text(0.5, min_theta + 1.0, paste("Final Theta: ", round(object@final_theta_est, digits = 2), " SE: ", round(object@final_se_est, digits = 2)), cex = 1.5, adj = 0)
      for (i in 1:n_items) {
        lines(rep(i, 2), c(object@interim_theta_est[i] - z_ci * object@interim_se_est[i], object@interim_theta_est[i] + z_ci * object@interim_se_est[i]), col = "purple4")
        lines(c(i - 0.25, i + 0.25), c(object@interim_theta_est[i] - z_ci * object@interim_se_est[i], object@interim_theta_est[i] - z_ci * object@interim_se_est[i]), col = "purple4")
        lines(c(i - 0.25, i + 0.25), c(object@interim_theta_est[i] + z_ci * object@interim_se_est[i], object@interim_theta_est[i] + z_ci * object@interim_se_est[i]), col = "purple4")
      }
      lines(1:n_items, object@interim_theta_est, lty = 3, col = "blue", lwd = 1.5)
      points(1:n_items, object@interim_theta_est, pch = 16, cex = 2.5, col = "blue")
      points(1:n_items, object@interim_theta_est, pch = 1, cex = 2.5, col = "purple4")
      if (!is.null(object@true_theta)) {
        abline(h = object@true_theta, lty = 1, col = "red")
      }
      for (i in 1:n_items) {
        if (object@shadow_test_refreshed[i]) {
          text(i, min_theta, "S", col = "red", cex = 1.5)
        }
      }
      plot(1:n_items, seq(min_score, max_score, length.out = n_items), type = "n", xaxt = "n", ylim = c(min_score - 1, max_score + 1), xlim = c(0, n_items), yaxt = "n", ylab = "")
      mtext("Position", side = 1, line = 1, outer = FALSE, cex = 1.5)
      axis(2, at = (min_score + max_score) / 2, labels = "Response", cex.axis = 2, tick = FALSE)
      for (i in 1:n_items) {
        x <- i
        y <- object@administered_item_resp[i]
        if (!is.na(y)) {
          if (object@administered_item_resp[i] == min_score) {
            rect(x - 0.25, min_score - 1, x + 0.25, y, col = "red", border = "black")
          } else {
            rect(x - 0.25, min_score - 1, x + 0.25, y, col = "lime green", border = "black")
          }
        }
      }
    } else {
      cat("output_Shadow is empty\n")
    }
  }
)

#' Draw an item exposure plot
#'
#' Draw a plot of item exposure rates
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param max_rate A target exposure rate.
#' @param theta_segment True or Estimated theta used to create segments ("Estimated" or "True").
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' true_theta <- runif(10, min = -3.5, max = 3.5)
#' resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
#' constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))
#' config_science <- createShadowTestConfig(
#'   MIP = list(solver = "LPSOLVE"),
#'   exposure_control = list(method = "ELIGIBILITY")
#' )
#' solution <- Shadow(itempool_science, config_science,
#'   true_theta, constraints_science2, data = resp_science)
#' p <- plotExposure(solution)
#'
#' @docType methods
#' @rdname plotExposure-methods
#' @export
setGeneric(
  name = "plotExposure",
  def = function(object, max_rate = 0.25, theta_segment = "Estimated", file_pdf = NULL, ...) {
    standardGeneric("plotExposure")
  }
)

#' @docType methods
#' @rdname plotExposure-methods
#' @export

setMethod(
  f = "plotExposure",
  signature = "list",
  definition = function(object, max_rate = 0.25, theta_segment = "Estimated", file_pdf = NULL, ...) {
    nj <- length(object$true_theta)
    ni <- ncol(object$usage_matrix)
    segment_cut   <- object$config@exposure_control$segment_cut
    n_segment     <- object$config@exposure_control$n_segment
    cut_lower     <- segment_cut[1:n_segment]
    cut_upper     <- segment_cut[2:(n_segment + 1)]
    segment_label <- character(n_segment)
    theta_segment_index <- numeric(nj)
    if (toupper(theta_segment) == "TRUE") {
      theta_segment_index <- find_segment(segment_cut, object$true_theta)
    } else {
      theta_segment_index <- find_segment(segment_cut, object$final_theta_est)
    }
    segment_n    <- numeric(n_segment)
    segment_dist <- table(theta_segment_index)
    segment_n[as.numeric(names(segment_dist))] <- segment_dist
    segment_index_table <- matrix(NA, nj, object$constraints$test_length)
    for (k in 1:n_segment) {
      if (k < n_segment) {
        segment_label[k] <- paste0("(", cut_lower[k], ",", cut_upper[k], "]")
      } else {
        segment_label[k] <- paste0("(", cut_lower[k], ",", cut_upper[k], ")")
      }
    }
    usage_matrix <- object$usage_matrix
    usage_matrix_final <- object$usage_matrix
    for (j in 1:nj) {
      usage_matrix_final[j, object$output[[j]]@administered_item_index[object$output[[j]]@theta_segment_index != theta_segment_index[j]]] <- FALSE
      segment_index_table[j, ] <- object$output[[j]]@theta_segment_index
    }

    segment_freq <- matrix(0, n_segment, n_segment)
    for (i in 1:object$constraints$test_length) {
      factor(segment_index_table[, i], levels = 1:n_segment)
      segment_table <- tapply(factor(segment_index_table[, i], levels = 1:n_segment), theta_segment_index, table)
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
      avg_visit = matrix(t(segment_rate),
      nrow = n_segment^2, ncol = 1)
    )
    exposure_rate               <- colSums(usage_matrix) / nj
    exposure_rate_final         <- colSums(usage_matrix_final) / nj
    exposure_rate_segment       <- vector("list", n_segment)
    exposure_rate_segment_final <- vector("list", n_segment)
    names(exposure_rate_segment)       <- segment_label
    names(exposure_rate_segment_final) <- segment_label

    for (k in 1:n_segment) {
      if (segment_n[k] > 2) {
        exposure_rate_segment[[k]] <- colMeans(usage_matrix[theta_segment_index == k, ])
        exposure_rate_segment_final[[k]] <- colMeans(usage_matrix_final[theta_segment_index == k, ])
      }
      if (is.null(exposure_rate_segment[[k]])) {
        exposure_rate_segment[[k]] <- numeric(ni)
      } else if (any(is.nan(exposure_rate_segment[[k]]))) {
        exposure_rate_segment[[k]][is.nan(exposure_rate_segment[[k]])] <- 0
      }
      if (is.null(exposure_rate_segment_final[[k]])) {
        exposure_rate_segment_final[[k]] <- numeric(ni)
      } else if (any(is.nan(exposure_rate_segment_final[[k]]))) {
        exposure_rate_segment_final[[k]][is.nan(exposure_rate_segment_final[[k]])] <- 0
      }
    }

    if (!is.null(file_pdf)) {
      pdf(file = file_pdf, ...)
    }

    old_oma <- par()$oma
    old_mar <- par()$mar
    on.exit(par(oma = old_oma, mar = old_mar))
    par(oma = c(3, 3, 0, 0), mar = c(3, 3, 2, 2))

    plotER(ni, exposure_rate, exposure_rate_final, max_rate = max_rate, title = "Overall", color = "black", color_final = "black", simple = TRUE)
    for (k in 1:n_segment) {
      plotER(ni, exposure_rate_segment[[k]], exposure_rate_segment_final[[k]], max_rate = max_rate, title = segment_label[k], color = "black", color_final = "black", simple = TRUE)
    }
    mtext(text = "Item", side = 1, line = 0, outer = T)
    mtext(text = "Exposure Rate", side = 2, line = 0, outer = T)
    if (!is.null(file_pdf)) {
      dev.off()
    }
    return(
      list(
        exposure_rate = exposure_rate,
        exposure_rate_segment = exposure_rate_segment,
        exposure_rate_segment_final = exposure_rate_segment_final,
        segment_rate_table = segment_rate_table,
        n_segment = n_segment,
        segment_n = segment_n,
        segment_cut = segment_cut,
        segment_label = segment_label
      )
    )
  }
)

#' Find matching theta to supplied probability
#'
#' Find theta corresponding to a response probability value for each item.
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param rp A response probability value.
#' @param max_iter A maximum number of iterations.
#' @param conv A convergence criterion.
#' @param start_theta A starting theta value.

calcRP <- function(object, rp = .50, max_iter = 100, conv = 0.0001, start_theta = 0) {
  RP <- numeric(object@ni)
  for (i in 1:object@ni) {
    max_score <- object@NCAT[i] - 1
    theta <- start_theta
    ep    <- as.vector(calcEscore(object@parms[[i]], theta)) / max_score
    gap   <- abs(rp - ep)
    done  <- gap < conv
    iter  <- 0
    while (!done && iter < max_iter) {
      iter  <- iter + 1
      h     <- gap / -calcFisher(object@parms[[i]], theta)
      theta <- theta - h
      ep    <- as.vector(calcEscore(object@parms[[i]], theta)) / max_score
      gap   <- abs(rp - ep)
      done  <- gap < conv
    }
    RP[i] <- theta
  }
  return(RP)
}

#' @rdname simResp-methods
#' @aliases simResp,pool_cluster,numeric-method

setMethod(
  f = "simResp",
  signature = c("pool_cluster", "list"),
  definition = function(object, theta) {
    if (length(theta) != length(object@np)) {
      data <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        if (all(!is.na(theta[[i]]))) {
          data[[i]] <- simResp(object@pools[[i]], theta[[i]])
        } else {
          stop(paste0("invalid values in theta", "[[", i, "]]"))
        }
      }
      return(data)
    } else {
      stop("length of theta not equal to np")
    }
  }
)

#' @name item_pool.operators
#' @title Item pool and pool cluster operators
#'
#' @description \code{pool1 + pool2} combines two \code{\linkS4class{item_pool}} objects.
#'
#' @param pool1 An \code{\linkS4class{item_pool}} object.
#' @param pool2 An \code{\linkS4class{item_pool}} object.
#'
#' @examples
#' itempool <- itempool_science + itempool_reading
#'
#' @rdname item_pool.operators
#' @export

`+.item_pool` <- function(pool1, pool2) {
  if (class(pool1) != "item_pool" || class(pool2) != "item_pool") stop("operarands must be of class \"item_pool\" ")
  if (validObject(pool1) && validObject(pool2)) {
    combined_pool <- new("item_pool")
    id        <- c(pool1@id, pool2@id)
    model     <- c(pool1@model, pool2@model)
    NCAT      <- c(pool1@NCAT, pool2@NCAT)
    parms     <- c(pool1@parms, pool2@parms)

    nfield1   <- dim(pool1@ipar)[2]
    nfield2   <- dim(pool2@ipar)[2]
    nfield    <- max(nfield1, nfield2)

    ipar1              <- matrix(NA, dim(pool1@ipar)[1], nfield)
    ipar1[, 1:nfield1] <- pool1@ipar
    ipar2              <- matrix(NA, dim(pool2@ipar)[1], nfield)
    ipar2[, 1:nfield2] <- pool2@ipar
    ipar               <- rbind(ipar1, ipar2)

    se1                <- matrix(NA, dim(pool1@se)[1], nfield)
    se1[, 1:nfield1]   <- pool1@se
    se2                <- matrix(NA, dim(pool2@se)[1], nfield)
    se2[, 1:nfield2]   <- pool2@se
    se                 <- rbind(se1, se2)

    is_unique <- which(!duplicated(id))

    combined_pool@ni      <- length(is_unique)
    combined_pool@max_cat <- max(NCAT[is_unique])
    combined_pool@index   <- 1:combined_pool@ni
    combined_pool@id      <- id[is_unique]
    combined_pool@model   <- model[is_unique]
    combined_pool@NCAT    <- NCAT[is_unique]
    combined_pool@parms   <- parms[is_unique]
    combined_pool@ipar    <- ipar[is_unique, , drop = FALSE]
    combined_pool@se      <- se[is_unique, , drop = FALSE]

    if (sum(duplicated(id)) > 0) {
      warning("duplicate items were found and removed")
      warning("duplicate ids: %s", paste0(id[duplicated(id)], collapse = ", "))
    }
    return(combined_pool)
  } else {
    stop("invlid pool(s) submitted")
  }
}

#' @description \code{pool1 - pool2} excludes the items in the second item pool from the first. The two \code{\linkS4class{item_pool}} objects must overlap for this to be performed.
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:500)
#' itempool <- itempool_science - subitempool
#'
#' @rdname item_pool.operators
#' @export

`-.item_pool` <- function(pool1, pool2) {
  if (class(pool1) != "item_pool" || class(pool2) != "item_pool") stop("operarands must be of class \"item_pool\" ")
  if (any(pool2@id %in% pool1@id)) {
    left <- which(!(pool1@id %in% pool2@id))
    if (length(left) > 0) {
      pool1@ni      <- length(left)
      pool1@max_cat <- max(pool1@NCAT[left])
      pool1@index   <- 1:length(left)
      pool1@id      <- pool1@id[left]
      pool1@model   <- pool1@model[left]
      pool1@NCAT    <- pool1@NCAT[left]
      pool1@parms   <- pool1@parms[left]
      pool1@ipar    <- pool1@ipar[left, , drop = FALSE]
      pool1@se      <- pool1@se[left, , drop = FALSE]
    } else {
      return("item pool is empty")
    }
  }
  return(pool1)
}

#' @description \code{pool1 == pool2} tests equality of the two item_pool objects.
#' @examples
#' itempool <- subsetItemPool(itempool_science, 1:500)
#' subitempool1 <- itempool_science - itempool
#' subitempool2 <- subsetItemPool(itempool_science, 501:1000)
#' subitempool1 == subitempool2  ## TRUE
#'
#' @rdname item_pool.operators
#' @export

`==.item_pool` <- function(pool1, pool2) {
  if (class(pool1) != "item_pool" || class(pool2) != "item_pool") stop("operarands must be of class \"item_pool\" ")
  return(identical(pool1, pool2))
}

#' @description \code{pool_cluster1 == pool_cluster2} tests equality of the two pool_cluster objects.
#'
#' @param pool_cluster1 A \code{\linkS4class{pool_cluster}} object.
#' @param pool_cluster2 A \code{\linkS4class{pool_cluster}} object.
#'
#' @examples
#' cluster1 <- makeItemPoolCluster(c(itempool_science, itempool_reading))
#' cluster2 <- makeItemPoolCluster(c(cluster1@pools[[1]], cluster1@pools[[2]]))
#' cluster1 == cluster2  ## TRUE
#'
#' @rdname item_pool.operators
#' @export

`==.pool_cluster` <- function(pool_cluster1, pool_cluster2) {
  if (class(pool_cluster1) != "pool_cluster" || class(pool_cluster2) != "pool_cluster") stop("operarands must be of class \"pool_cluster\" ")
  return(identical(pool_cluster1, pool_cluster2))
}

#' Extract
#'
#' @param x x
#' @param i i
#' @param j j
#' @param ... ...
#' @param drop drop
#'
#' @name extract-methods
#' @aliases [,test,ANY,ANY,ANY-method
#' @docType methods

setMethod(
  f = "[",
  signature = "test",
  definition = function(x, i, j, ...) {
    if (i == "pool") {
      return(x@pool)
    }
    if (i == "theta") {
      return(x@theta)
    }
    if (i == "prob") {
      return(x@prob)
    }
    if (i == "info") {
      return(x@info)
    }
    if (i == "true_theta") {
      return(x@true_theta)
    }
    if (i == "data") {
      return(x@data)
    }
  }
)

#' @name extract-methods
#' @aliases [,item_pool,ANY,ANY,ANY-method
#' @docType methods

setMethod(
  f = "[",
  signature = "item_pool",
  definition = function(x, i, j, ...) {
    if (i == "ni") {
      return(x@ni)
    }
    if (i == "max_cat") {
      return(x@max_cat)
    }
    if (i == "index") {
      return(x@index)
    }
    if (i == "id") {
      return(x@id)
    }
    if (i == "model") {
      return(x@model)
    }
    if (i == "NCAT") {
      return(x@NCAT)
    }
    if (i == "parms") {
      return(x@parms)
    }
    if (i == "ipar") {
      return(x@ipar)
    }
    if (i == "se") {
      return(x@se)
    }
  }
)

#' Create a subset of an item pool object
#'
#' Create a subset of an \code{\linkS4class{item_pool}} object.
#'
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param select A vector of indices identifying the items to subset.
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:100)
#' @export

subsetItemPool <- function(pool, select = NULL) {
  if (class(pool) != "item_pool") {
    stop("pool must be of class \"item_pool\"")
  }
  if (is.null(select)) {
    return(pool)
  } else if (all(select %in% 1:pool@ni)) {
    select           <- unique(select)
    n_select         <- length(select)
    sub_pool         <- new("item_pool")
    sub_pool@ni      <- n_select
    sub_pool@index   <- 1:n_select
    sub_pool@id      <- pool@id[select]
    sub_pool@model   <- pool@model[select]
    sub_pool@NCAT    <- pool@NCAT[select]
    sub_pool@parms   <- pool@parms[select]
    sub_pool@max_cat <- max(sub_pool@NCAT)
    sub_pool@ipar    <- pool@ipar[select, , drop = FALSE]
    sub_pool@se      <- pool@se[select, , drop = FALSE]
    return(sub_pool)
  } else {
    stop("select contains invalid item indices")
  }
}

#' Create a subset of a test object
#'
#' Create a subset of a test object.
#'
#' @param test An \code{\linkS4class{test}} object.
#' @param select A vector of item indices to subset.
#'
#' @examples
#' test <- makeTest(itempool_science, seq(-3, 3, 1))
#' subtest <- subsetTest(test, 1:100)
#' @export

subsetTest <- function(test, select = NULL) {
  if (class(test) != "test") {
    stop("test must be of class \"test\"")
  }
  if (is.null(select)) {
    return(test)
  } else if (all(select %in% 1:test@pool@ni) && anyDuplicated(select) == 0) {
    n_select            <- length(select)
    sub_test            <- new("test")
    sub_test@pool       <- subsetItemPool(test@pool, select = select)
    sub_test@theta      <- test@theta
    sub_test@prob       <- test@prob[select]
    sub_test@info       <- test@info[, select, drop = FALSE]
    sub_test@true_theta <- test@true_theta
    sub_test@data       <- test@data[, select, drop = FALSE]
    return(sub_test)
  } else {
    stop("select contains invalid values")
  }
}

#' Generate a test object
#'
#' Generate a \code{\linkS4class{test}} object
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param theta A grid of theta values.
#' @param info_type An information type.
#' @param true_theta An optional vector of true theta values to simulate response data.
#'
#' @docType methods
#' @rdname makeTest-methods
#'
#' @examples
#' test <- makeTest(itempool_science, seq(-3, 3, 1))
#' @export

setGeneric(
  name = "makeTest",
  def = function(object, theta = seq(-4, 4, .1), info_type = "FISHER", true_theta = NULL) {
    standardGeneric("makeTest")
  }
)

#' @docType methods
#' @rdname makeTest-methods
#' @export

setMethod(
  f = "makeTest",
  signature = "item_pool",
  definition = function(object, theta = seq(-4, 4, .1), info_type = "FISHER", true_theta = NULL) {
    prob <- calcProb(object, theta)
    if (toupper(info_type) == "FISHER") {
      info <- calcFisher(object, theta)
    } else {
      stop("Invalid info_type specified")
    }
    if (!is.null(true_theta)) {
      data <- simResp(object, true_theta)
    } else {
      data <- NULL ## this is provision for cases where data is imported from elsewhere
    }
    return(new("test", pool = object, theta = theta, prob = prob, info = info, true_theta = true_theta, data = data))
  }
)

#' Generate a test cluster object
#'
#' Generate a \code{\linkS4class{test_cluster}} object
#'
#' @param object An \code{\linkS4class{pool_cluster}} object
#' @param theta A grid of theta values
#' @param true_theta An optional vector of true theta values to simulate response data
#'
#' @docType methods
#' @rdname makeTestCluster-methods

setGeneric(
  name = "makeTestCluster",
  def = function(object, theta, true_theta) {
    standardGeneric("makeTestCluster")
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods

setMethod(
  f = "makeTestCluster",
  signature = c("pool_cluster", "numeric", "numeric"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta)
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods

setMethod(
  f = "makeTestCluster",
  signature = c("pool_cluster", "numeric", "list"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta[[p]])
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)

#' Generate maximum likelihood estimates of theta
#'
#' Generate maximum likelihood estimates of theta.
#'
#' @param object A \code{\linkS4class{item_pool}} object.
#' @param resp A vector (or matrix) of item responses.
#' @param start_theta An optional vector of start theta values.
#' @param max_iter Maximum number of iterations.
#' @param crit Convergence criterion.
#' @param select A vector of indices identifying the items to subset.
#' @param theta_range A range of theta values.
#' @param truncate Set \code{TRUE} to bound MLE to theta_range: c(minTheta, maxTheta).
#' @param max_change Maximum change between iterations.
#' @param do_Fisher \code{TRUE} to use Fisher's method of scoring.
#'
#' @docType methods
#' @rdname mle-methods
#' @examples
#' mle(itempool_fatigue, resp_fatigue_raw[10,])
#' @export

setGeneric(
  name = "mle",
  def = function(object, resp, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    standardGeneric("mle")
  }
)

#' @docType methods
#' @rdname mle-methods

setMethod(
  f = "mle",
  signature = "item_pool",
  definition = function(object, resp, start_theta = NULL, max_iter = 50, crit = 0.005, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    ni <- object@ni
    theta <- seq(min(theta_range), max(theta_range), .1)
    nq <- length(theta)
    if (is.vector(resp)) {
      nj <- 1
      resp <- matrix(resp, 1)
    } else if (is.matrix(resp)) {
      nj <- nrow(resp)
    } else if (is.data.frame(resp)) {
      nj <- nrow(resp)
      resp <- as.matrix(resp)
    } else {
      stop("resp must be of class vector, matrix, or data.frame")
    }
    if (!is.null(select)) {
      if (length(resp) != length(select)) {
        stop("resp and select must be equal in length when select is not NULL")
      }
      if (anyDuplicated(select) > 0) {
        warning("select contains duplicated indices")
        select <- select[-duplicated(select)]
      }
      if (!all(select %in% 1:ni)) {
        stop("select contains invalid indices")
      }
      items <- select
    } else {
      items <- 1:ni
    }
    if (ncol(resp) != length(items)) {
      stop("resp must be of length ni or match the length of select")
    }
    if (is.null(start_theta)) {
      start_theta <- eap(object, theta, rep(1 / nq, nq), resp, select = select)$th
    } else if (length(start_theta) == 1) {
      start_theta <- rep(start_theta, nj)
    } else if (length(start_theta) != nj) {
      stop("start_theta must be of length 1 or the number of examinees")
    }

    th    <- numeric(nj)
    se    <- numeric(nj)
    conv  <- logical(nj)
    trunc <- logical(nj)

    for (j in 1:nj) {
      theta_1 <- start_theta[j]
      max_raw_score <- sum(object@NCAT[items[!is.na(resp[j, ])]] - 1)
      raw_score <- sum(resp[j, ], na.rm = TRUE)
      if (raw_score > 0 && raw_score < max_raw_score) {
        converged <- FALSE
        done <- FALSE
        iteration <- 0
        while (!converged && !done && iteration <= max_iter) {
          iteration <- iteration + 1
          theta_0 <- theta_1
          deriv1 <- 0
          deriv2 <- 0
          for (i in 1:length(items)) {
            if (!is.na(resp[j, i])) {
              deriv1 <- deriv1 + calcJacobian(object@parms[[items[i]]], theta_0, resp[j, i])
              if (do_Fisher) {
                deriv2 <- deriv2 + calcFisher(object@parms[[items[i]]], theta_0)
              } else {
                deriv2 <- deriv2 - calcHessian(object@parms[[items[i]]], theta_0, resp[j, i])
              }
            }
          }
          change <- deriv1 / deriv2
          if (is.nan(change)) {
            done <- TRUE
          } else {
            if (abs(change) > max_change) {
              change <- sign(change) * max_change
            } else if (abs(change) < crit) {
              converged <- conv[j] <- TRUE
            }
            theta_1 <- theta_0 + change
          }
        }
      }
      if (conv[j]) {
        th[j] <- theta_1
        se[j] <- 1 / sqrt(abs(deriv2))
      } else {
        th[j] <- start_theta[j]
        sum_fisher <- 0
        for (i in 1:length(items)) {
          sum_fisher <- sum_fisher + calcFisher(object@parms[[items[i]]], th[j])
        }
        se[j] <- 1 / sqrt(sum_fisher)
      }
    }
    if (truncate) {
      min_theta <- min(theta_range)
      max_theta <- max(theta_range)
      th[th > max_theta] <- max_theta
      th[th < min_theta] <- min_theta
    }
    return(list(th = th, se = se, conv = conv, trunc = trunc))
  }
)

#' Generate maximum likelihood estimates of theta
#'
#' Generate maximum likelihood estimates of theta.
#'
#' @param object A \code{\linkS4class{test}} object.
#' @param start_theta An optional vector of start theta values.
#' @param max_iter Maximum number of iterations.
#' @param crit Convergence criterion.
#' @param select A vector of indices identifying the items to subset.
#' @param theta_range A range of theta values: c(minTheta, maxTheta).
#' @param truncate Set \code{TRUE} to bound MLE to theta_range.
#' @param max_change Maximum change between iterations.
#' @param do_Fisher Set \code{TRUE} to use Fisher's method of scoring.
#'
#' @docType methods
#' @rdname mlearray-methods

setGeneric(
  name = "MLE",
  def = function(object, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    standardGeneric("MLE")
  }
)

#' @docType methods
#' @rdname mlearray-methods

setMethod(
  f = "MLE",
  signature = "test",
  definition = function(object, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL, theta_range = c(-4, 4), truncate = FALSE, max_change = 1.0, do_Fisher = TRUE) {
    ni <- ncol(object@data)
    nj <- nrow(object@data)
    nq <- length(object@theta)

    if (is.null(select)) {
      select <- 1:object@pool@ni
      resp <- object@data
    } else {
      if (!all(select %in% 1:object@pool@ni)) {
        stop("select contains invalid item indices")
      }
      resp <- object@data[, unique(select)]
    }

    if (!is.null(select)) {
      if (anyDuplicated(select) > 0) {
        warning("select contains duplicated indices")
        select <- select[-duplicated(select)]
      }
      if (!all(select %in% 1:ni)) {
        stop("select contains invalid indices")
      }
      items <- select
    } else {
      items <- 1:ni
    }

    if (is.null(start_theta)) {
      prior <- rep(1 / nq, nq)
      start_theta <- EAP(object, prior, select = select)$th
    } else if (length(start_theta) == 1) {
      start_theta <- rep(start_theta, nj)
    } else if (length(start_theta) != nj) {
      stop("start_theta must be of length 1 or the number of examinees")
    }

    th <- numeric(nj)
    se <- numeric(nj)
    conv  <- logical(nj)
    trunc <- logical(nj)

    for (j in 1:nj) {
      theta_1 <- start_theta[j]
      max_raw_score <- sum(object@pool@NCAT[items[!is.na(object@data[j, items])]] - 1)
      raw_score <- sum(object@data[j, items], na.rm = TRUE)

      if (raw_score > 0 && raw_score < max_raw_score) {

        converged <- FALSE
        done <- FALSE
        iteration <- 0

        while (!converged && !done && iteration <= max_iter) {
          iteration <- iteration + 1
          theta_0 <- theta_1
          deriv1 <- 0
          deriv2 <- 0
          for (i in items) {
            resp <- object@data[j, i]
            deriv1 <- deriv1 + calcJacobian(object@pool@parms[[i]], theta_0, resp)
            if (do_Fisher) {
              deriv2 <- deriv2 + calcFisher(object@pool@parms[[i]], theta_0)
            } else {
              deriv2 <- deriv2 - calcHessian(object@pool@parms[[i]], theta_0, resp)
            }
          }
          change <- deriv1 / deriv2
          if (is.nan(change)) {
            done <- TRUE
          } else {
            if (abs(change) > max_change) {
              change <- sign(change) * max_change
            } else if (abs(change) < crit) {
              converged <- conv[j] <- TRUE
            }
            theta_1 <- theta_0 + change
          }
        }
      }

      if (conv[j]) {
        th[j] <- theta_1
        se[j] <- 1 / sqrt(abs(deriv2))
      } else {
        th[j] <- start_theta[j]
        sum_fisher <- 0
        for (i in 1:length(items)) {
          sum_fisher <- sum_fisher + calcFisher(object@parms[[items[i]]], th[j])
        }
        se[j] <- 1 / sqrt(sum_fisher)
      }

    }
    if (truncate) {
      min_theta <- min(theta_range)
      max_theta <- max(theta_range)
      th[th > max_theta] <- max_theta
      th[th < min_theta] <- min_theta
    }
    RMSE <- NULL
    if (!is.null(object@true_theta)) {
      RMSE <- sqrt(mean((th - object@true_theta)^2))
    }
    return(list(th = th, se = se, conv = conv, trunc = trunc, RMSE = RMSE))
  }
)

#' @docType methods
#' @rdname mlearray-methods

setMethod(
  f = "MLE",
  signature = "test_cluster",
  definition = function(object, start_theta = NULL, max_iter = 100, crit = 0.001, select = NULL) {
    MLE_cluster <- vector(mode = "list", length = object@nt)
    for (t in 1:object@nt) {
      MLE_cluster[[t]] <- MLE(object@tests[[t]], start_theta = start_theta, max_iter = max_iter, crit = crit, select = NULL)
    }
    return(MLE_cluster)
  }
)

#' Generate expected a posteriori estimates of theta
#'
#' Generate expected a posteriori estimates of theta.
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param theta A theta grid.
#' @param prior A prior distribution, a numeric vector for a common prior or a matrix for individualized priors.
#' @param resp A numeric matrix of item responses, one row per examinee.
#' @param select A vector of indices identifying the items to subset.
#'
#' @docType methods
#' @rdname eap-methods
#' @export

setGeneric(
  name = "eap",
  def = function(object, theta, prior, resp, select = NULL) {
    standardGeneric("eap")
  }
)

#' @docType methods
#' @rdname eap-methods

#' @export
setMethod(
  f = "eap",
  signature = "item_pool",
  definition = function(object, theta, prior, resp, select = NULL) {
    ni <- object@ni
    nq <- length(theta)
    prob <- calcProb(object, theta)
    if (is.vector(resp)) {
      nj <- 1
    } else if (is.matrix(resp)) {
      nj <- nrow(resp)
    } else if (is.data.frame(resp)) {
      nj <- nrow(resp)
      resp <- as.matrix(resp)
    } else {
      stop("resp must be of class either vector or matrix")
    }
    posterior <- matrix(rep(prior, nj), nj, nq, byrow = TRUE)
    if (length(prior) != nq) {
      stop("theta and prior must be equal in length")
    }
    if (!is.null(select)) {
      if (length(resp) != length(select)) {
        stop("resp and select must be equal in length when select is not NULL")
      }
      if (anyDuplicated(select) > 0) {
        warning("select contains duplicated indices")
        select <- select[-duplicated(select)]
        response <- response[-duplicated(select)]
      }
      if (!all(select %in% 1:ni)) {
        stop("select contains invalid indices")
      }
      items <- select
    } else {
      items <- 1:ni
    }
    if (nj == 1) {
      for (i in 1:length(items)) {
        if (resp[i] >= 0 && resp[i] < object@max_cat) {
          posterior <- posterior * prob[[items[i]]][, resp[i] + 1]
        }
      }
      th <- sum(posterior * theta) / sum(posterior)
      se <- sqrt(sum(posterior * (theta - th)^2) / sum(posterior))
    } else {
      for (i in items) {
        response <- matrix(resp[, i] + 1, nj, 1)
        if (!all(is.na(response))) {
          prob <- t(prob[[items[i]]][, response])
          prob[is.na(prob)] <- 1
          posterior <- posterior * prob
        }
      }
      th <- as.vector(posterior %*% theta / rowSums(posterior))
      se <- as.vector(sqrt(rowSums(posterior * (matrix(theta, nj, nq, byrow = TRUE) - matrix(th, nj, nq))^2) / rowSums(posterior)))
    }
    return(list(th = th, se = se))
  }
)

#' Generate expected a posteriori estimates of theta
#'
#' Generate expected a posteriori estimates of theta.
#'
#' @param object A \code{\linkS4class{test}} or a \code{\linkS4class{test_cluster}} object.
#' @param prior A prior distribution, a numeric vector for a common prior or a matrix for individualized priors.
#' @param select A vector of indices identifying the items to subset.
#' @param reset_prior Set \code{TRUE} to reset the prior distribution for each test when object is of class \code{\linkS4class{test_cluster}}.
#'
#' @docType methods
#' @rdname eaparray-methods

setGeneric(
  name = "EAP",
  def = function(object, prior, select = NULL, reset_prior = FALSE) {
    standardGeneric("EAP")
  }
)

#' @docType methods
#' @rdname eaparray-methods

setMethod(
  f = "EAP",
  signature = "test",
  definition = function(object, prior, select = NULL, reset_prior = FALSE) {
    nj <- nrow(object@data)
    if (is.matrix(prior)) {
      nq <- ncol(prior)
      if (nj != nrow(prior)) stop("nrow(prior) is not equal to nrow(data)")
      posterior <- prior
    } else {
      nq <- length(prior)
      posterior <- matrix(rep(prior, nj), nj, nq, byrow = TRUE)
    }
    if (is.null(select)) {
      select <- 1:object@pool@ni
    } else {
      if (!all(select %in% 1:object@pool@ni)) {
        stop("select contains invalid item indices")
      }
    }
    for (i in unique(select)) {
      resp <- matrix(object@data[, i] + 1, nj, 1)
      if (!all(is.na(resp))) {
        prob <- t(object@prob[[i]][, resp])
        prob[is.na(prob)] <- 1
        posterior <- posterior * prob
      }
    }
    th <- as.vector(posterior %*% object@theta / rowSums(posterior))
    se <- as.vector(sqrt(rowSums(posterior * (matrix(object@theta, nj, nq, byrow = TRUE) - matrix(th, nj, nq))^2) / rowSums(posterior)))
    if (is.null(object@true_theta)) {
      RMSE <- NULL
    } else {
      RMSE <- sqrt(mean((th - object@true_theta)^2))
    }
    return(list(th = th, se = se, prior = prior, posterior = posterior, RMSE = RMSE))
  }
)

#' @docType methods
#' @rdname eaparray-methods

setMethod(
  f = "EAP",
  signature = "test_cluster",
  definition = function(object, prior, select = NULL, reset_prior = FALSE) {
    EAP_cluster <- vector(mode = "list", length = object@nt)
    EAP_cluster[[1]] <- EAP(object@tests[[1]], prior, select)
    if (reset_prior) {
      for (t in 2:object@nt) {
        EAP_cluster[[t]] <- EAP(object@tests[[t]], prior, select)
      }
    } else {
      for (t in 2:object@nt) {
        EAP_cluster[[t]] <- EAP(object@tests[[t]], EAP_cluster[[t - 1]]@posterior, select)
      }
    }
    return(EAP_cluster)
  }
)

#' Create an item pool cluster object
#'
#' Create a \code{\linkS4class{pool_cluster}} object.
#'
#' @param pools A list of \code{\linkS4class{item_pool}} objects.
#' @param names An optional vector of \code{\linkS4class{item_pool}} names.
#' @examples
#'
#' cluster <- makeItemPoolCluster(c(itempool_science, itempool_reading))
#' @export
makeItemPoolCluster <- function(pools, names = NULL) {
  np <- length(pools)
  if (np == 0) {
    stop("pools is empty")
  } else if (np == 1) {
    stop("only one pool found in pools - expecting 2 or more")
  }
  if (is.null(names)) {
    names <- paste0("Pool_", 1:np)
  } else {
    if (length(names) != np) stop("pools and names are of different lengths")
  }
  pool_cluster <- new("pool_cluster")
  pool_cluster@np <- np
  pool_cluster@pools <- vector(mode = "list", length = np)
  pool_cluster@names <- names
  for (i in 1:np) {
    if (class(pools[[i]]) != "item_pool") stop(paste0("pool.list[[", i, "]] is not of class \"item_pool\""))
    pool_cluster@pools[[i]] <- pools[[i]]
  }
  if (validObject(pool_cluster)) {
    return(pool_cluster)
  }
}


#' Run computerized adaptive testing with generalized shadow-test approach
#'
#' Run computerized adaptive testing with generalized shadow-test approach.
#'
#' @param object An \code{\linkS4class{item_pool}} object. Use \code{\link{loadItemPool}} for this.
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param true_theta Numeric. A vector of true theta values to be used in simulation.
#' @param constraints A list representing optimization constraints. Use \code{\link{loadConstraints}} for this.
#' @param prior Numeric. A matrix or a vector containing priors.
#' @param prior_par Numeric. A vector of parameters for prior distribution.
#' @param data Numeric. A matrix containing item response data.
#' @param session Used to communicate with a Shiny session.
#'
#' @references{
#'   \insertRef{van_der_linden_model_1998}{TestDesign}
#'
#'   \insertRef{van_der_linden_optimal_1998}{TestDesign}
#'
#'   \insertRef{van_der_linden_optimal_2000}{TestDesign}
#'
#'   \insertRef{van_der_linden_linear_2005}{TestDesign}
#' }
#' @rdname Shadow-methods
#'
#' @examples
#' object <- itempool_science
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(itempool_science, config, true_theta, constraints_science)
#' solution$output
#' @export

setGeneric(
  name = "Shadow",
  def = function(object, config, true_theta = NULL, constraints = NULL, prior = NULL, prior_par = NULL, data = NULL, session = NULL) {
    standardGeneric("Shadow")
  }
)

#' @rdname Shadow-methods
#' @export

setMethod(
  f = "Shadow",
  signature = "item_pool",
  definition = function(object, config, true_theta, constraints, prior, prior_par, data, session) {
    if (!validObject(config)) {
      stop("invalid configuration options specified")
    }

    if (!is.null(constraints)) {
      ni <- constraints$ni
      ns <- constraints$ns
      nv <- constraints$nv
    } else {
      ni <- object@ni
    }

    model <- object@model
    model[which(model == "item_1PL")] <- 1
    model[which(model == "item_2PL")] <- 2
    model[which(model == "item_3PL")] <- 3
    model[which(model == "item_PC")]  <- 4
    model[which(model == "item_GPC")] <- 5
    model[which(model == "item_GR")]  <- 6
    model <- as.numeric(model)

    if (!is.null(true_theta)) {
      nj <- length(true_theta)
    } else if (!is.null(data)) {
      nj <- nrow(data)
    } else {
      stop("either true_theta or data should be provided at a minimum")
    }

    nq        <- length(config@theta_grid)
    min_theta <- min(config@theta_grid)
    max_theta <- max(config@theta_grid)
    exposure_control <- toupper(config@exposure_control$method)
    refresh_policy   <- toupper(config@refresh_policy$method)
    if (toupper(config@content_balancing$method %in% c("STA", "SHADOW", "SHADOWTEST", "SHADOW TEST"))) {
      if (is.null(constraints)) {
        stop("constraints must not be NULL for STA")
      } else {
        sta <- TRUE
        set_based    <- constraints$set_based
        test_length  <- constraints$test_length
        min_ni       <- constraints$test_length
        max_ni       <- constraints$test_length
        refresh_shadow <- rep(FALSE, test_length)
        if (refresh_policy %in% c("ALWAYS", "THRESHOLD")) {
          refresh_shadow <- rep(TRUE, test_length)
        } else if (refresh_policy == "POSITION") {
          if (all(config@refresh_policy$position %in% 1:test_length)) {
            refresh_shadow[config@refresh_policy$position] <- TRUE
          } else {
            stop("invalid entries in config@refresh_policy$position")
          }
        } else if (refresh_policy %in% c("INTERVAL", "INTERVAL-THRESHOLD")) {
          if (config@refresh_policy$interval >= 1 && config@refresh_policy$interval <= test_length) {
            refresh_shadow[seq(1, test_length, config@refresh_policy$interval)] <- TRUE
          } else {
            stop("invalid entry in config@refresh_policy$interval")
          }
        } else if (refresh_policy %in% c("STIMULUS", "SET", "PASSAGE")) {
          if (!set_based) {
            stop("set_based must be TRUE when config@refresh_policy$method equals \"STIMULUS\"")
          }
        }
        refresh_shadow[1] <- TRUE
      }
    } else {
      sta <- FALSE
      set_based <- FALSE
      min_ni <- config@stopping_criterion$min_ni
      max_ni <- config@stopping_criterion$max_ni
      max_se <- config@stopping_criterion$se_threshold
    }

    if (!is.null(data)) {
      test <- makeTest(object, config@theta_grid, info_type = "FISHER", true_theta = NULL)
      data <- as.matrix(data)
      for (i in 1:ni) {
        invalid_resp <- !(data[, i] %in% 0:(object@NCAT[i] - 1))
        data[invalid_resp, i] <- NA
      }
      test@data <- data
    } else if (!is.null(true_theta)) {
      test <- makeTest(object, config@theta_grid, info_type = "FISHER", true_theta)
    } else {
      stop("both data and true_theta cannot be NULL")
    }

    max_info <- max(test@info)

    if (is.null(prior)) {
      if (!is.null(prior_par)) {
        if (is.vector(prior_par) && length(prior_par) == 2) {
          posterior <- matrix(dnorm(config@theta_grid, mean = prior_par[1], sd = prior_par[2]), nj, nq, byrow = TRUE)
        } else if (is.matrix(prior_par) && all(dim(prior_par) == c(nj, 2))) {
          posterior <- matrix(NA, nj, nq)
          for (j in 1:nj) {
            posterior[j, ] <- dnorm(config@theta_grid, mean = prior_par[j, 1], sd = prior_par[j, 2])
          }
        } else {
          stop("prior_par must be a vector of length 2, c(mean, sd), or a matrix of dim c(nj x 2)")
        }
      } else if (toupper(config@interim_theta$prior_dist) == "NORMAL") {
        posterior <- matrix(dnorm(config@theta_grid, mean = config@interim_theta$prior_par[1], sd = config@interim_theta$prior_par[2]), nj, nq, byrow = TRUE)
      } else if (toupper(config@interim_theta$prior_dist) == "UNIFORM") {
        posterior <- matrix(1, nj, nq)
      } else {
        stop("invalid configuration option for interim_theta$prior_dist")
      }
    } else if (is.vector(prior) && length(prior) == nq) {
      posterior <- matrix(prior, nj, nq, byrow = TRUE)
    } else if (is.matrix(prior) && all(dim(prior) == c(nj, nq))) {
      posterior <- prior
    } else {
      stop("misspecification for prior or prior_par")
    }

    if (toupper(config@interim_theta$method) %in% c("EB", "FB")) {
      n_sample <- config@MCMC$burn_in + config@MCMC$post_burn_in
      if (toupper(config@interim_theta$method) == "FB" || toupper(config@final_theta$method) == "FB") {
        ipar_list <- iparPosteriorSample(object, n_sample)
      }
    }
    if (!is.null(config@item_selection$initial_theta)) {
      initial_theta <- rep(config@item_selection$initial_theta, nj)
    } else {
      initial_theta <- as.vector(posterior %*% matrix(config@theta_grid, ncol = 1))
    }
    items_administered <- matrix(FALSE, nj, ni)
    output_list <- vector(mode = "list", length = nj)
    if (exposure_control %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
      item_eligibility_control <- TRUE
      max_exposure_rate   <- config@exposure_control$max_exposure_rate
      fading_factor       <- config@exposure_control$fading_factor
      acceleration_factor <- config@exposure_control$acceleration_factor

      n_segment <- config@exposure_control$n_segment
      if (!length(max_exposure_rate) %in% c(1, n_segment)) {
        stop("length(max_exposure_rate) must be 1 or n_segment")
      }

      true_segment_freq  <- numeric(n_segment)
      est_segment_freq   <- numeric(n_segment)
      true_segment_count <- numeric(nj)
      est_segment_count  <- numeric(nj)
      segment_cut  <- config@exposure_control$segment_cut
      cut_lower    <- segment_cut[1:n_segment]
      cut_upper    <- segment_cut[2:(n_segment + 1)]
      pe_i <- matrix(1, n_segment, ni)

      if (set_based) {
        pe_s <- matrix(1, n_segment, ns)
      } else {
        pe_s <- NULL
        alpha_sjk <- NULL
        rho_sjk <- NULL
      }

      if (config@exposure_control$diagnostic_stats) {
        alpha_g_i   <- matrix(0, nrow = nj, ncol = n_segment * ni)
        epsilon_g_i <- matrix(0, nrow = nj, ncol = n_segment * ni)
        if (set_based) {
          alpha_g_s   <- matrix(0, nrow = nj, ncol = n_segment * ns)
          epsilon_g_s <- matrix(0, nrow = nj, ncol = n_segment * ns)
        }
        if (fading_factor != 1) {
          noFading_alpha_g_i   <- matrix(0, nrow = nj, ncol = n_segment * ni)
          noFading_epsilon_g_i <- matrix(0, nrow = nj, ncol = n_segment * ni)
          if (set_based) {
            noFading_alpha_g_s   <- matrix(0, nrow = nj, ncol = n_segment * ns)
            noFading_epsilon_g_s <- matrix(0, nrow = nj, ncol = n_segment * ns)
          }
        }
      }

      if (!is.null(config@exposure_control$initial_eligibility_stats)) {
        n_jk      <- config@exposure_control$initial_eligibility_stats$n_jk
        alpha_ijk <- config@exposure_control$initial_eligibility_stats$alpha_ijk
        phi_jk    <- config@exposure_control$initial_eligibility_stats$phi_jk
        rho_ijk   <- config@exposure_control$initial_eligibility_stats$rho_ijk
        if (set_based) {
          alpha_sjk <- config@exposure_control$initial_eligibility_stats$alpha_sjk
          rho_sjk   <- config@exposure_control$initial_eligibility_stats$rho_sjk
        }
      } else {
        n_jk      <- numeric(n_segment)
        alpha_ijk <- matrix(0, n_segment, ni)
        phi_jk    <- numeric(n_segment)
        rho_ijk   <- matrix(0, n_segment, ni)
        if (set_based) {
          alpha_sjk <- matrix(0, n_segment, ns)
          rho_sjk   <- matrix(0, n_segment, ns)
        }
      }

      if (fading_factor != 1) {
        no_fading_n_jk      <- n_jk
        no_fading_alpha_ijk <- alpha_ijk
        no_fading_rho_ijk   <- rho_ijk
        if (set_based) {
          no_fading_alpha_sjk <- alpha_sjk
          no_fading_rho_sjk   <- rho_sjk
        }
      }

    } else {
      item_eligibility_control <- FALSE
      true_segment_count <- NULL
      est_segment_count  <- NULL
    }
    if (!is.null(config@item_selection$fixed_theta)) {
      if (length(config@item_selection$fixed_theta) == 1) {
        info_fixed_theta <- vector(mode = "list", length = nj)
        info_fixed_theta[1:nj] <- test@info[which.min(abs(test@theta - config@item_selection$fixed_theta)), ]
        config@item_selection$fixed_theta <- rep(config@item_selection$fixed_theta, nj)
        select_at_fixed_theta <- TRUE
      } else if (length(config@item_selection$fixed_theta) == nj) {
        info_fixed_theta <- lapply(seq_len(nj), function(j) calc_info(config@item_selection$fixed_theta[j], object@ipar, object@NCAT, model))
        select_at_fixed_theta <- TRUE
      } else {
        stop("length of config@item_selection$fixed_theta must be either 1 or nj")
      }
    } else {
      select_at_fixed_theta <- FALSE
    }

    if (set_based) {
      usage_matrix <- matrix(FALSE, nrow = nj, ncol = nv)
    } else {
      usage_matrix <- matrix(FALSE, nrow = nj, ncol = ni)
    }

    getInfo <- function() {
      if (select_at_fixed_theta) {
        info <- info_fixed_theta[[j]]
      } else if (config@item_selection$method == "MPWI") {
        info <- as.vector(matrix(posterior[j, ], nrow = 1) %*% test@info)
      } else if (config@item_selection$method == "MFI") {
        info <- calc_info(current_theta, object@ipar, object@NCAT, model)
      } else if (config@item_selection$method == "EB") {
        info <- calc_info_EB(output@posterior_sample, object@ipar, object@NCAT, model)
      } else if (config@item_selection$method == "FB") {
        if (config@item_selection$info_type == "FISHER") {
          info <- calc_info_FB(output@posterior_sample, ipar_list, object@NCAT, model)
        } else if (config@item_selection$info_type %in% c("MI", "MUTUAL")) {
          info <- calc_MI_FB(output@posterior_sample, ipar_list, object@NCAT, model)
        }
      } else {
        stop("invalid option for config@item_selection$method")
      }
      return(info)
    }

    selectItem <- function() {
      if (position > 1) {
        info[output@administered_item_index[1:(position - 1)]] <- -1
      }
      info_index    <- order(info, decreasing = TRUE)
      item_selected <- info_index[1]
      if (item_selected %in% output@administered_item_index[1:(position - 1)]) {
        stop(sprintf("the selected item %i has been already administered", item_selected))
      }
      return(item_selected)
    }

    selectItemShadowTest <- function() {
      n_remaining <- test_length - position
      new_stimulus_selected <- FALSE
      last_stimulus_index <- 0
      if (!set_based) {
        stimulus_selected <- NA
        stimulus_finished <- FALSE
      }
      if (position == 1) {
        selected <- 1
        if (set_based) {
          stimulus_selected <- optimal$shadow_test[["STINDEX"]][1]
          new_stimulus_selected <- TRUE
          if (sum(optimal$shadow_test[["STINDEX"]] == stimulus_selected) == 1) {
            stimulus_finished <- TRUE
          } else {
            stimulus_finished <- FALSE
          }
        }
      } else {
        remaining <- which(!optimal$shadow_test[["INDEX"]] %in% output@administered_item_index[1:(position - 1)])
        if (!set_based) {
          selected <- remaining[1]
        } else {
          last_stimulus_index <- output@administered_stimulus_index[position - 1]
          if (any(optimal$shadow_test[["STINDEX"]][remaining] == last_stimulus_index)) {
            remaining_in_stimulus <- remaining[which(optimal$shadow_test[["STINDEX"]][remaining] == last_stimulus_index)]
            selected <- remaining_in_stimulus[1]
          } else {
            selected <- remaining[1]
          }
          stimulus_selected <- optimal$shadow_test[["STINDEX"]][selected]
          if (last_stimulus_index != stimulus_selected) {
            new_stimulus_selected <- TRUE
          }
          if (n_remaining == 0) {
            stimulus_finished <- TRUE
          } else {
            if (sum(optimal$shadow_test[["STINDEX"]][remaining] == stimulus_selected) == 1) {
              stimulus_finished <- TRUE
            } else {
              stimulus_finished <- FALSE
            }
          }
        }
      }
      item_selected <- optimal$shadow_test[["INDEX"]][selected]
      return(
        list(
          item_selected = item_selected,
          stimulus_selected = stimulus_selected,
          stimulus_finished = stimulus_finished,
          last_stimulus_index = last_stimulus_index,
          new_stimulus_selected = new_stimulus_selected,
          n_remaining = n_remaining
        )
      )
    }

    plotAuditTrail <- function() {

      old_mar   <- par()$mar
      old_mfrow <- par()$mfrow
      on.exit(par(mar = old_mar, mfrow = old_mfrow))
      par(mar = c(2, 3, 1, 1) + 0.1, mfrow = c(2, 1))

      plot(1:max_ni, seq(min_theta, max_theta, length = max_ni), main = paste0("Examinee ", j), xlab = "Items Administered", ylab = "Theta", type = "n", las = 1)
      points(1:max_ni, output@interim_theta_est, type = "b", pch = 9, col = "blue")

      if (!is.null(true_theta)) {
        abline(h = output@true_theta, lty = 2, col = "red")
      } else {
        abline(h = output@final_theta_est, lty = 2, col = "red")
      }

      item_string <- paste(output@administered_item_index, collapse = ",")
      text(1, max_theta, paste0("Items: ", item_string), cex = 0.7, adj = 0)
      text(1, min_theta + 0.3, paste("Theta: ", round(output@final_theta_est, digits = 2), " SE: ", round(output@final_se_est, digits = 2)), cex = 0.8, adj = 0)

      for (i in 1:max_ni) {
        lines(rep(i, 2), c(output@interim_theta_est[i] - 1.96 * output@interim_se_est[i], output@interim_theta_est[i] + 1.96 * output@interim_se_est[i]))
        if (sta) {
          if (output@shadow_test_refreshed[i]) {
            points(i, output@interim_theta_est[i], pch = 18, col = "red")
          }
        }
      }

      resp_string <- paste(output@administered_item_resp, collapse = ",")
      plot(config@theta_grid, output@posterior, main = "Final Posterior Distribution", xlab = "Theta", ylab = "Posterior", type = "l", col = "blue", yaxt = "n")
      text(min_theta, max(output@posterior), paste0("Responses: ", resp_string), cex = 0.7, adj = 0)

    }

    pb <- txtProgressBar(0, nj, char = "|", style = 3)

    ###########################################################################################
    ############## FOR LOOP OVER SIMULEES #####################################################
    ###########################################################################################

    for (j in 1:nj) {
      output <- new("output_Shadow")
      output@simulee_id <- j

      if (!is.null(true_theta)) {
        output@true_theta <- true_theta[j]
      } else {
        output@true_theta <- NULL
      }

      output@prior <- posterior[j, ]
      output@administered_item_index <- numeric(max_ni)
      output@administered_item_resp <- numeric(max_ni)
      output@theta_segment_index <- numeric(max_ni)
      output@interim_theta_est <- numeric(max_ni)
      output@interim_se_est <- numeric(max_ni)
      output@administered_stimulus_index <- NaN
      output@shadow_test <- vector(mode = "list", length = max_ni)

      if (config@interim_theta$method %in% c("EAP", "MLE")) {
        current_theta <- initial_theta[j]
      } else if (toupper(config@interim_theta$method) %in% c("EB", "FB")) {
        if (is.vector(prior_par) && length(prior_par) == 2) {
          output@prior_par <- prior_par
        } else if (is.matrix(prior_par) && all(dim(prior_par) == c(nj, 2))) {
          output@prior_par <- prior_par[j, ]
        } else {
          output@prior_par <- config@interim_theta$prior_par
        }
        output@posterior_sample <- rnorm(n_sample, mean = output@prior_par[1], sd = output@prior_par[2])
        output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
        current_theta <- mean(output@posterior_sample)
        current_se <- sd(output@posterior_sample) * config@MCMC$jump_factor
      }

      if (set_based) {
        output@administered_stimulus_index <- numeric(max_ni)
        end_set <- TRUE
        finished_stimulus_index <- NULL
        finished_stimulus_item_count <- NULL
      }

      if (sta) {
        output@shadow_test_feasible <- logical(test_length)
        output@shadow_test_refreshed <- logical(test_length)
        imat <- NULL
        idir <- NULL
        irhs <- NULL
        if (set_based) {
          smat <- NULL
          sdir <- NULL
          srhs <- NULL
        }
      }

      likelihood   <- rep(1, nq)
      theta_change <- 10000
      done         <- FALSE
      position     <- 0

      if (exposure_control %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
        ineligible_i <- matrix(0, n_segment, ni)
        prob_random <- matrix(runif(n_segment * ni), n_segment, ni)
        ineligible_i[prob_random >= pe_i] <- 1
        if (set_based) {
          ineligible_s <- matrix(0, n_segment, ns)
          prob_random <- matrix(runif(n_segment * ns), n_segment, ns)
          ineligible_s[prob_random >= pe_s] <- 1
          for (k in 1:n_segment) {
            for (s in which(ineligible_s[k, ] == 1)) {
              ineligible_i[k, constraints$item_index_by_stimulus[[s]]] <- 1
            }
            for (s in which(ineligible_s[k, ] == 0)) {
              ineligible_i[k, constraints$item_index_by_stimulus[[s]]] <- 0
            }
          }
        }
        if (exposure_control %in% c("ELIGIBILITY")) {
          xmat <- NULL
          xdir <- NULL
          xrhs <- NULL
        }
      }

      while (!done) {
        position <- position + 1
        info <- getInfo()
        if (sta) {
          if (exposure_control %in% c("ELIGIBILITY", "BIGM")) {
            if (!is.null(config@exposure_control$first_segment) && length(config@exposure_control$first_segment) >= position && all(config@exposure_control$first_segment >= 1) && all(config@exposure_control$first_segment <= n_segment)) {
              output@theta_segment_index[position] <- config@exposure_control$first_segment[position]
            } else {
              output@theta_segment_index[position] <- find_segment(segment_cut, current_theta)
            }
          } else if (exposure_control %in% c("BIGM-BAYESIAN")) {
            sample_segment <- find_segment(segment_cut, output@posterior_sample)
            segment_distribution <- table(sample_segment) / length(sample_segment)
            segment_classified <- as.numeric(names(segment_distribution))
            segment_prob <- numeric(n_segment)
            segment_prob[segment_classified] <- segment_distribution
            output@theta_segment_index[position] <- which.max(segment_prob)
          }
          if (position == 1 ||
            (refresh_policy == "ALWAYS") ||
            (refresh_policy %in% c("POSITION", "INTERVAL") && refresh_shadow[position]) ||
            (refresh_policy == "THRESHOLD" && abs(theta_change) > config@refresh_policy$threshold) ||
            (refresh_policy == "INTERVAL-THRESHOLD" && refresh_shadow[position] && abs(theta_change) > config@refresh_policy$threshold) ||
            (refresh_policy %in% c("STIMULUS", "SET", "PASSAGE") && set_based && end_set)) {
            output@shadow_test_refreshed[position] <- TRUE
            if (position > 1) {
              imat <- matrix(0, nrow = position - 1, ncol = nv)
              for (p in 1:(position - 1)) {
                imat[p, output@administered_item_index[p]] <- 1
              }
              idir <- rep("==", position - 1)
              irhs <- rep(1, position - 1)
              if (set_based) {
                if (sum(output@administered_stimulus_index[1:(position - 1)] > 0) > 0) {
                  administered_stimulus_index <- unique(output@administered_stimulus_index[1:(position - 1)])
                  administered_stimulus_index <- administered_stimulus_index[administered_stimulus_index > 0]
                  smat <- matrix(0, nrow = length(administered_stimulus_index), ncol = nv)
                  for (s in 1:length(administered_stimulus_index)) {
                    smat[s, ni + administered_stimulus_index[s]] <- 1
                  }
                  sdir <- rep("==", length(administered_stimulus_index))
                  srhs <- rep(1, length(administered_stimulus_index))
                  imat <- rbind(imat, smat)
                  idir <- c(idir, sdir)
                  irhs <- c(irhs, srhs)
                  if (refresh_policy %in% c("STIMULUS", "SET", "PASSAGE") && set_based && end_set) {
                    n_administered_stimulus <- length(administered_stimulus_index)
                    if (n_administered_stimulus > 0) {
                      smat <- matrix(0, nrow = n_administered_stimulus, ncol = nv)
                      sdir <- rep("==", n_administered_stimulus)
                      srhs <- numeric(n_administered_stimulus)
                      for (s in 1:n_administered_stimulus) {
                        smat[s, constraints$item_index_by_stimulus[[administered_stimulus_index[s]]]] <- 1
                        srhs[s] <- sum(output@administered_stimulus_index[1:(position - 1)] == administered_stimulus_index[s])
                      }
                      imat <- rbind(imat, smat)
                      idir <- c(idir, sdir)
                      irhs <- c(irhs, srhs)
                    }
                  } else {
                    n_finished_stimulus <- length(finished_stimulus_index)
                    if (n_finished_stimulus > 0) {
                      smat <- matrix(0, nrow = n_finished_stimulus, ncol = nv)
                      sdir <- rep("==", n_finished_stimulus)
                      srhs <- finished_stimulus_item_count
                      for (s in 1:n_finished_stimulus) {
                        smat[s, constraints$item_index_by_stimulus[[finished_stimulus_index[s]]]] <- 1
                      }
                      imat <- rbind(imat, smat)
                      idir <- c(idir, sdir)
                      irhs <- c(irhs, srhs)
                    }
                  }
                }
              }
            }
            if (item_eligibility_control) {
              item_ineligible <- ineligible_i[output@theta_segment_index[position], ]
              if (set_based) {
                stimulus_ineligible <- ineligible_s[output@theta_segment_index[position], ]
              }
              if (position > 1) {
                item_ineligible[output@administered_item_index[1:(position - 1)]] <- 0
                if (set_based) {
                  stimulus_ineligible[unique(output@administered_stimulus_index[1:(position - 1)])] <- 0
                }
              }
              if (exposure_control %in% c("ELIGIBILITY")) {
                if (any(item_ineligible == 1)) {
                  xmat <- numeric(nv)
                  xmat[1:ni] <- item_ineligible
                  xdir <- "=="
                  xrhs <- 0
                  if (set_based) {
                    if (any(stimulus_ineligible == 1)) {
                      xmat[(ni + 1):nv] <- stimulus_ineligible
                      for (s in which(stimulus_ineligible == 1)) {
                        xmat[constraints$item_index_by_stimulus[[s]]] <- 1
                      }
                      for (s in which(stimulus_ineligible == 0)) {
                        xmat[constraints$item_index_by_stimulus[[s]]] <- 0
                      }
                    }
                  }
                }

                optimal <- STA(constraints, info, xmat = rbind(xmat, imat), xdir = c(xdir, idir), xrhs = c(xrhs, irhs), maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$time_limit, gap_limit = config@MIP$gap_limit, solver = config@MIP$solver)

                is_optimal <- isOptimal(optimal$status, config@MIP$solver)
                if (!is_optimal) {
                  output@shadow_test_feasible[position] <- FALSE
                  optimal <- STA(constraints, info, xmat = imat, xdir = idir, xrhs = irhs, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$time_limit, gap_limit = config@MIP$gap_limit, solver = config@MIP$solver)
                } else {
                  output@shadow_test_feasible[position] <- TRUE
                }

              } else if (exposure_control %in% c("BIGM", "BIGM-BAYESIAN")) {
                if (!is.null(config@exposure_control$M)) {
                  info[item_ineligible == 1] <- info[item_ineligible == 1] - config@exposure_control$M
                } else {
                  info[item_ineligible == 1] <- -1 * max_info - 1
                }
                optimal <- STA(constraints, info, xmat = imat, xdir = idir, xrhs = irhs, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$time_limit, gap_limit = config@MIP$gap_limit, solver = config@MIP$solver)
                output@shadow_test_feasible[position] <- TRUE
              }
            } else {
              optimal <- STA(constraints, info, xmat = imat, xdir = idir, xrhs = irhs, maximize = TRUE, mps = FALSE, lp = FALSE, verbosity = config@MIP$verbosity, time_limit = config@MIP$time_limit, gap_limit = config@MIP$gap_limit, solver = config@MIP$solver)
              output@shadow_test_feasible[position] <- TRUE
            }

            is_optimal <- isOptimal(optimal$status, config@MIP$solver)
            if (!is_optimal) {
              stop(sprintf("MIP returned non-zero status: Examinee %i at position %i", j, position))
            }

            output@solve_time[position] <- optimal$solve_time

          } else {
            output@shadow_test_refreshed[position] <- FALSE
            output@shadow_test_feasible[position]  <- TRUE
          }
          selection <- selectItemShadowTest()
          output@administered_item_index[position] <- selection$item_selected
          output@shadow_test[[position]]           <- optimal$shadow_test[["INDEX"]]
        } else {
          output@administered_item_index[position] <- selectItem()
        }
        if (set_based) {
          output@administered_stimulus_index[position] <- selection$stimulus_selected
          if (selection$stimulus_finished) {
            end_set <- TRUE
          } else {
            end_set <- FALSE
          }
          if (selection$new_stimulus_selected && selection$last_stimulus_index != 0) {
            finished_stimulus_index      <- c(finished_stimulus_index, selection$last_stimulus_index)
            finished_stimulus_item_count <- c(finished_stimulus_item_count, sum(output@administered_stimulus_index[1:(position - 1)] == selection$last_stimulus_index))
          }
        }
        output@administered_item_resp[position] <- test@data[j, output@administered_item_index[position]]
        items_administered[j, output@administered_item_index[position]] <- TRUE
        prob_resp      <- test@prob[[output@administered_item_index[position]]][, output@administered_item_resp[position] + 1]
        posterior[j, ] <- posterior[j, ] * prob_resp
        likelihood     <- likelihood * prob_resp
        if (toupper(config@interim_theta$method) == "EAP") {
          output@interim_theta_est[position] <- sum(posterior[j, ] * test@theta) / sum(posterior[j, ])
          output@interim_se_est[position]    <- sqrt(sum(posterior[j, ] * (test@theta - output@interim_theta_est[position])^2) / sum(posterior[j, ]))
          if (toupper(config@interim_theta$prior_dist) == "NORMAL" && config@interim_theta$shrinkage_correction) {
            output@interim_theta_est[position] <- output@interim_theta_est[position] * (1 + output@interim_se_est[position]^2)
            if (output@interim_se_est[position] < config@interim_theta$prior_par[2]) {
              output@interim_se_est[position] <- 1 / sqrt(1 / output@interim_se_est[position]^2 - 1 / config@interim_theta$prior_par[2]^2)
            }
          }
        } else if (toupper(config@interim_theta$method) == "MLE") {
          interim_EAP <- sum(posterior[j, ] * test@theta) / sum(posterior[j, ])
          interim_MLE <- mle(object, output@administered_item_resp[1:position], start_theta = interim_EAP, theta_range = config@interim_theta$bound_ml, max_iter = config@interim_theta$max_iter, crit = config@interim_theta$crit, select = output@administered_item_index[1:position])
          output@interim_theta_est[position] <- interim_MLE$th
          output@interim_se_est[position]    <- interim_MLE$se
        } else if (toupper(config@interim_theta$method) %in% c("EB", "FB")) {
          current_item <- output@administered_item_index[position]
          if (toupper(config@interim_theta$method == "EB")) {
            output@posterior_sample <- theta_EB_single(
              n_sample, current_theta, current_se,
              object@ipar[current_item, ],
              output@administered_item_resp[position], object@NCAT[current_item],
              model[current_item], 1, c(current_theta, current_se)
            )
          } else {
            output@posterior_sample <- theta_FB_single(
              n_sample, current_theta, current_se, ipar_list[[current_item]],
              object@ipar[current_item, ],
              output@administered_item_resp[position], object@NCAT[current_item],
              model[current_item], 1, c(current_theta, current_se)
            )
          }
          output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
          output@interim_theta_est[position] <- mean(output@posterior_sample)
          output@interim_se_est[position] <- sd(output@posterior_sample)
        } else {
          stop("invalid interim_theta@method specified")
        }
        theta_change  <- output@interim_theta_est[position] - current_theta
        current_theta <- output@interim_theta_est[position]
        current_se    <- output@interim_se_est[position]
        if (refresh_policy == "THRESHOLD") {
          if ((abs(theta_change) > config@refresh_policy$threshold) && (position < test_length)) {
            refresh_shadow[position + 1] <- TRUE
          }
        }
        if (position == max_ni) {
          done <- TRUE
          output@likelihood <- likelihood
          output@posterior <- posterior[j, ]
        }
      }

      if (identical(config@final_theta, config@interim_theta)) {
        output@final_theta_est <- output@interim_theta_est[position]
        output@final_se_est   <- output@interim_se_est[position]
      } else if (toupper(config@final_theta$method == "EAP")) {

        if (toupper(config@final_theta$prior_dist) == "NORMAL") {
          final_prior <- dnorm(config@theta_grid, mean = config@final_theta$prior_par[1], sd = config@final_theta$prior_par[2])
        } else if (toupper(config@final_theta$prior_dist) == "UNIFORM") {
          final_prior <- rep(1, nq)
        } else {
          stop("invalid configuration option for final_theta$prior_dist")
        }

        output@posterior       <- output@likelihood * final_prior
        output@final_theta_est <- sum(output@posterior * config@theta_grid) / sum(output@posterior)
        output@final_se_est    <- sqrt(sum(output@posterior * (config@theta_grid - output@final_theta_est)^2) / sum(output@posterior))
        if (toupper(config@final_theta$prior_dist) == "NORMAL" && config@final_theta$shrinkageCorrection) {
          output@final_theta_est <- output@final_theta_est * (1 + output@final_se_est^2)
          if (output@final_se_est < config@final_theta$prior_par[2]) {
            output@final_se_est <- 1 / sqrt(1 / output@final_se_est^2 - 1 / config@final_theta$prior_par[2]^2)
          }
        }

      } else if (toupper(config@final_theta$method) == "MLE") {
        final_MLE <- mle(object, output@administered_item_resp[1:max_ni], start_theta = output@interim_theta_est[max_ni], theta_range = config@final_theta$bound_ml, max_iter = config@final_theta$max_iter, crit = config@final_theta$crit, select = output@administered_item_index[1:max_ni], truncate = config@final_theta$truncateML)
        output@final_theta_est <- final_MLE$th
        output@final_se_est    <- final_MLE$se
      } else if (toupper(config@final_theta$method) %in% c("EB", "FB")) {

        if (toupper(config@interim_theta$method) == toupper(config@final_theta$method) && identical(config@interim_theta$prior_par, config@final_theta$prior_par)) {
          output@final_theta_est <- output@interim_theta_est[position]
          output@final_se_est    <- output@interim_se_est[position]
        } else {
          output@posterior_sample <- rnorm(n_sample, mean = output@prior_par[1], sd = output@prior_par[2])
          output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
          current_theta <- mean(output@posterior_sample)
          current_se    <- sd(output@posterior_sample) * config@MCMC$jump_factor
          if (toupper(config@final_theta$method == "EB")) {
            output@posterior_sample <- theta_EB(
              n_sample, current_theta, current_se,
              object@ipar[output@administered_item_index[1:position], ],
              output@administered_item_resp[1:position], object@NCAT[output@administered_item_index[1:position]],
              model[output@administered_item_index[1:position]], 1, c(current_theta, current_se)
            )
          } else {
            output@posterior_sample <- theta_FB(
              n_sample, current_theta, current_se, ipar_list[output@administered_item_index[1:position]],
              object@ipar[output@administered_item_index[1:position], ],
              output@administered_item_resp[1:position], object@NCAT[output@administered_item_index[1:position]],
              model[output@administered_item_index[1:position]], 1, c(current_theta, current_se)
            )
          }
          output@posterior_sample <- output@posterior_sample[seq(from = config@MCMC$burn_in + 1, to = n_sample, by = config@MCMC$thin)]
          output@final_theta_est  <- mean(output@posterior_sample)
          output@final_se_est     <- sd(output@posterior_sample)
        }

      }

      usage_matrix[j, output@administered_item_index] <- TRUE

      if (set_based) {
        usage_matrix[j, ni + unique(output@administered_stimulus_index)] <- TRUE
      }

      output_list[[j]] <- output

      if (item_eligibility_control) {
        if (!is.null(true_theta)) {
          segment_true <- find_segment(segment_cut, output@true_theta)
          output_list[[j]]@true_theta_segment <- segment_true
          true_segment_freq[segment_true] <- true_segment_freq[segment_true] + 1
          true_segment_count[j]           <- true_segment_freq[segment_true]
        }
        segment_final <- find_segment(segment_cut, output@final_theta_est)
        eligible_in_final_segment <- ineligible_i[segment_final, ] == 0
        est_segment_freq[segment_final] <- est_segment_freq[segment_final] + 1
        est_segment_count[j]            <- est_segment_freq[segment_final]

        segment_visited <- sort(unique(output@theta_segment_index))
        segment_other   <- segment_visited[segment_visited != segment_final]

        if (exposure_control %in% c("ELIGIBILITY")) {
          n_jk[segment_final] <- fading_factor * n_jk[segment_final] + 1
          alpha_ijk[segment_final, ] <- fading_factor * alpha_ijk[segment_final, ]
          alpha_ijk[segment_final, output@administered_item_index] <- alpha_ijk[segment_final, output@administered_item_index] + 1
          if (length(segment_other) > 0) {
            if (any(!eligible_in_final_segment[output@administered_item_index])) {
              for (k in segment_other) {
                for (i in output@administered_item_index[output@theta_segment_index == k]) {
                  if (!eligible_in_final_segment[i]) {
                    alpha_ijk[k, i] <- alpha_ijk[k, i] + 1
                  }
                }
              }
            }
          }
          if (fading_factor != 1) {
            no_fading_n_jk[segment_final] <- no_fading_n_jk[segment_final] + 1
            no_fading_alpha_ijk[segment_final, output@administered_item_index] <- no_fading_alpha_ijk[segment_final, output@administered_item_index] + 1
          }

          segment_feasible   <- unique(output@theta_segment_index[output@shadow_test_feasible == TRUE])
          segment_infeasible <- unique(output@theta_segment_index[output@shadow_test_feasible == FALSE])
          phi_jk[segment_final]    <- fading_factor * phi_jk[segment_final]
          rho_ijk[segment_final, ] <- fading_factor * rho_ijk[segment_final, ]

          if (segment_final %in% segment_feasible) {
            phi_jk[segment_final] <- phi_jk[segment_final] + 1
            rho_ijk[segment_final, eligible_in_final_segment] <- rho_ijk[segment_final, eligible_in_final_segment] + 1
            if (fading_factor != 1) {
              no_fading_rho_ijk[segment_final, eligible_in_final_segment] <- no_fading_rho_ijk[segment_final, eligible_in_final_segment] + 1
            }
          } else {
            rho_ijk[segment_final, ] <- rho_ijk[segment_final, ] + 1
            if (fading_factor != 1) {
              no_fading_rho_ijk[segment_final, ] <- no_fading_rho_ijk[segment_final, ] + 1
            }
          }

          nf_ijk <- matrix(n_jk / phi_jk, n_segment, ni)

          if (acceleration_factor > 1) {
            p_alpha_ijk <- alpha_ijk / matrix(n_jk, n_segment, ni)
            p_rho_ijk <- rho_ijk / matrix(n_jk, n_segment, ni)
            p_alpha_ijk[is.na(p_alpha_ijk)] <- 0
            p_rho_ijk[is.na(p_rho_ijk)] <- 1
            flag_alpha_ijk <- p_alpha_ijk > max_exposure_rate
            if (length(max_exposure_rate) == n_segment) {
              for (k in 1:n_segment) {
                pe_i[k, flag_alpha_ijk[k, ]]  <- 1 - nf_ijk[k, flag_alpha_ijk[k, ]] + (max_exposure_rate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^acceleration_factor * nf_ijk[k, flag_alpha_ijk[k, ]] * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                pe_i[k, !flag_alpha_ijk[k, ]] <- 1 - nf_ijk[k, !flag_alpha_ijk[k, ]] + max_exposure_rate[k] * nf_ijk[k, !flag_alpha_ijk[k, ]] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
              }
            } else {
              pe_i[flag_alpha_ijk]  <- 1 - nf_ijk[flag_alpha_ijk] + (max_exposure_rate / p_alpha_ijk[flag_alpha_ijk])^acceleration_factor * nf_ijk[flag_alpha_ijk] * p_rho_ijk[flag_alpha_ijk]
              pe_i[!flag_alpha_ijk] <- 1 - nf_ijk[!flag_alpha_ijk] + max_exposure_rate * nf_ijk[!flag_alpha_ijk] * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
            }
          } else {
            pe_i <- 1 - nf_ijk + max_exposure_rate * nf_ijk * rho_ijk / alpha_ijk
          }

          pe_i[is.na(pe_i) | alpha_ijk == 0] <- 1
          pe_i[pe_i > 1] <- 1

          if (set_based) {
            alpha_sjk[segment_final, ] <- fading_factor * alpha_sjk[segment_final, ]
            alpha_sjk[segment_final, output@administered_stimulus_index] <- alpha_sjk[segment_final, output@administered_stimulus_index] + 1
            eligible_set_in_final_segment <- ineligible_s[segment_final, ] == 0
            if (fading_factor != 1) {
              no_fading_alpha_sjk[segment_final, output@administered_stimulus_index] <- no_fading_alpha_sjk[segment_final, output@administered_stimulus_index] + 1
            }
            rho_sjk[segment_final, ] <- fading_factor * rho_sjk[segment_final, ]
            if (segment_final %in% segment_feasible) {
              rho_sjk[segment_final, eligible_set_in_final_segment] <- rho_sjk[segment_final, eligible_set_in_final_segment] + 1
              if (fading_factor != 1) {
                no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] <- no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] + 1
              }
            } else {
              rho_sjk[segment_final, ] <- rho_sjk[segment_final, ] + 1
              if (fading_factor != 1) {
                no_fading_rho_sjk[segment_final, ] <- no_fading_rho_sjk[segment_final, ] + 1
              }
            }
            nf_sjk <- matrix(n_jk / phi_jk, n_segment, ns)
            if (acceleration_factor > 1) {
              p_alpha_sjk <- alpha_sjk / matrix(n_jk, n_segment, ns)
              p_rho_sjk   <- rho_sjk / matrix(n_jk, n_segment, ns)
              p_alpha_sjk[is.na(p_alpha_sjk)] <- 0
              p_rho_sjk[is.na(p_rho_sjk)]     <- 1
              flag_alpha_sjk <- p_alpha_sjk > max_exposure_rate
              if (length(max_exposure_rate) == n_segment) {
                for (k in 1:n_segment) {
                  pe_s[k, flag_alpha_sjk[k, ]]  <- 1 - nf_sjk[k, flag_alpha_sjk[k, ]] + (max_exposure_rate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^acceleration_factor * nf_sjk[k, flag_alpha_sjk[k, ]] * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                  pe_s[k, !flag_alpha_sjk[k, ]] <- 1 - nf_sjk[k, !flag_alpha_sjk[k, ]] + max_exposure_rate[k] * nf_sjk[k, !flag_alpha_sjk[k, ]] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                }
              } else {
                pe_s[flag_alpha_sjk] <- 1 - nf_sjk[flag_alpha_sjk] + (max_exposure_rate / p_alpha_sjk[flag_alpha_sjk])^acceleration_factor * nf_sjk[flag_alpha_sjk] * p_rho_sjk[flag_alpha_sjk]
                pe_s[!flag_alpha_sjk] <- 1 - nf_sjk[!flag_alpha_sjk] + max_exposure_rate * nf_sjk[!flag_alpha_sjk] * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
              }
            } else {
              pe_s <- 1 - nf_sjk + max_exposure_rate * nf_sjk * rho_sjk / alpha_sjk
            }
            pe_s[is.na(pe_s) | alpha_sjk == 0] <- 1
            pe_s[pe_s > 1] <- 1
          }

        } else if (exposure_control %in% c("BIGM")) {

          n_jk[segment_final] <- fading_factor * n_jk[segment_final] + 1
          alpha_ijk[segment_final, ] <- fading_factor * alpha_ijk[segment_final, ]
          alpha_ijk[segment_final, output@administered_item_index] <- alpha_ijk[segment_final, output@administered_item_index] + 1

          if (length(segment_other) > 0) {
            if (any(!eligible_in_final_segment[output@administered_item_index])) {
              for (k in segment_other) {
                for (i in output@administered_item_index[output@theta_segment_index == k]) {
                  if (!eligible_in_final_segment[i]) {
                    alpha_ijk[k, i] <- alpha_ijk[k, i] + 1
                  }
                }
              }
            }
          }

          rho_ijk[segment_final, ] <- fading_factor * rho_ijk[segment_final, ]
          rho_ijk[segment_final, eligible_in_final_segment] <- rho_ijk[segment_final, eligible_in_final_segment] + 1

          if (fading_factor != 1) {
            no_fading_n_jk[segment_final] <- no_fading_n_jk[segment_final] + 1
            no_fading_alpha_ijk[segment_final, output@administered_item_index] <- no_fading_alpha_ijk[segment_final, output@administered_item_index] + 1
            no_fading_rho_ijk[segment_final, eligible_in_final_segment] <- no_fading_rho_ijk[segment_final, eligible_in_final_segment] + 1
          }

          if (acceleration_factor > 1) {

            p_alpha_ijk <- alpha_ijk / matrix(n_jk, n_segment, ni)
            p_rho_ijk   <- rho_ijk / matrix(n_jk, n_segment, ni)
            p_alpha_ijk[is.na(p_alpha_ijk)] <- 0
            p_rho_ijk[is.na(p_rho_ijk)]     <- 1
            flag_alpha_ijk <- p_alpha_ijk > max_exposure_rate
            if (length(max_exposure_rate) == n_segment) {
              for (k in 1:n_segment) {
                pe_i[k, flag_alpha_ijk[k, ]]  <- (max_exposure_rate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^acceleration_factor * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                pe_i[k, !flag_alpha_ijk[k, ]] <- max_exposure_rate[k] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
              }
            } else {
              pe_i[flag_alpha_ijk]  <- (max_exposure_rate / p_alpha_ijk[flag_alpha_ijk])^acceleration_factor * p_rho_ijk[flag_alpha_ijk]
              pe_i[!flag_alpha_ijk] <- max_exposure_rate * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
            }

          } else {
            pe_i <- max_exposure_rate * rho_ijk / alpha_ijk
          }

          pe_i[is.na(pe_i) | alpha_ijk == 0] <- 1
          pe_i[pe_i > 1] <- 1

          if (set_based) {
            alpha_sjk[segment_final, ] <- fading_factor * alpha_sjk[segment_final, ]
            alpha_sjk[segment_final, output@administered_stimulus_index] <- alpha_sjk[segment_final, output@administered_stimulus_index] + 1
            eligible_set_in_final_segment <- ineligible_s[segment_final, ] == 0
            rho_sjk[segment_final, ] <- fading_factor * rho_sjk[segment_final ]
            rho_sjk[segment_final, eligible_set_in_final_segment] <- rho_sjk[segment_final, eligible_set_in_final_segment] + 1

            if (fading_factor != 1) {
              no_fading_alpha_sjk[segment_final, output@administered_stimulus_index] <- no_fading_alpha_sjk[segment_final, output@administered_stimulus_index] + 1
              no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] <- no_fading_rho_sjk[segment_final, eligible_set_in_final_segment] + 1
            }

            if (acceleration_factor > 1) {
              p_alpha_sjk <- alpha_sjk / matrix(n_jk, n_segment, ns)
              p_rho_sjk   <- rho_sjk / matrix(n_jk, n_segment, ns)
              p_alpha_sjk[is.na(p_alpha_sjk)] <- 0
              p_rho_sjk[is.na(p_rho_sjk)]     <- 1
              flag_alpha_sjk <- p_alpha_sjk > max_exposure_rate
              if (length(max_exposure_rate) == n_segment) {
                for (k in 1:n_segment) {
                  pe_s[k, flag_alpha_sjk[k, ]]  <- (max_exposure_rate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^acceleration_factor * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                  pe_s[k, !flag_alpha_sjk[k, ]] <- max_exposure_rate[k] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                }
              } else {
                pe_s[flag_alpha_sjk]  <- (max_exposure_rate / p_alpha_sjk[flag_alpha_sjk])^acceleration_factor * p_rho_sjk[flag_alpha_sjk]
                pe_s[!flag_alpha_sjk] <- max_exposure_rate * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
              }

            } else {
              pe_s <- max_exposure_rate * rho_sjk / alpha_sjk
            }
            pe_s[is.na(pe_s) | alpha_sjk == 0] <- 1
            pe_s[pe_s > 1] <- 1
          }

        } else if (exposure_control %in% c("BIGM-BAYESIAN")) {

          segment_visited <- sort(unique(output@theta_segment_index))
          sample_segment  <- find_segment(segment_cut, output@posterior_sample)
          segment_distribution <- table(sample_segment) / length(sample_segment)
          segment_classified   <- as.numeric(names(segment_distribution))
          segment_prob <- numeric(n_segment)
          segment_prob[segment_classified] <- segment_distribution

          n_jk      <- fading_factor * n_jk + segment_prob
          rho_ijk   <- fading_factor * rho_ijk
          alpha_ijk <- fading_factor * alpha_ijk
          alpha_ijk[, output@administered_item_index] <- alpha_ijk[, output@administered_item_index] + segment_prob

          if (length(segment_other) > 0) {
            if (any(!eligible_in_final_segment[output@administered_item_index])) {
              for (k in segment_other) {
                for (i in output@administered_item_index[output@theta_segment_index == k]) {
                  if (!eligible_in_final_segment[i]) {
                    alpha_ijk[k, i] <- alpha_ijk[k, i] + segment_prob[k]
                  }
                }
              }
            }
          }

          for (segment in 1:n_segment) {
            eligible <- ineligible_i[segment, ] == 0
            rho_ijk[segment, eligible] <- rho_ijk[segment, eligible] + segment_prob[segment]
          }
          if (fading_factor != 1) {
            no_fading_n_jk <- no_fading_n_jk + segment_prob
            no_fading_alpha_ijk[, output@administered_item_index] <- no_fading_alpha_ijk[, output@administered_item_index] + segment_prob
            for (segment in 1:n_segment) {
              eligible <- ineligible_i[segment, ] == 0
              no_fading_rho_ijk[segment, eligible] <- no_fading_rho_ijk[segment, eligible] + segment_prob[segment]
            }
          }
          if (acceleration_factor > 1) {
            p_alpha_ijk <- alpha_ijk / matrix(n_jk, n_segment, ni)
            p_rho_ijk   <- rho_ijk / matrix(n_jk, n_segment, ni)
            p_alpha_ijk[is.na(p_alpha_ijk)] <- 0
            p_rho_ijk[is.na(p_rho_ijk)]     <- 1
            flag_alpha_ijk <- p_alpha_ijk > max_exposure_rate
            if (length(max_exposure_rate) == n_segment) {
              for (k in 1:n_segment) {
                pe_i[k, flag_alpha_ijk[k, ]] <- (max_exposure_rate[k] / p_alpha_ijk[k, flag_alpha_ijk[k, ]])^acceleration_factor * p_rho_ijk[k, flag_alpha_ijk[k, ]]
                pe_i[k, !flag_alpha_ijk[k, ]] <- max_exposure_rate[k] * rho_ijk[k, !flag_alpha_ijk[k, ]] / alpha_ijk[k, !flag_alpha_ijk[k, ]]
              }
            } else {
              pe_i[flag_alpha_ijk]  <- (max_exposure_rate / p_alpha_ijk[flag_alpha_ijk])^acceleration_factor * p_rho_ijk[flag_alpha_ijk]
              pe_i[!flag_alpha_ijk] <- max_exposure_rate * rho_ijk[!flag_alpha_ijk] / alpha_ijk[!flag_alpha_ijk]
            }

          } else {
            pe_i <- max_exposure_rate * rho_ijk / alpha_ijk
          }

          pe_i[is.na(pe_i) | alpha_ijk == 0] <- 1
          pe_i[pe_i > 1] <- 1
          if (set_based) {
            alpha_sjk <- fading_factor * alpha_sjk
            rho_sjk   <- fading_factor * rho_sjk
            alpha_sjk[, output@administered_stimulus_index] <- alpha_sjk[, output@administered_stimulus_index] + segment_prob
            for (segment in 1:n_segment) {
              rho_sjk[segment, ineligible_s[segment, ] == 0] <- rho_sjk[segment, ineligible_s[segment, ] == 0] + segment_prob[segment]
            }
            if (fading_factor != 1) {
              no_fading_alpha_sjk[, output@administered_stimulus_index] <- no_fading_alpha_sjk[, output@administered_stimulus_index] + segment_prob
              for (segment in 1:n_segment) {
                no_fading_rho_sjk[segment, ineligible_s[segment, ] == 0] <- no_fading_rho_sjk[segment, ineligible_s[segment, ] == 0] + segment_prob[k]
              }
            }
            if (acceleration_factor > 1) {
              p_alpha_sjk <- alpha_sjk / matrix(n_jk, n_segment, ns)
              p_rho_sjk <- rho_sjk / matrix(n_jk, n_segment, ns)
              p_alpha_sjk[is.na(p_alpha_sjk)] <- 0
              p_rho_sjk[is.na(p_rho_sjk)] <- 1
              flag_alpha_sjk <- p_alpha_sjk > max_exposure_rate
              if (length(max_exposure_rate) == n_segment) {
                for (k in 1:n_segment) {
                  pe_s[k, flag_alpha_sjk[k, ]]  <- (max_exposure_rate[k] / p_alpha_sjk[k, flag_alpha_sjk[k, ]])^acceleration_factor * p_rho_sjk[k, flag_alpha_sjk[k, ]]
                  pe_s[k, !flag_alpha_sjk[k, ]] <- max_exposure_rate[k] * rho_sjk[k, !flag_alpha_sjk[k, ]] / alpha_sjk[k, !flag_alpha_sjk[k, ]]
                }
              } else {
                pe_s[flag_alpha_sjk]  <- (max_exposure_rate / p_alpha_sjk[flag_alpha_sjk])^acceleration_factor * p_rho_sjk[flag_alpha_sjk]
                pe_s[!flag_alpha_sjk] <- max_exposure_rate * rho_sjk[!flag_alpha_sjk] / alpha_sjk[!flag_alpha_sjk]
              }
            } else {
              pe_s <- max_exposure_rate * rho_sjk / alpha_sjk
            }
            pe_s[is.na(pe_s) | alpha_sjk == 0] <- 1
            pe_s[pe_s > 1] <- 1
          }
        }

        if (config@exposure_control$diagnostic_stats) {

          for (g in 1:n_segment) {
            alpha_g_i[j, (g - 1) * ni + 1:ni]   <- alpha_ijk[g, ]
            epsilon_g_i[j, (g - 1) * ni + 1:ni] <- rho_ijk[g, ]
            if (set_based) {
              alpha_g_s[j, (g - 1) * ns + 1:ns]   <- alpha_sjk[g, ]
              epsilon_g_s[j, (g - 1) * ns + 1:ns] <- rho_sjk[g, ]
            }
          }

          if (fading_factor != 1) {
            for (g in 1:n_segment) {
              no_fading_alpha_g_i[j, (g - 1) * ni + 1:ni]   <- no_fading_alpha_ijk[g, ]
              no_fading_epsilon_g_i[j, (g - 1) * ni + 1:ni] <- no_fading_rho_ijk[g, ]
              if (set_based) {
                no_fading_alpha_g_s[j, (g - 1) * ns + 1:ns]   <- no_fading_alpha_sjk[g, ]
                no_fading_epsilon_g_s[j, (g - 1) * ns + 1:ns] <- no_fading_rho_sjk[g, ]
              }
            }
          }

        }

      }

      if (config@audit_trail) {
        plotAuditTrail()
      }

      if (!is.null(session)) {
        shinyWidgets::updateProgressBar(session = session, id = "pb", value = j, total = nj)
      } else {
        setTxtProgressBar(pb, j)
      }

    }

    final_theta_est <- unlist(lapply(1:nj, function(j) output_list[[j]]@final_theta_est))
    final_se_est    <- unlist(lapply(1:nj, function(j) output_list[[j]]@final_se_est))
    exposure_rate <- colSums(usage_matrix) / nj

    eligibility_stats           <- NULL
    check_eligibility_stats     <- NULL
    no_fading_eligibility_stats <- NULL

    if (item_eligibility_control) {
      eligibility_stats <- list(pe_i = pe_i, n_jk = n_jk, alpha_ijk = alpha_ijk, phi_jk = phi_jk, rho_ijk = rho_ijk, pe_s = pe_s, alpha_sjk = alpha_sjk, rho_sjk = rho_sjk)
      if (config@exposure_control$diagnostic_stats) {
        check_eligibility_stats <- as.data.frame(cbind(1:nj, true_theta, find_segment(segment_cut, true_theta), true_segment_count, alpha_g_i, epsilon_g_i), row.names = NULL)
        names(check_eligibility_stats) <- c("Examinee", "TrueTheta", "TrueSegment", "TrueSegmentCount", paste("a", "g", rep(1:n_segment, rep(ni, n_segment)), "i", rep(1:ni, n_segment), sep = "_"), paste("e", "g", rep(1:n_segment, rep(ni, n_segment)), "i", rep(1:ni, n_segment), sep = "_"))
        if (set_based) {
          check_eligibility_stats_stimulus <- as.data.frame(cbind(alpha_g_s, epsilon_g_s), row.names = NULL)
          names(check_eligibility_stats_stimulus) <- c(paste("a", "g", rep(1:n_segment, rep(ns, n_segment)), "s", rep(1:ns, n_segment), sep = "_"), paste("e", "g", rep(1:n_segment, rep(ns, n_segment)), "s", rep(1:ns, n_segment), sep = "_"))
          check_eligibility_stats <- cbind(check_eligibility_stats, check_eligibility_stats_stimulus)
        }
        if (fading_factor != 1) {
          no_fading_eligibility_stats <- as.data.frame(cbind(1:nj, true_theta, find_segment(segment_cut, true_theta), true_segment_count, noFading_alpha_g_i, noFading_epsilon_g_i), row.names = NULL)
          names(no_fading_eligibility_stats) <- c("Examinee", "TrueTheta", "TrueSegment", "TrueSegmentCount", paste("a", "g", rep(1:n_segment, rep(ni, n_segment)), "i", rep(1:ni, n_segment), sep = "_"), paste("e", "g", rep(1:n_segment, rep(ni, n_segment)), "i", rep(1:ni, n_segment), sep = "_"))
          if (set_based) {
            no_fading_eligibility_stats_stimulus <- as.data.frame(cbind(noFading_alpha_g_s, noFading_epsilon_g_s), row.names = NULL)
            names(no_fading_eligibility_stats_stimulus) <- c(paste("a", "g", rep(1:n_segment, rep(ns, n_segment)), "s", rep(1:ns, n_segment), sep = "_"), paste("e", "g", rep(1:n_segment, rep(ns, n_segment)), "s", rep(1:ns, n_segment), sep = "_"))
            no_fading_eligibility_stats <- cbind(no_fading_eligibility_stats, no_fading_eligibility_stats_stimulus)
          }
        }
      }
    }
    if (sta) {
      freq_infeasible <- table(unlist(lapply(1:nj, function(j) sum(!output_list[[j]]@shadow_test_feasible))))
    } else {
      freq_infeasible <- NULL
    }
    return(
      list(
        output = output_list, pool = object, config = config, true_theta = true_theta, constraints = constraints,
        prior = prior, prior_par = prior_par, data = test@data, final_theta_est = final_theta_est, final_se_est = final_se_est,
        exposure_rate = exposure_rate, usage_matrix = usage_matrix, true_segment_count = true_segment_count, est_segment_count = est_segment_count,
        eligibility_stats = eligibility_stats, check_eligibility_stats = check_eligibility_stats, no_fading_eligibility_stats = no_fading_eligibility_stats,
        freq_infeasible = freq_infeasible
      )
    )
  }
)

#' Add transparancy to color
#'
#' Add transparancy to color.
#'
#' @param color A vector of color names or RGB color codes.
#' @param alpha A vector of integers between 0 and 255 (0 = fully transparent, 255 = fully visible).

addTrans <- function(color, alpha) {
  if (length(color) != length(alpha) & !any(c(length(color), length(alpha)) == 1)) {
    stop("Vector lengths not correct")
  }
  if (length(color) == 1 & length(alpha) > 1) {
    color <- rep(color, length(alpha))
  }
  if (length(alpha) == 1 & length(color) > 1) {
    alpha <- rep(alpha, length(color))
  }
  num2hex <- function(x) {
    hex <- unlist(strsplit("0123456789ABCDEF", split = ""))
    return(paste(hex[(x - x %% 16) / 16 + 1], hex[x %% 16 + 1], sep = ""))
  }
  rgb <- rbind(col2rgb(color), alpha)
  res <- paste("#", apply(apply(rgb, 2, num2hex), 2, paste, collapse = ""), sep = "")
  return(res)
}

#' Draw item eligibility statistics plots
#'
#' Draw item eligibility statistics plots.
#'
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param object An object containing eligibility statistics generated by \code{\link{Shadow}}.
#' @param object_no_fading An object containing eligibility statistics generated without fading.
#' @param file The filename of an object containing eligibility statistics generated by \code{\link{Shadow}}.
#' @param file_no_fading The filename of an object containing eligibility statistics generated without fading.
#' @param segment A theta segment index.
#' @param items A vector of item indices to generate the plots.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param max_rate A target item exposure rate.
#' @param discard_first A integer identifying the first x simulees to discard as burn-in.

plotEligibilityStats <- function(config, object = NULL, object_no_fading = NULL, file = NULL, file_no_fading = NULL, segment = 1, items = c(1), file_pdf = NULL, max_rate = 0.25, discard_first = NULL) {
  fading_factor <- config@exposure_control$fading_factor
  if (!is.null(file_pdf)) {
    pdf(file = file_pdf)
  }
  if (is.null(object) && is.null(file)) {
    stop("Both object and file are NULL")
  } else if (!is.null(object)) {
    eligibility_stats <- object
  } else if (!is.null(file)) {
    eligibility_stats <- read.csv(file, header = TRUE, sep = ",")
  }

  eligibility_stats_no_fading <- NULL
  if (!is.null(object_no_fading)) {
    eligibility_stats_no_fading <- object_no_fading
  } else if (!is.null(file_no_fading)) {
    eligibility_stats_no_fading <- read.csv(file_no_fading, header = TRUE, sep = ",")
  }

  eligibility_stats_segment <- split(eligibility_stats, eligibility_stats$TrueSegment)[[segment[1]]]
  if (!is.null(eligibility_stats_no_fading)) {
    eligibility_stats_segment_no_fading <- split(eligibility_stats_no_fading, eligibility_stats_no_fading$TrueSegment)[[segment[1]]]
  }
  if (!is.null(discard_first) && discard_first < nrow(eligibility_stats_segment)) {
    eligibility_stats_segment <- eligibility_stats_segment[eligibility_stats_segment$TrueSegmentCount > discard_first, ]
    if (!is.null(eligibility_stats_no_fading)) {
      eligibility_stats_segment_no_fading <- eligibility_stats_segment_no_fading[eligibility_stats_segment_no_fading$TrueSegmentCount > discard_first, ]
    }
  }

  examinee <- eligibility_stats_segment$TrueSegmentCount
  n_examinee <- length(examinee)
  fading_examinee <- examinee
  for (j in 2:length(examinee)) {
    fading_examinee[j] <- fading_examinee[j - 1] * fading_factor + 1
  }

  for (i in items) {
    alpha <- eligibility_stats_segment[[paste("a_g", segment, "i", i, sep = "_")]]
    epsilon <- eligibility_stats_segment[[paste("e_g", segment, "i", i, sep = "_")]]
    p_alpha <- alpha / fading_examinee
    p_epsilon <- epsilon / fading_examinee
    p_epsilon[p_epsilon > 1] <- 1
    p_eligibility <- rep(1, n_examinee)
    for (j in 2:n_examinee) {
      if (alpha[j - 1] > 0) {
        p_eligibility[j] <- min(epsilon[j - 1] * max_rate / alpha[j - 1], 1)
      }
    }
    if (!is.null(eligibility_stats_no_fading)) {
      alpha_no_fading <- eligibility_stats_segment_no_fading[[paste("a_g", segment, "i", i, sep = "_")]]
      epsilon_no_fading <- eligibility_stats_segment_no_fading[[paste("e_g", segment, "i", i, sep = "_")]]
      p_alpha_no_fading <- alpha_no_fading / examinee
      p_epsilon_no_fading <- epsilon_no_fading / examinee
    }
    plot(examinee, p_alpha, main = paste("Segment", segment, "- Item", i), type = "n", ylim = c(0, 1), xlab = "Examinees", ylab = "Rate")
    lines(examinee, p_alpha, col = "red", lty = 1, lwd = 3)
    lines(examinee, p_epsilon, col = "blue", lty = 2, lwd = 3)
    lines(examinee, p_eligibility, col = "purple", lty = 3, lwd = 3)
    if (is.null(eligibility_stats_no_fading)) {
      legend("topright", c("alpha", "epsilon", "Pr{eligible}"), lty = c(1, 2, 3), col = c("red", "blue", "purple"), lwd = c(2, 2, 2), bg = "white")
    } else {
      lines(examinee, p_epsilon_no_fading, col = addTrans("blue", 20), lty = 1, type = "h")
      lines(examinee, p_alpha_no_fading, col = addTrans("red", 20), lty = 1, type = "h")
      legend("topright", c("alpha", "epsilon", "Pr{eligible}", "alpha empirical", "epsilon empirical"), lty = c(1, 2, 3, 1, 1), lwd = c(2, 2, 2, 5, 5), col = c("red", "blue", "purple", addTrans("red", 100), addTrans("blue", 100)))
    }
    abline(h = max_rate, col = "gray")
  }
  if (!is.null(file_pdf)) {
    dev.off()
  }
}

#' Calculate Root Mean Squared Error
#'
#' Calculate Root Mean Squared Error.
#'
#' @param x A vector of values.
#' @param y A vector of values.
#' @param conditional If \code{TRUE}, calculate RMSE conditional on x.

RMSE <- function(x, y, conditional = TRUE) {
  if (length(x) != length(y)) {
    stop("length(x) and length(y) are not equal")
  }
  if (conditional) {
    MSE <- tapply((x - y)^2, x, mean)
  } else {
    MSE <- mean((x - y)^2)
  }
  return(sqrt(MSE))
}

#' Calculate Relative Errors
#'
#' Calculate Relative Errors.
#'
#' @param RMSE_foc A vector of RMSE values for the focal group.
#' @param RMSE_ref A vector of RMSE values for the reference group.

RE <- function(RMSE_foc, RMSE_ref) {
  if (length(RMSE_foc) != length(RMSE_ref)) {
    stop("length(x) and length(y) are not equal")
  }
  RE <- RMSE_ref^2 / RMSE_foc^2
  return(RE)
}

#' Check the consistency of constraints and item usage
#'
#' Check the consistency of constraints and item usage.
#'
#' @param constraints A list constraints generated by \code{\link{loadConstraints}}.
#' @param usage_matrix A matrix of item usage data from \code{\link{Shadow}}.
#' @param true_theta A vector of true theta values.

checkConstraints <- function(constraints, usage_matrix, true_theta = NULL) {
  constraints      <- constraints$constraints
  list_constraints <- constraints$listConstraints

  nc <- nrow(constraints)
  nj <- nrow(usage_matrix)
  ni <- ncol(usage_matrix)

  MET <- matrix(FALSE, nrow = nj, ncol = nc)
  COUNT <- matrix(NA, nrow = nj, ncol = nc)
  if (ni != constraints$ni) {
    stop("unequal number of items in constraints and usage_matrix ")
  }
  byTheta <- FALSE
  MEAN <- rep(NA, nc)
  SD   <- rep(NA, nc)
  MIN  <- rep(NA, nc)
  MAX  <- rep(NA, nc)
  HIT  <- rep(NA, nc)
  if (!is.null(true_theta)) {
    if (length(true_theta) != nj) {
      stop("length of true_theta is not equal to nrow of usage_matrix")
    }
    byTheta <- TRUE
    groupMEAN <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupSD <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupMIN <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupMAX <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupHIT <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
  } else {
    groupMEAN <- NULL
    groupSD <- NULL
    groupMIN <- NULL
    groupMAX <- NULL
    groupHIT <- NULL
  }
  nEnemy <- sum(constraints$TYPE == "ENEMY")
  if (nEnemy > 0) {
    enemyIndex <- which(constraints$TYPE == "ENEMY")
    constraints$LB[enemyIndex] <- 0
    constraints$UB[enemyIndex] <- 1
  }
  numberIndex <- which(constraints$TYPE == "NUMBER")
  for (index in 1:nc) {
    if (constraints$WHAT[index] == "ITEM") {
      if (constraints$TYPE[index] %in% c("NUMBER", "ENEMY")) {
        items <- which(list_constraints[[index]]@mat[1, ] == 1)
        COUNT[, index] <- rowSums(usage_matrix[, items])
        MET[, index] <- COUNT[, index] >= constraints$LB[index] & COUNT[, index] <= constraints$UB[index]
        if (byTheta) {
          groupMEAN[index, ] <- round(tapply(COUNT[, index], true_theta, mean), 3)
          groupSD[index, ] <- round(tapply(COUNT[, index], true_theta, sd), 3)
          groupMIN[index, ] <- tapply(COUNT[, index], true_theta, min)
          groupMAX[index, ] <- tapply(COUNT[, index], true_theta, max)
          groupHIT[index, ] <- round(tapply(MET[, index], true_theta, mean), 3)
        }
        MEAN[index] <- round(mean(COUNT[, index]), 2)
        SD[index] <- round(sd(COUNT[, index]), 2)
        MIN[index] <- min(COUNT[, index])
        MAX[index] <- max(COUNT[, index])
        HIT[index] <- round(mean(MET[, index]), 3)
      }
    }
  }
  LD <- NULL
  if (nEnemy > 0) {
    LD <- rowSums(COUNT[, enemyIndex] > 1)
  }
  Check <- data.frame(constraints, MEAN = MEAN, SD = SD, MIN = MIN, MAX = MAX, HIT = HIT)
  return(list(Check = Check[constraints[["TYPE"]] == "NUMBER", ], LD = LD, groupMEAN = groupMEAN[numberIndex, ], groupSD = groupSD[numberIndex, ], groupMIN = groupMIN[numberIndex, ], groupMAX = groupMAX[numberIndex, ], groupHIT = groupHIT[numberIndex, ]))
}

#' Draw RMSE plots
#'
#' Draw RMSE plots.
#'
#' @param ... A series of RMSE values.
#' @param title A plot title.
#' @param legend_title A legend title.
#' @param legend_labels A vector of labels for the series.
#' @param lty_set A vector of line types for the series.
#' @param col_set A vector of colors for the series.
#' @param theta A theta grid.

plotRMSE <- function(..., title = NULL, legend_title = NULL, legend_labels = NULL, lty_set = NULL, col_set = NULL, theta = seq(-2, 2, 1)) {

  output_list <- list(...)
  n_output <- length(output_list)

  if (is.null(lty_set)) {
    lty_set <- 1:n_output
  } else if (length(lty_set) != n_output) {
    warning("... and lty_set are of different lengths")
    lty_set <- 1:n_output
  }

  if (is.null(col_set)) {
    col_set <- 1:n_output
  } else if (length(col_set) != n_output) {
    warning("... and col_set are of different lengths")
    col_set <- 1:n_output
  }

  plot(unique(output_list[[1]]$true_theta), RMSE(output_list[[1]]$true_theta, output_list[[1]]$final_theta_est), xlim = range(theta), ylim = c(0, 1), xlab = "Theta", ylab = "RMSE", type = "n", xaxt = "n", yaxt = "n", main = title)
  axis(1, at = theta, labels = theta)
  axis(2, at = seq(0, 1.0, .2), labels = format(seq(0, 1.0, .2), digits = 1), las = 2)
  grid()

  for (i in 1:n_output) {
    lines(unique(output_list[[i]]$true_theta), RMSE(output_list[[i]]$true_theta, output_list[[i]]$final_theta_est), lty = lty_set[i], col = col_set[i], lwd = 2)
  }

  if (!is.null(legend_labels)) {
    if (length(legend_labels) != n_output) {
      warning("... and legend_labels are of different lengths")
      legend_labels <- 1:n_output
    }
    legend("top", labels, lty = lty_set, col = col_set, title = legend_title, bg = "white")
  }
}

#' @noRd

plotER <- function(ni, exposure_rate, exposure_rate_final = NULL, max_rate = max_rate, title = NULL, color = "blue", color_final = "yellow", simple = FALSE) {
  idx_sort <- order(exposure_rate, decreasing = TRUE)
  exposure_rate_ordered <- exposure_rate[idx_sort]

  if (!simple) {
    plot(1:ni, exposure_rate_ordered, type = "n", lwd = 2, ylim = c(0, 1), xlab = "Item", ylab = "Exposure Rate", main = title)
    lines(1:ni, exposure_rate_ordered, type = "l", lty = 1, lwd = 2, col = color)
    points(1:ni, exposure_rate_ordered, type = "h", lwd = 1, col = color)
    abline(h = max_rate, col = "gray")
  } else {
    plot(1:ni, exposure_rate_ordered, type = "n", lwd = 2, ylim = c(0, 1), xlab = "", ylab = "", main = title)
    points(1:ni, exposure_rate_ordered, type = "h", lwd = 1, col = color)
    abline(h = max_rate, col = "dark gray", lty = 2)
  }

  if (!is.null(exposure_rate_final)) {
    exposure_rate_final_ordered <- exposure_rate_final[idx_sort]
    points(1:ni, exposure_rate_final_ordered, type = "h", lwd = 1, lty = 1, col = color_final)
  }
}

#' Draw exposure rate plots by theta segment
#'
#' Draw exposure rate plots by theta segment.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param max_rate A target item exposure rate.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of the graphics device.
#' @param height Height of the graphics device.
#' @param mfrow Number of multiple figures defined as c(nrow, ncol).

plotExposureRateBySegment <- function(object, config, max_rate = 0.25, file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4)) {
  nj <- length(object$true_theta)
  ni <- ncol(object$usage_matrix)
  segment_cut <- config@exposure_control$segment_cut
  n_segment   <- config@exposure_control$n_segment
  cut_lower   <- segment_cut[1:n_segment]
  cut_upper   <- segment_cut[2:(n_segment + 1)]
  segment_label <- character(n_segment)
  for (k in 1:n_segment) {
    if (k < n_segment) {
      segment_label[k] <- paste0("(", round(cut_lower[k], 1), ",", round(cut_upper[k], 1), "]")
    } else {
      segment_label[k] <- paste0("(", round(cut_lower[k], 1), ",", round(cut_upper[k], 1), ")")
    }
  }
  exposure_rate <- colSums(object$usage_matrix) / nj
  exposure_rate_segment <- vector("list", n_segment)
  names(exposure_rate_segment) <- segment_label
  for (k in 1:n_segment) {
    if (object$eligibility_stats$n_jk[k] == 0) {
      exposure_rate_segment[[k]] <- numeric(ni)
    } else {
      exposure_rate_segment[[k]] <- object$eligibility_stats$alpha_ijk[k, ] / object$eligibility_stats$n_jk[k]
    }
  }

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  old_mfrow <- par()$mfrow
  on.exit(par(mfrow = old_mfrow))
  par(mfrow = mfrow)

  plotER(ni, exposure_rate, NULL, max_rate = max_rate, title = paste0("Overall (N = ", nj, ")"), color = "blue")
  for (k in 1:config@exposure_control$n_segment) {
    plotER(
      ni, exposure_rate_segment[[k]], NULL, max_rate = max_rate,
      title = paste0(segment_label[k], " (n = ", round(object$eligibility_stats$n_jk[k], 1), ")"),
      color = "blue"
    )
  }
  if (!is.null(file_pdf)) {
    dev.off()
  }

  return(exposure_rate_segment)
}

#' Draw exposure rate plots by final theta segment
#'
#' Draw exposure rate plots by final theta segment.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param config A \code{\linkS4class{config_Shadow}} object.
#' @param max_rate A target item exposure rate.
#' @param theta By which theta to generate the plots, either "Estimated" or "True".
#' @param segment_cut A vector of cut values defining theta segments.
#' @param color A vector of colors.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of the graphics object.
#' @param height Height of the graphics object.
#' @param mfrow Number of multiple figures defined as c(nrow, ncol).
#' @param burn An integer identifying the first x simulees to discard as burn-in.
#' @param retain An optional vector of indices identifying the simulees to retain.
#'
#' @examples
#' true_theta <- runif(10, min = -3.5, max = 3.5)
#' resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
#' constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))
#' config_science <- createShadowTestConfig(
#'   MIP = list(solver = "LPSOLVE"),
#'   exposure_control = list(method = "ELIGIBILITY")
#' )
#' solution <- Shadow(itempool_science, config_science,
#'   true_theta, constraints_science2, data = resp_science)
#' p <- plotExposureRateFinal(solution, config_science, 0.25)
#'
#' @export
plotExposureRateFinal <- function(object, config = NULL, max_rate = 0.25, theta = "Estimated", segment_cut = NULL, color = "red", file_pdf = NULL, width = 7, height = 6, mfrow = c(2, 4), burn = 0, retain = NULL) {

  true_theta <- object$true_theta
  est_theta  <- object$final_theta_est
  nj         <- length(true_theta)

  if (burn > 0) {
    if (toupper(theta) == "TRUE") {
      retained <- object$true_segment_count > burn
    } else {
      retained <- object$est_segment_count > burn
    }
  } else if (!is.null(retain)) {
    retained <- (1:nj) %in% retain
  } else {
    retained <- rep(TRUE, nj)
  }
  n_retained <- sum(retained)

  ni <- ncol(object$usage_matrix)

  if (is.null(config)) {
    config <- object$config
  }
  if (is.null(segment_cut)) {
    segment_cut <- config@exposure_control$segment_cut
  }

  n_segment <- length(segment_cut) - 1
  cut_lower <- segment_cut[1:n_segment]
  cut_upper <- segment_cut[2:(n_segment + 1)]
  segment_label <- character(n_segment)
  theta_segment_index <- numeric(sum(retained))
  if (toupper(theta) == "TRUE") {
    theta_segment_index <- find_segment(segment_cut, true_theta[retained])
  } else {
    theta_segment_index <- find_segment(segment_cut, est_theta[retained])
  }

  segment_n    <- numeric(n_segment)
  segment_dist <- table(theta_segment_index)
  segment_n[as.numeric(names(segment_dist))] <- segment_dist
  segment_index_table <- matrix(NA, n_retained, object$constraints$test_length)
  for (k in 1:n_segment) {
    if (k < n_segment) {
      segment_label[k] <- paste0("(", cut_lower[k], ",", cut_upper[k], "]")
    } else {
      segment_label[k] <- paste0("(", cut_lower[k], ",", cut_upper[k], ")")
    }
  }

  usage_matrix       <- object$usage_matrix[retained, ]
  usage_matrix_final <- object$usage_matrix[retained, ]
  idx <- 0
  for (j in 1:nj) {
    if (retained[j]) {
      idx <- idx + 1
      usage_matrix_final[idx, object$output[[j]]@administered_item_index[object$output[[j]]@theta_segment_index != theta_segment_index[idx]]] <- FALSE
      segment_index_table[idx, ] <- object$output[[j]]@theta_segment_index
    }
  }

  segment_freq <- matrix(0, n_segment, n_segment)
  for (i in 1:object$constraints$test_length) {
    factor(segment_index_table[, i], levels = 1:n_segment)
    segment_table <- tapply(factor(segment_index_table[, i], levels = 1:n_segment), theta_segment_index, table)
    for (s in 1:length(segment_table)) {
      idx_r <- as.numeric(names(segment_table)[s])
      idx_c <- as.numeric(names(segment_table[[s]]))
      segment_freq[idx_r, idx_c] <- segment_freq[idx_r, idx_c] + segment_table[[s]]
    }
  }

  segment_rate <- segment_freq / segment_n
  segment_rate_table <- data.frame(
    segment_class = factor(rep(segment_label, rep(n_segment, n_segment)),
    levels = segment_label),
    segment = rep(1:n_segment, n_segment),
    avg_visit = matrix(t(segment_rate),
    nrow = n_segment^2, ncol = 1
    )
  )

  exposure_rate               <- colSums(usage_matrix) / n_retained
  exposure_rate_final         <- colSums(usage_matrix_final) / n_retained
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
      exposure_rate_segment[[k]] <- numeric(ni)
    } else if (any(is.nan(exposure_rate_segment[[k]]))) {
      exposure_rate_segment[[k]][is.nan(exposure_rate_segment[[k]])] <- 0
    }
    if (is.null(exposure_rate_segment_final[[k]])) {
      exposure_rate_segment_final[[k]] <- numeric(ni)
    } else if (any(is.nan(exposure_rate_segment_final[[k]]))) {
      exposure_rate_segment_final[[k]][is.nan(exposure_rate_segment_final[[k]])] <- 0
    }
  }

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  old_mfrow <- par()$mfrow
  on.exit(par(mfrow = old_mfrow))
  par(mfrow = mfrow)

  plotER(ni, exposure_rate, exposure_rate_final, max_rate = max_rate, title = paste0("Overall (N = ", n_retained, ")"), color = color)
  for (k in 1:n_segment) {
    plotER(
      ni, exposure_rate_segment[[k]], exposure_rate_segment_final[[k]],
      max_rate = max_rate, title = paste0(segment_label[k], " (n = ", segment_n[k], ")"),
      color = color)
  }
  if (!is.null(file_pdf)) {
    dev.off()
  }

  return(
    list(
      exposure_rate = exposure_rate,
      exposure_rate_segment = exposure_rate_segment,
      exposure_rate_segment_final = exposure_rate_segment_final,
      segment_rate_table = segment_rate_table,
      n_segment = n_segment,
      segment_n = segment_n,
      segment_cut = segment_cut,
      segment_label = segment_label
    )
  )
}

#' Draw item information plots for flagged items by segment
#'
#' Draw item information plots for flagged items by segment.
#'
#' @param object A list object generated by \code{\link{plotExposureRateFinal}}.
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param theta A theta grid.
#' @param flag_from A flagging criterion.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param width Width of the graphics device.
#' @param height Height of the graphics device.
#' @param color Plotting color.
#' @param mfrow Number of multiple figures defined as c(nrow, ncol).

plotExposureRateFinalFlag <- function(object, pool, theta = seq(-3, 3, .1), flag_from = 0.4, file_pdf = NULL, width = 7, height = 6, color = "red", mfrow = c(2, 4)) {
  info <- calcFisher(pool, theta)
  ni <- pool@ni
  n_segment <- object$n_segment
  segment_cut <- object$segment_cut
  segment_cut[1] <- min(theta)
  segment_cut[length(segment_cut)] <- max(theta)
  segment_label <- object$segment_label
  items_flagged_segment <- lapply(seq_len(object$n_segment), function(j) which(object$exposure_rate_segment[[j]] > flag_from))

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  old_mfrow <- par()$mfrow
  on.exit(par(mfrow = old_mfrow))
  par(mfrow = mfrow)

  for (k in 1:n_segment) {
    theta_segment_range         <- which(theta >= segment_cut[k] & theta <= segment_cut[k + 1])
    theta_segment_range_outside <- which(theta <= segment_cut[k] | theta >= segment_cut[k + 1])
    plot(theta, info[, 1], xlab = "Theta", ylab = "Info", main = segment_label[k], type = "n", ylim = c(0, max(info)))
    for (i in 1:ni) {
      lines(theta, info[, i], col = "light grey", lwd = 0.5)
      lines(theta[theta_segment_range], info[theta_segment_range, i], col = "grey", lwd = 1.0)
    }
    items_flagged <- items_flagged_segment[[k]]
    if (length(items_flagged) > 0) {
      for (i in items_flagged) {
        lines(theta[theta_segment_range]        , info[theta_segment_range, i]        , col = color, lwd = 2)
        lines(theta[theta_segment_range_outside], info[theta_segment_range_outside, i], col = color, lwd = 1)
      }
    }
    abline(v = segment_cut[k]    , col = "dark grey")
    abline(v = segment_cut[k + 1], col = "dark grey")
  }

  if (!is.null(file_pdf)) {
    dev.off()
  }

  return(items_flagged_segment)
}

#' Draw item information plots
#'
#' Draw item information plots.
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param theta A theta grid. Default is \code{seq(-3, 3, .1)}.
#' @param info_type Type of information.
#' @param select A vector of indices identifying the items to subset.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param color Plotting color.
#' @param width Width of graphics device.
#' @param height Width of graphics device.
#' @param mfrow Number of multiple figures defined as c(nrow, ncol).
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:8)
#' plotInfo(subitempool)
#'
#' @export

plotInfo <- function(object, theta = seq(-3, 3, .1), info_type = "FISHER", select = NULL, file_pdf = NULL, color = "blue", width = 7, height = 6, mfrow = c(2, 4)) {
  if (toupper(info_type) == "FISHER") {
    info <- calcFisher(object, theta)
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
  for (i in items) {
    plot(theta, info[, i], xlab = "Theta", ylab = "Info", main = object@id[i], type = "l", col = color, ylim = c(0, max(info)))
  }

  if (!is.null(file_pdf)) {
    dev.off()
  }
}

#' Overlay item information plots
#'
#' Overlay item information plots.
#'
#' @param object An \code{\linkS4class{item_pool}} object.
#' @param theta A theta grid.
#' @param info_type Type of information.
#' @param select A vector of indices identifying the items to subset.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param color Plotting color.
#' @param width Width of the graphics device.
#' @param height Height of the graphics device.

plotInfoOverlay <- function(object, theta, info_type = "FISHER", select = NULL, file_pdf = NULL, color = "red", width = 7, height = 6) {

  if (toupper(info_type) == "FISHER") {
    info <- calcFisher(object, theta)
  } else {
    stop("Invalid info_type specified")
  }

  if (!is.null(file_pdf)) {
    pdf(file = file_pdf, width = width, height = height)
  }

  items <- 1:object@ni
  if (!is.null(select) && all(select %in% items)) {
    items <- select
  }

  plot(theta, info[, 1], xlab = "Theta", ylab = "Info", main = "", type = "n", ylim = c(0, max(info)))
  for (i in 1:object@ni) {
    lines(theta, info[, i], col = "light grey", lwd = 0.5)
  }
  for (i in items) {
    lines(theta, info[, i], col = color, lwd = 2)
  }

  if (!is.null(file_pdf)) {
    dev.off()
  }
}

#' Calculate hyperparameters for log-normal distribution
#'
#' Calculate hyperparameters for log-normal distribution.
#'
#' @param mean Mean of the distribution.
#' @param sd Standard deviation of the distribution.
#'
#' @examples
#' lnHyperPars(.5, 1)
#'
#' @export
lnHyperPars <- function(mean, sd) {
  location <- log(mean^2 / sqrt(sd^2 + mean^2))
  scale    <- sqrt(log(1 + sd^2 / mean^2))
  return(c(location, scale))
}

#' Calculate hyperparameters for logit-normal distribution
#'
#' Calculate hyperparameters for logit-normal distribution.
#'
#' @param mean Mean of the distribution.
#' @param sd Standard deviation of the distribution.
#'
#' @examples
#' logitHyperPars(.5, 1)
#'
#' @export
logitHyperPars <- function(mean, sd) {

  n_max <- 10000
  n     <- 0
  logit_samples <- numeric(n_max)

  while (n_max - n > 0) {
    norm_sample <- rnorm(n_max - n, mean, sd)
    idx <- (norm_sample >= 0) & (norm_sample <= 1)
    norm_sample <- norm_sample[idx]
    n_new <- n + length(norm_sample)
    if (length(norm_sample) > 0) {
      logit_samples[(n + 1):n_new] <- logitnorm::logit(norm_sample)
    }
    n <- n_new
  }

  return(c(mean(logit_samples), sd(logit_samples)))
}

#' Sample item parameter estimates from their posterior distributions
#'
#' Sample item parameter estimates from their posterior distributions.
#'
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param n_sample An integer as the number of sampled parameters.
#'
#' @examples
#' ipar <- iparPosteriorSample(itempool_science, 5)
#'
#' @export
iparPosteriorSample <- function(pool, n_sample = 500) {

  requireNamespace("logitnorm")
  ipar_list <- vector(mode = "list", length = pool@ni)

  for (i in 1:pool@ni) {

    if (pool@model[i] == "item_1PL") {
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 1)
      ipar_list[[i]][, 1] <- rnorm(n_sample, pool@ipar[i, 1], pool@se[i, 1])

    } else if (pool@model[i] == "item_2PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 2)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])

    } else if (pool@model[i] == "item_3PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      c_hyp <- logitHyperPars(pool@ipar[i, 3], pool@se[i, 3])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 3)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])
      ipar_list[[i]][, 3] <- rlogitnorm(n_sample, mu = c_hyp[1], sigma = c_hyp[2])

    } else if (pool@model[i] == "item_PC") {
      ipar_list[[i]] <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k] <- rnorm(n_sample, pool@ipar[i, k], pool@se[i, k])
      }

    } else if (pool@model[i] == "item_GPC") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i])
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k + 1] <- rnorm(n_sample, pool@ipar[i, k + 1], pool@se[i, k + 1])
      }

    } else if (pool@model[i] == "item_GR") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i])
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k + 1] <- rnorm(n_sample, pool@ipar[i, k + 1], pool@se[i, k + 1])
      }
      for (s in 1:n_sample) {
        if (is.unsorted(ipar_list[[i]][s, 2:pool@NCAT[i]])) {
          ipar_list[[i]][s, 2:pool@NCAT[i]] <- sort(ipar_list[[i]][s, 2:pool@NCAT[i]])
        }
      }

    }
  }
  return(ipar_list)
}

#' Draw a plot of maximum attainable information given the imposed constraints
#'
#' Draw a plot of maximum attainable information given the imposed constraints.
#'
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param constraints A list constraints generated by \code{\link{loadConstraints}}.
#' @param theta A theta grid.
#'
#' @examples
#' p <- plotMaxInfo(itempool_science, constraints_science)
#'
#' @export
plotMaxInfo <- function(pool, constraints, theta = seq(-3, 3, .5)) {
  idx_n_items <- which(toupper(constraints$constraints[["WHAT"]]) == "ITEM" &
    toupper(constraints$constraints[["CONDITION"]]) == "")
  n_items <- constraints$constraints[idx_n_items, ]["LB"][1, 1]
  max_info <- theta * 0
  min_info <- theta * 0
  for (i in 1:length(theta)) {
    max_info[i] <- sum(sort(calcFisher(pool, theta[i]), TRUE)[1:n_items])
    min_info[i] <- sum(sort(calcFisher(pool, theta[i]), FALSE)[1:n_items])
  }

  pdf(NULL, bg = "white")
  dev.control(displaylist = "enable")

  plot(0, 0,
    type = "n", xlim = c(-3, 3), ylim = c(0, max(max_info)),
    xlab = "Theta", ylab = "Information", main = "Range of attainable information based on the number of items"
  )
  lines(theta, max_info, lty = 2, lwd = 2)
  lines(theta, min_info, lty = 2, lwd = 2)
  grid()

  p <- recordPlot()
  plot.new()
  dev.off()

  return(p)
}
