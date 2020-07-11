#' @include shadow_functions.R
NULL

#' @noRd
getConstants <- function(constraints, config, arg_data, true_theta) {

  o <- list()
  o$ni <- constraints@ni
  o$ns <- constraints@ns
  o$nv <- constraints@nv
  o$theta_q <- config@theta_grid
  o$nq      <- length(config@theta_grid)
  o$min_q   <- min(config@theta_grid)
  o$max_q   <- max(config@theta_grid)

  if (!is.null(arg_data)) {
    o$nj <- nrow(arg_data)
  }
  if (!is.null(true_theta)) {
    o$nj <- length(true_theta)
  }
  if (is.null(o$nj)) {
    stop("either 'data' or 'true_theta' must be supplied")
  }

  content_balancing_method <- toupper(config@content_balancing$method)
  if (content_balancing_method %in% c("STA", "SHADOW", "SHADOWTEST", "SHADOW TEST")) {
    if (is.null(constraints)) {
      stop(sprintf("config@content_balancing: 'constraints' must be supplied when $method is '%s'", content_balancing_method))
    }
    o$use_shadow     <- TRUE
    o$set_based      <- constraints@set_based
    o$test_length    <- constraints@test_length
    o$min_ni         <- constraints@test_length
    o$max_ni         <- constraints@test_length
    o$max_se         <- NULL
  } else {
    o$use_shadow     <- FALSE
    o$set_based      <- FALSE
    o$test_length    <- NULL
    o$min_ni         <- config@stopping_criterion$min_ni
    o$max_ni         <- config@stopping_criterion$max_ni
    o$max_se         <- config@stopping_criterion$se_threshold
  }

  refresh_method <- toupper(config@refresh_policy$method)
  if (refresh_method %in% c("STIMULUS", "SET", "PASSAGE")) {
    o$set_based_refresh <- TRUE
  } else {
    o$set_based_refresh <- FALSE
  }

  return(o)

}

#' @noRd
sanitizeModel <- function(model) {
  model[which(model == "item_1PL")] <- 1
  model[which(model == "item_2PL")] <- 2
  model[which(model == "item_3PL")] <- 3
  model[which(model == "item_PC")]  <- 4
  model[which(model == "item_GPC")] <- 5
  model[which(model == "item_GR")]  <- 6
  model <- as.numeric(model)
  return(model)
}

#' @noRd
initializeShadowEngine <- function(constants, refresh_policy) {

  refresh_method   <- toupper(refresh_policy$method)
  refresh_position <- refresh_policy$position
  refresh_interval <- refresh_policy$interval
  test_length      <- constants$test_length

  refresh_shadow    <- rep(FALSE, test_length)
  refresh_shadow[1] <- TRUE
  if (refresh_method %in% c("ALWAYS", "THRESHOLD")) {
    refresh_shadow[1:test_length] <- TRUE
  }
  if (refresh_method %in% c("POSITION")) {
    if (!all(refresh_position %in% 1:test_length)) {
      stop("config@refresh_policy: $position must be within test length")
    }
    refresh_shadow[refresh_position] <- TRUE
  }
  if (refresh_method %in% c("INTERVAL", "INTERVAL-THRESHOLD")) {
    if (!(refresh_interval >= 1 && refresh_interval <= test_length)) {
      stop("config@refresh_policy: $interval must be at least 1 and not greater than test length")
    }
    refresh_shadow[seq(1, test_length, refresh_interval)] <- TRUE
  }
  if (constants$set_based_refresh && !constants$set_based) {
    stop(sprintf("config@refresh_policy: stimulus-based constraint is required for $method '%s'", refresh_method))
  }

  return(refresh_shadow)

}

#' @noRd
makeData <- function(pool, true_theta, arg_data, constants) {

  o <- list()
  theta_grid <- constants$theta_q

  if (!is.null(arg_data)) {
    o$test <- makeTest(pool, theta_grid, info_type = "FISHER", true_theta = NULL)
    o$test@data <- as.matrix(arg_data)
    for (i in 1:constants$ni) {
      invalid_resp <- !(o$test@data[, i] %in% 0:(pool@NCAT[i] - 1))
      o$test@data[invalid_resp, i] <- NA
    }
  } else if (!is.null(true_theta)) {
    o$test <- makeTest(pool, theta_grid, info_type = "FISHER", true_theta)
  } else {
    stop("either 'data' or 'true_theta' must be supplied")
  }

  o$max_info <- max(o$test@info)

  return(o)

}

#' @noRd
getInfoFixedTheta <- function(item_selection, constants, test, pool, model) {

  nj <- constants$nj
  o <- list()

  if (!is.null(item_selection$fixed_theta)) {
    if (length(item_selection$fixed_theta) == 1) {
      o$info_fixed_theta <- vector(mode = "list", length = nj)
      o$info_fixed_theta <- test@info[which.min(abs(constants$theta_grid - item_selection$fixed_theta)), ]
      o$select_at_fixed_theta <- TRUE
    }
    if (length(item_selection$fixed_theta) == nj) {
      o$info_fixed_theta <- lapply(seq_len(nj), function(j) calc_info(item_selection$fixed_theta[j], pool@ipar, pool@NCAT, model))
      o$select_at_fixed_theta <- TRUE
    }
    if (is.null(o$info_fixed_theta)) {
      stop("config@item_selection: length($fixed_theta) must be either 1 or nj")
    }
  } else {
    o$select_at_fixed_theta <- FALSE
  }

  return(o)

}

#' @noRd
getInfo <- function(item_selection, j, info_fixed_theta, current_theta, pool, model, posterior_record, info_grid) {

  item_method <- toupper(item_selection$method)
  info_type   <- toupper(item_selection$info_type)

  if (item_method == "FIXED") {
    info <- info_fixed_theta[[j]]
    return(info)
  }
  if (item_method == "MFI") {
    info <- calc_info(current_theta$theta, pool@ipar, pool@NCAT, model)
    return(info)
  }
  if (item_method == "MPWI") {
    info <- as.vector(matrix(posterior_record$posterior[j, ], nrow = 1) %*% info_grid)
    return(info)
  }
  if (item_method == "EB") {
    info <- calc_info_EB(
      matrix(current_theta$posterior_sample),
      pool@ipar, pool@NCAT, model)[, 1]
    return(info)
  }
  if (item_method == "FB" & info_type == "FISHER") {
    info <- calc_info_FB(
      matrix(current_theta$posterior_sample),
      posterior_record$ipar_list, pool@NCAT, model)[, 1]
    return(info)
  }
  if (item_method == "FB" & info_type %in% c("MI", "MUTUAL")) {
    info <- calc_MI_FB(
      matrix(current_theta$posterior_sample),
      posterior_record$ipar_list, pool@NCAT, model)[, 1]
    return(info)
  }
}

#' @noRd
initializeStimulusRecord <- function() {
  o <- list()
  o$end_set <- TRUE
  o$finished_stimulus_index      <- NULL
  o$finished_stimulus_item_count <- NULL
  return(o)
}

#' @noRd
shouldShadowBeRefreshed <- function(position, refresh_policy, refresh_shadow, theta_change, constants, stimulus_record) {

  refresh_method <- toupper(refresh_policy$method)

  if (position == 1) {
    return(TRUE)
  }
  if (refresh_method == "ALWAYS") {
    return(TRUE)
  }
  if (refresh_method %in% c("POSITION", "INTERVAL") && refresh_shadow[position]) {
    return(TRUE)
  }
  if (refresh_method == "THRESHOLD") {
    if (abs(theta_change) > refresh_policy$threshold) {
      return(TRUE)
    }
  }
  if (refresh_method == "INTERVAL-THRESHOLD") {
    if (abs(theta_change) > refresh_policy$threshold && refresh_shadow[position]) {
      return(TRUE)
    }
  }
  if (constants$set_based_refresh && constants$set_based && stimulus_record$end_set) {
    return(TRUE)
  }

  return(FALSE)

}

#' @noRd
selectItem <- function(info, position, o) {

  info[o@administered_item_index[0:(position - 1)]] <- -1

  info_index    <- order(info, decreasing = TRUE)
  item_selected <- info_index[1]

  if (item_selected %in% o@administered_item_index[0:(position - 1)]) {
    stop(sprintf("item %i has been already administered", item_selected))
  }

  return(item_selected)

}

#' @noRd
selectItemFromShadowTest <- function(shadow_test, position, constants, x) {

  o <- list()

  o$n_remaining <- constants$test_length - position
  o$stimulus_finished     <- FALSE
  o$new_stimulus_selected <- FALSE

  o$stimulus_of_previous_item <- 0

  remaining <- which(!shadow_test[["INDEX"]] %in% x@administered_item_index[0:(position - 1)])

  if (position == 1 & !constants$set_based) {
    idx                 <- remaining[1]
    o$item_selected     <- shadow_test[["INDEX"]][idx]
    o$stimulus_selected <- NA
  }
  if (position == 1 & constants$set_based) {
    idx                 <- remaining[1]
    o$item_selected     <- shadow_test[["INDEX"]][idx]
    o$stimulus_selected <- shadow_test[["STINDEX"]][idx]
  }
  if (position > 1 & !constants$set_based) {
    idx                 <- remaining[1]
    o$item_selected     <- shadow_test[["INDEX"]][idx]
    o$stimulus_selected <- NA
  }
  if (position > 1 & constants$set_based) {
    o$stimulus_of_previous_item <- x@administered_stimulus_index[position - 1]
    if (!is.na(o$stimulus_of_previous_item)) {
      remaining_items_within_stimulus <- shadow_test[["STINDEX"]][remaining] == o$stimulus_of_previous_item
      if (any(remaining_items_within_stimulus, na.rm = TRUE)) {
        idx <- remaining[which(remaining_items_within_stimulus)][1]
      } else {
        idx <- remaining[1]
      }
    }
    if (is.na(o$stimulus_of_previous_item)) {
      idx <- remaining[1]
    }
    o$item_selected     <- shadow_test[["INDEX"]][idx]
    o$stimulus_selected <- shadow_test[["STINDEX"]][idx]
  }


  if (is.na(
    o$stimulus_of_previous_item != o$stimulus_selected) |
    o$stimulus_of_previous_item != o$stimulus_selected) {
    o$new_stimulus_selected <- TRUE
  }

  if (sum(shadow_test[["STINDEX"]][remaining] == o$stimulus_selected, na.rm = TRUE) == 1) {
    o$stimulus_finished <- TRUE
  }
  if (is.na(o$stimulus_selected)) {
    o$stimulus_finished <- TRUE
  }
  if (o$n_remaining == 0) {
    o$stimulus_finished <- TRUE
  }

  return(o)

}

#' @noRd
plotAuditTrail <- function(o, j, constants, config) {

  old_mar   <- par()$mar
  old_mfrow <- par()$mfrow
  on.exit({
    close_dev <- ifelse(dev.cur() == 1, TRUE, FALSE)
    par(mar = old_mar, mfrow = old_mfrow)
    if (close_dev) {
      dev.off()
    }
  })
  par(mar = c(2, 3, 1, 1) + 0.1, mfrow = c(2, 1))

  max_ni     <- constants$max_ni
  theta_grid <- constants$theta_q
  min_q      <- constants$min_q
  max_q      <- constants$max_q

  plot(1:max_ni, seq(min_q, max_q, length = max_ni),
    main = paste0("Examinee ", j), xlab = "Items Administered", ylab = "Theta",
    type = "n", las = 1)
  points(1:max_ni, o@interim_theta_est,
    type = "b", pch = 9, col = "blue")

  if (!is.null(o@true_theta)) {
    abline(h = o@true_theta, lty = 2, col = "red")
  } else {
    abline(h = o@final_theta_est, lty = 2, col = "red")
  }

  item_string <- paste(o@administered_item_index, collapse = ",")
  text(1, max_q,
    paste0("Items: ", item_string),
    cex = 0.7, adj = 0)
  text(1, min_q + 0.3,
    paste("Theta: ", round(o@final_theta_est, digits = 2), " SE: ", round(o@final_se_est, digits = 2)),
    cex = 0.8, adj = 0)

  for (i in 1:max_ni) {
    lines(rep(i, 2), c(o@interim_theta_est[i] - 1.96 * o@interim_se_est[i], o@interim_theta_est[i] + 1.96 * o@interim_se_est[i]))
    if (constants$use_shadow) {
      if (o@shadow_test_refreshed[i]) {
        points(i, o@interim_theta_est[i], pch = 18, col = "red")
      }
    }
  }

  resp_string <- paste(o@administered_item_resp, collapse = ",")
  plot(theta_grid, o@posterior, main = "Final Posterior Distribution", xlab = "Theta", ylab = "Posterior", type = "l", col = "blue", yaxt = "n")
  text(min_q, max(o@posterior), paste0("Responses: ", resp_string), cex = 0.7, adj = 0)

}
