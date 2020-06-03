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