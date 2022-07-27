#' @include shadow_functions.R
NULL

#' @noRd
parseShadowTestRefreshSchedule <- function(constants, refresh_policy) {

  refresh_method   <- toupper(refresh_policy$method)
  refresh_position <- refresh_policy$position
  refresh_interval <- refresh_policy$interval
  test_length      <- constants$test_length

  shadowtest_refresh_schedule    <- rep(FALSE, test_length)
  shadowtest_refresh_schedule[1] <- TRUE
  if (refresh_method %in% c("ALWAYS", "THRESHOLD")) {
    shadowtest_refresh_schedule[1:test_length] <- TRUE
  }
  if (refresh_method %in% c("POSITION")) {
    if (!all(refresh_position %in% 1:test_length)) {
      stop("config@refresh_policy: $position must be within test length")
    }
    shadowtest_refresh_schedule[refresh_position] <- TRUE
  }
  if (refresh_method %in% c("INTERVAL", "INTERVAL-THRESHOLD")) {
    if (!(refresh_interval >= 1 && refresh_interval <= test_length)) {
      stop("config@refresh_policy: $interval must be at least 1 and not greater than test length")
    }
    shadowtest_refresh_schedule[seq(1, test_length, refresh_interval)] <- TRUE
  }
  if (constants$set_based_refresh && !constants$set_based) {
    stop(sprintf("config@refresh_policy: stimulus-based constraint is required for $method '%s'", refresh_method))
  }

  return(shadowtest_refresh_schedule)

}

#' @noRd
assembleShadowTest <- function(
  j, position, o,
  eligible_flag,
  exclude_index,
  stimulus_record,
  info,
  config,
  constants,
  constraints
) {

  administered_stimulus_index <- na.omit(unique(o@administered_stimulus_index))

  xdata         <- getXdataOfAdministered(constants, position, o, stimulus_record, constraints)
  if (constants$exclude_method == "HARD") {
    xdata_exclude <- getXdataOfExcludedEntry(constants, exclude_index[[j]])
    xdata         <- combineXdata(xdata, xdata_exclude)
  }
  if (constants$exclude_method == "SOFT") {
    info <- getInfoOfExcludedEntry(info, exclude_index[[j]], constants)
  }

  if (constants$use_eligibility_control) {

    # Get eligible items in the current theta segment
    current_segment <- o@theta_segment_index[position]
    eligible_flag_in_current_theta_segment <- getEligibleFlagInSegment(eligible_flag, current_segment, constants)
    eligible_flag_in_current_theta_segment <- flagAdministeredAsEligible(eligible_flag_in_current_theta_segment, o, position, constants)

  }

  if (constants$use_eligibility_control && constants$exposure_control_method %in% c("ELIGIBILITY")) {

    xdata_elg  <- applyEligibilityConstraintsToXdata(xdata, eligible_flag_in_current_theta_segment, constants, constraints)
    shadowtest <- runAssembly(config, constraints, xdata = xdata_elg, objective = info)
    is_optimal <- isShadowTestOptimal(shadowtest)

    if (is_optimal) {
      shadowtest$feasible <- TRUE
      return(shadowtest)
    }

    # If not optimal, retry without xmat
    shadowtest <- runAssembly(config, constraints, xdata = xdata, objective = info)
    shadowtest$feasible <- FALSE
    return(shadowtest)

  }

  if (constants$use_eligibility_control && constants$exposure_control_method %in% c("BIGM", "BIGM-BAYESIAN")) {

    # Do Big-M based exposure control: penalize item info
    info <- applyEligibilityConstraintsToInfo(
      info, eligible_flag_in_current_theta_segment, config, constants
    )

    shadowtest <- runAssembly(config, constraints, xdata = xdata, objective = info)
    shadowtest$feasible <- TRUE
    return(shadowtest)

  }

  if (!constants$use_eligibility_control) {

    shadowtest <- runAssembly(config, constraints, xdata = xdata, objective = info)
    shadowtest$feasible <- TRUE
    return(shadowtest)

  }

}

#' @noRd
isShadowTestOptimal <- function(shadowtest) {
  return(isOptimal(shadowtest$status, shadowtest$solver))
}

#' @noRd
selectItemFromShadowTest <- function(shadow_test, position, constants, x) {

  o <- list()

  o$n_remaining <- constants$test_length - position
  o$is_last_item_in_this_set <- FALSE
  o$new_stimulus_selected    <- FALSE

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
    o$is_last_item_in_this_set <- TRUE
  }
  if (is.na(o$stimulus_selected)) {
    o$is_last_item_in_this_set <- TRUE
  }
  if (o$n_remaining == 0) {
    o$is_last_item_in_this_set <- TRUE
  }

  return(o)

}

#' @noRd
shouldShadowTestBeRefreshed <- function(position, refresh_policy, refresh_schedule, theta_change, constants, stimulus_record) {

  refresh_method <- toupper(refresh_policy$method)

  if (position == 1) {
    return(TRUE)
  }
  if (refresh_method == "ALWAYS") {
    return(TRUE)
  }
  if (refresh_method %in% c("POSITION", "INTERVAL") && refresh_schedule[position]) {
    return(TRUE)
  }
  if (refresh_method == "THRESHOLD") {
    if (abs(theta_change) > refresh_policy$threshold) {
      return(TRUE)
    }
  }
  if (refresh_method == "INTERVAL-THRESHOLD") {
    if (abs(theta_change) > refresh_policy$threshold && refresh_schedule[position]) {
      return(TRUE)
    }
  }
  if (constants$set_based_refresh && constants$set_based && stimulus_record$just_finished_this_set) {
    return(TRUE)
  }

  return(FALSE)

}
