#' @include shadow_functions.R
NULL

#' @noRd
parseShadowTestRefreshSchedule <- function(constants, refresh_policy) {

  refresh_method   <- toupper(refresh_policy$method)
  refresh_position <- refresh_policy$position
  refresh_interval <- refresh_policy$interval
  test_length      <- constants$test_length

  o <- list()
  o$dynamic       <- FALSE
  o$use_threshold <- FALSE
  o$use_setbased  <- FALSE
  o$schedule <- rep(FALSE, test_length)

  o$schedule[1] <- TRUE

  if (refresh_method %in% c("ALWAYS")) {
    o$schedule[1:test_length] <- TRUE
  }
  if (refresh_method %in% c("THRESHOLD")) {
    o$dynamic       <- TRUE
    o$use_threshold <- TRUE
    o$threshold     <- refresh_policy$threshold
    # scheduled values are later overridden
    o$schedule[1:test_length] <- TRUE
  }
  if (refresh_method %in% c("POSITION")) {
    if (!all(refresh_position %in% 1:test_length)) {
      stop("config@refresh_policy: $position must be within test length")
    }
    o$schedule[refresh_position] <- TRUE
  }
  if (refresh_method %in% c("INTERVAL")) {
    if (!(refresh_interval >= 1 && refresh_interval <= test_length)) {
      stop("config@refresh_policy: $interval must be at least 1 and not greater than test length")
    }
    o$schedule[seq(1, test_length, refresh_interval)] <- TRUE
  }
  if (refresh_method %in% c("INTERVAL-THRESHOLD")) {
    if (!(refresh_interval >= 1 && refresh_interval <= test_length)) {
      stop("config@refresh_policy: $interval must be at least 1 and not greater than test length")
    }
    o$dynamic       <- TRUE
    o$use_threshold <- TRUE
    o$threshold     <- refresh_policy$threshold
    o$schedule[seq(1, test_length, refresh_interval)] <- TRUE
  }
  if (refresh_method %in% c("STIMULUS", "SET", "PASSAGE")) {
    if (!constants$group_by_stimulus) {
      stop(sprintf("config@refresh_policy: stimulus-based constraint is required for $method '%s'", refresh_method))
    }
    o$dynamic      <- TRUE
    o$use_setbased <- TRUE
    o$schedule[1:test_length] <- TRUE
  }

  return(o)

}

#' @noRd
assembleShadowTest <- function(
  j, position, o,
  eligibility_flag,
  exclude_index,
  groupings_record,
  info,
  config,
  constants,
  constraints
) {

  administered_stimulus_index <- na.omit(unique(o@administered_stimulus_index))

  xdata         <- getXdataOfAdministered(constants, position, o, groupings_record, constraints)
  if (constants$exclude_method == "HARD") {
    xdata_exclude <- getXdataOfExcludedEntry(constants, exclude_index[[j]])
    xdata         <- combineXdata(xdata, xdata_exclude)
  }
  if (constants$exclude_method == "SOFT") {
    info <- getInfoOfExcludedEntry(info, exclude_index[[j]], constants)
  }

  if (constants$use_exposure_control) {

    # Get eligible items in the current theta segment
    current_segment <- o@theta_segment_index[position]
    eligibility_flag_in_current_theta_segment <- getEligibilityFlagInSegment(eligibility_flag, current_segment, constants)
    eligibility_flag_in_current_theta_segment <- flagAdministeredAsEligible(eligibility_flag_in_current_theta_segment, o, position, constants)

  }

  if (constants$use_exposure_control && constants$exposure_control_method %in% c("ELIGIBILITY")) {

    xdata_elg  <- applyEligibilityConstraintsToXdata(xdata, eligibility_flag_in_current_theta_segment, constants, constraints)
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

  if (constants$use_exposure_control && constants$exposure_control_method %in% c("BIGM", "BIGM-BAYESIAN")) {

    # Do Big-M based exposure control: penalize item info
    info <- applyEligibilityConstraintsToInfo(
      info, eligibility_flag_in_current_theta_segment, config, constants
    )

    shadowtest <- runAssembly(config, constraints, xdata = xdata, objective = info)
    shadowtest$feasible <- TRUE
    return(shadowtest)

  }

  if (!constants$use_exposure_control) {

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
selectItemFromShadowTest <- function(shadow_test, position, constants, x, previous_selection) {

  o <- list()

  # filter out administered items ----------------------------------------------
  shadow_test <- subset(shadow_test, !(shadow_test$INDEX %in% x@administered_item_index))

  if (constants$group_by_stimulus) {

    # if set-based and we just completed a set, select a new set ---------------
    # this is also triggered at the start of test
    if (previous_selection$is_last_item_in_this_set) {
      current_stimulus_index <- shadow_test$STINDEX[1]
    }

    # if set-based and we are in mid-set, read from previous item --------------
    if (!previous_selection$is_last_item_in_this_set) {
      current_stimulus_index <- x@administered_stimulus_index[position - 1]
    }

  }

  # filter to current set ------------------------------------------------------
  if (constants$group_by_stimulus) {

    if (!is.na(current_stimulus_index)) {

      shadow_test_filtered <- subset(shadow_test, shadow_test$STINDEX == current_stimulus_index)

      # sometimes this leads to no items available -------------------------------
      # this is because the # of items in the set may change between shadowtests
      # e.g.) 4 items have been given out from set S1
      # for position 5, set S1 had 6 items
      # after giving 5th item, code thinks set S1 is not completed yet
      # thus code sets is_last_item_in_this_set to FALSE
      # for position 6, refresh is triggered from schedule and set S1 has 5 items
      # in this case, shadow_test_filtered will have 0 rows
      # another set has to be selected if this happens

      if (nrow(shadow_test_filtered) == 0) {

        current_stimulus_index <- shadow_test$STINDEX[1]
        shadow_test_filtered <- subset(shadow_test, shadow_test$STINDEX == current_stimulus_index)

      }

      shadow_test <- shadow_test_filtered

    }

    if (is.na(current_stimulus_index)) {
      # this happens when a discrete item is selected from a mixed pool
      # do nothing
    }

  }

  # select item
  o$item_selected <- shadow_test$INDEX[1]

  if (constants$group_by_stimulus) {
    o$stimulus_selected <- shadow_test$STINDEX[1]
  }

  if (constants$group_by_stimulus) {

    # this is used to trigger a shadowtest refresh
    o$is_last_item_in_this_set <- nrow(shadow_test) == 1
    # the next item may have to be selected from another set even if this is FALSE

    # if a discrete item is selected from a mixed pool
    # treat it as size-one set
    if (is.na(o$stimulus_selected)) {
      o$is_last_item_in_this_set <- TRUE
    }

  }

  return(o)

}

#' @noRd
shouldShadowTestBeRefreshed <- function(x, position, theta_change, previous_selection) {

  scheduled_value <- x$schedule[position]

  if (!x$dynamic) {
    return(scheduled_value)
  }

  if (x$dynamic) {
    if (x$use_threshold) {
      if (abs(theta_change) > x$threshold) {
        # for THRESHOLD method, if threshold is exceeded, then this always returns true
        # for INTERVAL-THRESHOLD method, if threshold is exceeded, then this returns scheduled value
        # - (equivalent to & operation)
        return(scheduled_value)
      } else {
        return(FALSE)
      }
    }
    if (x$use_setbased) {
      if (previous_selection$is_last_item_in_this_set) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }

  stop("unexpected error: could not parse shadowtest refresh schedule")

}

#' @noRd
updateCompletedGroupingsRecordForStimulus <- function(
  groupings_record,
  selection,
  o,
  position
) {

  if (selection$is_last_item_in_this_set) {

    # if this item is discrete
    if (is.na(selection$stimulus_selected)) {
      return(groupings_record)
    }

    # record the number of items from this set
    # so that the next shadow test can take account for it

    groupings_record$completed_stimulus_index <- c(
      groupings_record$completed_stimulus_index,
      selection$stimulus_selected
    )
    groupings_record$completed_stimulus_size <- c(
      groupings_record$completed_stimulus_size,
      sum(o@administered_stimulus_index == selection$stimulus_selected, na.rm = TRUE)
    )

    return(groupings_record)

  }

  if (!selection$is_last_item_in_this_set) {

    # a new set may have been selected when is_last_item_in_this_set is FALSE
    # detect this and populate groupings_record accordingly

    if (position == 1) {
      return(groupings_record)
    }

    # if the previous item was a discrete item then there is nothing to populate
    if (is.na(o@administered_stimulus_index[position - 1])) {
      return(groupings_record)
    }

    if (
      o@administered_stimulus_index[position] ==
      o@administered_stimulus_index[position - 1]
    ) {
      return(groupings_record)
    }

    if (
      o@administered_stimulus_index[position - 1] %in%
      groupings_record$completed_stimulus_index
    ) {
      return(groupings_record)
    }

    groupings_record$completed_stimulus_index <- c(
      groupings_record$completed_stimulus_index,
      o@administered_stimulus_index[position - 1]
    )
    groupings_record$completed_stimulus_size <- c(
      groupings_record$completed_stimulus_size,
      sum(o@administered_stimulus_index == o@administered_stimulus_index[position - 1], na.rm = TRUE)
    )

  }

  return(groupings_record)

}

#' @noRd
appendMeanInfo <- function(shadow_test, v, mean_info_name) {

  mean_info <- tapply(shadow_test$info, shadow_test[v], mean)
  mean_info <- data.frame(v = names(mean_info), mean_info = mean_info)
  names(mean_info) <- c(v, mean_info_name)
  shadow_test <- merge(
    shadow_test, mean_info, by = v, all.x = TRUE, sort = FALSE
  )

  return(shadow_test)

}
