#' @include shadow_functions.R
NULL

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
    is_optimal <- isShadowtestOptimal(shadowtest)

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
isShadowtestOptimal <- function(shadowtest) {
  return(isOptimal(shadowtest$status, shadowtest$solver))
}
