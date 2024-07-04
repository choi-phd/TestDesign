#' @include shadow_functions.R
NULL

#' (Internal) Obtain item/set level eligibility flags
#'
#' \code{\link{flagIneligible}} is an internal function for obtaining item/set-level eligibility flags based on segment-wise exposure rates.
#'
#' @template parameter_exposure_record
#' @template parameter_simulation_constants
#' @param item_index_by_stimulus a list containing item indices by stimulus.
#' @param seed,j (optional) a random seed, and the examinee index. Used to determine the random seed as \code{seed * 123 + j}.
#'
#' @returns \code{\link{flagIneligible}} returns a named list containing the following:
#' \describe{
#'   \item{\code{i}}{a (\emph{n_segment}, \emph{ni}) matrix of 1 and 0 values.}
#'   \item{\code{s}}{a (\emph{n_segment}, \emph{ns}) matrix of 1 and 0 values. Only returned when \code{simulation_constants$group_by_stimulus} is \code{TRUE}.}
#' }
#' In each matrix,
#' 1 indicates the item/set is eligible to be selected in a shadowtest, and
#' 0 indicates the item/set is not eligible to be selected in a shadowtest.
#' The higher the observed exposure rate, the more likely the item/set will be flagged as 0.
#' The rows represent theta segments, and the flags in the row corresponding to the examinee's current interim theta estimate is used for the shadowtest assembly.
#'
#' @keywords internal
flagIneligible <- function(
  exposure_record, simulation_constants,
  item_index_by_stimulus, seed, j, usage_flag = NULL
) {

  if (!is.null(seed)) {
    set.seed(seed * 123 + j)
  }

  o <- list()
  n_segment <- simulation_constants$n_segment

  # Randomly flag items in each segment to be ineligible

  ni       <- simulation_constants$ni
  p_e_i    <- exposure_record$p_e_i
  o$i      <- matrix(1, n_segment, ni)
  p_random <- matrix(runif(n_segment * ni), n_segment, ni)
  o$i[p_random >= p_e_i] <- 0

  # Randomly flag previously seen items in each segment to be ineligible

  if (!is.null(usage_flag) && simulation_constants$use_overlap_control) {
    p_random     <- matrix(runif(n_segment * ni), n_segment, ni)
    overlap_flag <- matrix(rep(usage_flag[1:ni] > 0, n_segment), n_segment, byrow = TRUE)
    o$i[overlap_flag & p_random >= simulation_constants$max_overlap_rate] <- 0
  }

  if (!simulation_constants$group_by_stimulus) {
    return(o)
  }

  # Randomly flag stimuli in each segment to be ineligible

  ns       <- simulation_constants$ns
  p_e_s    <- exposure_record$p_e_s
  o$s      <- matrix(1, n_segment, ns)
  p_random <- matrix(runif(n_segment * ns), n_segment, ns)
  o$s[p_random >= p_e_s] <- 0

  # Randomly flag previously seen stimuli in each segment to be ineligible

  if (!is.null(usage_flag) && simulation_constants$use_overlap_control) {
    p_random <- matrix(runif(n_segment * ns), n_segment, ns)
    overlap_flag <- matrix(rep(usage_flag[(ni + 1):(ni + ns)] > 0, n_segment), n_segment, byrow = TRUE)
    o$i[overlap_flag & p_random >= simulation_constants$max_overlap_rate] <- 0
  }

  for (k in 1:simulation_constants$n_segment) {
    for (s in which(o$s[k, ] == 1)) {
      o$i[k, item_index_by_stimulus[[s]]] <- 1
    }
    for (s in which(o$s[k, ] == 0)) {
      o$i[k, item_index_by_stimulus[[s]]] <- 0
    }
  }

  return(o)

}

#' (Internal) Parse eligibility flags for a specific theta segment
#'
#' \code{\link{getEligibilityFlagInSegment}} is an internal function for obtaining item/set-level eligibility flags for a specific theta segment.
#'
#' @param eligiblity_flag a list containing segment-wise eligibility flags.
#' @param segment the segment index to read from.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{getEligibilityFlagInSegment}} returns a named list containing the following:
#' \describe{
#'   \item{\code{i}}{a length-\emph{ni}) vector of 1 and 0 values.}
#'   \item{\code{s}}{a length-\emph{ns}) vector of 1 and 0 values. Only returned when \code{simulation_constants$group_by_stimulus} is \code{TRUE}.}
#' }
#' In each vector,
#' 1 indicates the item/set is eligible to be selected in a shadowtest, and
#' 0 indicates the item/set is not eligible to be selected in a shadowtest.
#' The higher the observed exposure rate, the more likely the item/set will be flagged as 0.
#'
#' @keywords internal
getEligibilityFlagInSegment <- function(eligiblity_flag, segment, simulation_constants) {
  o <- list()
  o$i <- eligiblity_flag$i[segment, ]
  if (!simulation_constants$group_by_stimulus) {
    return(o)
  }
  o$s <- eligiblity_flag$s[segment, ]
  return(o)
}

#' (Internal) Update eligibility flags to mark administered items as eligible
#'
#' \code{\link{flagAdministeredAsEligible}} is an internal function for updating eligibility flags.
#' Specifically, the function marks items/sets that are already administered to the current examinee as eligible.
#' This is necessary to ensure already administered items/sets are included in the shadowtest.
#'
#' @template parameter_eligibility_flag_in_current_theta_segment
#' @param x an \code{\linkS4class{output_Shadow}} object, containing data for a single examinee.
#' @template parameter_position
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{flagAdministeredAsEligible}} returns an updated eligibility flag list.
#'
#' @keywords internal
flagAdministeredAsEligible <- function(
  eligibility_flag_in_current_theta_segment, x, position, simulation_constants
) {

  eligibility_flag_in_current_theta_segment$i[
    x@administered_item_index[0:(position - 1)]
  ] <- 1
  if (!simulation_constants$group_by_stimulus) {
    return(eligibility_flag_in_current_theta_segment)
  }

  eligibility_flag_in_current_theta_segment$s[
    x@administered_stimulus_index[0:(position - 1)]
  ] <- 1
  return(eligibility_flag_in_current_theta_segment)

}

#' (Internal) Augment constraint matrix-data with eligibility constraints
#'
#' \code{\link{applyEligibilityConstraintsToXdata}} is an internal function for augmenting constraint matrix-data with eligibility constraints.
#' The function marks items/sets marked as ineligibile to be formally excluded in the constraint matrix-data.
#'
#' @param xdata the constriant matrix-data.
#' @template parameter_eligibility_flag_in_current_theta_segment
#' @template parameter_simulation_constants
#' @template parameter_constraints
#'
#' @returns \code{\link{applyEligibilityConstraintsToXdata}} returns an updated constriant matrix-data.
#'
#' @keywords internal
applyEligibilityConstraintsToXdata <- function(
  xdata, eligibility_flag_in_current_theta_segment,
  simulation_constants, constraints) {

  o <- list()

  ni <- simulation_constants$ni
  nv <- simulation_constants$nv
  item_index_by_stimulus <- constraints@item_index_by_stimulus

  if (any(eligibility_flag_in_current_theta_segment$i == 0)) {
    o$xmat_elg <- numeric(nv)
    o$xmat_elg[1:ni] <- (eligibility_flag_in_current_theta_segment$i == 0) * 1
    o$xdir_elg <- "=="
    o$xrhs_elg <- 0
  }

  if (any(eligibility_flag_in_current_theta_segment$s == 0)) {
    o$xmat_elg[(ni + 1):nv] <- eligibility_flag_in_current_theta_segment$s
    for (s in which(eligibility_flag_in_current_theta_segment$s == 1)) {
      o$xmat_elg[item_index_by_stimulus[[s]]] <- 1
    }
    for (s in which(eligibility_flag_in_current_theta_segment$s == 0)) {
      o$xmat_elg[item_index_by_stimulus[[s]]] <- 0
    }
  }

  xdata_elg = list(
    xmat = rbind(o$xmat_elg, xdata$xmat),
    xdir =     c(o$xdir_elg, xdata$xdir),
    xrhs =     c(o$xrhs_elg, xdata$xrhs))

  return(xdata_elg)

}

#' (Internal) Modify item information using eligibility constraints
#'
#' \code{\link{applyEligibilityConstraintsToInfo}} is an internal function for modifying item information using eligibility constraints.
#' This is known as the big M method.
#' The function penalizes item information of items that are marked as ineligibile.
#' This leads to those items being deterred from selected in shadowtest assembly, unless necessary.
#'
#' @param info a length-\emph{ni} vector containing item information on each item, intended for shadow-test assembly.
#' @template parameter_eligibility_flag_in_current_theta_segment
#' @template parameter_config_Shadow
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{applyEligibilityConstraintsToInfo}} returns an updated item information vector.
#'
#' @keywords internal
applyEligibilityConstraintsToInfo <- function(
  info, eligibility_flag_in_current_theta_segment, config, simulation_constants
) {

  if (config@item_selection$method == "GFI") {
    info[eligibility_flag_in_current_theta_segment$i == 0] <-
    info[eligibility_flag_in_current_theta_segment$i == 0] + simulation_constants$exposure_M # add because GFI performs minimization
    return(info)
  }
  if (config@item_selection$method != "GFI") {
    info[eligibility_flag_in_current_theta_segment$i == 0] <-
    info[eligibility_flag_in_current_theta_segment$i == 0] - simulation_constants$exposure_M
    return(info)
  }

}

#' (Internal) Modify item information using overlap constraints
#'
#' \code{\link{applyOverlapConstraintsToInfo}} is an internal function for modifying item information using eligibility constraints.
#' This is known as the big M method.
#' The function penalizes item information of items that have been administered previously (within examinees).
#' This leads to those items being deterred from selected in shadowtest assembly, unless necessary.
#'
#' @param info a length-\emph{ni} vector containing item information on each item, intended for shadow-test assembly.
#' @param usage_flag a length-\emph{ni} vector containing how many times each item has been administered previously to the examinee.
#' @param config a config object.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{applyEligibilityConstraintsToInfo}} returns an updated item information vector.
#'
#' @keywords internal
applyOverlapConstraintsToInfo <- function(
  info, usage_flag, config, simulation_constants
) {

  p_random <- runif(length(usage_flag)) * usage_flag

  if (config@item_selection$method == "GFI") {
    info <-
      info + (p_random >= simulation_constants$max_overlap_rate) *
      usage_flag * simulation_constants$overlap_M # add because GFI performs minimization
    return(info)
  }
  if (config@item_selection$method != "GFI") {
    info <-
      info - (p_random >= simulation_constants$max_overlap_rate) *
      usage_flag * simulation_constants$overlap_M
    return(info)
  }

}

#' (Internal) Apply fading to exposure record
#'
#' \code{\link{applyFading}} is an internal function for applying fading to exposure record.
#' Specifically, the following exposure records are multiplied by \code{fading_factor}:
#' \itemize{
#'   \item{\code{n_k}: the number of examinees completed the test so far, at each segment \emph{k}.}
#'   \item{\code{f_k}: }
#'   \item{\code{a_ik}: the number of times each item \emph{i} was administered to examinees, at each segment \emph{k}}
#'   \item{\code{r_ik}: }
#' }
#' @template parameter_exposure_record
#' @param segments_to_apply which segments to apply fading to.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{applyFading}} returns an updated exposure record.
#'
#' @keywords internal
applyFading <- function(exposure_record, segments_to_apply, simulation_constants) {

  fading_factor <- simulation_constants$fading_factor

  exposure_record$n_jk[segments_to_apply]    <- fading_factor * exposure_record$n_jk[segments_to_apply]

  if (!is.null(exposure_record$f_jk)) {
    exposure_record$f_jk[segments_to_apply]  <- fading_factor * exposure_record$f_jk[segments_to_apply]
  }

  exposure_record$a_ijk[segments_to_apply, ] <- fading_factor * exposure_record$a_ijk[segments_to_apply, ]
  exposure_record$r_ijk[segments_to_apply, ] <- fading_factor * exposure_record$r_ijk[segments_to_apply, ]

  if (!simulation_constants$group_by_stimulus) {
    return(exposure_record)
  }

  exposure_record$a_sjk[segments_to_apply, ] <- fading_factor * exposure_record$a_sjk[segments_to_apply, ]
  exposure_record$r_sjk[segments_to_apply, ] <- fading_factor * exposure_record$r_sjk[segments_to_apply, ]

  return(exposure_record)

}

#' (Internal) Increment exposure record variable: n
#'
#' \code{\link{incrementPhi}} is an internal function for incrementing an exposure record variable.
#' Specifically, the \code{n_k} is incremented by one.
#'
#' @template parameter_exposure_record
#' @param segments_to_apply which segments to apply the incrementing.
#' @param segment_prob the amount of increment.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{incrementN}} returns an updated exposure record.
#'
#' @keywords internal
incrementN <- function(
  exposure_record,
  segments_to_apply, segment_prob,
  simulation_constants
) {

  exposure_record$n_jk[segments_to_apply] <-
  exposure_record$n_jk[segments_to_apply] + segment_prob
  if (simulation_constants$fading_factor != 1) {
    exposure_record$n_jk_nofade[segments_to_apply] <-
    exposure_record$n_jk_nofade[segments_to_apply] + segment_prob
  }

  return(exposure_record)

}

#' (Internal) Increment exposure record variable: phi
#'
#' \code{\link{incrementPhi}} is an internal function for incrementing an exposure record variable.
#' Specifically, the \code{f_k} is incremented by one if the following conditions are simultaneously met:
#' \itemize{
#'   \item{For the final theta segment \emph{k}, shadowtest assembly was feasible at least one times in any item position while interim theta segment was \emph{k}.}
#' }
#'
#' @template parameter_exposure_record
#' @param segments_to_apply which segments to apply fading to.
#' @param segment_prob a vector containing segment-wise classification probabilities of an ability estimate.
#' @param assembly_was_feasible see \code{\link{assemblyInFinalThetaSegmentWasFeasibleAtLeastOnceInInterimThetaSegments}}.
#'
#' @returns \code{\link{incrementPhi}} returns an updated exposure record.
#'
#' @keywords internal
incrementPhi <- function(exposure_record, segments_to_apply, segment_prob, assembly_was_feasible) {

  # for soft constraint exposure control, incrementPhi() is not called for the purpose of code optimization
  # technically, incrementPhi() should be always called because test is always feasible when using soft constraint exposure control
  # but always incrementing f_jk gives f_jk == n_jk, which is only used to compute n_jk / f_jk = 1
  # hence, skip incrementing and just use 1
  # see updateEligibilityRates()

  if (assembly_was_feasible) {
    exposure_record$f_jk[segments_to_apply] <-
    exposure_record$f_jk[segments_to_apply] + segment_prob
  }

  return(exposure_record)

}

#' (Internal) Increment exposure record variable: alpha
#'
#' \code{\link{incrementAlpha}} is an internal function for incrementing an exposure record variable.
#' Specifically, the \code{a_ijk} is incremented by one for administered items/sets.
#'
#' @template parameter_exposure_record
#' @param segments_to_apply which segments to apply the incrementing.
#' @param segment_prob the amount of increment.
#' @param x an \code{\linkS4class{output_Shadow}} object, containing data for a single examinee.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{incrementAlpha}} returns an updated exposure record.
#'
#' @keywords internal
incrementAlpha <- function(
  exposure_record,
  segments_to_apply, segment_prob,
  x,
  simulation_constants
) {

  # van der Linden & Veldkamp (2007)
  # Conditional Item-Exposure Control in Adaptive Testing Using Item-Ineligibility Probabilities
  # page 407

  # a_ijk (alpha): number of times an examinee was in segment k, and
  # was administered the item
  # (used as numerator)

  administered_i <- x@administered_item_index

  exposure_record$a_ijk[segments_to_apply, administered_i] <-
  exposure_record$a_ijk[segments_to_apply, administered_i] + segment_prob
  if (simulation_constants$fading_factor != 1) {
    exposure_record$a_ijk_nofade[segments_to_apply, administered_i] <-
    exposure_record$a_ijk_nofade[segments_to_apply, administered_i] + segment_prob
  }

  if (!simulation_constants$group_by_stimulus) {
    return(exposure_record)
  }

  administered_s <- na.omit(x@administered_stimulus_index)

  exposure_record$a_sjk[segments_to_apply, administered_s] <-
  exposure_record$a_sjk[segments_to_apply, administered_s] + segment_prob
  if (simulation_constants$fading_factor != 1) {
    exposure_record$a_sjk_nofade[segments_to_apply, administered_s] <-
    exposure_record$a_sjk_nofade[segments_to_apply, administered_s] + segment_prob
  }

  return(exposure_record)

}

#' (Internal) Determine if shadowtest assembly was feasible for exposure control purposes
#'
#' \code{\link{assemblyInFinalThetaSegmentWasFeasibleAtLeastOnceInInterimThetaSegments}}
#' is an internal function for determining if shadowtest assembly was feasible for exposure control purposes.
#' Specifically, for the segment \emph{k} the final theta estimate belonged to,
#' it returns \code{TRUE} if shadowtest assembly was feasible at any interim theta that also belonged to \emph{k},
#' and returns \code{FALSE} otherwise.
#'
#' Example 1:
#' \itemize{
#'   \item{Interim theta segments are \code{1, 2, 3, 4, 5, 1, 2, 3, 4, 5}}
#'   \item{Shadowtest feasibility are \code{1, 1, 1, 0, 0, 1, 0, 0, 0, 0}}
#'   \item{Final theta estimate is in segment \code{3}}
#'   \item{From the two vectors, segments where shadowtest assembly was feasible: \code{1, 2, 3, 1}}
#'   \item{Final theta segment \code{3} is an element of the above vector. Return \code{TRUE}}
#' }
#'
#' Example 2:
#' \itemize{
#'   \item{Interim theta segments are \code{1, 2, 3, 4, 5, 1, 2, 3, 4, 5}}
#'   \item{Shadowtest feasibility are \code{1, 1, 1, 0, 0, 1, 0, 0, 0, 0}}
#'   \item{Final theta estimate is in segment \code{4}}
#'   \item{From the two vectors, segments where shadowtest assembly was feasible: \code{1, 2, 3, 1}}
#'   \item{Final theta segment \code{4} is not an element of the above vector. Return \code{FALSE}}
#' }
#'
#' @param x an \code{\linkS4class{output_Shadow}} object, containing data for a single examinee.
#' @param final_theta_segment the segment the final theta estimate belonged to.
#'
#' @returns \code{\link{assemblyInFinalThetaSegmentWasFeasibleAtLeastOnceInInterimThetaSegments}}
#' returns \code{TRUE} or \code{FALSE}.
#'
#' @keywords internal
assemblyInFinalThetaSegmentWasFeasibleAtLeastOnceInInterimThetaSegments <- function(
  x, final_theta_segment
) {

  segments_where_assembly_was_feasible  <- unique(x@theta_segment_index[x@shadow_test_feasible == TRUE])
  assembly_was_feasible <- final_theta_segment %in% segments_where_assembly_was_feasible

  return(assembly_was_feasible)

}

#' (Internal) Increment exposure record variable: rho
#'
#' \code{\link{incrementRho}} is an internal function for incrementing an exposure record variable.
#' Specifically, the \code{s_ijk} variable is incremented for accounting for infeasible shadowtests.
#'
#' @template parameter_exposure_record
#' @param segments_to_apply which segments to apply the incrementing.
#' @param segment_prob the amount of increment.
#' @template parameter_eligibility_flag
#' @param assembly_was_feasible see \code{\link{assemblyInFinalThetaSegmentWasFeasibleAtLeastOnceInInterimThetaSegments}}.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{incrementRho}} returns an updated exposure record.
#'
#' @keywords internal
incrementRho <- function(
  exposure_record, segments_to_apply, segment_prob,
  eligibility_flag, assembly_was_feasible, simulation_constants
) {

  # van der Linden & Veldkamp (2007)
  # Conditional Item-Exposure Control in Adaptive Testing Using Item-Ineligibility Probabilities
  # page 407

  # r_ijk (rho): number of times an examinee was in segment k, and
  # either the item was eligible or the test was infeasible
  # (used as denominator)

  r_flag_i <- eligibility_flag$i
  if (!assembly_was_feasible) r_flag_i <- TRUE
  exposure_record$r_ijk <- exposure_record$r_ijk + r_flag_i * segments_to_apply * segment_prob
  if (simulation_constants$fading_factor != 1) {
    exposure_record$r_ijk_nofade <- exposure_record$r_ijk_nofade + r_flag_i * segments_to_apply * segment_prob
  }

  if (!simulation_constants$group_by_stimulus) {
    return(exposure_record)
  }

  r_flag_s <- eligibility_flag$s
  if (!assembly_was_feasible) r_flag_s <- TRUE
  exposure_record$r_sjk <- exposure_record$r_sjk + r_flag_s * segments_to_apply * segment_prob
  if (simulation_constants$fading_factor != 1) {
    exposure_record$r_sjk_nofade <- exposure_record$r_sjk_nofade + r_flag_s * segments_to_apply * segment_prob
  }

  return(exposure_record)

}

#' (Internal) Clip eligibility rates into 0-1 bounds
#'
#' \code{\link{clipEligibilityRates}} is an internal function for
#' processing eligibility rate updates.
#'
#' @template parameter_exposure_record
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{clipEligibilityRates}} returns an updated exposure record.
#'
#' @keywords internal
clipEligibilityRates <- function(exposure_record, simulation_constants) {

  exposure_record$p_e_i[is.na(exposure_record$p_e_i) | exposure_record$a_ijk == 0] <- 1
  exposure_record$p_e_i[exposure_record$p_e_i > 1] <- 1

  if (!simulation_constants$group_by_stimulus) {
    return(exposure_record)
  }

  exposure_record$p_e_s[is.na(exposure_record$p_e_s) | exposure_record$a_sjk == 0] <- 1
  exposure_record$p_e_s[exposure_record$p_e_s > 1] <- 1

  return(exposure_record)

}

#' (Internal) Apply spike-reduction mechanism on exposure rates
#'
#' \code{\link{adjustAlphaToReduceSpike}} is an internal function for
#' applying spike-reduction mechanism on exposure rates.
#'
#' @template parameter_exposure_record
#' @param segment_prob_of_final_theta the certainty that the final theta estimate
#' belongs in the segment for the final theta estimate. This is \code{1} when exposure control method is
#' \code{ELIGIBILITY, BIGM}, and a probability when exposure control method is \code{BIGM-BAYESIAN}.
#' @param segments_visited visited segments. This means a set of all segments
#' that all interim theta estimates for this examinee belonged to. The segment
#' that the final theta estimate belonged to is not considered a visited segment.
#' @param eligibility_flag_in_final_theta_segment a named list containing eligibility flags
#' in the segment the final theta estimate belongs to.
#' @param x an \code{\linkS4class{output_Shadow}} object, containing data for a single examinee.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{clipEligibilityRates}} returns an updated exposure record.
#'
#' @keywords internal
adjustAlphaToReduceSpike <- function(
  exposure_record, segment_prob_of_final_theta, segments_visited,
  eligibility_flag_in_final_theta_segment, x, simulation_constants
) {

  # van der Linden & Choi (2018)
  # Improving Item-Exposure Control in Adaptive Testing
  # page 13

  # adjust a_ijk (alpha) to reduce overexposure spikes
  # increment a_ijk for visited segments for administered items that were
  # - administered in visited segments AND
  # - ineligible in the final-theta segment
  # visited segments do not include final-theta segment

  segments_to_apply <- getSegmentsToApply(simulation_constants$n_segment, segments_visited)
  if (all(segments_to_apply == 0)) {
    return(exposure_record)
  }

  administered_i <- x@administered_item_index
  if (all(eligibility_flag_in_final_theta_segment$i[administered_i] == 1)) {
    return(exposure_record)
  }

  for (segment_visited in segments_visited) {
    administered_in_visited_segment <-
      x@administered_item_index[
        x@theta_segment_index == segment_visited
      ]
    items_to_apply <-
      administered_in_visited_segment[
        eligibility_flag_in_final_theta_segment$i[administered_in_visited_segment] == 0
      ]
    exposure_record$a_ijk[segment_visited, items_to_apply] <-
    exposure_record$a_ijk[segment_visited, items_to_apply] + segment_prob_of_final_theta
  }

  if (!simulation_constants$group_by_stimulus) {
    return(exposure_record)
  }

  # x@administered_stimulus_index may contain NA if set-based items and discrete items are mixed
  administered_s <- na.omit(unique(x@administered_stimulus_index))
  if (all(eligibility_flag_in_final_theta_segment$s[administered_s] == 1)) {
    return(exposure_record)
  }

  for (segment_visited in segments_visited) {
    administered_in_visited_segment <-
      x@administered_stimulus_index[
        x@theta_segment_index == segment_visited &
        x@administered_stimulus_index %in% administered_s
      ]
    administered_in_visited_segment <- unique(
      administered_in_visited_segment
    )
    stimuli_to_apply <-
      administered_in_visited_segment[
        eligibility_flag_in_final_theta_segment$s[administered_in_visited_segment] == 0
      ]
    exposure_record$a_sjk[segment_visited, stimuli_to_apply] <-
    exposure_record$a_sjk[segment_visited, stimuli_to_apply] + segment_prob_of_final_theta
  }

  return(exposure_record)

}

#' (Internal) Update eligibility rates based on exposure rates
#'
#' \code{\link{updateEligibilityRates}} is an internal function for
#' updating eligibility rates based on exposure rates.
#'
#' @template parameter_exposure_record
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{updateEligibilityRates}} returns an updated exposure record.
#'
#' @keywords internal
updateEligibilityRates <- function(
  exposure_record, simulation_constants
) {

  max_exposure_rate <- simulation_constants$max_exposure_rate
  acc_factor        <- simulation_constants$acceleration_factor
  n_segment         <- simulation_constants$n_segment

  ni <- simulation_constants$ni
  # f_jk: examinees who took a feasible test
  # f_jk: only available in ELIGIBILITY method
  # nf_ijk: correction term for administering infeasible tests
  if (is.null(exposure_record$f_jk)) {
    nf_ijk <- matrix(1              , n_segment, ni)
  } else {
    nf_ijk <- matrix(exposure_record$n_jk / exposure_record$f_jk, n_segment, ni)
  }

  p_a_ijk <- exposure_record$a_ijk / matrix(exposure_record$n_jk, n_segment, ni)
  p_r_ijk <- exposure_record$r_ijk / matrix(exposure_record$n_jk, n_segment, ni)
  p_a_ijk[is.na(p_a_ijk)] <- 0
  p_r_ijk[is.na(p_r_ijk)] <- 1
  if (acc_factor > 1) {
    idx <- p_a_ijk > max_exposure_rate
    for (k in 1:n_segment) {
      exposure_record$p_e_i[k,  idx[k, ]] <-
        1 - nf_ijk[k,  idx[k, ]] +
        (max_exposure_rate[k] / p_a_ijk[k, idx[k, ]]) ** acc_factor * p_r_ijk[k, idx[k, ]] * nf_ijk[k, idx[k, ]]
      exposure_record$p_e_i[k, !idx[k, ]] <-
        1 - nf_ijk[k, !idx[k, ]] +
        (max_exposure_rate[k] / p_a_ijk[k, !idx[k, ]]) * p_r_ijk[k, !idx[k, ]] * nf_ijk[k, !idx[k, ]]
    }
  } else {
    exposure_record$p_e_i <- 1 - nf_ijk + (max_exposure_rate / p_a_ijk * p_r_ijk * nf_ijk)
  }

  if (!simulation_constants$group_by_stimulus) {
    return(exposure_record)
  }

  ns <- simulation_constants$ns
  # f_jk: examinees who took a feasible test
  # f_jk: only available in ELIGIBILITY method
  # nf_ijk: correction term for administering infeasible tests
  if (is.null(exposure_record$f_jk)) {
    nf_sjk <- matrix(1              , n_segment, ns)
  } else {
    nf_sjk <- matrix(exposure_record$n_jk / exposure_record$f_jk, n_segment, ns)
  }

  p_a_sjk <- exposure_record$a_sjk / matrix(exposure_record$n_jk, n_segment, ns)
  p_r_sjk <- exposure_record$r_sjk / matrix(exposure_record$n_jk, n_segment, ns)
  p_a_sjk[is.na(p_a_sjk)] <- 0
  p_r_sjk[is.na(p_r_sjk)] <- 1
  if (acc_factor > 1) {
    idx <- p_a_sjk > max_exposure_rate
    for (k in 1:n_segment) {
      exposure_record$p_e_s[k,  idx[k, ]] <-
        1 - nf_sjk[k,  idx[k, ]] +
        (max_exposure_rate[k] / p_a_sjk[k, idx[k, ]]) ** acc_factor * p_r_sjk[k, idx[k, ]] * nf_sjk[k, idx[k, ]]
      exposure_record$p_e_s[k, !idx[k, ]] <-
        1 - nf_sjk[k, !idx[k, ]] +
        (max_exposure_rate[k] / p_a_sjk[k, !idx[k, ]]) * p_r_sjk[k, !idx[k, ]] * nf_sjk[k, !idx[k, ]]
    }
  } else {
    exposure_record$p_e_s <- 1 - nf_sjk + (max_exposure_rate / p_a_sjk * p_r_sjk * nf_sjk)
  }

  return(exposure_record)

}

#' (Internal) Collect diagnostic exposure record
#'
#' \code{\link{makeDiagnosticExposureRecord}} is an internal function for
#' collecting diagnostic exposure record.
#'
#' @param true_theta examinee's true theta. Used to determine the segment the true theta belongs to.
#' @template parameter_segment_record
#' @param diagnostic_exposure_record a named list containing the diagnostic exposure record.
#' @template parameter_config_Shadow
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{makeDiagnosticExposureRecord}} returns an updated exposure record.
#'
#' @keywords internal
makeDiagnosticExposureRecord <- function(
  true_theta, segment_record,
  diagnostic_exposure_record,
  config,
  simulation_constants
) {

  o <- list()

  if (
    !simulation_constants$use_exposure_control |
    !config@exposure_control$diagnostic_stats
  ) {
    return(o)
  }

  o$elg_stats <- list()

  for (j in 1:simulation_constants$nj) {

    tmp <- list()
    tmp$true_theta         <- true_theta[j, ]
    # find_segment() needs to be updated for multidimensional segments
    tmp$true_segment       <- find_segment(true_theta[j, ], simulation_constants$segment_cut)
    tmp$true_segment_count <- segment_record$count_true[j]
    o$elg_stats[[j]] <- tmp

    a_g_i <- lapply(diagnostic_exposure_record$a_g_i, function(x) { x[j, ] })
    a_g_i <- do.call(rbind, a_g_i)
    o$elg_stats[[j]]$a_g_i <- a_g_i

    e_g_i <- lapply(diagnostic_exposure_record$e_g_i, function(x) { x[j, ] })
    e_g_i <- do.call(rbind, e_g_i)
    o$elg_stats[[j]]$e_g_i <- e_g_i

    if (simulation_constants$group_by_stimulus) {

      a_g_s <- lapply(diagnostic_exposure_record$a_g_s, function(x) { x[j, ] })
      a_g_s <- do.call(rbind, a_g_s)
      o$elg_stats[[j]]$a_g_s <- a_g_s

      e_g_s <- lapply(diagnostic_exposure_record$e_g_s, function(x) { x[j, ] })
      e_g_s <- do.call(rbind, e_g_s)
      o$elg_stats[[j]]$e_g_s <- e_g_s

    }

  }

  if (simulation_constants$fading_factor == 1) {
    return(o)
  }

  o$elg_stats_nofade <- list()

  for (j in 1:simulation_constants$nj) {

    tmp <- list()
    tmp$true_theta         <- true_theta[j, ]
    tmp$true_segment       <- find_segment(true_theta[j, ], simulation_constants$segment_cut)
    tmp$true_segment_count <- segment_record$count_true[j]
    o$elg_stats_nofade[[j]] <- tmp

    a_g_i_nofade <- lapply(diagnostic_exposure_record$a_g_i_nofade, function(x) { x[j, ] })
    a_g_i_nofade <- do.call(rbind, a_g_i_nofade)
    o$elg_stats_nofade[[j]]$a_g_i_nofade <- a_g_i_nofade

    e_g_i_nofade <- lapply(diagnostic_exposure_record$e_g_i_nofade, function(x) { x[j, ] })
    e_g_i_nofade <- do.call(rbind, e_g_i_nofade)
    o$elg_stats_nofade[[j]]$e_g_i_nofade <- e_g_i_nofade

    if (simulation_constants$group_by_stimulus) {

      a_g_s_nofade <- lapply(diagnostic_exposure_record$a_g_s_nofade, function(x) { x[j, ] })
      a_g_s_nofade <- do.call(rbind, a_g_s_nofade)
      o$elg_stats_nofade[[j]]$a_g_s_nofade <- a_g_s_nofade

      e_g_s_nofade <- lapply(diagnostic_exposure_record$e_g_s_nofade, function(x) { x[j, ] })
      e_g_s_nofade <- do.call(rbind, e_g_s_nofade)
      o$elg_stats_nofade[[j]]$e_g_s_nofade <- e_g_s_nofade

    }

  }

  return(o)

}
