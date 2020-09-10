#' @include shadow_functions.R
NULL

#' @noRd
flagIneligible <- function(exposure_record, exposure_constants, constants, item_index_by_stimulus) {

  o <- list()
  n_segment <- exposure_constants$n_segment

  # Randomly flag items in each segment to be ineligible

  ni       <- constants$ni
  pe_i     <- exposure_record$pe_i
  o$i      <- matrix(0, n_segment, ni)
  p_random <- matrix(runif(n_segment * ni), n_segment, ni)
  o$i[p_random >= pe_i] <- 1

  if (!constants$set_based) {
    return(o)
  }

  # Randomly flag stimuli in each segment to be ineligible

  ns       <- constants$ns
  pe_s     <- exposure_record$pe_s
  o$s      <- matrix(0, n_segment, ns)
  p_random <- matrix(runif(n_segment * ns), n_segment, ns)
  o$s[p_random >= pe_s] <- 1

  for (k in 1:exposure_constants$n_segment) {
    for (s in which(o$s[k, ] == 1)) {
      o$i[k, item_index_by_stimulus[[s]]] <- 1
    }
    for (s in which(o$s[k, ] == 0)) {
      o$i[k, item_index_by_stimulus[[s]]] <- 0
    }
  }

  return(o)

}

#' @noRd
getIneligibleFlagInSegment <- function(ineligible_flag, segment, constants) {
  o <- list()
  o$i <- ineligible_flag$i[segment, ]
  if (!constants$set_based) {
    return(o)
  }
  o$s <- ineligible_flag$s[segment, ]
  return(o)
}

#' @noRd
flagAdministeredAsEligible <- function(o, x, position, constants) {

  o$i[x@administered_item_index[0:(position - 1)]] <- 0
  if (!constants$set_based) {
    return(o)
  }

  o$s[x@administered_stimulus_index[0:(position - 1)]] <- 0
  return(o)

}

#' @noRd
applyIneligibleFlagtoXdata <- function(xdata, ineligible_flag_in_segment, constants, constraints) {

  o <- list()

  ni <- constants$ni
  nv <- constants$nv
  item_index_by_stimulus <- constraints@item_index_by_stimulus

  if (any(ineligible_flag_in_segment$i == 1)) {
    o$xmat_elg <- numeric(nv)
    o$xmat_elg[1:ni] <- ineligible_flag_in_segment$i
    o$xdir_elg <- "=="
    o$xrhs_elg <- 0
  }

  if (any(ineligible_flag_in_segment$s == 1)) {
    o$xmat_elg[(ni + 1):nv] <- ineligible_flag_in_segment$s
    for (s in which(ineligible_flag_in_segment$s == 1)) {
      o$xmat_elg[item_index_by_stimulus[[s]]] <- 1
    }
    for (s in which(ineligible_flag_in_segment$s == 0)) {
      o$xmat_elg[item_index_by_stimulus[[s]]] <- 0
    }
  }

  xdata_elg = list(
    xmat = rbind(o$xmat_elg, xdata$xmat),
    xdir =     c(o$xdir_elg, xdata$xdir),
    xrhs =     c(o$xrhs_elg, xdata$xrhs))

  return(xdata_elg)

}

#' @noRd
applyFading <- function(o, segments_to_apply, exposure_constants, constants) {

  fading_factor <- exposure_constants$fading_factor

  o$n_jk[segments_to_apply]    <- fading_factor * o$n_jk[segments_to_apply]

  if (!is.null(o$p_jk)) {
    o$p_jk[segments_to_apply]  <- fading_factor * o$p_jk[segments_to_apply]
  }

  o$a_ijk[segments_to_apply, ] <- fading_factor * o$a_ijk[segments_to_apply, ]
  o$r_ijk[segments_to_apply, ] <- fading_factor * o$r_ijk[segments_to_apply, ]

  if (!constants$set_based) {
    return(o)
  }

  o$a_sjk[segments_to_apply, ] <- fading_factor * o$a_sjk[segments_to_apply, ]
  o$r_sjk[segments_to_apply, ] <- fading_factor * o$r_sjk[segments_to_apply, ]

  return(o)

}

#' @noRd
getEligibleFlagInSegment <- function(ineligible_flag, segment, constants) {
  o <- list()
  o$i <- !ineligible_flag$i[segment, ]
  if (!constants$set_based) {
    return(o)
  }
  o$s <- !ineligible_flag$s[segment, ]
  return(o)
}

#' @noRd
applyIncrement <- function(o, segments_to_apply, segment_prob, theta_is_feasible, eligible_flag, x, exposure_constants, constants) {

  fading_factor <- exposure_constants$fading_factor

  o$n_jk[segments_to_apply] <- o$n_jk[segments_to_apply] + segment_prob
  if (fading_factor != 1) {
    o$n_jk_nofade[segments_to_apply] <-
    o$n_jk_nofade[segments_to_apply] + segment_prob
  }
  if (theta_is_feasible) {
    o$p_jk[segments_to_apply] <-
    o$p_jk[segments_to_apply] + segment_prob
  }

  administered_i <- x@administered_item_index

  o$a_ijk[segments_to_apply, administered_i] <-
  o$a_ijk[segments_to_apply, administered_i] + segment_prob
  if (fading_factor != 1) {
    o$a_ijk_nofade[segments_to_apply, administered_i] <-
    o$a_ijk_nofade[segments_to_apply, administered_i] + segment_prob
  }
  o$r_ijk <- o$r_ijk + eligible_flag$i * segments_to_apply * segment_prob
  if (exposure_constants$fading_factor != 1) {
    o$r_ijk_nofade <- o$r_ijk_nofade + eligible_flag$i * segments_to_apply * segment_prob
  }

  if (!constants$set_based) {
    return(o)
  }

  administered_s <- na.omit(x@administered_stimulus_index)

  o$a_sjk[segments_to_apply, administered_s] <-
  o$a_sjk[segments_to_apply, administered_s] + segment_prob
  if (fading_factor != 1) {
    o$a_sjk_nofade[segments_to_apply, administered_s] <-
    o$a_sjk_nofade[segments_to_apply, administered_s] + segment_prob
  }
  o$r_sjk <- o$r_sjk + eligible_flag$s * segments_to_apply * segment_prob
  if (exposure_constants$fading_factor != 1) {
    o$r_sjk_nofade <- o$r_sjk_nofade + eligible_flag$s * segments_to_apply * segment_prob
  }

  return(o)

}


#' @noRd
applyClip <- function(o, constants) {

  o$pe_i[is.na(o$pe_i) | o$alpha_ijk == 0] <- 1
  o$pe_i[o$pe_i > 1] <- 1

  if (!constants$set_based) {
    return(o)
  }

  o$pe_s[is.na(o$pe_s) | o$alpha_sjk == 0] <- 1
  o$pe_s[o$pe_s > 1] <- 1

  return(o)

}

#' @noRd
getEligibleFlag <- function(ineligible_flag, constants, force_true) {
  o <- list()
  o$i <- !ineligible_flag$i
  if (force_true) o$i <- TRUE
  if (!constants$set_based) {
    return(o)
  }
  o$s <- !ineligible_flag$s
  if (force_true) o$s <- TRUE
  return(o)
}

#' @noRd
applyIncrementVisitedSegments <- function(o, segment_prob, segment_visited, ineligible_flag_in_segment, x, exposure_constants, constants) {

  segments_to_apply <- getSegmentsToApply(exposure_constants$n_segment, segment_visited)

  administered_i <- x@administered_item_index
  if (any(segments_to_apply)) {
    if (any(ineligible_flag_in_segment$i[administered_i] == 1)) {
      items_visited  <- administered_i[
        x@theta_segment_index %in% segment_visited
      ]
      items_to_apply <- items_visited[ineligible_flag_in_segment$i[items_visited] == 1]
      o$a_ijk[, items_to_apply] <- o$a_ijk[, items_to_apply] + segments_to_apply * segment_prob
    }
  }

  if (!constants$set_based) {
    return(o)
  }

  administered_s <- x@administered_stimulus_index
  if (any(segments_to_apply)) {
    if (any(ineligible_flag_in_segment$s[administered_s] == 1, na.rm = TRUE)) {
      stimuli_visited  <- administered_s[
        x@theta_segment_index %in% segment_visited &
        x@administered_stimulus_index %in% administered_s
      ]
      stimuli_to_apply <- stimuli_visited[ineligible_flag_in_segment$s[stimuli_visited] == 1]
      stimuli_to_apply <- na.omit(stimuli_to_apply)
      o$a_sjk[, stimuli_to_apply] <- o$a_sjk[, stimuli_to_apply] + segments_to_apply * segment_prob
    }
  }

  return(o)

}

#' @noRd
applyAcceleration <- function(o, exposure_constants, constants) {

  max_exposure_rate <- exposure_constants$max_exposure_rate
  acc_factor        <- exposure_constants$acceleration_factor
  n_segment         <- exposure_constants$n_segment

  ni <- constants$ni
  # p_jk is only avalilable in ELIGIBILITY method
  if (is.null(o$p_jk)) {
    nf_ijk <- matrix(1              , n_segment, ni)
  } else {
    nf_ijk <- matrix(o$n_jk / o$p_jk, n_segment, ni)
  }

  if (acc_factor > 1) {
    p_a_ijk <- o$a_ijk / matrix(o$n_jk, n_segment, ni)
    p_r_ijk <- o$r_ijk / matrix(o$n_jk, n_segment, ni)
    p_a_ijk[is.na(p_a_ijk)] <- 0
    p_r_ijk[is.na(p_r_ijk)] <- 1
    idx <- p_a_ijk > max_exposure_rate
    for (k in 1:n_segment) {
      o$pe_i[k,  idx[k, ]] <-
        1 - nf_ijk[k,  idx[k, ]] +
        (max_exposure_rate[k] / p_a_ijk[k, idx[k, ]]) ** acc_factor * nf_ijk[k, idx[k, ]] * p_r_ijk[k, idx[k, ]]
      o$pe_i[k, !idx[k, ]] <-
        1 - nf_ijk[k, !idx[k, ]] +
        max_exposure_rate[k] * nf_ijk[k, !idx[k, ]] * o$r_ijk[k, !idx[k, ]] / o$a_ijk[k, !idx[k, ]]
    }
  } else {
    o$pe_i <- 1 - nf_ijk + max_exposure_rate * nf_ijk * o$r_ijk / o$a_ijk
  }

  if (!constants$set_based) {
    return(o)
  }

  ns <- constants$ns
  # p_jk is only avalilable in ELIGIBILITY method
  if (is.null(o$p_jk)) {
    nf_sjk <- matrix(1              , n_segment, ns)
  } else {
    nf_sjk <- matrix(o$n_jk / o$p_jk, n_segment, ns)
  }

  if (acc_factor > 1) {
    p_a_sjk <- o$a_sjk / matrix(o$n_jk, n_segment, ns)
    p_r_sjk <- o$r_sjk / matrix(o$n_jk, n_segment, ns)
    p_a_sjk[is.na(p_a_sjk)] <- 0
    p_r_sjk[is.na(p_r_sjk)] <- 1
    idx <- p_a_sjk > max_exposure_rate
    for (k in 1:n_segment) {
      o$pe_s[k,  idx[k, ]] <-
        1 - nf_sjk[k,  idx[k, ]] +
        (max_exposure_rate[k] / p_a_sjk[k, idx[k, ]]) ** acc_factor * nf_sjk[k, idx[k, ]] * p_r_sjk[k, idx[k, ]]
      o$pe_s[k, !idx[k, ]] <-
        1 - nf_sjk[k, !idx[k, ]] +
        max_exposure_rate[k] * nf_sjk[k, !idx[k, ]] * o$r_sjk[k, !idx[k, ]] / o$a_sjk[k, !idx[k, ]]
    }
  } else {
    o$pe_s <- 1 - nf_sjk + max_exposure_rate * nf_sjk * o$r_sjk / o$a_sjk
  }

  return(o)

}
