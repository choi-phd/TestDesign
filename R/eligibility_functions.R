#' @include shadow_functions.R
NULL

#' @noRd
flagIneligible <- function(exposure_record, constants, item_index_by_stimulus, seed, j) {

  if (!is.null(seed)) {
    set.seed(seed * 123 + j)
  }

  o <- list()
  n_segment <- constants$n_segment

  # Randomly flag items in each segment to be ineligible

  ni       <- constants$ni
  p_e_i    <- exposure_record$p_e_i
  o$i      <- matrix(1, n_segment, ni)
  p_random <- matrix(runif(n_segment * ni), n_segment, ni)
  o$i[p_random >= p_e_i] <- 0

  if (!constants$set_based) {
    return(o)
  }

  # Randomly flag stimuli in each segment to be ineligible

  ns       <- constants$ns
  p_e_s    <- exposure_record$p_e_s
  o$s      <- matrix(1, n_segment, ns)
  p_random <- matrix(runif(n_segment * ns), n_segment, ns)
  o$s[p_random >= p_e_s] <- 0

  for (k in 1:constants$n_segment) {
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
getEligibleFlagInSegment <- function(eligible_flag, segment, constants) {
  o <- list()
  o$i <- eligible_flag$i[segment, ]
  if (!constants$set_based) {
    return(o)
  }
  o$s <- eligible_flag$s[segment, ]
  return(o)
}

#' @noRd
flagAdministeredAsEligible <- function(o, x, position, constants) {

  o$i[x@administered_item_index[0:(position - 1)]] <- 1
  if (!constants$set_based) {
    return(o)
  }

  o$s[x@administered_stimulus_index[0:(position - 1)]] <- 1
  return(o)

}

#' @noRd
applyEligibilityConstraintsToXdata <- function(xdata, eligible_flag_in_current_theta_segment, constants, constraints) {

  o <- list()

  ni <- constants$ni
  nv <- constants$nv
  item_index_by_stimulus <- constraints@item_index_by_stimulus

  if (any(eligible_flag_in_current_theta_segment$i == 0)) {
    o$xmat_elg <- numeric(nv)
    o$xmat_elg[1:ni] <- (eligible_flag_in_current_theta_segment$i == 0) * 1
    o$xdir_elg <- "=="
    o$xrhs_elg <- 0
  }

  if (any(eligible_flag_in_current_theta_segment$s == 0)) {
    o$xmat_elg[(ni + 1):nv] <- eligible_flag_in_current_theta_segment$s
    for (s in which(eligible_flag_in_current_theta_segment$s == 1)) {
      o$xmat_elg[item_index_by_stimulus[[s]]] <- 1
    }
    for (s in which(eligible_flag_in_current_theta_segment$s == 0)) {
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
applyEligibilityConstraintsToInfo <- function(o, eligible_flag_in_current_theta_segment, config, constants) {

  if (is.null(constants$M) & config@item_selection$method == "GFI") {
    o[eligible_flag_in_current_theta_segment$i == 0] <- 1 * constants$max_info + 1 # add because GFI performs minimization
    return(o)
  }
  if (is.null(constants$M) & config@item_selection$method != "GFI") {
    o[eligible_flag_in_current_theta_segment$i == 0] <- -1 * constants$max_info - 1
    return(o)
  }
  if (!is.null(constants$M) & config@item_selection$method == "GFI") {
    o[eligible_flag_in_current_theta_segment$i == 0] <-
    o[eligible_flag_in_current_theta_segment$i == 0] + constants$M # add because GFI performs minimization
    return(o)
  }
  if (!is.null(constants$M) & config@item_selection$method != "GFI") {
    o[eligible_flag_in_current_theta_segment$i == 0] <-
    o[eligible_flag_in_current_theta_segment$i == 0] - constants$M
    return(o)
  }

}

#' @noRd
applyFading <- function(o, segments_to_apply, constants) {

  fading_factor <- constants$fading_factor

  o$n_jk[segments_to_apply]    <- fading_factor * o$n_jk[segments_to_apply]

  if (!is.null(o$f_jk)) {
    o$f_jk[segments_to_apply]  <- fading_factor * o$f_jk[segments_to_apply]
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
getEligibleFlagInSegment <- function(eligible_flag, segment, constants) {
  o <- list()
  o$i <- eligible_flag$i[segment, ]
  if (!constants$set_based) {
    return(o)
  }
  o$s <- eligible_flag$s[segment, ]
  return(o)
}

#' @noRd
incrementN <- function(o, segments_to_apply, segment_prob, constants) {

  o$n_jk[segments_to_apply] <-
  o$n_jk[segments_to_apply] + segment_prob
  if (constants$fading_factor != 1) {
    o$n_jk_nofade[segments_to_apply] <-
    o$n_jk_nofade[segments_to_apply] + segment_prob
  }

  return(o)

}

#' @noRd
incrementPhi <- function(o, segments_to_apply, segment_prob, theta_is_feasible) {

  # for soft costraint exposure control, incrementPhi() is not called for the purpose of code optimization
  # techinally, incrementPhi() should be always called because test is always feasible when using soft costraint exposure control
  # but always incrementing f_jk gives f_jk == n_jk, which is only used to compute n_jk / f_jk = 1
  # hence, skip incrementing and just use 1
  # see updateEligibilityRates()

  if (theta_is_feasible) {
    o$f_jk[segments_to_apply] <-
    o$f_jk[segments_to_apply] + segment_prob
  }

  return(o)

}

#' @noRd
incrementAlpha <- function(o, segments_to_apply, segment_prob, x, constants) {

  # van der Linden & Veldkamp (2007)
  # Conditional Item-Exposure Control in Adaptive Testing Using Item-Ineligibility Probabilities
  # page 407

  # a_ijk (alpha): number of times an examinee was in segment k, and
  # was administered the item
  # (used as numerator)

  administered_i <- x@administered_item_index

  o$a_ijk[segments_to_apply, administered_i] <-
  o$a_ijk[segments_to_apply, administered_i] + segment_prob
  if (constants$fading_factor != 1) {
    o$a_ijk_nofade[segments_to_apply, administered_i] <-
    o$a_ijk_nofade[segments_to_apply, administered_i] + segment_prob
  }

  if (!constants$set_based) {
    return(o)
  }

  administered_s <- na.omit(x@administered_stimulus_index)

  o$a_sjk[segments_to_apply, administered_s] <-
  o$a_sjk[segments_to_apply, administered_s] + segment_prob
  if (constants$fading_factor != 1) {
    o$a_sjk_nofade[segments_to_apply, administered_s] <-
    o$a_sjk_nofade[segments_to_apply, administered_s] + segment_prob
  }

  return(o)

}

#' @noRd
incrementRho <- function(o, segments_to_apply, segment_prob, eligible_flag, theta_is_feasible, constants) {

  # van der Linden & Veldkamp (2007)
  # Conditional Item-Exposure Control in Adaptive Testing Using Item-Ineligibility Probabilities
  # page 407

  # r_ijk (rho): number of times an examinee was in segment k, and
  # either the item was eligible or the test was infeasible
  # (used as denominator)

  r_flag_i <- eligible_flag$i
  if (!theta_is_feasible) r_flag_i <- TRUE
  o$r_ijk <- o$r_ijk + r_flag_i * segments_to_apply * segment_prob
  if (constants$fading_factor != 1) {
    o$r_ijk_nofade <- o$r_ijk_nofade + r_flag_i * segments_to_apply * segment_prob
  }

  if (!constants$set_based) {
    return(o)
  }

  r_flag_s <- eligible_flag$s
  if (!theta_is_feasible) r_flag_s <- TRUE
  o$r_sjk <- o$r_sjk + r_flag_s * segments_to_apply * segment_prob
  if (constants$fading_factor != 1) {
    o$r_sjk_nofade <- o$r_sjk_nofade + r_flag_s * segments_to_apply * segment_prob
  }

  return(o)

}

#' @noRd
clipEligibilityRates <- function(o, constants) {

  o$p_e_i[is.na(o$p_e_i) | o$a_ijk == 0] <- 1
  o$p_e_i[o$p_e_i > 1] <- 1

  if (!constants$set_based) {
    return(o)
  }

  o$p_e_s[is.na(o$p_e_s) | o$a_sjk == 0] <- 1
  o$p_e_s[o$p_e_s > 1] <- 1

  return(o)

}

#' @noRd
adjustAlphaToReduceSpike <- function(o, segment_prob_of_final_theta, segments_visited, eligible_flag_in_final_theta_segment, x, constants) {

  # van der Linden & Choi (2018)
  # Improving Item-Exposure Control in Adaptive Testing
  # page 13

  # adjust a_ijk (alpha) to reduce overexposure spikes
  # increment a_ijk for visited segments for administered items that were
  # - administered in visited segments AND
  # - ineligible in the final-theta segment
  # visited segments do not include final-theta segment

  segments_to_apply <- getSegmentsToApply(constants$n_segment, segments_visited)
  if (all(segments_to_apply == 0)) {
    return(o)
  }

  administered_i <- x@administered_item_index
  if (all(eligible_flag_in_final_theta_segment$i[administered_i] == 1)) {
    return(o)
  }

  for (segment_visited in segments_visited) {
    administered_in_visited_segment <-
      x@administered_item_index[
        x@theta_segment_index == segment_visited
      ]
    items_to_apply <-
      administered_in_visited_segment[
        eligible_flag_in_final_theta_segment$i[administered_in_visited_segment] == 0
      ]
    o$a_ijk[segment_visited, items_to_apply] <-
    o$a_ijk[segment_visited, items_to_apply] + segment_prob_of_final_theta
  }

  if (!constants$set_based) {
    return(o)
  }

  # x@administered_stimulus_index may contain NA if set-based items and discrete items are mixed
  administered_s <- na.omit(unique(x@administered_stimulus_index))
  if (all(eligible_flag_in_final_theta_segment$s[administered_s] == 1)) {
    return(o)
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
        eligible_flag_in_final_theta_segment$s[administered_in_visited_segment] == 0
      ]
    o$a_sjk[segment_visited, stimuli_to_apply] <-
    o$a_sjk[segment_visited, stimuli_to_apply] + segment_prob_of_final_theta
  }

  return(o)

}

#' @noRd
updateEligibilityRates <- function(o, constants) {

  max_exposure_rate <- constants$max_exposure_rate
  acc_factor        <- constants$acceleration_factor
  n_segment         <- constants$n_segment

  ni <- constants$ni
  # f_jk: examinees who took a feasible test
  # f_jk: only available in ELIGIBILITY method
  # nf_ijk: correction term for administering infeasible tests
  if (is.null(o$f_jk)) {
    nf_ijk <- matrix(1              , n_segment, ni)
  } else {
    nf_ijk <- matrix(o$n_jk / o$f_jk, n_segment, ni)
  }

  p_a_ijk <- o$a_ijk / matrix(o$n_jk, n_segment, ni)
  p_r_ijk <- o$r_ijk / matrix(o$n_jk, n_segment, ni)
  p_a_ijk[is.na(p_a_ijk)] <- 0
  p_r_ijk[is.na(p_r_ijk)] <- 1
  if (acc_factor > 1) {
    idx <- p_a_ijk > max_exposure_rate
    for (k in 1:n_segment) {
      o$p_e_i[k,  idx[k, ]] <-
        1 - nf_ijk[k,  idx[k, ]] +
        (max_exposure_rate[k] / p_a_ijk[k, idx[k, ]]) ** acc_factor * p_r_ijk[k, idx[k, ]] * nf_ijk[k, idx[k, ]]
      o$p_e_i[k, !idx[k, ]] <-
        1 - nf_ijk[k, !idx[k, ]] +
        (max_exposure_rate[k] / p_a_ijk[k, !idx[k, ]]) * p_r_ijk[k, !idx[k, ]] * nf_ijk[k, !idx[k, ]]
    }
  } else {
    o$p_e_i <- 1 - nf_ijk + (max_exposure_rate / p_a_ijk * p_r_ijk * nf_ijk)
  }

  if (!constants$set_based) {
    return(o)
  }

  ns <- constants$ns
  # f_jk: examinees who took a feasible test
  # f_jk: only available in ELIGIBILITY method
  # nf_ijk: correction term for administering infeasible tests
  if (is.null(o$f_jk)) {
    nf_sjk <- matrix(1              , n_segment, ns)
  } else {
    nf_sjk <- matrix(o$n_jk / o$f_jk, n_segment, ns)
  }

  p_a_sjk <- o$a_sjk / matrix(o$n_jk, n_segment, ns)
  p_r_sjk <- o$r_sjk / matrix(o$n_jk, n_segment, ns)
  p_a_sjk[is.na(p_a_sjk)] <- 0
  p_r_sjk[is.na(p_r_sjk)] <- 1
  if (acc_factor > 1) {
    idx <- p_a_sjk > max_exposure_rate
    for (k in 1:n_segment) {
      o$p_e_s[k,  idx[k, ]] <-
        1 - nf_sjk[k,  idx[k, ]] +
        (max_exposure_rate[k] / p_a_sjk[k, idx[k, ]]) ** acc_factor * p_r_sjk[k, idx[k, ]] * nf_sjk[k, idx[k, ]]
      o$p_e_s[k, !idx[k, ]] <-
        1 - nf_sjk[k, !idx[k, ]] +
        (max_exposure_rate[k] / p_a_sjk[k, !idx[k, ]]) * p_r_sjk[k, !idx[k, ]] * nf_sjk[k, !idx[k, ]]
    }
  } else {
    o$p_e_s <- 1 - nf_sjk + (max_exposure_rate / p_a_sjk * p_r_sjk * nf_sjk)
  }

  return(o)

}

#' @noRd
parseDiagnosticStats <- function(
  true_theta, segment_record,
  exposure_record_detailed,
  config,
  constants
) {

  o <- list()

  if (!constants$use_eligibility_control | !config@exposure_control$diagnostic_stats) {
    return(o)
  }

  o$elg_stats <- list()

  for (j in 1:constants$nj) {

    tmp <- list()
    tmp$true_theta         <- true_theta[j]
    tmp$true_segment       <- find_segment(true_theta[j], constants$segment_cut)
    tmp$true_segment_count <- segment_record$count_true[j]
    o$elg_stats[[j]] <- tmp

    a_g_i <- lapply(exposure_record_detailed$a_g_i, function(x) { x[j, ] })
    a_g_i <- do.call(rbind, a_g_i)
    o$elg_stats[[j]]$a_g_i <- a_g_i

    e_g_i <- lapply(exposure_record_detailed$e_g_i, function(x) { x[j, ] })
    e_g_i <- do.call(rbind, e_g_i)
    o$elg_stats[[j]]$e_g_i <- e_g_i

    if (constants$set_based) {

      a_g_s <- lapply(exposure_record_detailed$a_g_s, function(x) { x[j, ] })
      a_g_s <- do.call(rbind, a_g_s)
      o$elg_stats[[j]]$a_g_s <- a_g_s

      e_g_s <- lapply(exposure_record_detailed$e_g_s, function(x) { x[j, ] })
      e_g_s <- do.call(rbind, e_g_s)
      o$elg_stats[[j]]$e_g_s <- e_g_s

    }

  }

  if (constants$fading_factor == 1) {
    return(o)
  }

  o$elg_stats_nofade <- list()

  for (j in 1:constants$nj) {

    tmp <- list()
    tmp$true_theta         <- true_theta[j]
    tmp$true_segment       <- find_segment(true_theta[j], constants$segment_cut)
    tmp$true_segment_count <- segment_record$count_true[j]
    o$elg_stats_nofade[[j]] <- tmp

    a_g_i_nofade <- lapply(exposure_record_detailed$a_g_i_nofade, function(x) { x[j, ] })
    a_g_i_nofade <- do.call(rbind, a_g_i_nofade)
    o$elg_stats_nofade[[j]]$a_g_i_nofade <- a_g_i_nofade

    e_g_i_nofade <- lapply(exposure_record_detailed$e_g_i_nofade, function(x) { x[j, ] })
    e_g_i_nofade <- do.call(rbind, e_g_i_nofade)
    o$elg_stats_nofade[[j]]$e_g_i_nofade <- e_g_i_nofade

    if (constants$set_based) {

      a_g_s_nofade <- lapply(exposure_record_detailed$a_g_s_nofade, function(x) { x[j, ] })
      a_g_s_nofade <- do.call(rbind, a_g_s_nofade)
      o$elg_stats_nofade[[j]]$a_g_s_nofade <- a_g_s_nofade

      e_g_s_nofade <- lapply(exposure_record_detailed$e_g_s_nofade, function(x) { x[j, ] })
      e_g_s_nofade <- do.call(rbind, e_g_s_nofade)
      o$elg_stats_nofade[[j]]$e_g_s_nofade <- e_g_s_nofade

    }

  }

  return(o)

}
