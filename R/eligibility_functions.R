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
