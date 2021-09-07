#' @include shadow_functions.R
NULL

#' @noRd
initializeSegmentRecord <- function(constants) {

  o <- list()

  if (!constants$use_eligibility_control) {
    return(o)
  }

  o$freq_true  <- numeric(constants$n_segment)
  o$freq_est   <- numeric(constants$n_segment)
  o$count_true <- numeric(constants$nj)
  o$count_est  <- numeric(constants$nj)

  return(o)

}

#' @noRd
initializeExposureRecord <- function(exposure_control, constants) {

  o <- list()

  ni <- constants$ni
  ns <- constants$ns
  n_segment <- constants$n_segment

  o$n_jk  <- numeric(n_segment)
  if (toupper(exposure_control$method) == "ELIGIBILITY") {
    o$f_jk  <- numeric(n_segment)
  }

  if (constants$fading_factor != 1) {
    o$n_jk_nofade  <- o$n_jk
  }

  o$a_ijk <- matrix(0, n_segment, ni)
  o$r_ijk <- matrix(0, n_segment, ni)
  o$p_e_i  <- matrix(1, n_segment, ni)
  if (constants$fading_factor != 1) {
    o$a_ijk_nofade <- o$a_ijk
    o$r_ijk_nofade <- o$r_ijk
  }

  if (!constants$set_based) {
    return(o)
  }

  o$a_sjk <- matrix(0, n_segment, ns)
  o$r_sjk <- matrix(0, n_segment, ns)
  o$pe_s  <- matrix(1, n_segment, ns)
  if (constants$fading_factor != 1) {
    o$a_sjk_nofade <- o$a_sjk
    o$r_sjk_nofade <- o$r_sjk
  }

  return(o)

}

#' @noRd
getInitialEligibilityStats <- function(o, initial_stats, constants) {

  o$n_jk  <- initial_stats$n_jk
  o$f_jk  <- initial_stats$f_jk
  o$a_ijk <- initial_stats$a_ijk
  o$r_ijk <- initial_stats$r_ijk

  if (!constants$set_based) {
    return(o)
  }

  o$a_sjk <- initial_stats$a_sjk
  o$r_sjk <- initial_stats$r_sjk

  return(o)

}

#' @noRd
initializeExposureRecordSegmentwise <- function(constants) {

  o <- list()
  ni <- constants$ni
  ns <- constants$ns
  nj <- constants$nj
  n_segment     <- constants$n_segment
  fading_factor <- constants$fading_factor

  if (!constants$set_based) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (constants$set_based) {
    o$a_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  if (fading_factor != 1 & !constants$set_based) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
  }
  if (fading_factor != 1 & constants$set_based) {
    o$a_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$e_g_i_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ni), simplify = FALSE)
    o$a_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
    o$e_g_s_nofade <- replicate(n_segment, matrix(0, nrow = nj, ncol = ns), simplify = FALSE)
  }

  return(o)

}

#' @noRd
getSegmentOf <- function(x, constants) {

  o <- list()

  o$final_theta_est   <- find_segment(x@final_theta_est, constants$segment_cut)

  tmp                 <- sort(unique(x@theta_segment_index))
  o$visited           <- tmp[tmp != o$final_theta_est]

  if (is.null(x@true_theta)) {
    return(o)
  }

  o$true_theta        <- find_segment(x@true_theta, constants$segment_cut)
  return(o)

}

#' @noRd
updateSegmentRecord <- function(segment_record, segment_of, j) {

  segment_record$freq_est[segment_of$final_theta_est] <-
  segment_record$freq_est[segment_of$final_theta_est] + 1
  segment_record$count_est[j] <-
  segment_record$freq_est[segment_of$final_theta_est]

  if (is.null(segment_of$true_theta)) {
    return(segment_record)
  }

  segment_record$freq_true[segment_of$true_theta] <-
  segment_record$freq_true[segment_of$true_theta] + 1
  segment_record$count_true[j] <-
  segment_record$freq_true[segment_of$true_theta]

  return(segment_record)

}

#' @noRd
getSegmentsToApply <- function(n_segment, segments) {
  o <- rep(FALSE, n_segment)
  o[segments] <- TRUE
  return(o)
}

#' @noRd
updateExposureRecordSegmentwise <- function(o, j, x, constants) {

  n_segment     <- constants$n_segment
  fading_factor <- constants$fading_factor

  ni <- constants$ni
  for (g in 1:n_segment) {
    o$a_g_i[[g]][j, 1:ni] <- x$a_ijk[g, ]
    o$e_g_i[[g]][j, 1:ni] <- x$r_ijk[g, ]
    if (fading_factor != 1) {
      o$a_g_i_nofade[[g]][j, 1:ni] <- x$a_ijk_nofade[g, ]
      o$e_g_i_nofade[[g]][j, 1:ni] <- x$r_ijk_nofade[g, ]
    }
  }

  if (!constants$set_based) {
    return(o)
  }

  ns <- constants$ns
  for (g in 1:n_segment) {
    o$a_g_s[[g]][j, 1:ns] <- x$a_sjk[g, ]
    o$e_g_s[[g]][j, 1:ns] <- x$r_sjk[g, ]
    if (fading_factor != 1) {
      o$a_g_s_nofade[[g]][j, 1:ns] <- x$a_sjk_nofade[g, ]
      o$e_g_s_nofade[[g]][j, 1:ns] <- x$r_sjk_nofade[g, ]
    }
  }

  return(o)

}

#' @noRd
initializeUsageMatrix <- function(constants) {

  if (!constants$set_based) {
    o <- matrix(FALSE, nrow = constants$nj, ncol = constants$ni)
    return(o)
  }
  if (constants$set_based) {
    o <- matrix(FALSE, nrow = constants$nj, ncol = constants$nv)
    return(o)
  }

}

#' @noRd
updateUsageMatrix <- function(o, j, x, constants) {

  o[j, x@administered_item_index] <- TRUE

  if (!constants$set_based) {
    return(o)
  }

  o[j, constants$ni + x@administered_stimulus_index] <- TRUE

  return(o)

}

#' @noRd
aggregateUsageMatrix <- function(usage_matrix, constants, constraints) {

  if (!constants$set_based) {
    o <- matrix(NA, constants$ni, 2)
    colnames(o) <- c("Item", "Item ER")
    o[, 1] <- 1:constants$ni
    o[, 2] <- apply(usage_matrix, 2, sum) / constants$nj
    return(o)
  }
  if (constants$set_based) {
    o <- matrix(NA, constants$ni, 4)
    colnames(o) <- c("Item", "Stimulus", "Item ER", "Stimulus ER")
    x <- apply(usage_matrix, 2, sum) / constants$nj
    o[, 1] <- 1:constants$ni
    o[, 2] <- constraints@stimulus_index_by_item
    o[, 3] <- x[1:constants$ni]
    o[, 4] <- x[(constants$ni + 1):constants$nv][constraints@stimulus_index_by_item]
    return(o)
  }

}
