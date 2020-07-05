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
