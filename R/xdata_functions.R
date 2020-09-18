#' @include shadow_functions.R
NULL

#' @noRd
getXdataOfAdministered <- function(constants, position, output, stimulus_record, constraints) {

  o <- list()

  if (position == 1) {
    return(o)
  }

  nv <- constants$nv
  ni <- constants$ni

  # Include administered items in selection

  o$xmat <- matrix(0, position - 1, nv)
  o$xdir <- rep("==", position - 1)
  o$xrhs <- rep(1   , position - 1)
  for (p in 1:(position - 1)) {
    o$xmat[p, output@administered_item_index[p]] <- 1
  }

  if (!constants$set_based) {
    return(o)
  }

  # Include administered stimulus in selection

  unique_administered_stimulus_index <- na.omit(unique(output@administered_stimulus_index))
  unique_ns <- length(unique_administered_stimulus_index)

  if (unique_ns > 0) {

    o$smat <- matrix(0, unique_ns, nv)
    o$sdir <- rep("==", unique_ns)
    o$srhs <- rep(1,    unique_ns)
    for (s in 1:unique_ns) {
      stimulus_id                 <- unique_administered_stimulus_index[s]
      o$smat[s, ni + stimulus_id] <- 1
    }

    o$xmat <- rbind(o$xmat, o$smat)
    o$xdir <-     c(o$xdir, o$sdir)
    o$xrhs <-     c(o$xrhs, o$srhs)

  }

  if (unique_ns > 0 && stimulus_record$end_set) {

    o$smat <- matrix(0, unique_ns, nv)
    o$sdir <- rep("==", unique_ns)
    o$srhs <- rep(0,    unique_ns)
    for (s in 1:unique_ns) {
      stimulus_id                      <- unique_administered_stimulus_index[s]
      all_items_of_stimulus            <- constraints@item_index_by_stimulus[[stimulus_id]]
      n_administered_items_of_stimulus <- sum(output@administered_stimulus_index[1:(position - 1)] == stimulus_id, na.rm = TRUE)
      o$smat[s, all_items_of_stimulus] <- 1
      o$srhs[s]                        <- n_administered_items_of_stimulus
    }

    o$xmat <- rbind(o$xmat, o$smat)
    o$xdir <-     c(o$xdir, o$sdir)
    o$xrhs <-     c(o$xrhs, o$srhs)

  }

  # include administered set

  end_set                      <- stimulus_record$end_set
  finished_stimulus_index      <- stimulus_record$finished_stimulus_index
  finished_stimulus_item_count <- stimulus_record$finished_stimulus_item_count
  n_finished_stimulus          <- length(finished_stimulus_index)

  if (unique_ns > 0 && !end_set && n_finished_stimulus > 0) {

    o$smat <- matrix(0, nrow = n_finished_stimulus, ncol = nv)
    o$sdir <- rep("==", n_finished_stimulus)
    o$srhs <- finished_stimulus_item_count
    for (s in 1:n_finished_stimulus) {
      stimulus_id <- constraints@item_index_by_stimulus[[finished_stimulus_index[s]]]
      o$smat[s, stimulus_id] <- 1
    }

    o$xmat <- rbind(o$xmat, o$smat)
    o$xdir <-     c(o$xdir, o$sdir)
    o$xrhs <-     c(o$xrhs, o$srhs)

  }

  return(o)

}

#' @noRd
getIndexOfExcludedItems <- function(excluded_items, item_pool) {

  if (is.null(excluded_items)) {
    return(excluded_items)
  }

  o <- lapply(
    excluded_items,
    function(x) {
      which(item_pool@id %in% x)
    }
  )

  return(o)

}

#' @noRd
getXdataOfExcludedItems <- function(constants, excluded_items) {

  o <- list()

  nv <- constants$nv
  ni <- constants$ni

  # Exclude specified items

  o$xmat <- matrix(0, 1, nv)
  o$xdir <- rep("==", 1)
  o$xrhs <- rep(0   , 1)
  o$xmat[1, excluded_items] <- 1

  return(o)

}

#' @noRd
combineXdata <- function(x1, x2) {

  o <- list()
  o$xmat <- rbind(x1$xmat, x2$xmat)
  o$xdir <-     c(x1$xdir, x2$xdir)
  o$xrhs <-     c(x1$xrhs, x2$xrhs)

  return(o)

}
