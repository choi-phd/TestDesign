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

  # include administered items

  o$xmat <- matrix(0, position - 1, nv)
  o$xdir <- rep("==", position - 1)
  o$xrhs <- rep(1   , position - 1)
  for (p in 1:(position - 1)) {
    o$xmat[p, output@administered_item_index[p]] <- 1
  }

  if (!constants$set_based) {
    return(o)
  }

  # include sets being administered

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

  # constrain set sizes for fully administered sets

  administered_stimulus_index <- stimulus_record$administered_stimulus_index
  administered_stimulus_size  <- stimulus_record$administered_stimulus_size
  ns_completed <- length(administered_stimulus_index)

  if (unique_ns > 0 && ns_completed > 0) {

    o$smat <- matrix(0, ns_completed, nv)
    o$sdir <- rep("==", ns_completed)
    o$srhs <- administered_stimulus_size
    for (s in 1:ns_completed) {
      i <- constraints@item_index_by_stimulus[[administered_stimulus_index[s]]]
      o$smat[s, i] <- 1
    }

    o$xmat <- rbind(o$xmat, o$smat)
    o$xdir <-     c(o$xdir, o$sdir)
    o$xrhs <-     c(o$xrhs, o$srhs)

  }

  return(o)

}

#' @noRd
getIndexOfExcludedEntry <- function(exclude, constraints) {

  if (is.null(exclude)) {
    return(NULL)
  }

  o <- list()
  o <- lapply(
    exclude,
    function(x) {
      tmp <- list()
      tmp$i <- which(constraints@pool@id %in% x$i)
      if (constraints@set_based) {
        tmp$s <- which(constraints@st_attrib@data$STID %in% x$s)
      }
      return(tmp)
    }
  )

  return(o)

}

#' @noRd
getXdataOfExcludedEntry <- function(constants, exclude_index) {

  o <- list()

  nv <- constants$nv
  ni <- constants$ni

  if (is.null(exclude_index)) {
    return(o)
  }

  # Exclude specified items
  o$xmat <- matrix(0, 1, nv)
  o$xdir <- rep("==", 1)
  o$xrhs <- rep(0   , 1)
  o$xmat[1, exclude_index$i] <- 1

  if (!constants$set_based) {
    return(o)
  }

  # Exclude specified stimuli
  o$xmat[1, ni + exclude_index$s] <- 1

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

#' @noRd
getInfoOfExcludedEntry <- function(info, exclude_index, constants) {

  info[exclude_index$i, 1] <-
  info[exclude_index$i, 1] - constants$exclude_M

  if (!constants$set_based) {
    return(info)
  }

  info[exclude_index$s, 1] <-
  info[exclude_index$s, 1] - constants$exclude_M

  return(info)

}
