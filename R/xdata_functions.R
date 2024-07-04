#' @include shadow_functions.R
NULL

#' (Internal) Obtain constraint matrix-data of administered items/sets
#'
#' \code{\link{getXdataOfAdministered}} is an internal function for obtaining constraint matrix-data of administered items/sets.
#' Specifically, it returns a constraint matrix-data that tells the solver to include items/sets that are already administered to the current examinee.
#' This is necessary for shadow-test assembly for adaptive test assembly.
#'
#' @template parameter_simulation_constants
#' @template parameter_position
#' @param x an \code{\linkS4class{output_Shadow}} object, containing data for a single examinee.
#' @template parameter_groupings_record
#' @param constraints a \code{\linkS4class{constraints}} object.
#'
#' @returns \code{\link{getXdataOfAdministered}} returns a constraint matrix-data.
#' A constraint matrix-data is a named list containing the following:
#' \describe{
#'   \item{\code{xmat}}{a (\emph{nc}, \emph{ni}) matrix for the left-hand side in a MIP problem.}
#'   \item{\code{xdir}}{a length-\emph{nc}) vector of relational operators, for comparing the two sides in a MIP problem.}
#'   \item{\code{xrhs}}{a length-\emph{nc}) vector, for the right-hand side in a MIP problem.}
#' }
#'
#' @keywords internal
getXdataOfAdministered <- function(
  simulation_constants,
  position, x,
  groupings_record,
  constraints
) {

  o <- list()

  if (position == 1) {
    return(o)
  }

  nv <- simulation_constants$nv
  ni <- simulation_constants$ni

  # include administered items

  o$xmat <- matrix(0, position - 1, nv)
  o$xdir <- rep("==", position - 1)
  o$xrhs <- rep(1   , position - 1)
  for (p in 1:(position - 1)) {
    o$xmat[p, x@administered_item_index[p]] <- 1
  }

  if (!simulation_constants$group_by_stimulus) {
    return(o)
  }

  # include sets being administered

  unique_administered_stimulus_index <- na.omit(unique(x@administered_stimulus_index))
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

  completed_stimulus_index <- groupings_record$completed_stimulus_index
  completed_stimulus_size  <- groupings_record$completed_stimulus_size
  ns_completed <- length(completed_stimulus_index)

  if (unique_ns > 0 && ns_completed > 0) {

    o$smat <- matrix(0, ns_completed, nv)
    o$sdir <- rep("==", ns_completed)
    o$srhs <- completed_stimulus_size
    for (s in 1:ns_completed) {
      i <- constraints@item_index_by_stimulus[[completed_stimulus_index[s]]]
      o$smat[s, i] <- 1
    }

    o$xmat <- rbind(o$xmat, o$smat)
    o$xdir <-     c(o$xdir, o$sdir)
    o$xrhs <-     c(o$xrhs, o$srhs)

  }

  return(o)

}

#' (Internal) Convert item IDs to item indices for exclusion
#'
#' \code{\link{getIndexOfExcludedEntry}} is an internal function for
#' converting item IDs to item indices for exclusion.
#'
#' Notation:
#' \itemize{
#'   \item{\emph{nj} the number of examinees}
#' }
#'
#' @param exclude a length-\emph{nj} list, where each entry is a named list containing:
#' \itemize{
#'   \item{\code{i} item IDs to exclude for this examinee}
#'   \item{\code{s} item set IDs to exclude for this examinee}
#' }
#' @template parameter_constraints
#'
#' @returns \code{\link{getIndexOfExcludedEntry}} returns a length-\emph{nj} list in the same structure,
#' with its contents converted to item indices.
#'
#' @keywords internal
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

#' (Internal) Translate item exclusion instructions into a constraint matrix-data
#'
#' @template parameter_simulation_constants
#' @param exclude_index a named list containing item/set indices that need to be excluded.
#'
#' @returns \code{\link{getXdataOfExcludedEntry}} returns
#' a named list containing a constraint matrix-data.
#' \itemize{
#'   \item{\code{xmat} The left-hand side multipliers on decision variables.}
#'   \item{\code{xdir} The relation operator.}
#'   \item{\code{xrhs} The right-hand side value.}
#' }
#'
#' @examples
#' simulation_constants <- list(
#'   nv = 5,
#'   ni = 5,
#'   group_by_stimulus = FALSE
#' )
#' exclude_index <- list(
#'   i = c(1, 2)
#' )
#' \dontrun{
#' getXdataOfExcludedEntry(simulation_constants, exclude_index)
#' }
#' @keywords internal
getXdataOfExcludedEntry <- function(simulation_constants, exclude_index) {

  o <- list()

  nv <- simulation_constants$nv
  ni <- simulation_constants$ni

  if (is.null(exclude_index)) {
    return(o)
  }

  # Exclude specified items
  o$xmat <- matrix(0, 1, nv)
  o$xdir <- rep("==", 1)
  o$xrhs <- rep(0   , 1)
  o$xmat[1, exclude_index$i] <- 1

  if (!simulation_constants$group_by_stimulus) {
    return(o)
  }

  # Exclude specified stimuli
  o$xmat[1, ni + exclude_index$s] <- 1

  return(o)

}

#' (Internal) Combine two constraint matrix-data
#'
#' @param x1,x2 a named list containing constraint matrix-data
#'
#' @returns \code{\link{combineXdata}} returns
#' the combined constraint matrix-data.
#'
#' @keywords internal
combineXdata <- function(x1, x2) {

  o <- list()
  o$xmat <- rbind(x1$xmat, x2$xmat)
  o$xdir <-     c(x1$xdir, x2$xdir)
  o$xrhs <-     c(x1$xrhs, x2$xrhs)

  return(o)

}

#' (Internal) Apply information penalty on items to be excluded
#'
#' @param info a one-row matrix containing information values on each item.
#' @param exclude_index a named list containing item/set indices that need to be excluded.
#' @template parameter_simulation_constants
#'
#' @returns \code{\link{getInfoOfExcludedEntry}} returns
#' an updated one-row matrix containing information values.
#'
#' @keywords internal
getInfoOfExcludedEntry <- function(info, exclude_index, simulation_constants) {

  info[exclude_index$i, 1] <-
  info[exclude_index$i, 1] - simulation_constants$exclude_M

  if (!simulation_constants$group_by_stimulus) {
    return(info)
  }

  info[exclude_index$s, 1] <-
  info[exclude_index$s, 1] - simulation_constants$exclude_M

  return(info)

}
