#' @include shadow_functions.R
NULL

#' (Internal) Parse constants for adaptive test assembly simulation
#'
#' \code{\link{getConstants}} is an internal function for
#' parsing constants for adaptive test assembly simulation.
#'
#' @template parameter_constraints
#' @template parameter_config_Shadow
#' @param arg_data the \code{data} argument from \code{\link{Shadow}}.
#' @param true_theta the \code{true_theta} argument from \code{\link{Shadow}}.
#' @param max_info the maximum possible information attainable by any single item in the pool.
#' This is used to determine the M value if M is not supplied.
#'
#' @returns \code{\link{getConstants}} returns a named list containing constants.
#'
#' @keywords internal
getConstants <- function(constraints, config, arg_data, true_theta, max_info) {

  o <- list()
  o$ni <- constraints@ni
  o$ns <- constraints@ns
  o$nv <- constraints@nv
  o$nd <- 1
  o$theta_q <- config@theta_grid
  if (inherits(o$theta_q, "numeric")) {
    o$theta_q <- matrix(o$theta_q, , 1)
  }
  o$nq      <- nrow(o$theta_q)
  o$min_q   <- min(o$theta_q)
  o$max_q   <- max(o$theta_q)

  if (!is.null(arg_data)) {
    o$nj <- nrow(arg_data)
  }
  if (!is.null(true_theta)) {
    o$nj <- nrow(true_theta)
  }
  if (is.null(o$nj)) {
    stop("either 'data' or 'true_theta' must be supplied")
  }

  content_balancing_method <- toupper(config@content_balancing$method)
  if (content_balancing_method %in% c("STA", "SHADOW", "SHADOWTEST", "SHADOW TEST")) {
    if (is.null(constraints)) {
      stop(sprintf("config@content_balancing: 'constraints' must be supplied when $method is '%s'", content_balancing_method))
    }
    o$use_shadowtest    <- TRUE
    o$group_by_stimulus <- constraints@set_based
    o$test_length       <- constraints@test_length
    o$min_ni            <- constraints@test_length
    o$max_ni            <- constraints@test_length
    o$max_se            <- NULL
  } else {
    o$use_shadowtest    <- FALSE
    o$group_by_stimulus <- FALSE
    o$test_length       <- NULL
    o$min_ni            <- config@stopping_criterion$min_ni
    o$max_ni            <- config@stopping_criterion$max_ni
    o$max_se            <- config@stopping_criterion$se_threshold
  }

  o$exclude_method <- toupper(config@exclude_policy$method)
  o$exclude_M      <- config@exclude_policy$M
  if (is.null(o$exclude_M)) {
    o$exclude_M <- max_info * 100 + 1
  }

  o$use_hand_scored <- !is.null(config@interim_theta$hand_scored_attribute)
  if (o$use_hand_scored) {
    if (length(config@interim_theta$hand_scored_attribute) != 1) {
      stop(sprintf(
        "config@interim_theta$hand_scored_attribute: too many values (expecting one item attribute name)"
      ))
    }
  }
  if (o$use_hand_scored) {
    if (!config@interim_theta$hand_scored_attribute %in% names(constraints@item_attrib)) {
      stop(sprintf(
        "column not found in item attribute table: '%s'",
        config@interim_theta$hand_scored_attribute
      ))
    }
  }
  if (o$use_hand_scored) {
    o$item_is_hand_scored <-
      constraints@item_attrib@data[[config@interim_theta$hand_scored_attribute]]
  }
  if (o$use_hand_scored) {
    # normalize
    if (all(sort(unique(o$item_is_hand_scored)) == c("N", "Y"))) {
      o$item_is_hand_scored <- o$item_is_hand_scored == "Y"
    }
    if (all(sort(unique(o$item_is_hand_scored)) == c("n", "y"))) {
      o$item_is_hand_scored <- o$item_is_hand_scored == "y"
    }
    if (all(sort(unique(o$item_is_hand_scored)) == c("NO", "YES"))) {
      o$item_is_hand_scored <- o$item_is_hand_scored == "YES"
    }
    if (all(sort(unique(o$item_is_hand_scored)) == c("no", "yes"))) {
      o$item_is_hand_scored <- o$item_is_hand_scored == "yes"
    }
    if (all(sort(unique(o$item_is_hand_scored)) == c("0", "1"))) {
      o$item_is_hand_scored <- o$item_is_hand_scored == "1"
    }
    if (!all(sort(unique(o$item_is_hand_scored)) == c(FALSE, TRUE))) {
      stop(sprintf(
        "hand_scored_attribute '%s': {%s} could not be normalized to {FALSE, TRUE} (expecting logical values in item attribute '%s')",
        config@interim_theta$hand_scored_attribute,
        paste0(sort(unique(o$item_is_hand_scored)), collapse = ", "),
        config@interim_theta$hand_scored_attribute
      ))
    }
  }

  o$exposure_control_method <- toupper(config@exposure_control$method)
  if (o$exposure_control_method %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
    o$use_exposure_control <- TRUE
  } else {
    o$use_exposure_control <- FALSE
  }
  o$exposure_M <- config@exposure_control$M
  if (is.null(o$exposure_M)) {
    o$exposure_M <- max_info + 1
  }

  o$max_exposure_rate   <- config@exposure_control$max_exposure_rate
  o$fading_factor       <- config@exposure_control$fading_factor
  o$acceleration_factor <- config@exposure_control$acceleration_factor
  o$n_segment           <- config@exposure_control$n_segment
  o$segment_cut         <- config@exposure_control$segment_cut
  o$cut_lower           <- o$segment_cut[(1:o$n_segment)]
  o$cut_upper           <- o$segment_cut[(1:o$n_segment) + 1]
  o$custom_first_segment_available <- !is.null(config@exposure_control$first_segment)
  o$custom_first_segment_n_values  <- length(config@exposure_control$first_segment)

  o$overlap_control_method <- toupper(config@overlap_control$method)
  if (o$overlap_control_method %in% c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
    o$use_overlap_control <- TRUE
  } else {
    o$use_overlap_control <- FALSE
  }
  o$overlap_M <- config@overlap_control$M
  if (is.null(o$overlap_M)) {
    o$overlap_M <- max_info * 10 + 1
  }
  o$max_overlap_rate <- config@overlap_control$max_overlap_rate

  o$id <- constraints@id

  return(o)

}

#' (Internal) Sanitize item model names for C++ functions
#'
#' \code{\link{sanitizeModel}} is an internal function for
#' sanitizing item model names to be passed onto C++ functions.
#'
#' @param model a vector containing item model names.
#'
#' @returns \code{\link{sanitizeModel}} returns a numeric vector, with \itemize{
#'   \item{\code{item_1PL} converted to \code{1}}
#'   \item{\code{item_2PL} converted to \code{2}}
#'   \item{\code{item_3PL} converted to \code{3}}
#'   \item{\code{item_PC} converted to \code{4}}
#'   \item{\code{item_GPC} converted to \code{5}}
#'   \item{\code{item_GR} converted to \code{6}}
#' }
#'
#' @keywords internal
sanitizeModel <- function(model) {
  model[which(model == "item_1PL")] <- 1
  model[which(model == "item_2PL")] <- 2
  model[which(model == "item_3PL")] <- 3
  model[which(model == "item_PC")]  <- 4
  model[which(model == "item_GPC")] <- 5
  model[which(model == "item_GR")]  <- 6
  model <- as.numeric(model)
  return(model)
}

#' (Internal) Precalculate item information for fixed-theta item selection methods
#'
#' \code{\link{getInfoFixedTheta}} is an internal function for
#' calculating item information for fixed-theta item selection methods.
#' This is done once at the start of the simulation and cached for speed gain.
#'
#' @param item_selection a list containing item selection config.
#' @template parameter_simulation_constants
#' @param item_pool an item pool object.
#' @param model sanitized item model codes to be fed to C++ functions.
#'
#' @returns \code{\link{getInfoFixedTheta}} returns a named list containing item information values
#' at designated thetas for each simulee.
#'
#' @keywords internal
getInfoFixedTheta <- function(
  item_selection, simulation_constants,
  item_pool, model
) {

  nj <- simulation_constants$nj
  info_fixed_theta <- NULL

  if (!is.null(item_selection$fixed_theta)) {
    if (length(item_selection$fixed_theta) == 1) {
      info_fixed_theta <- lapply(
        seq_len(nj), function(j) {
          calc_info(item_selection$fixed_theta, item_pool@ipar, item_pool@NCAT, model)
        }
      )
    }
    if (length(item_selection$fixed_theta) == nj) {
      info_fixed_theta <- lapply(
        seq_len(nj), function(j) {
          calc_info(item_selection$fixed_theta[j], item_pool@ipar, item_pool@NCAT, model)
        }
      )
    }
    if (is.null(info_fixed_theta)) {
      stop("config@item_selection: length($fixed_theta) must be either 1 or nj")
    }
  }

  return(info_fixed_theta)

}

#' (Internal) Compute item information at current theta estimate
#'
#' \code{\link{computeInfoAtCurrentTheta}} is an internal function for
#' computing item information at current theta estimate.
#'
#' @param item_selection the \code{item_selection} slot of a \code{\linkS4class{config_Shadow}} object.
#' @param j the examinee index.
#' @param current_theta a named list containing current theta estimate.
#' @param item_pool an \code{\linkS4class{item_pool}} object.
#' @param model_code machine-sanitized item model codes for passing onto C++ functions.
#' @param info_fixed_theta a list containing item information at prespecified theta points.
#' @param info_grid a matrix containing item information across quadrature points.
#' @param item_parameter_sample a list containing item parameter samples for Bayesian estimation.
#'
#' @keywords internal
computeInfoAtCurrentTheta <- function(
  item_selection,
  j,
  current_theta,
  item_pool,
  model_code,
  info_fixed_theta,
  info_grid,
  item_parameter_sample
) {

  item_method <- toupper(item_selection$method)
  info_type   <- toupper(item_selection$info_type)

  if (item_method == "FIXED") {
    info <- info_fixed_theta[[j]]
    return(info)
  }
  if (item_method == "MFI") {
    info <- calc_info(current_theta$theta, item_pool@ipar, item_pool@NCAT, model_code)
    return(info)
  }
  if (item_method == "GFI") {
    info <- calc_info(current_theta$theta, item_pool@ipar, item_pool@NCAT, model_code)
    return(info)
  }
  if (item_method == "MPWI") {
    info <- as.vector(matrix(current_theta$posterior, nrow = 1) %*% info_grid)
    return(info)
  }
  if (item_method == "EB") {
    info <- calc_info_EB(
      matrix(current_theta$posterior_sample),
      item_pool@ipar, item_pool@NCAT, model_code)[, 1]
    return(info)
  }
  if (item_method == "FB" && info_type == "FISHER") {
    info <- calc_info_FB(
      matrix(current_theta$posterior_sample),
      item_parameter_sample, item_pool@NCAT, model_code)[, 1]
    return(info)
  }
  if (item_method == "FB" && info_type %in% c("MI", "MUTUAL")) {
    info <- calc_MI_FB(
      matrix(current_theta$posterior_sample),
      item_parameter_sample, item_pool@NCAT, model_code)[, 1]
    return(info)
  }
  if (item_method == "RANDOM") {
    random <- runif(item_pool@ni, min = 0, max = 1)
    return(random)
  }
}

#' (Internal) Initialize groupings record
#'
#' \code{\link{initializeCompletedGroupingsRecord}} is an internal function for
#' creating a new groupings record. Used for keeping track of completed groups such as item sets.
#'
#' @returns \code{\link{initializeCompletedGroupingsRecord}} returns a groupings record.
#'
#' @keywords internal
initializeCompletedGroupingsRecord <- function() {
  o <- list()
  o$completed_stimulus_index <- NULL
  o$completed_stimulus_size  <- NULL
  return(o)
}

#' @noRd
selectItem <- function(info, position, o) {

  info[o@administered_item_index[0:(position - 1)]] <- -1

  info_index    <- order(info, decreasing = TRUE)
  item_selected <- info_index[1]

  if (item_selected %in% o@administered_item_index[0:(position - 1)]) {
    stop(sprintf("item %i has been already administered", item_selected))
  }

  return(item_selected)

}
