#' @include static_functions.R
NULL

#' @rdname simResp-methods
#' @aliases simResp,item_pool_cluster,numeric-method
setMethod(
  f = "simResp",
  signature = c("item_pool_cluster", "list"),
  definition = function(object, theta) {
    if (length(theta) != length(object@np)) {
      data <- vector(mode = "list", length = object@np)
      for (i in 1:object@np) {
        if (all(!is.na(theta[[i]]))) {
          data[[i]] <- simResp(object@pools[[i]], theta[[i]])
        } else {
          stop(paste0("invalid values in theta", "[[", i, "]]"))
        }
      }
      return(data)
    } else {
      stop("length of theta not equal to np")
    }
  }
)

#' Generate a test cluster object
#'
#' Generate a \code{\linkS4class{test_cluster}} object
#'
#' @param object An \code{\linkS4class{item_pool_cluster}} object
#' @param theta A grid of theta values
#' @param true_theta An optional vector of true theta values to simulate response data
#'
#' @docType methods
#' @rdname makeTestCluster-methods
setGeneric(
  name = "makeTestCluster",
  def = function(object, theta, true_theta) {
    standardGeneric("makeTestCluster")
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods
setMethod(
  f = "makeTestCluster",
  signature = c("item_pool_cluster", "numeric", "numeric"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta)
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)

#' @docType methods
#' @rdname makeTestCluster-methods
setMethod(
  f = "makeTestCluster",
  signature = c("item_pool_cluster", "numeric", "list"),
  definition = function(object, theta, true_theta) {
    tests <- vector(mode = "list", length = object@np)
    for (p in 1:object@np) {
      tests[[p]] <- makeTest(object@pools[[p]], theta, true_theta[[p]])
    }
    return(new("test_cluster", nt = object@np, names = object@names))
  }
)

#' Run adaptive test assembly
#'
#' \code{\link{Shadow}} is a test assembly function to perform adaptive test assembly based on the generalized shadow-test framework.
#'
#' @template config_Shadow-param
#' @template constraints-param
#' @param true_theta (optional) true theta values to use in simulation. Either \code{true_theta} or \code{data} must be supplied.
#' @param data (optional) a matrix containing item response data to use in simulation. Either \code{true_theta} or \code{data} must be supplied.
#' @param prior (optional) prior density at each \code{config@theta_grid}. This overrides \code{prior_par}. Can be a vector to use the same prior for all \emph{nj} participants, or a \emph{nj}-row matrix to use a different prior for each participant.
#' @param prior_par (optional) normal distribution parameters \code{c(mean, sd)} to use as prior. Can be a vector to use the same prior for all \emph{nj} participants, or a \emph{nj}-row matrix to use a different prior for each participant.
#' @param excluded_items (optional) a list containing item names to exclude from selection for each participant.
#' @template force_solver_param
#' @param session (optional) used to communicate with Shiny app \code{\link{TestDesign}}.
#'
#' @return \code{\link{Shadow}} returns an \code{\linkS4class{output_Shadow_all}} object containing assembly results.
#'
#' @template mip-ref
#' @template mipversus-ref
#' @template mipset-ref
#' @template mipbook-ref
#' @rdname Shadow-methods
#'
#' @examples
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(config, constraints_science, true_theta)
#' solution@output
#' @export
setGeneric(
  name = "Shadow",
  def = function(config, constraints = NULL, true_theta = NULL, data = NULL, prior = NULL, prior_par = NULL, excluded_items = NULL, force_solver = FALSE, session = NULL) {
    standardGeneric("Shadow")
  }
)

#' @rdname Shadow-methods
#' @export
setMethod(
  f = "Shadow",
  signature = "config_Shadow",
  definition = function(config, constraints, true_theta, data, prior, prior_par, excluded_items, force_solver = FALSE, session) {

    if (!validObject(config)) {
      stop("'config' argument is not a valid 'config_Shadow' object")
    }

    if (is.null(constraints)) {
      stop("'constraints' must be supplied")
    }

    if (!force_solver) {
      o <- validateSolver(config, constraints)
      if (!o) {
        return(invisible())
      }
    }

    pool                <- constraints@pool
    model               <- sanitizeModel(pool@model)
    constants           <- getConstants(constraints, config, data, true_theta)
    all_data            <- makeData(pool, true_theta, data, constants)
    info_fixed_theta    <- getInfoFixedTheta(config@item_selection, constants, all_data$test, pool, model)
    posterior_constants <- getPosteriorConstants(config)
    posterior_record    <- initializePosterior(prior, prior_par, config, constants, pool, posterior_constants)
    initial_theta       <- initializeTheta(config, constants, posterior_record)
    excluded_items      <- getIndexOfExcludedItems(excluded_items, pool)

    if (constants$use_shadow) {
      refresh_shadow <- initializeShadowEngine(constants, config@refresh_policy)
    }

    # Initialize exposure rate control

    exposure_control   <- toupper(config@exposure_control$method)
    exposure_constants <- getExposureConstants(config@exposure_control)
    items_administered <- matrix(FALSE, constants$nj, constants$ni)
    o_list <- vector(mode = "list", length = constants$nj)

    if (exposure_control %in% c("NONE", "ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {

      segment_record           <- initializeSegmentRecord(exposure_constants, constants)
      exposure_record          <- initializeExposureRecord(config@exposure_control, exposure_constants, constants)
      exposure_record_detailed <- initializeExposureRecordSegmentwise(exposure_constants, constants)
      if (!is.null(config@exposure_control$initial_eligibility_stats)) {
        exposure_record <- config@exposure_control$initial_eligibility_stats
      }

    }

    # Initialize usage matrix

    if (constants$set_based) {
      usage_matrix <- matrix(FALSE, nrow = constants$nj, ncol = constants$nv)
    } else {
      usage_matrix <- matrix(FALSE, nrow = constants$nj, ncol = constants$ni)
    }

    # Loop over nj simulees

    has_progress_pkg <- requireNamespace("progress")
    if (has_progress_pkg) {
      w <- options()$width - 50
      if (w <= 20) {
        w <- options()$width - 2
      }
      pb <- progress::progress_bar$new(
        format = "[:bar] :spin :current/:total (:percent) eta :eta | ",
        total = constants$nj, clear = FALSE,
        width = w)
      pb$tick(0)
    } else {
      pb <- txtProgressBar(0, constants$nj, char = "|", style = 3)
    }

    for (j in 1:constants$nj) {

      o <- new("output_Shadow")
      o@simulee_id <- j

      if (!is.null(true_theta)) {
        o@true_theta <- true_theta[j]
      }

      o@prior <- posterior_record$posterior[j, ]
      o@administered_item_index     <- rep(NA_real_, constants$max_ni)
      o@administered_item_resp      <- rep(NA_real_, constants$max_ni)
      o@administered_stimulus_index <- NaN
      o@theta_segment_index         <- rep(NA_real_, constants$max_ni)
      o@interim_theta_est           <- rep(NA_real_, constants$max_ni)
      o@interim_se_est              <- rep(NA_real_, constants$max_ni)
      o@shadow_test                 <- vector("list", constants$max_ni)
      o@max_cat_pool                <- pool@max_cat
      o@test_length_constraints     <- constants$max_ni
      o@ni_pool                     <- constants$ni
      o@ns_pool                     <- constants$ns
      o@set_based                   <- constants$set_based
      o@item_index_by_stimulus      <- constraints@item_index_by_stimulus

      current_theta <- estimateInitialTheta(
        config@interim_theta, initial_theta, prior_par,
        constants$nj, j,
        posterior_constants)

      # Simulee: initialize stimulus record

      if (constants$set_based) {
        o@administered_stimulus_index <- rep(NA_real_, constants$max_ni)
        stimulus_record <- initializeStimulusRecord()
      }

      # Simulee: initialize shadow test record

      if (constants$use_shadow) {
        o@shadow_test_feasible  <- logical(constants$test_length)
        o@shadow_test_refreshed <- logical(constants$test_length)
      }

      posterior_record$likelihood <- rep(1, constants$nq)

      theta_change <- 10000
      done         <- FALSE
      position     <- 0

      # Simulee: flag ineligibile items

      if (exposure_constants$use_eligibility_control) {
        ineligible_flag <- flagIneligible(exposure_record, exposure_constants, constants, constraints@item_index_by_stimulus)
      }

      # Simulee: administer items

      position <- 0

      while (!done) {

        position <- position + 1
        info     <- getInfo(
          config@item_selection, j, info_fixed_theta, current_theta, pool, model,
          posterior_record, all_data$test@info
        )

        # Item position / simulee: do shadow test assembly

        if (constants$use_shadow) {

          o@theta_segment_index[position] <- getThetaSegment(current_theta, position, config@exposure_control, exposure_constants)

          if (shouldShadowBeRefreshed(
            position, config@refresh_policy, refresh_shadow,
            theta_change, constants, stimulus_record
          )) {

            administered_stimulus_index <- na.omit(unique(o@administered_stimulus_index))
            o@shadow_test_refreshed[position] <- TRUE

            xdata         <- getXdataOfAdministered(constants, position, o, stimulus_record, constraints)
            xdata_exclude <- getXdataOfExcludedItems(constants, excluded_items[[j]])
            xdata         <- combineXdata(xdata, xdata_exclude)

            # Do exposure control

            if (exposure_constants$use_eligibility_control) {

              # Get ineligibile items in the current theta segment

              current_segment            <- o@theta_segment_index[position]
              ineligible_flag_in_segment <- getIneligibleFlagInSegment(ineligible_flag, current_segment, constants)
              ineligible_flag_in_segment <- flagAdministeredAsEligible(ineligible_flag_in_segment, o, position, constants)

            }

            if (exposure_constants$use_eligibility_control && exposure_control %in% c("ELIGIBILITY")) {

              xdata_elg  <- applyIneligibleFlagtoXdata(xdata, ineligible_flag_in_segment, constants, constraints)
              optimal    <- runAssembly(config, constraints, xdata = xdata_elg, objective = info)
              is_optimal <- isOptimal(optimal$status, config@MIP$solver)

              # If not optimal, retry without xmat

              if (is_optimal) {
                o@shadow_test_feasible[position] <- TRUE
              } else {
                o@shadow_test_feasible[position] <- FALSE
                optimal <- runAssembly(config, constraints, xdata = xdata, objective = info)
              }

            }

            if (exposure_constants$use_eligibility_control && exposure_control %in% c("BIGM", "BIGM-BAYESIAN")) {

              # Do Big-M based exposure control
              # Penalize item info

              if (!is.null(config@exposure_control$M)) {
                info[ineligible_flag_in_segment$i == 1] <- info[ineligible_flag_in_segment$i == 1] - config@exposure_control$M
              } else {
                info[ineligible_flag_in_segment$i == 1] <- -1 * all_data$max_info - 1
              }

              optimal <- runAssembly(config, constraints, xdata = xdata, objective = info)
              o@shadow_test_feasible[position] <- TRUE

            }

            if (!exposure_constants$use_eligibility_control) {

              optimal <- runAssembly(config, constraints, xdata = xdata, objective = info)
              o@shadow_test_feasible[position] <- TRUE

            }

            is_optimal <- isOptimal(optimal$status, config@MIP$solver)
            if (!is_optimal) {
              printSolverNewline(config@MIP$solver)
              msg <- getSolverStatusMessage(optimal$status, config@MIP$solver)
              warning(msg, immediate. = TRUE)
              stop(sprintf("MIP solver returned non-zero status at examinee %i position %i", j, position))
            }

            o@solve_time[position] <- optimal$solve_time

          } else {

            o@shadow_test_refreshed[position] <- FALSE
            o@shadow_test_feasible[position]  <- o@shadow_test_feasible[position - 1]

          }

          # Select an item from shadow test

          selection <- selectItemFromShadowTest(optimal$shadow_test, position, constants, o)
          o@administered_item_index[position] <- selection$item_selected
          o@shadow_test[[position]]$i         <- optimal$shadow_test$INDEX
          o@shadow_test[[position]]$s         <- optimal$shadow_test$STINDEX

        } else {

          # If not doing shadow

          o@administered_item_index[position] <- selectItem(info, position, o)

        }

        # Item position / simulee: record which stimulus was administered

        if (constants$set_based) {
          o@administered_stimulus_index[position] <- selection$stimulus_selected

          if (selection$stimulus_finished) {
            stimulus_record$end_set <- TRUE
          } else {
            stimulus_record$end_set <- FALSE
          }

          if (!is.na(selection$stimulus_of_previous_item)) {
            if (selection$new_stimulus_selected && selection$stimulus_of_previous_item > 0) {
              stimulus_record$finished_stimulus_index <- c(
              stimulus_record$finished_stimulus_index,
              selection$stimulus_of_previous_item)
              stimulus_record$finished_stimulus_item_count <- c(
              stimulus_record$finished_stimulus_item_count,
              sum(o@administered_stimulus_index[1:(position - 1)] == selection$stimulus_of_previous_item, na.rm = TRUE))
            }
          }
        }

        # Item position / simulee: record which item was administered

        o@administered_item_resp[position] <- all_data$test@data[j, o@administered_item_index[position]]
        o@administered_item_ncat[position] <- pool@NCAT[o@administered_item_index[position]]
        items_administered[j, o@administered_item_index[position]] <- TRUE

        # Item position / simulee: update posterior

        prob_resp <- all_data$test@prob[[o@administered_item_index[position]]][, o@administered_item_resp[position] + 1]
        posterior_record <- updatePosterior(posterior_record, j, prob_resp)

        # Item position / simulee: estimate theta

        if (toupper(config@interim_theta$method) == "EAP") {

          interim_EAP <- estimateThetaEAP(posterior_record$posterior[j, ], constants$theta_q)
          interim_EAP <- applyShrinkageCorrection(interim_EAP, config@interim_theta)

          o@interim_theta_est[position] <- interim_EAP$theta
          o@interim_se_est[position]    <- interim_EAP$se

        } else if (toupper(config@interim_theta$method) == "MLE") {

          interim_EAP <- estimateThetaEAP(posterior_record$posterior[j, ], constants$theta_q)
          interim_MLE <- mle(pool,
            select      = o@administered_item_index[1:position],
            resp        = o@administered_item_resp[1:position],
            start_theta = interim_EAP$theta,
            max_iter    = config@interim_theta$max_iter,
            crit        = config@interim_theta$crit,
            theta_range = config@interim_theta$bound_ML,
            truncate    = config@interim_theta$truncate_ML,
            max_change  = config@interim_theta$max_change,
            do_Fisher   = config@interim_theta$do_Fisher
          )

          o@interim_theta_est[position] <- interim_MLE$th
          o@interim_se_est[position]    <- interim_MLE$se

        } else if (toupper(config@interim_theta$method) == "MLEF") {

          interim_EAP <- estimateThetaEAP(posterior_record$posterior[j, ], constants$theta_q)
          interim_MLEF <- mlef(pool,
            select           = o@administered_item_index[1:position],
            resp             = o@administered_item_resp[1:position],
            fence_slope      = config@interim_theta$fence_slope,
            fence_difficulty = config@interim_theta$fence_difficulty,
            start_theta      = interim_EAP$theta,
            max_iter         = config@interim_theta$max_iter,
            crit             = config@interim_theta$crit,
            theta_range      = config@interim_theta$bound_ML,
            truncate         = config@interim_theta$truncate_ML,
            max_change       = config@interim_theta$max_change,
            do_Fisher        = config@interim_theta$do_Fisher
          )

          o@interim_theta_est[position] <- interim_MLEF$th
          o@interim_se_est[position]    <- interim_MLEF$se

        } else if (toupper(config@interim_theta$method) == "EB") {

          current_item <- o@administered_item_index[position]

          interim_EB <- theta_EB_single(
            posterior_constants$n_sample, current_theta$theta, current_theta$se,
            pool@ipar[current_item, ],
            o@administered_item_resp[position], pool@NCAT[current_item],
            model[current_item], 1, c(current_theta$theta, current_theta$se)
          )[, 1]

          interim_EB                    <- applyThin(interim_EB, posterior_constants)

          o@posterior_sample            <- interim_EB
          o@interim_theta_est[position] <- mean(interim_EB)
          o@interim_se_est[position]    <- sd(interim_EB)

        } else if (toupper(config@interim_theta$method) == "FB") {

          current_item <- o@administered_item_index[position]

          interim_FB <- theta_FB_single(
            posterior_constants$n_sample, current_theta$theta, current_theta$se, posterior_record$ipar_list[[current_item]],
            pool@ipar[current_item, ],
            o@administered_item_resp[position], pool@NCAT[current_item],
            model[current_item], 1, c(current_theta$theta, current_theta$se)
          )[, 1]

          interim_FB                    <- applyThin(interim_FB, posterior_constants)

          o@posterior_sample            <- interim_FB
          o@interim_theta_est[position] <- mean(interim_FB)
          o@interim_se_est[position]    <- sd(interim_FB)

        }

        theta_change                   <- o@interim_theta_est[position] - current_theta$theta
        current_theta$posterior_sample <- o@posterior_sample
        current_theta$theta            <- o@interim_theta_est[position]
        current_theta$se               <- o@interim_se_est[position]

        # Item position / simulee: trigger shadow test refresh if theta change is sufficient

        if (toupper(config@refresh_policy$method) == "THRESHOLD") {
          if ((abs(theta_change) > config@refresh_policy$threshold) && (position < constants$test_length)) {
            refresh_shadow[position + 1] <- TRUE
          }
        }

        # Item position / simulee: prepare for the next item position

        if (position == constants$max_ni) {
          done <- TRUE
          o@likelihood <- posterior_record$likelihood
          o@posterior  <- posterior_record$posterior[j, ]
        }

        if (has_progress_pkg) {
          pb$tick(0)
        }

      }

      # Simulee: test complete, estimate theta

      if (identical(config@final_theta, config@interim_theta)) {

        # Skip final theta estimation if methods are identical

        o@final_theta_est <- o@interim_theta_est[position]
        o@final_se_est    <- o@interim_se_est[position]

      } else if (toupper(config@final_theta$method == "EAP")) {

        final_prior <- generateDistributionFromPriorPar(
          toupper(config@final_theta$prior_dist),
          config@final_theta$prior_par,
          constants$theta_q,
          1
        )[1, ]

        o@posterior       <- o@likelihood * final_prior
        final_EAP <- estimateThetaEAP(o@posterior, constants$theta_q)
        final_EAP <- applyShrinkageCorrection(final_EAP, config@final_theta)
        o@final_theta_est <- final_EAP$theta
        o@final_se_est    <- final_EAP$se

      } else if (toupper(config@final_theta$method) == "MLE") {

        final_MLE <- mle(
          pool,
          select      = o@administered_item_index[1:constants$max_ni],
          resp        = o@administered_item_resp[1:constants$max_ni],
          start_theta = o@interim_theta_est[constants$max_ni],
          max_iter    = config@final_theta$max_iter,
          crit        = config@final_theta$crit,
          theta_range = config@final_theta$bound_ML,
          truncate    = config@final_theta$truncate_ML,
          max_change  = config@final_theta$max_change,
          do_Fisher   = config@final_theta$do_Fisher
        )

        o@final_theta_est <- final_MLE$th
        o@final_se_est    <- final_MLE$se

      } else if (toupper(config@final_theta$method) == "MLEF") {

        final_MLEF <- mlef(
          pool,
          select           = o@administered_item_index[1:constants$max_ni],
          resp             = o@administered_item_resp[1:constants$max_ni],
          fence_slope      = config@final_theta$fence_slope,
          fence_difficulty = config@final_theta$fence_difficulty,
          start_theta      = o@interim_theta_est[constants$max_ni],
          max_iter         = config@final_theta$max_iter,
          crit             = config@final_theta$crit,
          theta_range      = config@final_theta$bound_ML,
          truncate         = config@final_theta$truncate_ML,
          max_change       = config@final_theta$max_change,
          do_Fisher        = config@final_theta$do_Fisher
        )

        o@final_theta_est <- final_MLEF$th
        o@final_se_est    <- final_MLEF$se

      } else if (toupper(config@final_theta$method) == "EB") {

        final_prior <- getInitialThetaPrior(
          config@final_theta, prior_par, constants$nj, j,
          posterior_constants)

        final_EB <- theta_EB(
          posterior_constants$n_sample, final_prior$theta, final_prior$se,
          pool@ipar[o@administered_item_index[1:position], ],
          o@administered_item_resp[1:position], pool@NCAT[o@administered_item_index[1:position]],
          model[o@administered_item_index[1:position]], 1, c(final_prior$theta, final_prior$se)
        )

        final_EB           <- applyThin(final_EB, posterior_constants)

        o@prior_par        <- final_prior$prior_par
        o@posterior_sample <- final_EB
        o@final_theta_est  <- mean(final_EB)
        o@final_se_est     <- sd(final_EB)

      } else if (toupper(config@final_theta$method) == "FB") {

        final_prior <- getInitialThetaPrior(
          config@final_theta, prior_par, constants$nj, j,
          posterior_constants)

        final_FB <- theta_FB(
          posterior_constants$n_sample, final_prior$theta, final_prior$se,
          posterior_record$ipar_list[o@administered_item_index[1:position]],
          pool@ipar[o@administered_item_index[1:position], ],
          o@administered_item_resp[1:position], pool@NCAT[o@administered_item_index[1:position]],
          model[o@administered_item_index[1:position]], 1, c(final_prior$theta, final_prior$se)
        )

        final_FB           <- applyThin(final_FB, posterior_constants)

        o@prior_par        <- final_prior$prior_par
        o@posterior_sample <- final_FB
        o@final_theta_est  <- mean(final_FB)
        o@final_se_est     <- sd(final_FB)

      }

      # Simulee: record item usage

      usage_matrix[j, o@administered_item_index] <- TRUE
      if (constants$set_based) {
        usage_matrix[j, constants$ni + o@administered_stimulus_index] <- TRUE
      }

      o_list[[j]] <- o

      # Simulee: do exposure control

      if (exposure_constants$use_eligibility_control) {

        segment_of                 <- getSegmentOf(o, exposure_constants)
        segment_record             <- updateSegmentRecord(segment_record, segment_of, j)
        ineligible_flag_in_segment <- getIneligibleFlagInSegment(ineligible_flag, segment_of$final_theta_est, constants)
        eligible_flag_in_segment   <- getEligibleFlagInSegment(ineligible_flag, segment_of$final_theta_est, constants)

        o_list[[j]]@true_theta_segment <- segment_of$true_theta

        if (exposure_control %in% c("ELIGIBILITY")) {

          segments_to_apply <- getSegmentsToApply(exposure_constants$n_segment, segment_of$final_theta_est)
          exposure_record   <- applyFading(exposure_record, segments_to_apply, exposure_constants, constants)
          segment_prob      <- 1
          segment_feasible  <- unique(o@theta_segment_index[o@shadow_test_feasible == TRUE])
          theta_is_feasible <- segment_of$final_theta_est %in% segment_feasible
          eligible_flag     <- getEligibleFlag(ineligible_flag, constants, !theta_is_feasible)
          exposure_record   <- applyIncrement(exposure_record, segments_to_apply, segment_prob, theta_is_feasible, eligible_flag, o, exposure_constants, constants)
          exposure_record   <- applyIncrementVisitedSegments(exposure_record, segment_prob, segment_of$visited, ineligible_flag_in_segment, o, exposure_constants, constants)
          exposure_record   <- applyAcceleration(exposure_record, exposure_constants, constants)
          exposure_record   <- applyClip(exposure_record, constants)

        } else if (exposure_control %in% c("BIGM")) {

          segments_to_apply <- getSegmentsToApply(exposure_constants$n_segment, segment_of$final_theta_est)
          exposure_record   <- applyFading(exposure_record, segments_to_apply, exposure_constants, constants)
          segment_prob      <- 1
          eligible_flag     <- getEligibleFlag(ineligible_flag, constants, FALSE)
          exposure_record   <- applyIncrement(exposure_record, segments_to_apply, segment_prob, FALSE, eligible_flag, o, exposure_constants, constants)
          exposure_record   <- applyIncrementVisitedSegments(exposure_record, segment_prob, segment_of$visited, ineligible_flag_in_segment, o, exposure_constants, constants)
          exposure_record   <- applyAcceleration(exposure_record, exposure_constants, constants)
          exposure_record   <- applyClip(exposure_record, constants)

        } else if (exposure_control %in% c("BIGM-BAYESIAN")) {

          segments_to_apply <- getSegmentsToApply(exposure_constants$n_segment, 1:exposure_constants$n_segment)
          exposure_record   <- applyFading(exposure_record, segments_to_apply, exposure_constants, constants)
          segment_prob      <- getSegmentProb(current_theta$posterior_sample, exposure_constants)
          eligible_flag     <- getEligibleFlag(ineligible_flag, constants, FALSE)
          exposure_record   <- applyIncrement(exposure_record, segments_to_apply, segment_prob, FALSE, eligible_flag, o, exposure_constants, constants)
          exposure_record   <- applyIncrementVisitedSegments(exposure_record, segment_prob, segment_of$visited, ineligible_flag_in_segment, o, exposure_constants, constants)
          exposure_record   <- applyAcceleration(exposure_record, exposure_constants, constants)
          exposure_record   <- applyClip(exposure_record, constants)

        }

        if (config@exposure_control$diagnostic_stats) {

          exposure_record_detailed <- updateExposureRecordSegmentwise(
            exposure_record_detailed, j, exposure_record, exposure_constants, constants
          )

        }

      }

      if (config@audit_trail) {
        plotAuditTrail()
      }

      if (!is.null(session)) {
        shinyWidgets::updateProgressBar(session = session, id = "pb", value = j, total = constants$nj)
      } else {
        if (has_progress_pkg) {
          pb$tick()
        } else {
          setTxtProgressBar(pb, j)
        }
      }

      # Simulee: go to next simulee

    }

    if (!has_progress_pkg) {
      close(pb)
    }

    final_theta_est <- unlist(lapply(1:constants$nj, function(j) o_list[[j]]@final_theta_est))
    final_se_est    <- unlist(lapply(1:constants$nj, function(j) o_list[[j]]@final_se_est))

    # Aggregate exposure rates

    if (!constants$set_based) {
      exposure_rate <- matrix(NA, constants$ni, 2)
      colnames(exposure_rate) <- c('Item', 'Item ER')
      exposure_rate[, 1] <- 1:constants$ni
      exposure_rate[, 2] <- colSums(usage_matrix) / constants$nj
    } else {
      exposure_rate <- matrix(NA, constants$ni, 4)
      colnames(exposure_rate) <- c('Item', 'Stimulus', 'Item ER', 'Stimulus ER')
      exposure_rate_raw <- colSums(usage_matrix) / constants$nj
      exposure_rate[, 1] <- 1:constants$ni
      exposure_rate[, 2] <- constraints@stimulus_index_by_item
      exposure_rate[, 3] <- exposure_rate_raw[1:constants$ni]
      exposure_rate[, 4] <- exposure_rate_raw[(constants$ni + 1):constants$nv][constraints@stimulus_index_by_item]
    }

    eligibility_stats           <- NULL
    check_eligibility_stats     <- NULL
    no_fading_eligibility_stats <- NULL

    # Get exposure control diagnostic stats

    if (exposure_constants$use_eligibility_control) {

      if (config@exposure_control$diagnostic_stats) {

        check_eligibility_stats <- list()

        for (j in 1:constants$nj) {
          tmp <- list()
          tmp$true_theta         <- true_theta[j]
          tmp$true_segment       <- find_segment(true_theta[j], exposure_constants$segment_cut)
          tmp$true_segment_count <- segment_record$count_true[j]
          check_eligibility_stats[[j]] <- tmp

          check_eligibility_stats[[j]]$a_g_i <- exposure_record_detailed$a_g_i
          check_eligibility_stats[[j]]$e_g_i <- exposure_record_detailed$e_g_i

          if (constants$set_based) {
            check_eligibility_stats[[j]]$a_g_s <- exposure_record_detailed$a_g_s
            check_eligibility_stats[[j]]$e_g_s <- exposure_record_detailed$e_g_s
          }

        }

        if (exposure_constants$fading_factor != 1) {

          no_fading_eligibility_stats <- list()

          for (j in 1:constants$nj) {
            tmp <- list()
            tmp$true_theta         <- true_theta[j]
            tmp$true_segment       <- find_segment(true_theta[j], exposure_constants$segment_cut)
            tmp$true_segment_count <- segment_record$count_true[j]
            no_fading_eligibility_stats[[j]] <- tmp

            no_fading_eligibility_stats[[j]]$a_g_i_nofade <- exposure_record_detailed$a_g_i_nofade
            no_fading_eligibility_stats[[j]]$e_g_i_nofade <- exposure_record_detailed$e_g_i_nofade

            if (constants$set_based) {
              no_fading_eligibility_stats[[j]]$a_g_s_nofade <- exposure_record_detailed$a_g_s_nofade
              no_fading_eligibility_stats[[j]]$e_g_s_nofade <- exposure_record_detailed$e_g_s_nofade
            }

          }

        }

      }
    }

    if (constants$use_shadow) {
      freq_infeasible <- table(unlist(lapply(1:constants$nj, function(j) sum(!o_list[[j]]@shadow_test_feasible))))
    } else {
      freq_infeasible <- NULL
    }

    out                             <- new("output_Shadow_all")
    out@output                      <- o_list
    out@pool                        <- pool
    out@config                      <- config
    out@true_theta                  <- true_theta
    out@constraints                 <- constraints
    out@prior                       <- prior
    out@prior_par                   <- prior_par
    out@data                        <- all_data$test@data
    out@final_theta_est             <- final_theta_est
    out@final_se_est                <- final_se_est
    out@exposure_rate               <- exposure_rate
    out@usage_matrix                <- usage_matrix
    out@true_segment_count          <- segment_record$count_true
    out@est_segment_count           <- segment_record$count_est
    out@eligibility_stats           <- exposure_record
    out@check_eligibility_stats     <- check_eligibility_stats
    out@no_fading_eligibility_stats <- no_fading_eligibility_stats
    out@freq_infeasible             <- freq_infeasible

    return(out)
  }
)

#' Calculate Root Mean Squared Error
#'
#' Calculate Root Mean Squared Error.
#'
#' @param x A vector of values.
#' @param y A vector of values.
#' @param conditional If \code{TRUE}, calculate RMSE conditional on x.
RMSE <- function(x, y, conditional = TRUE) {
  if (length(x) != length(y)) {
    stop("length(x) and length(y) are not equal")
  }
  if (conditional) {
    MSE <- tapply((x - y)^2, x, mean)
  } else {
    MSE <- mean((x - y)^2)
  }
  return(sqrt(MSE))
}

#' Calculate Relative Errors
#'
#' Calculate Relative Errors.
#'
#' @param RMSE_foc A vector of RMSE values for the focal group.
#' @param RMSE_ref A vector of RMSE values for the reference group.
RE <- function(RMSE_foc, RMSE_ref) {
  if (length(RMSE_foc) != length(RMSE_ref)) {
    stop("length(x) and length(y) are not equal")
  }
  RE <- RMSE_ref^2 / RMSE_foc^2
  return(RE)
}

#' Check the consistency of constraints and item usage
#'
#' Check the consistency of constraints and item usage.
#'
#' @param constraints A \code{\linkS4class{constraints}} object generated by \code{\link{loadConstraints}}.
#' @param usage_matrix A matrix of item usage data from \code{\link{Shadow}}.
#' @param true_theta A vector of true theta values.
checkConstraints <- function(constraints, usage_matrix, true_theta = NULL) {


  raw_constraints <- constraints@constraints
  list_constraints <- constraints@list_constraints

  nc <- nrow(raw_constraints)
  nj <- nrow(usage_matrix)
  ni <- ncol(usage_matrix)

  MET <- matrix(FALSE, nrow = nj, ncol = nc)
  COUNT <- matrix(NA, nrow = nj, ncol = nc)
  if (ni != constraints@ni) {
    stop("unequal number of items in constraints and usage_matrix ")
  }
  byTheta <- FALSE
  MEAN <- rep(NA, nc)
  SD   <- rep(NA, nc)
  MIN  <- rep(NA, nc)
  MAX  <- rep(NA, nc)
  HIT  <- rep(NA, nc)
  if (!is.null(true_theta)) {
    if (length(true_theta) != nj) {
      stop("length of true_theta is not equal to nrow of usage_matrix")
    }
    byTheta <- TRUE
    groupMEAN <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupSD   <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupMIN  <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupMAX  <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
    groupHIT  <- matrix(NA, nrow = nc, ncol = length(unique(true_theta)))
  } else {
    groupMEAN <- NULL
    groupSD   <- NULL
    groupMIN  <- NULL
    groupMAX  <- NULL
    groupHIT  <- NULL
  }
  nEnemy <- sum(raw_constraints$TYPE == "ENEMY")
  if (nEnemy > 0) {
    enemyIndex <- which(raw_constraints$TYPE == "ENEMY")
    raw_constraints$LB[enemyIndex] <- 0
    raw_constraints$UB[enemyIndex] <- 1
  }
  numberIndex <- which(raw_constraints$TYPE == "NUMBER")
  for (index in 1:nc) {
    if (raw_constraints$WHAT[index] == "ITEM") {
      if (raw_constraints$TYPE[index] %in% c("NUMBER", "ENEMY")) {
        items <- which(list_constraints[[index]]@mat[1, ] == 1)
        COUNT[, index] <- rowSums(usage_matrix[, items])
        MET[, index] <- COUNT[, index] >= raw_constraints$LB[index] & COUNT[, index] <= raw_constraints$UB[index]
        if (byTheta) {
          groupMEAN[index, ] <- round(tapply(COUNT[, index], true_theta, mean), 3)
          groupSD[index, ] <- round(tapply(COUNT[, index], true_theta, sd), 3)
          groupMIN[index, ] <- tapply(COUNT[, index], true_theta, min)
          groupMAX[index, ] <- tapply(COUNT[, index], true_theta, max)
          groupHIT[index, ] <- round(tapply(MET[, index], true_theta, mean), 3)
        }
        MEAN[index] <- round(mean(COUNT[, index]), 2)
        SD[index] <- round(sd(COUNT[, index]), 2)
        MIN[index] <- min(COUNT[, index])
        MAX[index] <- max(COUNT[, index])
        HIT[index] <- round(mean(MET[, index]), 3)
      }
    }
  }
  LD <- NULL
  if (nEnemy > 0) {
    LD <- rowSums(COUNT[, enemyIndex] > 1)
  }
  Check <- data.frame(raw_constraints, MEAN = MEAN, SD = SD, MIN = MIN, MAX = MAX, HIT = HIT)
  return(list(
    Check = Check[raw_constraints[["TYPE"]] == "NUMBER", ],
    LD = LD,
    groupMEAN = groupMEAN[numberIndex, ], groupSD = groupSD[numberIndex, ],
    groupMIN = groupMIN[numberIndex, ], groupMAX = groupMAX[numberIndex, ],
    groupHIT = groupHIT[numberIndex, ]))
}

#' @noRd
plotER <- function(
  item_exposure_rate, item_exposure_rate_final = NULL,
  stim_exposure_rate = NULL, stim_index = NULL,
  max_rate = max_rate, title = NULL, color = "blue", color_final = "yellow", color_stim = "red", color_threshold = "dark gray", simple = FALSE) {

  if (!is.null(stim_index)) {
    idx_sort <- order(stim_exposure_rate, stim_index, item_exposure_rate, decreasing = TRUE)
    item_exposure_rate_ordered <- item_exposure_rate[idx_sort]
    stim_exposure_rate_ordered <- stim_exposure_rate[idx_sort]
    stim_index_ordered         <- stim_index[idx_sort]
  } else {
    idx_sort <- order(item_exposure_rate, decreasing = TRUE)
    item_exposure_rate_ordered <- item_exposure_rate[idx_sort]
  }

  ni <- length(item_exposure_rate)

  if (!simple) {
    xlab = "Item"
    ylab = "Exposure Rate"
  } else {
    xlab = ""
    ylab = ""
  }

  plot(1:ni, item_exposure_rate_ordered, type = "n", lwd = 2, ylim = c(0, 1), xlab = "Item", ylab = "Exposure Rate", main = title)
  points(1:ni, item_exposure_rate_ordered, type = "h", lwd = 1, col = color)
  if (!is.null(stim_exposure_rate)) {
    lines(1:ni, stim_exposure_rate_ordered, col = color_stim, type = "s")
    for (stim_id in unique(stim_index_ordered)) {
      x <- mean((1:ni)[which(stim_index_ordered == stim_id)])
      y <- stim_exposure_rate_ordered[which(stim_index_ordered == stim_id)][1]
      points(x, y, col = color_stim, pch = 21, bg = 'white', cex = .75)
    }
  }
  abline(h = max_rate, col = color_threshold, lty = 2)

  if (!is.null(item_exposure_rate_final)) {
    item_exposure_rate_final_ordered <- item_exposure_rate_final[idx_sort]
    points(1:ni, item_exposure_rate_final_ordered, type = "h", lwd = 1, lty = 1, col = color_final)
  }
}

#' Calculate hyperparameters for log-normal distribution
#'
#' Calculate hyperparameters for log-normal distribution.
#'
#' @param mean Mean of the distribution.
#' @param sd Standard deviation of the distribution.
#'
#' @examples
#' lnHyperPars(.5, 1)
#'
#' @export
lnHyperPars <- function(mean, sd) {
  location <- log(mean^2 / sqrt(sd^2 + mean^2))
  scale    <- sqrt(log(1 + sd^2 / mean^2))
  return(c(location, scale))
}

#' Calculate hyperparameters for logit-normal distribution
#'
#' Calculate hyperparameters for logit-normal distribution.
#'
#' @param mean Mean of the distribution.
#' @param sd Standard deviation of the distribution.
#'
#' @examples
#' logitHyperPars(.5, 1)
#'
#' @export
logitHyperPars <- function(mean, sd) {

  n_max <- 10000
  n     <- 0
  logit_samples <- numeric(n_max)

  while (n_max - n > 0) {
    norm_sample <- rnorm(n_max - n, mean, sd)
    idx <- (norm_sample >= 0) & (norm_sample <= 1)
    norm_sample <- norm_sample[idx]
    n_new <- n + length(norm_sample)
    if (length(norm_sample) > 0) {
      logit_samples[(n + 1):n_new] <- logitnorm::logit(norm_sample)
    }
    n <- n_new
  }

  return(c(mean(logit_samples), sd(logit_samples)))
}

#' Sample item parameter estimates from their posterior distributions
#'
#' Sample item parameter estimates from their posterior distributions.
#'
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param n_sample An integer as the number of sampled parameters.
#'
#' @examples
#' ipar <- iparPosteriorSample(itempool_science, 5)
#'
#' @export
iparPosteriorSample <- function(pool, n_sample = 500) {

  requireNamespace("logitnorm")
  ipar_list <- vector(mode = "list", length = pool@ni)

  for (i in 1:pool@ni) {

    if (pool@model[i] == "item_1PL") {
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 1)
      ipar_list[[i]][, 1] <- rnorm(n_sample, pool@ipar[i, 1], pool@se[i, 1])

    } else if (pool@model[i] == "item_2PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 2)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])

    } else if (pool@model[i] == "item_3PL") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      c_hyp <- logitHyperPars(pool@ipar[i, 3], pool@se[i, 3])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = 3)
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      ipar_list[[i]][, 2] <- rnorm(n_sample, pool@ipar[i, 2], pool@se[i, 2])
      ipar_list[[i]][, 3] <- rlogitnorm(n_sample, mu = c_hyp[1], sigma = c_hyp[2])

    } else if (pool@model[i] == "item_PC") {
      ipar_list[[i]] <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i] - 1)
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k] <- rnorm(n_sample, pool@ipar[i, k], pool@se[i, k])
      }

    } else if (pool@model[i] == "item_GPC") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i])
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k + 1] <- rnorm(n_sample, pool@ipar[i, k + 1], pool@se[i, k + 1])
      }

    } else if (pool@model[i] == "item_GR") {
      a_hyp <- lnHyperPars(pool@ipar[i, 1], pool@se[i, 1])
      ipar_list[[i]]      <- matrix(NA, nrow = n_sample, ncol = pool@NCAT[i])
      ipar_list[[i]][, 1] <- rlnorm(n_sample, a_hyp[1], a_hyp[2])
      for (k in 1:(pool@NCAT[i] - 1)) {
        ipar_list[[i]][, k + 1] <- rnorm(n_sample, pool@ipar[i, k + 1], pool@se[i, k + 1])
      }
      for (s in 1:n_sample) {
        if (is.unsorted(ipar_list[[i]][s, 2:pool@NCAT[i]])) {
          ipar_list[[i]][s, 2:pool@NCAT[i]] <- sort(ipar_list[[i]][s, 2:pool@NCAT[i]])
        }
      }

    }
  }
  return(ipar_list)
}

#' Save or print audit trails
#'
#' Save or print audit trails for all simulees.
#'
#' @param object_list A list of output objects generated from \code{STA}.
#' @param file An optional file name as a character string to save the output.
#'
#' @return None
saveOutput <- function(object_list, file = NULL) {
  nj <- length(object_list)
  for (j in 1:nj) {
    object <- object_list[[j]]
    output <- data.frame(
      simulee = object@simulee_id,
      true_theta = object@true_theta,
      true_theta_segment = object@true_theta_segment,
      stage = 1:length(object@administered_item_index),
      stimulus_index = ifelse(is.nan(object@administered_stimulus_index), rep(NA, length(object@administered_item_index)), object@administered_stimulus_index),
      item_index = object@administered_item_index,
      item_resp = object@administered_item_resp,
      interim_theta = object@interim_theta_est,
      interim_se = object@interim_se_est,
      interim_theta_segment = object@theta_segment_index
    )
    if (!is.null(file)) {
      write.table(output, file = file, append = j > 1, row.names = FALSE, col.names = j == 1, sep = ",")
    } else {
      print(output)
    }
  }
}

#' (deprecated) Plot a shadow test chart
#'
#' (deprecated) Use \code{\link[TestDesign:plot-methods]{plot}} with \code{type = 'shadow'} instead.
#'
#' @param object An output from \code{\link{Shadow}} function.
#' @param examinee_id Numeric ID of the examinee to draw the plot.
#' @param sort_by_difficulty Sort the items by difficulty. (not implemented)
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param simple If \code{TRUE}, simplity the chart by hiding unused items.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' \dontrun{
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(config, constraints_science, true_theta)
#' plotShadow(solution, 1)
#' plotShadow(solution, 1, simple = TRUE)
#' }
#' @docType methods
#' @rdname plotShadow-methods
#' @export
setGeneric(
  name = "plotShadow",
  def = function(object, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, simple = FALSE, ...) {
    standardGeneric("plotShadow")
  }
)

#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(
  f = "plotShadow",
  signature = "output_Shadow_all",
  definition = function(object, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, simple = FALSE, ...) {
    .Deprecated("plot", msg = "plotShadow() is deprecated. Use plot(type = 'shadow') instead.")
    p <- plot(
      object,
      type = "shadow",
      examinee_id = examinee_id,
      sort_by_difficulty = sort_by_difficulty,
      file_pdf = file_pdf,
      simple = simple,
      ...
    )
  }
)

#' @docType methods
#' @rdname plotShadow-methods
#' @export
setMethod(
  f = "plotShadow",
  signature = "list",
  definition = function(object, examinee_id = 1, sort_by_difficulty = FALSE, file_pdf = NULL, simple = FALSE, ...) {
    .Deprecated("plot", msg = "plotShadow() is deprecated. Use plot(type = 'shadow') instead.")
    message("Consider converting the object to 'output_Shadow_all' class.\n")
    new_object <- new("output_Shadow_all")
    for (n in names(object)) {
      slot(new_object, n) <- object[[n]]
    }
    p <- plot(
      new_object,
      type = "shadow",
      examinee_id = examinee_id,
      sort_by_difficulty = sort_by_difficulty,
      file_pdf = file_pdf,
      simple = simple,
      ...
    )
  }
)

#' (deprecated) Plot audit trail
#'
#' (deprecated) Use \code{\link[TestDesign:plot-methods]{plot}} with \code{type = 'audit'} instead.
#'
#' @param object An output object generated by \code{\link{Shadow}}.
#' @param examinee_id Numeric ID of the examinee to draw the plot.
#' @param min_theta A lower bound of theta.
#' @param max_theta An upper bound of theta.
#' @param min_score A minimum item score.
#' @param max_score A maximum item score.
#' @param z_ci A quantile of the normal distribution for confidence intervals.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' \dontrun{
#' config <- createShadowTestConfig()
#' true_theta <- rnorm(1)
#' solution <- Shadow(config, constraints_science, true_theta)
#' plotCAT(solution, 1)
#' }
#' @docType methods
#' @rdname plotCAT-methods
#' @export
setGeneric(
  name = "plotCAT",
  def = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    standardGeneric("plotCAT")
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "output_Shadow_all",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotCAT() is deprecated. Use plot(type = 'audit') instead.")
    p <- plot(object,
      type = 'audit',
      examinee_id = examinee_id,
      theta_range = c(min_theta, max_theta),
      z_ci = z_ci,
      ...
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "list",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotCAT() is deprecated. Use plot(type = 'audit') instead.")
    message("Consider converting the object to 'output_Shadow_all' class.\n")
    new_object <- new("output_Shadow_all")
    for (n in names(object)) {
      slot(new_object, n) <- object[[n]]
    }
    p <- plot(new_object,
      type = 'audit',
      examinee_id = examinee_id,
      theta_range = c(min_theta, max_theta),
      z_ci = z_ci,
      ...
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotCAT-methods
#' @export
setMethod(
  f = "plotCAT",
  signature = "output_Shadow",
  definition = function(object, examinee_id = 1, min_theta = -5, max_theta = 5, min_score = 0, max_score = 1, z_ci = 1.96, file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotCAT() function is deprecated. Use plot(type = 'audit') instead.")
    plot(object,
      type = 'audit',
      examinee_id = examinee_id,
      theta_range = c(min_theta, max_theta),
      z_ci = z_ci,
      ...
    )
  }
)

#' (deprecated) Plot item exposure rates
#'
#' (deprecated) Use \code{\link[TestDesign:plot-methods]{plot}} with \code{type = 'exposure'} instead.
#'
#' @param object an output object generated by \code{\link{Shadow}}.
#' @param max_rate the target exposure rate.
#' @param theta_segment the type of theta to use to create segments. Accepts \code{"estimated"} or \code{"true"}. (default = \code{"estimated"})
#' @param color Color of item-wise exposure rates.
#' @param color_final Color of item-wise exposure rates, only counting the items while in the final theta segment as exposed.
#' @param file_pdf If supplied a filename, save as a PDF file.
#' @param ... Additional options to be passed on to \code{pdf()}.
#'
#' @examples
#' \dontrun{
#' true_theta <- runif(10, min = -3.5, max = 3.5)
#' resp_science <- simResp(itempool_science, true_theta)
#' constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))
#' config_science <- createShadowTestConfig(
#'   MIP = list(solver = "lpSolve"),
#'   exposure_control = list(method = "ELIGIBILITY")
#' )
#' solution <- Shadow(config_science, constraints_science2, true_theta, data = resp_science)
#' p <- plotExposure(solution)
#' }
#' @docType methods
#' @rdname plotExposure-methods
#' @export
setGeneric(
  name = "plotExposure",
  def = function(object, max_rate = 0.25, theta_segment = "Estimated", color = "blue", color_final = "blue", file_pdf = NULL, ...) {
    standardGeneric("plotExposure")
  }
)

#' @docType methods
#' @rdname plotExposure-methods
#' @export
setMethod(
  f = "plotExposure",
  signature = "list",
  definition = function(object, max_rate = 0.25, theta_segment = "estimated", color = "blue", color_final = "blue", file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotExposure() is deprecated. Use plot(type = 'exposure') instead.")
    message("Consider converting the object to 'output_Shadow_all' class.\n")
    new_object <- new("output_Shadow_all")
    for (n in names(object)) {
      slot(new_object, n) <- object[[n]]
    }
    p <- plot(new_object,
      type = "exposure",
      theta_segment = theta_segment,
      color = color,
      color_final = color_final,
      ...
    )
    return(p)
  }
)

#' @docType methods
#' @rdname plotExposure-methods
#' @export
setMethod(
  f = "plotExposure",
  signature = "output_Shadow_all",
  definition = function(object, max_rate = 0.25, theta_segment = "estimated", color = "blue", color_final = "blue", file_pdf = NULL, ...) {
    .Deprecated("plot", msg = "plotExposure() is deprecated. Use plot(type = 'exposure') instead.")
    p <- plot(object,
      type = "exposure",
      theta_segment = theta_segment,
      color = color,
      color_final = color_final,
      ...
    )
    return(p)
  }
)
