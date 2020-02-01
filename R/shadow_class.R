#' @include static_class.R
NULL


#' An S4 class to represent a test
#'
#' An S4 class to represent a test.
#'
#' @slot pool An \code{\linkS4class{item_pool}} object.
#' @slot theta A theta grid.
#' @slot prob A list of item response probabilities.
#' @slot info A matrix of item information values.
#' @slot true_theta An optional vector of true theta values.
#' @slot data An optional matrix of item responses.
#'
#' @export
setClass("test",
  slots = c(
    pool = "item_pool",
    theta = "numeric",
    prob = "list",
    info = "matrix",
    true_theta = "numeric_or_null",
    data = "matrix_or_null"
  ),
  prototype = list(
    pool = new("item_pool"),
    theta = numeric(0),
    prob = list(0),
    info = matrix(0),
    true_theta = numeric(0),
    data = matrix(NA, 0, 0)
  ),
  validity = function(object) {
    errors <- NULL
    if (length(object@prob) != object@pool@ni) {
      errors <- c(errors, "length(@prob) must match @pool@ni.")
    }
    if (ncol(object@info) != object@pool@ni) {
      errors <- c(errors, "ncol(@info) must match @pool@ni.")
    }
    if (nrow(object@info) != length(object@theta)) {
      errors <- c(errors, "nrow(@info) must match length(@theta).")
    }
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)


#' An S4 class to represent a test cluster
#'
#' An S4 class to represent a test cluster from a list of \code{\linkS4class{test}} objects.
#'
#' @slot nt Numeric. A scalar to indicate the number of \code{\linkS4class{test}} objects to be clustered.
#' @slot tests A list \code{\linkS4class{test}} objects.
#' @slot names Character. A vector of names corresponding to the \code{\linkS4class{test}} objects.
#'
#' @export

setClass("test_cluster",
  slots = c(
    nt = "numeric",
    tests = "list",
    names = "character"
  ),
  prototype = list(
    nt = numeric(0),
    tests = list(0),
    names = character(0)
  ),
  validity = function(object) {
    errors <- NULL
    if (length(object@tests) != object@nt) {
      errors <- c(errors, "@nt must match length(@tests).")
    }
    if (length(object@names) != object@nt) {
      errors <- c(errors, "@nt must match length(@names).")
    }
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' createShadowTestConfig
#'
#' @rdname createShadowTestConfig
setClass("config_Shadow",
  slots = c(
    item_selection = "list",
    content_balancing = "list",
    MIP = "list",
    MCMC = "list",
    refresh_policy = "list",
    exposure_control = "list",
    stopping_criterion = "list",
    interim_theta = "list",
    final_theta = "list",
    theta_grid = "numeric",
    audit_trail = "logical"
  ),
  prototype = list(
    item_selection = list(
      method = "MFI",
      info_type = "FISHER",
      initial_theta = NULL,
      fixed_theta = NULL
    ),
    content_balancing = list(method = "STA"),
    MIP = list(
      solver = "LPSOLVE",
      verbosity = -2,
      time_limit = 60,
      gap_limit = .05,
      gap_limit_abs = .05
    ),
    MCMC = list(
      burn_in = 100,
      post_burn_in = 500,
      thin = 1,
      jump_factor = 1
    ),
    refresh_policy = list(
      method = "ALWAYS",
      interval = 1,
      threshold = 0.1,
      position = 1
    ),
    exposure_control = list(
      method = "ELIGIBILITY",
      M = NULL,
      max_exposure_rate = 0.25,
      acceleration_factor = 1.0,
      n_segment = 7,
      first_segment = NULL,
      segment_cut = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf),
      initial_eligibility_stats = NULL,
      fading_factor = 0.999,
      diagnostic_stats = FALSE
    ),
    stopping_criterion = list(
      method = "FIXED",
      test_length = NULL,
      min_ni = NULL,
      max_ni = NULL,
      se_threshold = NULL
    ),
    interim_theta = list(
      method = "EAP",
      shrinkage_correction = FALSE,
      prior_dist = "NORMAL",
      prior_par = c(0, 1),
      bound_ML = c(-4, 4),
      truncate_ML = FALSE,
      max_iter = 50,
      crit = 0.001,
      max_change = 1.0,
      do_fisher = TRUE
    ),
    final_theta = list(
      method = "EAP",
      shrinkage_correction = FALSE,
      prior_dist = "NORMAL",
      prior_par = c(0, 1),
      bound_ML = c(-4, 4),
      truncate_ML = FALSE,
      max_iter = 50,
      crit = 0.001,
      max_change = 1.0,
      do_fisher = TRUE
    ),
    theta_grid = seq(-4, 4, .1),
    audit_trail = FALSE
  ),
  validity = function(object) {
    errors <- NULL
    if (!toupper(object@MIP$solver) %in% c("LPSYMPHONY", "RSYMPHONY", "LPSOLVE", "GUROBI", "RGLPK")) {
      msg <- sprintf("Unrecognized option in @MIP$solver : %s", object@MIP$solver)
      errors <- c(errors, msg)
    }

    for (solver_name in c("gurobi", "Rsymphony", "lpsymphony", "Rglpk")) {
      if (toupper(object@MIP$solver) == toupper(solver_name)) {
        if (!requireNamespace(solver_name, quietly = TRUE)) {
          msg <- sprintf("could not find the specified solver package : %s", solver_name)
          errors <- c(errors, msg)
        }
      }
    }

    if (!toupper(object@item_selection$method) %in% c("MFI", "MPWI", "EB", "FB")) {
      msg <- sprintf("Unrecognized option in @item_selection$method : %s", object@item_selection$method)
      errors <- c(errors, msg)
    }

    if (!object@content_balancing$method %in% c("NONE", "STA")) {
      msg <- sprintf("Unrecognized option in @content_balancing$method : %s", object@content_balancing$method)
      errors <- c(errors, msg)
    }
    if (!object@refresh_policy$method %in%
      c("ALWAYS", "POSITION", "INTERVAL", "THRESHOLD", "INTERVAL-THRESHOLD", "STIMULUS", "SET", "PASSAGE")) {
      msg <- sprintf("Unrecognized option in @refresh_policy$method : %s", object@refresh_policy$method)
      errors <- c(errors, msg)
    }
    if (!object@exposure_control$method %in% c("NONE", "ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")) {
      msg <- sprintf("Unrecognized option in @exposure_control$method : %s", object@exposure_control$method)
      errors <- c(errors, msg)
    }
    if (object@exposure_control$n_segment != length(object@exposure_control$segment_cut) - 1) {
      msg <- "@exposure_control$n_segment must match @exposure_control$segment_cut."
      errors <- c(errors, msg)
    }
    if (!object@stopping_criterion$method %in% c("FIXED")) {
      msg <- sprintf("Unrecognized option in @stopping_criterion$method : %s", object@stopping_criterion$method)
      errors <- c(errors, msg)
    }
    if (!object@interim_theta$method %in% c("EAP", "MLE", "EB", "FB")) {
      msg <- sprintf("Unrecognized option in @interim_theta$method : %s (must be one of EAP, MLE, EB, or FB)", object@interim_theta$method)
      errors <- c(errors, msg)
    }
    if (!object@final_theta$method %in% c("EAP", "MLE", "EB", "FB")) {
      msg <- sprintf("Unrecognized option in @final_theta$method : %s (must be one of EAP, MLE, EB, or FB)", object@final_theta$method)
      errors <- c(errors, msg)
    }
    if (toupper(object@final_theta$method) == "EAP") {
      if (!toupper(object@final_theta$prior_dist) %in% c("NORMAL", "UNIFORM")) {
        msg <- sprintf("Unrecognized option in @final_theta$prior_dist : %s (must be one of NORMAL or UNIFORM when @final_theta$method is EAP)", object@final_theta$prior_dist)
        errors <- c(errors, msg)
      }
    }

    if ((object@exposure_control$method == c("BIGM-BAYESIAN")) &&
      (!object@interim_theta$method %in% c("EB", "FB"))) {
      errors <- c(errors, "exposure_control$method == 'BIGM-BAYESIAN' requires interim_theta$method to be EB or FB.")
    }
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      errors = paste0(c("", errors), collapse = '\n')
      return(errors)
    }
  }
)

#' Create a config_Shadow object
#'
#' Create a \code{\linkS4class{config_Shadow}} object for Shadow Test Assembly (STA).
#'
#' @param item_selection A list containing item selection criteria.
#' \itemize{
#'   \item{\code{method}} The type of criteria. Accepts one of \code{MFI, MPWI, FB, EB}.
#'   \item{\code{info_type}} The type of information. Accepts \code{FISHER}.
#'   \item{\code{initial_theta}} Initial theta value(s) for the first item selection.
#'   \item{\code{fixed_theta}} Fixed theta value(s) to optimize for all items to select.
#' }
#' @param content_balancing A list containing content balancing options.
#' \itemize{
#'   \item{\code{method}} The type of balancing method. Accepts one of \code{NONE, STA}.
#' }
#' @param MIP A list containing solver options.
#' \itemize{
#'   \item{\code{solver}} The type of solver. Accepts one of \code{lpsymphony, Rsymphony, gurobi, lpSolve, Rglpk}.
#'   \item{\code{verbosity}} Verbosity level.
#'   \item{\code{time_limit}} Time limit to be passed onto solver. Used in solvers \code{lpsymphony, Rsymphony, gurobi, Rglpk}.
#'   \item{\code{gap_limit}} Gap limit (relative) to be passed onto solver. Used in solver \code{gurobi}. Uses the solver default when \code{NULL}.
#'   \item{\code{gap_limit_abs}} Gap limit (absolute) to be passed onto solver. Used in solver \code{lpsymphony, Rsymphony}. Uses the solver default when \code{NULL}.
#' }
#' @param MCMC A list containing Markov-chain Monte Carlo configurations.
#' \itemize{
#'   \item{\code{burn_in}} Numeric. The number of chains from the start to discard.
#'   \item{\code{post_burn_in}}  Numeric. The number of chains to use after discarding the first \code{burn_in} chains.
#'   \item{\code{thin}} Numeric. Thinning interval.
#'   \item{\code{jumpfactor}} Numeric. Jump factor.
#' }
#' @param refresh_policy A list containing refresh policy for obtaining a new shadow test.
#' \itemize{
#'   \item{\code{method}} The type of policy. Accepts one of \code{ALWAYS, POSITION, INTERVAL, THRESHOLD, INTERVAL-THRESHOLD, STIMULUS, SET, PASSAGE}.
#'   \item{\code{interval}} Integer. Set to 1 to refresh at each position, 2 to refresh at every two positions, and so on.
#'   \item{\code{threshold}} Numeric. The shadow test is refreshed when the absolute change in theta estimate is greater than this value.
#'   \item{\code{position}} Numeric. Position(s) at which refresh to occur.
#' }
#' @param exposure_control A list containing exposure control settings.
#' \itemize{
#'   \item{\code{method}} Accepts one of \code{"NONE", "ELIGIBILITY", "BIGM", "BIGM-BAYESIAN"}.
#'   \item{\code{M}} Big M constant.
#'   \item{\code{max_exposure_rate}} Maximum target exposure rate.
#'   \item{\code{acceleration_factor}} Acceleration factor.
#'   \item{\code{n_segment}} Number of theta segments.
#'   \item{\code{first_segment}} Theta segment assumed at the begining of test.
#'   \item{\code{segment_cut}} A numeric vector of segment cuts.
#'   \item{\code{initial_eligibility_stats}} A list of eligibility statistics from a previous run.
#'   \item{\code{fading_factor}} Fading factor.
#'   \item{\code{diagnostic_stats}} \code{TRUE} to generate diagnostic statistics.
#' }
#' @param stopping_criterion A list containing stopping criterion.
#' \itemize{
#'   \item{\code{method}} Accepts one of \code{"FIXED"}.
#'   \item{\code{test_length}} Test length.
#'   \item{\code{min_ni}} Maximum number of items to administer.
#'   \item{\code{max_ni}} Minumum number of items to administer.
#'   \item{\code{se_threshold}} Standard error threshold for stopping.
#' }
#' @param interim_theta A list containing interim theta estimation options.
#' \itemize{
#'   \item{\code{method}} The type of estimation. Accepts one of \code{EAP, EB, FB}.
#'   \item{\code{shrinkage_correction}} Set \code{TRUE} to correct for shrinkage in EAP
#'   \item{\code{prior_dist}} The type of prior distribution. Accepts one of \code{NORMAL, UNIF}.
#'   \item{\code{prior_par}} Distributional parameters for the prior.
#'   \item{\code{bound_ML}} Theta bound for MLE.
#'   \item{\code{truncate_ML}} Set \code{TRUE} to truncate MLE within \code{bound_ML}
#'   \item{\code{max_iter}} Maximum number of Newton-Raphson iterations.
#'   \item{\code{crit}} Convergence criterion.
#'   \item{\code{max_change}} Maximum change in ML estimates between iterations.
#'   \item{\code{do_fisher}} Set \code{TRUE} to use Fisher's method of scoring.
#' }
#' @param final_theta A list containing final theta estimation options.
#' \itemize{
#'   \item{\code{method}} The type of estimation. Accepts one of \code{EAP, EB, FB}.
#'   \item{\code{shrinkage_correction}} Set \code{TRUE} to correct for shrinkage in EAP.
#'   \item{\code{prior_dist}} The type of prior distribution. Accepts one of \code{NORMAL, UNIF}.
#'   \item{\code{prior_par}} Distributional parameters for the prior.
#'   \item{\code{bound_ML}} Theta bound for MLE.
#'   \item{\code{truncate_ML}} Set \code{TRUE} to truncate MLE within \code{bound_ML}
#'   \item{\code{max_iter}} Maximum number of Newton-Raphson iterations.
#'   \item{\code{crit}} Convergence criterion.
#'   \item{\code{max_change}} Maximum change in ML estimates between iterations.
#'   \item{\code{do_fisher}} Set \code{TRUE} to use Fisher's method of scoring.
#' }
#' @param theta_grid A numeric vector. Theta values to represent the continuum.
#' @param audit_trail Set \code{TRUE} to generate audit trails.
#'
#' @examples
#' cfg1 <- createShadowTestConfig(refresh_policy = list(
#'   method = "STIMULUS"
#' ))
#' cfg2 <- createShadowTestConfig(refresh_policy = list(
#'   method = "POSITION",
#'   position = c(1, 5, 9)
#' ))
#' @rdname createShadowTestConfig
#' @export
createShadowTestConfig <- function(item_selection = NULL, content_balancing = NULL, MIP = NULL, MCMC = NULL,
                          refresh_policy = NULL, exposure_control = NULL, stopping_criterion = NULL,
                          interim_theta = NULL, final_theta = NULL, theta_grid = seq(-4, 4, .1), audit_trail = F) {
  conf <- new("config_Shadow")

  arg_names <- c(
    "item_selection", "content_balancing", "MIP", "MCMC",
    "refresh_policy", "exposure_control", "stopping_criterion",
    "interim_theta", "final_theta"
  )
  obj_names <- c()
  for (arg in arg_names) {
    if (!is.null(eval(parse(text = arg)))) {
      eval(parse(text = paste0("obj_names <- names(conf@", arg, ")")))
      for (entry in obj_names) {
        entry_l <- paste0("conf@", arg, "$", entry)
        entry_r <- paste0(arg, "$", entry)
        tmp <- eval(parse(text = entry_r))
        if (!is.null(tmp)) {
          eval(parse(text = paste0(entry_l, " <- ", entry_r)))
        }
      }
    }
  }
  if (!is.null(theta_grid)) {
    conf@theta_grid <- theta_grid
  }
  if (!is.null(audit_trail)) {
    conf@audit_trail <- audit_trail
  }
  v <- validObject(conf)
  if (v) {
    return(conf)
  }
}

#' @name show-method
#' @aliases show,config_Shadow-method
#' @docType methods
#' @noRd
setMethod("show", "config_Shadow", function(object) {
  cat("Shadow Configuration Settings \n\n")
  cat("  item_selection \n")
  cat("    method          :", object@item_selection$method, "\n")
  cat("    info_type       :", object@item_selection$info_type, "\n")
  cat("    initial_theta   :", object@item_selection$initial_theta, "\n")
  cat("    fixed_theta     :", object@item_selection$fixed_theta, "\n\n")
  cat("  content_balancing \n")
  cat("    method          :", object@content_balancing$method, "\n\n")
  cat("  MIP \n")
  cat("    solver          :", object@MIP$solver, "\n")
  cat("    verbosity       :", object@MIP$verbosity, "\n")
  cat("    time_limit      :", object@MIP$time_limit, "\n")
  cat("    gap_limit       :", object@MIP$gap_limit, "\n")
  cat("    gap_limit_abs   :", object@MIP$gap_limit_abs, "\n\n")
  cat("  MCMC \n")
  cat("    burn_in         :", object@MCMC$burn_in, "\n")
  cat("    post_burn_in    :", object@MCMC$post_burn_in, "\n")
  cat("    thin            :", object@MCMC$thin, "\n")
  cat("    jump_factor     :", object@MCMC$jump_factor, "\n\n")
  cat("  refresh_policy \n")
  cat("    method          :", object@refresh_policy$method, "\n")
  cat("    interval        :", object@refresh_policy$interval, "\n")
  cat("    threshold       :", object@refresh_policy$threshold, "\n")
  cat("    position        :", object@refresh_policy$position, "\n\n")
  cat("  exposure_control \n")
  cat("    method                    :", object@exposure_control$method, "\n")
  cat("    M                         :", object@exposure_control$M, "\n")
  cat("    max_exposure_rate         :", object@exposure_control$max_exposure_rate, "\n")
  cat("    acceleration_factor       :", object@exposure_control$acceleration_factor, "\n")
  cat("    n_segment                 :", object@exposure_control$n_segment, "\n")
  cat("    first_segment             :", object@exposure_control$first_segment, "\n")
  cat("    segment_cut               :", object@exposure_control$segment_cut, "\n")
  cat("    initial_eligibility_stats :", !is.null(object@exposure_control$initial_eligibility_stats), "\n")
  cat("    fading_factor             :", object@exposure_control$fading_factor, "\n")
  cat("    diagnosticsStats          :", object@exposure_control$diagnostic_stats, "\n\n")
  cat("  stopping_criterion \n")
  cat("    method          :", object@stopping_criterion$method, "\n")
  cat("    test_length     :", object@stopping_criterion$test_length, "\n")
  cat("    min_ni          :", object@stopping_criterion$min_ni, "\n")
  cat("    max_ni          :", object@stopping_criterion$max_ni, "\n")
  cat("    se_threshold    :", ifelse(toupper(object@stopping_criterion$method) == "VARIABLE", object@stopping_criterion$se_threshold, NA), "\n\n")
  cat("  interim_theta \n")
  cat("    method               :", object@interim_theta$method, "\n")
  cat("    shrinkage_correction :", object@interim_theta$shrinkage_correction, "\n")
  cat("    prior_dist           :", ifelse(toupper(object@interim_theta$method == "EAP"), object@interim_theta$prior_dist, NA), "\n")
  cat("    prior_par            :", ifelse(toupper(object@interim_theta$method == "EAP"), sprintf(ifelse(toupper(object@interim_theta$prior_dist) == "NORMAL", "Mean = %5.3f, SD = %5.3f", "Min = %5.3f, Max = %5.3f"), object@interim_theta$prior_par[1], object@interim_theta$prior_par[2]), NA), "\n")
  cat("    bound_ML             :", object@interim_theta$bound_ML, "\n")
  cat("    truncate_ML          :", object@interim_theta$truncate_ML, "\n")
  cat("    max_iter             :", object@interim_theta$max_iter, "\n")
  cat("    crit                 :", object@interim_theta$crit, "\n")
  cat("    max_change           :", object@interim_theta$max_change, "\n")
  cat("    do_fisher            :", object@interim_theta$do_fisher, "\n\n")
  cat("  final_theta \n")
  cat("    method               :", object@final_theta$method, "\n")
  cat("    shrinkage_correction :", object@final_theta$shrinkage_correction, "\n")
  cat("    prior_dist           :", ifelse(toupper(object@final_theta$method == "EAP"), object@final_theta$prior_dist, NA), "\n")
  cat("    prior_par            :", ifelse(toupper(object@final_theta$method == "EAP"), sprintf(ifelse(toupper(object@final_theta$prior_dist) == "NORMAL", "Mean = %5.3f, SD = %5.3f", "Min = %5.3f, Max = %5.3f"), object@final_theta$prior_par[1], object@final_theta$prior_par[2]), NA), "\n")
  cat("    bound_ML             :", object@final_theta$bound_ML, "\n")
  cat("    truncate_ML          :", object@final_theta$truncate_ML, "\n")
  cat("    max_iter             :", object@final_theta$max_iter, "\n")
  cat("    crit                 :", object@final_theta$crit, "\n")
  cat("    max_change           :", object@final_theta$max_change, "\n")
  cat("    do_fisher            :", object@final_theta$do_fisher, "\n\n")
  cat("  theta_grid \n")
  print(object@theta_grid)
  cat("\n  audit_trail : ", object@audit_trail, "\n")
})

#' output_Shadow
#'
#' @slot simulee_id Numeric. The index of the simulee.
#' @slot true_theta Numeric or NULL. True theta value of the simulee if supplied in advance.
#' @slot true_theta_segment Numeric or NULL. Which segment the true theta value is in.
#' @slot final_theta_est Numeric. The estimated theta after the last administered item.
#' @slot final_se_est Numeric. The standard error of estimation after the last administered item.
#' @slot administered_item_index Numeric. A vector of item indices administered at each position.
#' @slot administered_item_resp Numeric. A vector of item responses at each position.
#' @slot administered_item_ncat Numeric. A vector containing the number of categories for each administered item.
#' @slot administered_stimulus_index Numeric. A vector of stimulus indices administered at each position.
#' @slot shadow_test_refreshed Logical. A vector of logical values indicating whether the shadow test was refreshed before administering an item at each position.
#' @slot shadow_test_feasible Logical. A vector of logical values indicating whether a feasible solution to the shadow test was available in each position.
#' @slot solve_time Numeric. A vector of values indicating the time taken in obtaining a shadow test.
#' @slot interim_theta_est Numeric. A vector containing estimated thetas at each position.
#' @slot interim_se_est Numeric. A vector containing standard errors at each position.
#' @slot theta_segment_index Numeric. A vector containing which segments the estimated thetas were in at each position.
#' @slot prior Numeric. A prior distribution.
#' @slot prior_par Numeric. The hyper parameters for the prior distribution.
#' @slot posterior Numeric. A posterior distribution.
#' @slot posterior_sample Numeric. A vector containing MCMC samples.
#' @slot likelihood Numeric. A likelihood distribution.
#' @slot shadow_test A list of vectors containing item indices of the shadow test at each position.
#'
#' @export
setClass("output_Shadow",
  slots = c(
    simulee_id = "numeric",
    true_theta = "numeric_or_null",
    true_theta_segment = "numeric_or_null",
    final_theta_est = "numeric",
    final_se_est    = "numeric",
    administered_item_index = "numeric",
    administered_item_resp = "numeric",
    administered_item_ncat = "numeric",
    administered_stimulus_index = "numeric",
    shadow_test_refreshed = "logical",
    shadow_test_feasible = "logical",
    solve_time = "numeric",
    interim_theta_est = "numeric",
    interim_se_est    = "numeric",
    theta_segment_index = "numeric",
    prior = "numeric",
    prior_par = "numeric",
    posterior = "numeric",
    posterior_sample = "numeric",
    likelihood = "numeric",
    shadow_test = "list"
  ),
  prototype = list(
    simulee_id = numeric(0),
    true_theta = NULL,
    true_theta_segment = NULL,
    final_theta_est = numeric(0),
    final_se_est = numeric(0),
    administered_item_index = numeric(0),
    administered_item_resp = numeric(0),
    administered_item_ncat = numeric(0),
    administered_stimulus_index = numeric(0),
    shadow_test_refreshed = logical(0),
    shadow_test_feasible = logical(0),
    solve_time = numeric(0),
    interim_theta_est = numeric(0),
    interim_se_est = numeric(0),
    theta_segment_index = numeric(0),
    prior = numeric(0),
    prior_par = numeric(0),
    posterior = numeric(0),
    posterior_sample = numeric(0),
    likelihood = numeric(0),
    shadow_test = list(0)
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' @name show-method
#' @aliases show,output_Shadow-method
#' @docType methods
#' @noRd
setMethod("show", "output_Shadow", function(object) {
  if (length(object@administered_item_index) > 0) {
    cat("Simulee Index          :", object@simulee_id, "\n")
    cat("  True Theta           :", object@true_theta, "\n")
    cat("  Final Theta Estimate :", object@final_theta_est, "\n")
    cat("  Final SE Estimate    :", object@final_se_est, "\n")
    output <- data.frame(
      stage = 1:length(object@administered_item_index),
      stimulus_index = ifelse(is.nan(object@administered_stimulus_index), rep(NA, length(object@administered_item_index)), object@administered_stimulus_index),
      item_index = object@administered_item_index,
      item_resp = object@administered_item_resp,
      item_ncat = object@administered_item_ncat,
      interim_theta = object@interim_theta_est,
      interim_se = object@interim_se_est,
      theta_segment = object@theta_segment_index
    )
    print(output)
  } else {
    cat("The 'output_Shadow' object is empty.")
  }
  cat("\n")
})
