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
      do_Fisher = TRUE
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
      do_Fisher = TRUE
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
    if (!length(object@exposure_control$max_exposure_rate) %in% c(1, object@exposure_control$n_segment)) {
      msg <- sprintf("@exposure_control: unexpected length($max_exposure_rate) %s (must be 1 or $n_segment)", length(object@exposure_control$max_exposure_rate))
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
  if (length(conf@exposure_control$max_exposure_rate) == 1) {
    conf@exposure_control$max_exposure_rate <- rep(
      conf@exposure_control$max_exposure_rate,
      conf@exposure_control$n_segment
    )
  }
  v <- validObject(conf)
  if (v) {
    return(conf)
  }
}


#' An S4 class to represent the output from Shadow()
#'
#' @slot output List of \code{\linkS4class{output_Shadow}} objects. The list of assembled test for each examinee.
#' @slot pool A \code{\linkS4class{item_pool}} object. The item pool used in assembly.
#' @slot config A \code{\linkS4class{config_Shadow}} object. The config used in assembly.
#' @slot true_theta True theta values if were supplied.
#' @slot constraints A \code{\linkS4class{constraints}} object. The constraints used in assembly.
#' @slot prior Prior
#' @slot prior_par foo
#' @slot data The response data used in assembly.
#' @slot final_theta_est Final (i.e. not interim) estimates of theta for each examinee.
#' @slot final_se_est Final (i.e. not interim) standard error estimates of theta for each examinee.
#' @slot exposure_rate Exposure rate of each item in the pool.
#' @slot usage_matrix The matrix representing which items were used in each item position.
#' @slot true_segment_count foo
#' @slot est_segment_count foo
#' @slot eligibility_stats foo
#' @slot check_eligibility_stats foo
#' @slot no_fading_eligibility_stats foo
#' @slot freq_infeasible foo
#'
#' @export

setClass("output_Shadow_all",
  slots = c(
    output                      = "list_or_null",
    pool                        = "item_pool",
    config                      = "config_Shadow",
    true_theta                  = "numeric_or_null",
    constraints                 = "constraints",
    prior                       = "numeric_or_null",
    prior_par                   = "numeric_or_null",
    data                        = "matrix_or_null",
    final_theta_est             = "numeric_or_null",
    final_se_est                = "numeric_or_null",
    exposure_rate               = "matrix_or_null",
    usage_matrix                = "matrix_or_null",
    true_segment_count          = "numeric_or_null",
    est_segment_count           = "numeric_or_null",
    eligibility_stats           = "list_or_null",
    check_eligibility_stats     = "list_or_null",
    no_fading_eligibility_stats = "list_or_null",
    freq_infeasible             = "table"
  ),
  prototype = list(
    output                      = NULL,
    pool                        = new("item_pool"),
    config                      = new("config_Shadow"),
    true_theta                  = NULL,
    constraints                 = new("constraints"),
    prior                       = NULL,
    prior_par                   = NULL,
    data                        = NULL,
    final_theta_est             = NULL,
    final_se_est                = NULL,
    exposure_rate               = NULL,
    usage_matrix                = NULL,
    true_segment_count          = NULL,
    est_segment_count           = NULL,
    eligibility_stats           = NULL,
    check_eligibility_stats     = NULL,
    no_fading_eligibility_stats = NULL,
    freq_infeasible             = new("table")
  ),
  validity = function(object) {
    return(TRUE)
  }
)

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
    simulee_id                  = "numeric",
    true_theta                  = "numeric_or_null",
    true_theta_segment          = "numeric_or_null",
    final_theta_est             = "numeric",
    final_se_est                = "numeric",
    administered_item_index     = "numeric",
    administered_item_resp      = "numeric",
    administered_item_ncat      = "numeric",
    administered_stimulus_index = "numeric",
    shadow_test_refreshed       = "logical",
    shadow_test_feasible        = "logical",
    solve_time                  = "numeric",
    interim_theta_est           = "numeric",
    interim_se_est              = "numeric",
    theta_segment_index         = "numeric",
    prior                       = "numeric",
    prior_par                   = "numeric",
    posterior                   = "numeric",
    posterior_sample            = "numeric",
    likelihood                  = "numeric",
    shadow_test                 = "list"
  ),
  prototype = list(
    simulee_id                  = numeric(0),
    true_theta                  = NULL,
    true_theta_segment          = NULL,
    final_theta_est             = numeric(0),
    final_se_est                = numeric(0),
    administered_item_index     = numeric(0),
    administered_item_resp      = numeric(0),
    administered_item_ncat      = numeric(0),
    administered_stimulus_index = numeric(0),
    shadow_test_refreshed       = logical(0),
    shadow_test_feasible        = logical(0),
    solve_time                  = numeric(0),
    interim_theta_est           = numeric(0),
    interim_se_est              = numeric(0),
    theta_segment_index         = numeric(0),
    prior                       = numeric(0),
    prior_par                   = numeric(0),
    posterior                   = numeric(0),
    posterior_sample            = numeric(0),
    likelihood                  = numeric(0),
    shadow_test                 = list(0)
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' An S4 class to represent the exposure rate plot
#'
#' @noRd
setClass("exposure_rate_plot",
  slots = c(
    plot = "recordedplot_or_null",
    item_exposure_rate               = "numeric_or_null",
    item_exposure_rate_segment       = "list_or_null",
    item_exposure_rate_segment_final = "list_or_null",
    stim_exposure_rate               = "numeric_or_null",
    stim_exposure_rate_segment       = "list_or_null",
    stim_exposure_rate_segment_final = "list_or_null",
    segment_rate_table               = "dataframe_or_null",
    n_segment                        = "numeric_or_null",
    segment_n                        = "numeric_or_null",
    segment_cut                      = "numeric_or_null",
    segment_label                    = "character_or_null"
  ),
  prototype = list(
    plot = NULL,
    item_exposure_rate               = NULL,
    item_exposure_rate_segment       = NULL,
    item_exposure_rate_segment_final = NULL,
    stim_exposure_rate               = NULL,
    stim_exposure_rate_segment       = NULL,
    stim_exposure_rate_segment_final = NULL,
    segment_rate_table               = NULL,
    n_segment                        = NULL,
    segment_n                        = NULL,
    segment_cut                      = NULL,
    segment_label                    = NULL
  ),
  validity = function(object) {
    return(TRUE)
  }
)
