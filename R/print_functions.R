#' @include summary_class.R
# Print functions should return the argument 'x' invisibly.
NULL

#' @aliases print,item_1PL-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_1PL", function(x) {
  cat("One-parameter logistic or Rasch model (item_1PL)\n")
  cat("  Difficulty     :", x@difficulty, "\n")
  return(invisible(x))
})

#' @aliases print,item_2PL-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_2PL", function(x) {
  cat("Two-parameter logistic model (item_2PL) \n")
  cat("  Slope          :", x@slope, "\n")
  cat("  Difficulty     :", x@difficulty, "\n")
  return(invisible(x))
})

#' @aliases print,item_3PL-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_3PL", function(x) {
  cat("Three-parameter logistic model (item_3PL)\n")
  cat("  Slope          :", x@slope, "\n")
  cat("  Difficulty     :", x@difficulty, "\n")
  cat("  Guessing       :", x@guessing, "\n")
  return(invisible(x))
})

#' @aliases print,item_PC-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_PC", function(x) {
  cat("Partial credit model (item_PC)\n")
  cat("  Threshold     :", x@threshold, "\n")
  cat("  N categories  :", x@ncat, "\n")
  return(invisible(x))
})

#' @aliases print,item_GPC-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_GPC", function(x) {
  cat("Generalized partial credit model (item_GPC)\n")
  cat("  Slope         :", x@slope, "\n")
  cat("  Threshold     :", x@threshold, "\n")
  cat("  N categories  :", x@ncat, "\n")
  return(invisible(x))
})

#' @aliases print,item_GR-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_GR", function(x) {
  cat("Graded response model (item_GR)\n")
  cat("  Slope         :", x@slope, "\n")
  cat("  Category b    :", x@category, "\n")
  cat("  N categories  :", x@ncat, "\n")
  return(invisible(x))
})

#' @aliases print,item_pool-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_pool", function(x) {
  if (length(x@ni) > 0) {
    cat("@ni      :", x@ni, "\n")
    cat("@max_cat :", x@max_cat, "\n\n")
    print(data.frame(index = x@index, id = x@id, model = x@model, NCAT = x@NCAT))
    for (i in 1:x@ni) {
      cat("\n", paste0(x@index[i], ". "))
      print(x@parms[[i]])
    }
    cat("\n")
  } else {
    cat("'item_pool' object with 0 items")
  }
  return(invisible(x))
})

#' @aliases print,item_attrib-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "item_attrib", function(x) {
  print(x@data)
  return(invisible(x@data))
})

#' @aliases print,st_attrib-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "st_attrib", function(x) {
  print(x@data)
  return(invisible(x@data))
})

#' @aliases print,summary_item_attrib-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "summary_item_attrib", function(x) {
  cat("Item attributes\n")
  cat(sprintf("  # of attributes : %i\n", length(x@attribs)))
  for (i in 1:length(x@levels)) {
    if (length(x@levels[[i]]) <= 10) {
      lvl_txt <- paste0(x@levels[[i]], collapse = " ")
    } else {
      lvl_txt <- sprintf("(%i levels)", length(x@levels[[i]]))
    }
    cat(sprintf(" %16s : %s\n", names(x@levels)[i], lvl_txt))
  }
  return(invisible(x))
})

#' @aliases print,constraints-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "constraints", function(x) {
  print(x@constraints)
  return(invisible(x@constraints))
})

#' @aliases print,config_Static-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "config_Static", function(x) {
  cat("Static assembly configurations \n\n")
  cat("  item_selection \n")
  cat("    method          :", x@item_selection$method, "\n")
  cat("    info_type       :", x@item_selection$info_type, "\n")
  cat("    target_location :", x@item_selection$target_location, "\n")
  cat("    target_value    :", x@item_selection$target_value, "\n")
  cat("    target_weight   :", x@item_selection$target_weight, "\n")
  cat("\n")
  cat("  MIP \n")
  cat("    solver          :", x@MIP$solver, "\n")
  cat("    verbosity       :", x@MIP$verbosity, "\n")
  cat("    time_limit      :", x@MIP$time_limit, "\n")
  cat("    gap_limit       :", x@MIP$gap_limit, "(relative)\n")
  cat("    gap_limit_abs   :", x@MIP$gap_limit_abs, "(absolute)\n")
  cat("    obj_tol         :", x@MIP$obj_tol, "\n")
  cat("    retry           :", x@MIP$retry, "\n")
  cat("\n")
  return(invisible(x))
})

#' @aliases print,config_Shadow-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "config_Shadow", function(x) {
  cat("Shadow assembly configurations \n\n")
  cat("  item_selection \n")
  cat("    method                    :", x@item_selection$method, "\n")
  cat("    info_type                 :", x@item_selection$info_type, "\n")
  cat("    initial_theta             :", x@item_selection$initial_theta, "\n")
  cat("    fixed_theta               :", x@item_selection$fixed_theta, "\n")
  cat("\n")
  cat("  content_balancing \n")
  cat("    method                    :", x@content_balancing$method, "\n")
  cat("\n")
  cat("  MIP \n")
  cat("    solver                    :", x@MIP$solver, "\n")
  cat("    verbosity                 :", x@MIP$verbosity, "\n")
  cat("    time_limit                :", x@MIP$time_limit, "\n")
  cat("    gap_limit                 :", x@MIP$gap_limit, "\n")
  cat("    gap_limit_abs             :", x@MIP$gap_limit_abs, "\n")
  cat("    retry                     :", x@MIP$retry, "\n")
  cat("\n")
  cat("  MCMC \n")
  cat("    burn_in                   :", x@MCMC$burn_in, "\n")
  cat("    post_burn_in              :", x@MCMC$post_burn_in, "\n")
  cat("    thin                      :", x@MCMC$thin, "\n")
  cat("    jump_factor               :", x@MCMC$jump_factor, "\n")
  cat("\n")
  cat("  refresh_policy \n")
  cat("    method                    :", x@refresh_policy$method, "\n")
  cat("    interval                  :", x@refresh_policy$interval, "\n")
  cat("    threshold                 :", x@refresh_policy$threshold, "\n")
  cat("    position                  :", x@refresh_policy$position, "\n")
  cat("\n")
  cat("  exposure_control \n")
  cat("    method                    :", x@exposure_control$method, "\n")
  cat("    M                         :", x@exposure_control$M, "\n")
  if (length(unique(x@exposure_control$max_exposure_rate)) == 1) {
    tmp <- unique(x@exposure_control$max_exposure_rate)
  } else {
    tmp <- x@exposure_control$max_exposure_rate
  }
  cat("    max_exposure_rate         :", tmp, "\n")
  cat("    acceleration_factor       :", x@exposure_control$acceleration_factor, "\n")
  cat("    n_segment                 :", x@exposure_control$n_segment, "\n")
  cat("    first_segment             :", x@exposure_control$first_segment, "\n")
  cat("    segment_cut               :", x@exposure_control$segment_cut, "\n")
  cat("    initial_eligibility_stats :", !is.null(x@exposure_control$initial_eligibility_stats), "\n")
  cat("    fading_factor             :", x@exposure_control$fading_factor, "\n")
  cat("    diagnostic_stats          :", x@exposure_control$diagnostic_stats, "\n")
  cat("\n")
  cat("  stopping_criterion \n")
  cat("    method                    :", x@stopping_criterion$method, "\n")
  cat("    test_length               :", x@stopping_criterion$test_length, "\n")
  cat("    min_ni                    :", x@stopping_criterion$min_ni, "\n")
  cat("    max_ni                    :", x@stopping_criterion$max_ni, "\n")
  cat("    se_threshold              :",
    ifelse(
      toupper(x@stopping_criterion$method) == "VARIABLE",
      x@stopping_criterion$se_threshold,
      NA
    ), "\n")
  cat("\n")
  cat("  interim_theta \n")
  cat("    method                    :", x@interim_theta$method, "\n")
  cat("    shrinkage_correction      :", x@interim_theta$shrinkage_correction, "\n")
  cat("    prior_dist                :",
    ifelse(
      toupper(x@interim_theta$method == "EAP"),
      x@interim_theta$prior_dist,
      NA
    ), "\n")
  cat("    prior_par                 :",
    ifelse(
      toupper(x@interim_theta$method == "EAP"),
      sprintf(
        ifelse(
          toupper(x@interim_theta$prior_dist) == "NORMAL",
          "Mean = %5.3f, SD = %5.3f",
          "Min = %5.3f, Max = %5.3f"
        ),
        x@interim_theta$prior_par[1], x@interim_theta$prior_par[2]),
      NA), "\n")
  cat("    bound_ML                  :", x@interim_theta$bound_ML, "\n")
  cat("    truncate_ML               :", x@interim_theta$truncate_ML, "\n")
  cat("    max_iter                  :", x@interim_theta$max_iter, "\n")
  cat("    crit                      :", x@interim_theta$crit, "\n")
  cat("    max_change                :", x@interim_theta$max_change, "\n")
  cat("    do_Fisher                 :", x@interim_theta$do_Fisher, "\n")
  cat("    fence_slope               :", x@interim_theta$fence_slope, "\n")
  cat("    fence_difficulty          :", x@interim_theta$fence_difficulty, "\n")
  cat("\n")
  cat("  final_theta \n")
  cat("    method                    :", x@final_theta$method, "\n")
  cat("    shrinkage_correction      :", x@final_theta$shrinkage_correction, "\n")
  cat("    prior_dist                :",
    ifelse(
      toupper(x@final_theta$method == "EAP"),
      x@final_theta$prior_dist,
      NA
    ), "\n")
  cat("    prior_par                 :",
    ifelse(
      toupper(x@final_theta$method == "EAP"),
      sprintf(
        ifelse(
          toupper(x@final_theta$prior_dist) == "NORMAL",
          "Mean = %5.3f, SD = %5.3f",
          "Min = %5.3f, Max = %5.3f"
        ),
        x@final_theta$prior_par[1], x@final_theta$prior_par[2]),
      NA
    ), "\n")
  cat("    bound_ML                  :", x@final_theta$bound_ML, "\n")
  cat("    truncate_ML               :", x@final_theta$truncate_ML, "\n")
  cat("    max_iter                  :", x@final_theta$max_iter, "\n")
  cat("    crit                      :", x@final_theta$crit, "\n")
  cat("    max_change                :", x@final_theta$max_change, "\n")
  cat("    do_Fisher                 :", x@final_theta$do_Fisher, "\n")
  cat("    fence_slope               :", x@final_theta$fence_slope, "\n")
  cat("    fence_difficulty          :", x@final_theta$fence_difficulty, "\n")
  cat("\n")
  cat("  theta_grid \n")
  print(x@theta_grid)
  cat("\n")
  cat("  audit_trail                 : ", x@audit_trail, "\n")
  return(invisible(x))
})

#' @param index_only if \code{TRUE} then only print item indices. If \code{FALSE} then print all item attributes. (default = \code{TRUE})
#'
#' @aliases print,output_Static-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "output_Static", function(x, index_only = TRUE) {

  cat("Static assembly : selected items\n\n")

  tmp <- x@selected
  if (index_only) {
    tmp <- tmp[['INDEX']]
  }

  print(tmp)
  return(invisible(tmp))

})

#' @aliases print,output_Shadow-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "output_Shadow", function(x) {
  if (length(x@administered_item_index) > 0) {
    cat("Simulee index          :", x@simulee_id, "\n")
    cat("  True theta           :", x@true_theta, "\n")
    cat("  Final theta estimate :", x@final_theta_est, "\n")
    cat("  Final SE estimate    :", x@final_se_est, "\n")
    output <- data.frame(
      stage          = 1:length(x@administered_item_index),
      stimulus_index = ifelse(is.nan(x@administered_stimulus_index), rep(NA, length(x@administered_item_index)), x@administered_stimulus_index),
      item_index     = x@administered_item_index,
      item_resp      = x@administered_item_resp,
      item_ncat      = x@administered_item_ncat,
      interim_theta  = x@interim_theta_est,
      interim_se     = x@interim_se_est,
      theta_segment  = x@theta_segment_index
    )
    print(output)
  } else {
    cat("empty 'output_Shadow' object")
  }
  cat("\n")
  return(invisible(x))
})

#' @aliases print,output_Shadow_all-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "output_Shadow_all", function(x) {
  print(summary(x))
  return(invisible(x))
})

#' @aliases print,exposure_rate_plot-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "exposure_rate_plot", function(x) {
  print(x@plot)
  return(invisible(x))
})

#' @aliases print,summary_item_pool-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "summary_item_pool", function(x) {
  if (length(x@ni) > 0) {
    n_digits = max(floor(log(x@ni, 10)) + 3, 6)
    cat("Item pool\n")
    cat(sprintf("  # of items :% *i\n", n_digits, x@ni))
    for (i in 1:nrow(x@ni_per_model)) {
        cat(sprintf(" %11s :% *i\n", x@ni_per_model[i, 1], n_digits, x@ni_per_model[i, 2]))
    }
    cat(sprintf("      has SE : %5s\n", x@has_se))
  } else {
    cat("empty 'summary_item_pool' object\n")
  }
  return(invisible(x))
})

#' @aliases print,summary_item_pool-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "summary_constraints", function(x) {
  if (length(x@n_constraints) > 0) {
    n_digits = max(floor(log(x@n_constraints, 10)) + 3, 6)
    cat("Constraints\n")
    cat(sprintf("  # of constraints :% *i\n", n_digits, x@n_constraints))
    cat(sprintf("      (MIP-format) :% *i\n", n_digits, x@n_mip_constraints))
    cat("\n")
    cat(sprintf("       test length :% *i\n", n_digits, x@test_length))
    cat(sprintf("         set-based :% *s\n", n_digits, x@set_based))
  } else {
    cat("empty 'summary_constraints' object\n")
  }
  return(invisible(x))
})

#' @param digits minimal number of *significant* digits. See \code{\link{print.default}}.
#'
#' @aliases print,summary_output_Static-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "summary_output_Static", function(x, digits = 3) {
  cat("Static assembly\n\n")
  cat(sprintf("         # of targets: %7i\n", x@n_targets))
  cat(sprintf("    type of objective: %7s\n", x@obj_type))
  cat(sprintf("  # of selected items: %7i\n", length(x@selected_items)))
  if (x@set_based) {
    cat(sprintf("   # of selected sets: %7i\n", x@n_selected_sets))
  }
  cat("\n")
  cat("     theta      info     score\n")
  for (i in 1:x@n_targets) {
    cat(sprintf(
      "      % 2.1f % 9.3f % 9.3f\n",
      x@target_location[i],
      x@info[i],
      x@score[i]
    ))
  }
  cat("\n")
  print(x@achieved, digits = digits)
  return(invisible(x))
})

#' @aliases print,summary_output_Shadow_all-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "summary_output_Shadow_all", function(x, digits = 3) {
  cat("Shadow assembly\n\n")
  cat(sprintf("  # of simulees : %i\n",   x@n_simulee))
  cat(sprintf("    test length : %i\n\n", x@test_length))
  cat(sprintf("  theta estimation statistics\n"))
  if (!is.null(x@true_theta)) {
    cat(sprintf("            MSE : % 2.6f\n", x@mse))
    cat(sprintf("           bias : % 2.6f\n", x@bias))
    cat(sprintf("           corr : % 2.6f\n", x@corr))
  }
  cat(sprintf("     Average SE : % 2.6f\n\n", x@average_se))
  print(x@achieved, digits = digits)
  return(invisible(x))
})
