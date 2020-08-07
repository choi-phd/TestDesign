#' @include helper_functions.R
NULL

#' @aliases summary,item_pool-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "item_pool", function(object) {
  out <- new("summary_item_pool")
  out@ni           <- object@ni
  tmp              <- as.data.frame(table(object@model))
  colnames(tmp)    <- c("model", "items")
  out@ni_per_model <- tmp
  out@has_se       <- any(na.omit(object@se) > 0)
  return(out)
})

#' @aliases summary,item_attrib-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "item_attrib", function(object) {
  out <- new("summary_item_attrib")
  out@attribs <- names(object@data)
  out@levels  <- list()
  for (a in out@attribs) {
    out@levels[[a]] <- sort(unique(object@data[[a]]))
  }
  return(out)
})

#' @aliases summary,constraints-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "constraints", function(object) {
  out <- new("summary_constraints")
  out@n_constraints     <- dim(object@constraints)[1]
  out@n_mip_constraints <- sum(unlist(lapply(object@list_constraints, function(x) x@nc)))
  out@test_length       <- object@test_length
  out@set_based         <- object@set_based
  return(out)
})

#' @aliases summary,output_Static-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "output_Static", function(object) {
  out <- new("summary_output_Static")
  out@n_targets        <- length(object@config@item_selection$target_location)
  out@obj_type         <- object@config@item_selection$method
  out@target_location  <- object@config@item_selection$target_location
  out@selected_items   <- object@selected[['INDEX']]
  out@set_based        <- object@constraints@set_based
  out@n_selected_sets  <- NULL
  if (out@set_based) {
    out@n_selected_sets <- length(unique(object@selected[['STID']]))
  }
  subpool   <- subsetItemPool(object@pool, out@selected_items)
  info      <- calcFisher(subpool, out@target_location)
  out@info  <- rowSums(info)
  out@score <- calcEscore(subpool, out@target_location)

  out@achieved <- object@achieved

  return(out)

})

#' @aliases summary,output_Shadow_all-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "output_Shadow_all", function(object) {
  out <- new("summary_output_Shadow_all")
  out@n_simulee    <- length(object@output)
  out@test_length  <- object@constraints@test_length
  out@est_theta    <- sapply(object@output, function(x) x@final_theta_est)
  out@est_theta_se <- sapply(object@output, function(x) x@final_se_est)
  out@true_theta   <- object@true_theta
  if (!is.null(out@true_theta)) {
    out@diff <- out@est_theta - out@true_theta
    out@mse  <- mean(out@diff ** 2)
    out@bias <- mean(out@diff)
    out@corr <- cor(out@est_theta, out@true_theta)
  }
  out@average_se <- mean(out@est_theta_se)

  # achieved attribute matching each constraint
  nc <- length(object@constraints@list_constraints)
  tmp <- sapply(object@output, function(x) {
    getSolutionAttributes(
      object@constraints,
      x@administered_item_index,
      TRUE
    )
  })

  achieved <- vector("list", nc)
  for (i in 1:nc) {
    achieved[[i]] <- NA
    x <- do.call("c", tmp[i, ])
    if (!is.null(achieved)) {
      achieved[[i]] <- x
    }
  }
  a_mean <- sapply(achieved, mean)
  a_sd   <- sapply(achieved, sd)
  a_min  <- sapply(achieved, min)
  a_max  <- sapply(achieved, max)
  tmp <- object@constraints@constraints
  tmp <- cbind(tmp, mean = a_mean, sd = a_sd, min = a_min, max = a_max)
  out@achieved <- tmp
  return(out)
})
