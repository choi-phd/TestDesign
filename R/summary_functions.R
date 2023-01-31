#' @include helper_functions.R
NULL

#' @aliases summary,item_pool-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "item_pool", function(object) {
  o <- new("summary_item_pool")
  o@ni           <- object@ni
  x              <- as.data.frame(table(object@model))
  colnames(x)    <- c("model", "items")
  o@ni_per_model <- x
  o@has_se       <- any(na.omit(object@se) > 0)
  return(o)
})

#' @aliases summary,item_attrib-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "item_attrib", function(object) {
  o <- new("summary_item_attrib")
  o@attribs <- names(object@data)
  o@levels  <- list()
  for (a in o@attribs) {
    o@levels[[a]] <- sort(unique(object@data[[a]]))
  }
  return(o)
})

#' @aliases summary,st_attrib-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "st_attrib", function(object) {
  o <- new("summary_st_attrib")
  o@attribs <- names(object@data)
  o@levels  <- list()
  for (a in o@attribs) {
    o@levels[[a]] <- sort(unique(object@data[[a]]))
  }
  return(o)
})

#' @aliases summary,constraints-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "constraints", function(object) {
  o <- new("summary_constraints")
  o@n_constraints     <- dim(object@constraints)[1]
  o@n_mip_constraints <- sum(unlist(lapply(object@list_constraints, function(x) x@nc)))
  o@test_length       <- object@test_length
  o@set_based         <- object@set_based
  return(o)
})

#' @aliases summary,output_Static-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "output_Static", function(object, simple = FALSE) {
  o <- new("summary_output_Static")
  o@n_targets        <- length(object@config@item_selection$target_location)
  o@obj_type         <- object@config@item_selection$method
  o@target_location  <- object@config@item_selection$target_location
  o@selected_items   <- object@selected[['INDEX']]
  o@set_based        <- object@constraints@set_based
  o@n_selected_sets  <- NULL
  if (o@set_based) {
    o@n_selected_sets <- length(unique(object@selected[['STID']]))
  }
  subpool   <- subsetItemPool(object@pool, o@selected_items)
  info      <- calcFisher(subpool, o@target_location)
  o@info  <- rowSums(info)
  o@score <- calcEscore(subpool, o@target_location)

  if (!simple) {
    o@achieved <- object@achieved
  } else {
    o@achieved <- NULL
  }

  return(o)

})

#' @aliases summary,output_Shadow_all-method
#' @docType methods
#' @rdname summary-methods
setMethod("summary", "output_Shadow_all", function(object, simple = FALSE) {
  o <- new("summary_output_Shadow_all")
  o@n_simulee    <- length(object@output)
  o@test_length  <- object@constraints@test_length
  o@est_theta    <- sapply(object@output, function(x) x@final_theta_est)
  o@est_theta_se <- sapply(object@output, function(x) x@final_se_est)
  o@true_theta   <- object@true_theta
  if (!is.null(o@true_theta)) {
    o@diff <- o@est_theta - o@true_theta
    o@mse  <- mean(o@diff ** 2)
    o@bias <- mean(o@diff)
    o@corr <- cor(o@est_theta, o@true_theta)
  }
  o@average_se <- mean(o@est_theta_se)

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
    if (nc == 1) {
      x <- do.call("c", tmp)
    } else {
      x <- do.call("c", tmp[i, ])
    }
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

  if (!simple) {
    o@achieved <- tmp
  } else {
    o@achieved <- NULL
  }

  return(o)

})
