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
  subpool   <- subsetItemPool(object@pool, select = out@selected_items)
  info      <- calcFisher(subpool, out@target_location)
  out@info  <- rowSums(info)
  out@score <- calcEscore(subpool, out@target_location)
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
  # number of items matching each constraint
  tmp    <- sapply(object@output, function(x) countConstraints(object@constraints, x@administered_item_index))
  nc     <- dim(object@constraints@constraints)[1]
  counts <- vector("list", nc)
  for (i in 1:nc) {
    counts[[i]] <- NA
    count <- do.call("c", tmp[i, ])
    if (!is.null(count)) {
      counts[[i]] <- count
    }
  }
  c_mean <- sapply(counts, mean)
  c_sd   <- sapply(counts, sd)
  c_min  <- sapply(counts, min)
  c_max  <- sapply(counts, max)
  tmp <- object@constraints@constraints
  tmp <- tmp[, !names(tmp) %in% "COUNT"]
  tmp <- cbind(tmp, mean = c_mean, sd = c_sd, min = c_min, max = c_max)
  out@count <- tmp
  return(out)
})
