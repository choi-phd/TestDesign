#' @include shadow_functions.R
NULL

#' Calculate Adaptivity Measures
#'
#' \code{\link{calculateAdaptivityMeasures}} is a function for
#' calculating commonly used adaptivity measures.
#'
#' @param x an \code{\linkS4class{output_Shadow_all}} object.
#'
#' @returns \code{\link{calculateAdaptivityMeasures}} returns a named list:
#' \itemize{
#'   \item{\code{corr} the correlation between final theta estimates and average test locations.}
#'   \item{\code{ratio} the ratio of (1) standard deviation of average test locations, versus (2) standard deviation of final theta estimates.}
#'   \item{\code{PRV} the proportion of variance reduced, from (1) the variance of item locations of all items in the pool, by (2) the average of test location variances.}
#'   \item{\code{info} (1) average information of a test at final theta estimate, relative to (2) best average obtainable from item pool using same test length, adjusting for (3) average information from item pool using random selection.}
#' }
#'
#' @export
calculateAdaptivityMeasures <- function(x) {

  if (!inherits(x, "output_Shadow_all")) {
    stop("unexpected input object: must be an 'output_Shadow_all' object.")
  }

  adaptivity_measures <- list()
  item_location       <- list()

  item_location$item_pool <- unlist(calcLocation(x@pool))
  item_location$items_administered <-
    lapply(x@output, function(e) {
      unlist(
        calcLocation(subsetItemPool(x@pool, e@administered_item_index))
      )
    })
  item_location$mean <- sapply(item_location$items_administered, mean)
  item_location$var  <- sapply(item_location$items_administered, var)

  adaptivity_measures$corr  <- cor(x@final_theta_est, item_location$mean)
  adaptivity_measures$ratio <- sd(item_location$mean) / sd(x@final_theta_est)
  adaptivity_measures$PRV   <-
    (var(item_location$item_pool) - mean(item_location$var)) /
    var(item_location$item_pool)

  adaptivity_measures$info  <- mean(
    sapply(x@output, function(e) {
      info_pool <- as.vector(calcFisher(x@pool, e@final_theta_est))
      info_best <- mean(sort(info_pool, decreasing = TRUE)[1:length(e@administered_item_index)])
      info_mean <- mean(info_pool)
      info_real <- mean(info_pool[e@administered_item_index])
      return((info_real - info_mean) / (info_best - info_mean))
    })
  )

  return(adaptivity_measures)

}
