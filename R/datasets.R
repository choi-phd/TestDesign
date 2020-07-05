#' @include shadow_functions.R
NULL

#' Science dataset
#'
#' Item-based example item pool (1000 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool_science} An \code{\linkS4class{item_pool}} object containing 1000 items.
#'   \item \code{itemattrib_science} A \code{\linkS4class{item_attrib}} object containing 9 item-level attributes.
#'   \item \code{constraints_science} A \code{\linkS4class{constraints}} object containing 36 constraints.
#' }
#'
#' Also, the following objects are intended for illustrating expected data structures.
#' \itemize{
#'   \item \code{itempool_science_data} A \code{\link{data.frame}} containing item parameters.
#'   \item \code{itemattrib_science_data} A \code{\link{data.frame}} containing item attributes.
#'   \item \code{constraints_science_data} A \code{\link{data.frame}} containing constraint specifications.
#' }
#'
#' @examples
#' itempool_science    <- loadItemPool(itempool_science_data)
#' itemattrib_science  <- loadItemAttrib(itemattrib_science_data, itempool_science)
#' constraints_science <- loadConstraints(constraints_science_data,
#'   itempool_science, itemattrib_science)
#'
#' @aliases itempool_science_data itemattrib_science_data constraints_science_data itempool_science itemattrib_science constraints_science
#' @docType data
#' @keywords datasets
#' @name dataset_science
#' @rdname dataset_science
NULL

#' Reading dataset
#'
#' Stimulus-based example item pool (303 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool_reading} An \code{\linkS4class{item_pool}} object.
#'   \item \code{itemattrib_reading} An \code{\linkS4class{item_attrib}} object containing item attributes.
#'   \item \code{stimattrib_reading} A \code{\linkS4class{st_attrib}} object containing stimulus attributes.
#'   \item \code{constraints_reading} A \code{\linkS4class{constraints}} object containing 18 constraints.
#' }
#'
#' Also, the following objects are intended for illustrating expected data structures.
#' \itemize{
#'   \item \code{itempool_reading_data} A \code{\link{data.frame}} containing item parameters.
#'   \item \code{itemattrib_reading_data} A \code{\link{data.frame}} containing item attributes.
#'   \item \code{stimattrib_reading_data} A \code{\link{data.frame}} containing stimulus attributes.
#'   \item \code{constraints_reading_data} A \code{\link{data.frame}} containing constraint specifications.
#' }
#'
#' @examples
#' itempool_reading    <- loadItemPool(itempool_reading_data)
#' itemattrib_reading  <- loadItemAttrib(itemattrib_reading_data, itempool_reading)
#' stimattrib_reading  <- loadStAttrib(stimattrib_reading_data, itemattrib_reading)
#' constraints_reading <- loadConstraints(constraints_reading_data,
#'   itempool_reading, itemattrib_reading, stimattrib_reading)
#'
#' @aliases itempool_reading_data itemattrib_reading_data stimattrib_reading_data constraints_reading_data itempool_reading itemattrib_reading stimattrib_reading constraints_reading
#' @docType data
#' @keywords datasets
#' @name dataset_reading
#' @rdname dataset_reading
NULL

#' Fatigue dataset
#'
#' Item-based example pool with item contents (95 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool_fatigue} An \code{\linkS4class{item_pool}} object.
#'   \item \code{itemattrib_fatigue} An \code{\linkS4class{item_attrib}} object containing item attributes.
#'   \item \code{constraints_fatigue} A \code{\linkS4class{constraints}} object containing 111 constraints.
#' }
#'
#' Also, the following objects are intended for illustrating expected data structures.
#' \itemize{
#'   \item \code{itempool_fatigue_data} A \code{\link{data.frame}} containing item parameters.
#'   \item \code{itemattrib_fatigue_data} A \code{\link{data.frame}} containing item attributes.
#'   \item \code{itemcontent_fatigue_data} A \code{\link{data.frame}} containing item contents.
#'   \item \code{constraints_fatigue_data} A \code{\link{data.frame}} containing constraint specifications.
#'   \item \code{resp_fatigue_data} A \code{\link{data.frame}} containing raw response data.
#' }
#'
#' @examples
#' itempool_fatigue   <- loadItemPool(itempool_fatigue_data)
#' itemattrib_fatigue <- loadItemAttrib(itemattrib_fatigue_data, itempool_fatigue)
#' constraints_fatigue <- loadConstraints(constraints_fatigue_data,
#'   itempool_fatigue, itemattrib_fatigue)
#'
#' @aliases itempool_fatigue_data itemattrib_fatigue_data itemcontent_fatigue_data constraints_fatigue_data resp_fatigue_data itempool_fatigue itemattrib_fatigue constraints_fatigue
#'
#' @docType data
#' @keywords datasets
#' @name dataset_fatigue
#' @rdname dataset_fatigue
NULL

#' Bayes dataset
#'
#' Item-based example item pool with standard errors (320 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool_bayes} An \code{\linkS4class{item_pool}} object containing 320 items.
#'   \item \code{itemattrib_bayes} A \code{\linkS4class{item_attrib}} object containing 5 item-level attributes.
#'   \item \code{constraints_bayes} A \code{\linkS4class{constraints}} object containing 14 constraints.
#' }
#'
#' Also, the following objects are intended for illustrating expected data structures.
#' \itemize{
#'   \item \code{itempool_bayes_data} A \code{\link{data.frame}} containing item parameters.
#'   \item \code{itempool_se_bayes_data} A \code{\link{data.frame}} containing item parameter standard errors.
#'   \item \code{itemattrib_bayes_data} A \code{\link{data.frame}} containing item attributes.
#'   \item \code{constraints_bayes_data} A \code{\link{data.frame}} containing constraint specifications.
#' }
#'
#' @examples
#' itempool_bayes    <- loadItemPool(itempool_bayes_data, itempool_se_bayes_data)
#' itemattrib_bayes  <- loadItemAttrib(itemattrib_bayes_data, itempool_bayes)
#' constraints_bayes <- loadConstraints(constraints_bayes_data,
#'   itempool_bayes, itemattrib_bayes)
#'
#' \dontrun{
#'   View(itempool_bayes_data)
#'   View(itempool_se_bayes_data)
#'   View(itemattrib_bayes_data)
#'   View(constraints_bayes_data)
#' }
#'
#' @aliases itempool_bayes_data itempool_se_bayes_data itemattrib_bayes_data constraints_bayes_data itempool_bayes itemattrib_bayes constraints_bayes
#' @docType data
#' @keywords datasets
#' @name dataset_bayes
#' @rdname dataset_bayes
NULL
