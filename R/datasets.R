#' @include shadow_functions.R
NULL

#' Science dataset
#'
#' Item-based example item pool (1000 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool_science} An \code{\linkS4class{item_pool}} object.
#'   \item \code{itemattrib_science} A data frame containing item attributes.
#'   \item \code{constraints_science} A list containing 36 constraints.
#' }
#'
#' Also, the following datasets are intended for illustrating expected data structures. See examples below.
#' \itemize{
#'   \item \code{itempool_science_data} Item parameters.
#'   \item \code{itemattrib_science_data} Item attributes.
#'   \item \code{constraints_science_data} Constraints.
#' }
#'
#' @examples
#' ## Write to tempdir() and clean afterwards
#' f <- file.path(tempdir(), "itempool_science.csv")
#' write.csv(itempool_science_data, f, row.names = FALSE)
#' itempool_science <- loadItemPool(f)
#' file.remove(f)
#'
#' f <- file.path(tempdir(), "itemattrib_science.csv")
#' write.csv(itemattrib_science_data, f, row.names = FALSE)
#' itemattrib_science <- loadItemAttrib(f, itempool_science)
#' file.remove(f)
#'
#' f <- file.path(tempdir(), "constraints_science.csv")
#' write.csv(constraints_science_data, f, row.names = FALSE)
#' constraints_science <- loadConstraints(f,
#'   itempool_science, itemattrib_science)
#' file.remove(f)
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
#'   \item \code{itemattrib_reading} A data frame containing item attributes.
#'   \item \code{stimattrib_reading} A data frame containing stimulus attributes.
#'   \item \code{constraints_reading} A list containing 18 constraints.
#' }
#'
#' Also, the following datasets are intended for illustrating expected data structures. See examples below.
#' \itemize{
#'   \item \code{itempool_reading_data} Item parameters.
#'   \item \code{itemattrib_reading_data} Item attributes.
#'   \item \code{stimattrib_reading_data} Item attributes.
#'   \item \code{constraints_reading_data} Constraints.
#' }
#'
#' @examples
#' ## Write to tempdir() and clean afterwards
#' f <- file.path(tempdir(), "itempool_reading.csv")
#' write.csv(itempool_reading_data, f, row.names = FALSE)
#' itempool_reading <- loadItemPool(f)
#' file.remove(f)
#'
#' f <- file.path(tempdir(), "itemattrib_reading.csv")
#' write.csv(itemattrib_reading_data, f, row.names = FALSE)
#' itemattrib_reading <- loadItemAttrib(f, itempool_reading)
#' file.remove(f)
#'
#' f <- file.path(tempdir(), "stimattrib_reading.csv")
#' write.csv(stimattrib_reading_data, f, row.names = FALSE)
#' stimattrib_reading <- loadStAttrib(f, itemattrib_reading)
#' file.remove(f)
#'
#' f <- file.path(tempdir(), "constraints_reading.csv")
#' write.csv(constraints_reading_data, f, row.names = FALSE)
#' constraints_reading <- loadConstraints(f,
#'   itempool_reading, itemattrib_reading, stimattrib_reading)
#' file.remove(f)
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
#'   \item \code{itemattrib_fatigue} A data frame containing item attributes.
#'   \item \code{constraints_fatigue} A list containing 111 constraints.
#' }
#'
#' Also, the following datasets are intended for illustrating expected data structures. See examples below.
#' \itemize{
#'   \item \code{itempool_fatigue_data} Item parameters.
#'   \item \code{itemattrib_fatigue_data} Item attributes.
#'   \item \code{itemcontent_fatigue_data} Item contents.
#'   \item \code{constraints_fatigue_data} Constraints.
#'   \item \code{resp_fatigue_data} Raw response data.
#' }
#'
#' @examples
#' ## Write to tempdir() and clean afterwards
#' f <- file.path(tempdir(), "itempool_fatigue.csv")
#' write.csv(itempool_fatigue_data, f, row.names = FALSE)
#' itempool_fatigue <- loadItemPool(f)
#' file.remove(f)
#'
#' f <- file.path(tempdir(), "itemattrib_fatigue.csv")
#' write.csv(itemattrib_fatigue_data, f, row.names = FALSE)
#' itemattrib_fatigue <- loadItemAttrib(f, itempool_fatigue)
#' file.remove(f)
#'
#' f <- file.path(tempdir(), "constraints_fatigue.csv")
#' write.csv(constraints_fatigue_data, f, row.names = FALSE)
#' constraints_fatigue <- loadConstraints(f,
#'   itempool_fatigue, itemattrib_fatigue)
#' file.remove(f)
#'
#' ## Item contents for use in shiny app
#' f <- file.path(tempdir(), "itemcontent_fatigue.csv")
#' write.csv(itemcontent_fatigue_data, f, row.names = FALSE)
#' file.remove(f)
#'
#' ## Raw item responses for reference
#' f <- file.path(tempdir(), "resp_fatigue.csv")
#' write.table(resp_fatigue_data, f, row.names = FALSE, col.names = FALSE, sep = ",")
#' file.remove(f)
#'
#' @aliases itempool_fatigue_data itemattrib_fatigue_data itemcontent_fatigue_data constraints_fatigue_data resp_fatigue_data itempool_fatigue itemattrib_fatigue constraints_fatigue
#'
#' @docType data
#' @keywords datasets
#' @name dataset_fatigue
#' @rdname dataset_fatigue

NULL
