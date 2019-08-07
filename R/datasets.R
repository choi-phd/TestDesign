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
#' Also, the following datasets are intended for illustrating expected data structures.
#' \itemize{
#'   \item \code{itempool_science_raw} Item parameters. Use \code{write.csv(itempool_science_raw, "itempool_science.csv", row.names = FALSE)} to save into a file.
#'   \item \code{itemattrib_science_raw} Item attributes. Use \code{write.csv(itemattrib_science_raw, "itemattrib_science.csv", row.names = FALSE)} to save into a file.
#'   \item \code{constraints_science_raw} Constraints. Use \code{write.csv(constraints_science_raw, "constraints_science.csv", row.names = FALSE)} to save into a file.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_science
#' @name dataset_science
#' @aliases itempool_science_raw itemattrib_science_raw constraints_science_raw itempool_science itemattrib_science constraints_science
#' @examples
#' \donttest{
#' itempool_science <- loadItemPool("itempool_science.csv")
#' itemattrib_science <- loadItemAttrib("itemattrib_science.csv", itempool_science)
#' constraints_science <- loadConstraints("constraints_science.csv",
#'   itempool_science, itemattrib_science)
#' }
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
#'   \item \code{itempool_reading_raw} Item parameters. Use \code{write.csv(itempool_reading_raw, "itempool_reading.csv", row.names = FALSE)} to save into a file.
#'   \item \code{itemattrib_reading_raw} Item attributes. Use \code{write.csv(itemattrib_reading_raw, "itemattrib_reading.csv", row.names = FALSE)} to save into a file.
#'   \item \code{stimattrib_reading_raw} Item attributes. Use \code{write.csv(stimattrib_reading_raw, "stimattrib_reading.csv", row.names = FALSE)} to save into a file.
#'   \item \code{constraints_reading_raw} Constraints. Use \code{write.csv(constraints_reading_raw, "constraints_reading.csv", row.names = FALSE)} to save into a file.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_reading
#' @name dataset_reading
#' @aliases itempool_reading_raw itemattrib_reading_raw stimattrib_reading_raw constraints_reading_raw itempool_reading itemattrib_reading stimattrib_reading constraints_reading
#' @examples
#' \donttest{
#' itempool_reading <- loadItemPool("itempool_reading.csv")
#' itemattrib_reading <- loadItemAttrib("itemattrib_reading.csv", itempool_reading)
#' stimattrib_reading <- loadStAttrib("stimattrib_reading.csv", itemattrib_reading)
#' constraints_reading <- loadConstraints("constraints_reading.csv",
#'   itempool_reading, itemattrib_reading, stimattrib_reading)
#' }
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
#'   \item \code{itempool_fatigue_raw} Item parameters. Use \code{write.csv(itempool_fatigue_raw, "itempool_fatigue.csv", row.names = FALSE)} to save into a file.
#'   \item \code{itemattrib_fatigue_raw} Item attributes. Use \code{write.csv(itemattrib_fatigue, "itemattrib_fatigue.csv", row.names = FALSE)} to save into a file.
#'   \item \code{itemcontent_fatigue_raw} Item contents. Use \code{write.csv(itemcontent_fatigue_raw, "itemcontent_fatigue.csv", row.names = FALSE)} to save into a file.
#'   \item \code{constraints_fatigue_raw} Constraints. Use \code{write.csv(constraints_fatigue_raw, "constraints_fatigue.csv", row.names = FALSE)} to save into a file.
#'   \item \code{resp_fatigue_raw} Raw response data. Use \code{write.table(resp_fatigue_raw, "resp_fatigue.csv", row.names = FALSE, col.names = FALSE, sep = ",")} to save into a file.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_fatigue
#' @name dataset_fatigue
#' @aliases itempool_fatigue_raw itemattrib_fatigue_raw itemcontent_fatigue_raw constraints_fatigue_raw resp_fatigue_raw itempool_fatigue itemattrib_fatigue constraints_fatigue
#' @examples
#' \donttest{
#' itempool_fatigue <- loadItemPool("itempool_fatigue.csv")
#' itemattrib_fatigue <- loadItemAttrib("itemattrib_fatigue.csv", itempool_fatigue)
#' constraints_fatigue <- loadConstraints("constraints_fatigue.csv",
#'   itempool_fatigue, itemattrib_fatigue)
#' }
NULL
