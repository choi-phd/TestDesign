#' Science dataset
#'
#' Item-based example item pool (1000 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool.science} An \code{\linkS4class{item.pool}} object.
#'   \item \code{itemattrib.science} A data frame containing item attributes.
#'   \item \code{constraints.science} A list containing 36 constraints.
#' }
#'
#' Also, the following datasets are intended for illustrating expected data structures. See examples below.
#' \itemize{
#'   \item \code{itempool.science.raw} Item parameters.
#'   \item \code{itemattrib.science.raw} Item attributes.
#'   \item \code{constraints.science.raw} Constraints.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset.science
#' @name dataset.science
#' @aliases itempool.science.raw itemattrib.science.raw constraints.science.raw itempool.science itemattrib.science constraints.science
#' @examples
#' \dontrun{
#' write.csv(itempool.science.raw, "itempool.science.csv", row.names = FALSE)
#' itempool.science <- loadItemPool("itempool.science.csv")
#'
#' write.csv(itemattrib.science.raw, "itemattrib.science.csv", row.names = FALSE)
#' itemattrib.science <- loadItemAttrib("itemattrib.science.csv", itempool.science)
#'
#' write.csv(constraints.science.raw, "constraints.science.csv", row.names = FALSE)
#' constraints.science <- loadConstraints(
#'   "constraints.science.csv",
#'   itempool.science, itemattrib.science
#' )
#' }
NULL

#' Reading dataset
#'
#' Stimulus-based example item pool (303 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool.reading} An \code{\linkS4class{item.pool}} object.
#'   \item \code{itemattrib.reading} A data frame containing item attributes.
#'   \item \code{stimattrib.reading} A data frame containing stimulus attributes.
#'   \item \code{constraints.reading} A list containing 18 constraints.
#' }
#'
#' Also, the following datasets are intended for illustrating expected data structures. See examples below.
#' \itemize{
#'   \item \code{itempool.reading.raw} Item parameters.
#'   \item \code{itemattrib.reading.raw} Item attributes.
#'   \item \code{stimattrib.reading.raw} Item attributes.
#'   \item \code{constraints.reading.raw} Constraints.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset.reading
#' @name dataset.reading
#' @aliases itempool.reading.raw itemattrib.reading.raw stimattrib.reading.raw constraints.reading.raw itempool.reading itemattrib.reading stimattrib.reading constraints.reading
#' @examples
#' \dontrun{
#' write.csv(itempool.reading.raw, "itempool.reading.csv", row.names = FALSE)
#' itempool.reading <- loadItemPool("itempool.reading.csv")
#'
#' write.csv(itemattrib.reading.raw, "itemattrib.reading.csv", row.names = FALSE)
#' itemattrib.reading <- loadItemAttrib("itemattrib.reading.csv", itempool.reading)
#'
#' write.csv(stimattrib.reading.raw, "stimattrib.reading.csv", row.names = FALSE)
#' stimattrib.reading <- loadStAttrib("stimattrib.reading.csv", itemattrib.reading)
#'
#' write.csv(constraints.reading.raw, "constraints.reading.csv", row.names = FALSE)
#' constraints.reading <- loadConstraints(
#'   "constraints.reading.csv",
#'   itempool.reading, itemattrib.reading, stimattrib.reading
#' )
#' }
NULL

#' Fatigue dataset
#'
#' Item-based example pool with item contents (95 items).
#'
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{itempool.fatigue} An \code{\linkS4class{item.pool}} object.
#'   \item \code{itemattrib.fatigue} A data frame containing item attributes.
#'   \item \code{constraints.fatigue} A list containing 111 constraints.
#' }
#'
#' Also, the following datasets are intended for illustrating expected data structures. See examples below.
#' \itemize{
#'   \item \code{itempool.fatigue.raw} Item parameters.
#'   \item \code{itemattrib.fatigue.raw} Item attributes.
#'   \item \code{itemcontent.fatigue.raw} Item contents.
#'   \item \code{constraints.fatigue.raw} Constraints.
#'   \item \code{resp.fatigue.raw} Raw response data.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset.fatigue
#' @name dataset.fatigue
#' @aliases itempool.fatigue.raw itemattrib.fatigue.raw itemcontent.fatigue.raw constraints.fatigue.raw resp.fatigue.raw itempool.fatigue itemattrib.fatigue constraints.fatigue
#' @examples
#' \dontrun{
#' write.csv(itempool.fatigue.raw, "itempool.fatigue.csv", row.names = FALSE)
#' itempool.fatigue <- loadItemPool("itempool.fatigue.csv")
#'
#' write.csv(itemattrib.fatigue, "itemattrib.fatigue.csv", row.names = FALSE)
#' itemattrib.fatigue <- loadItemAttrib("itemattrib.fatigue.csv", itempool.fatigue)
#'
#' write.csv(constraints.fatigue.raw, "constraints.fatigue.csv", row.names = FALSE)
#' constraints.fatigue <- loadConstraints(
#'   "constraints.fatigue.csv",
#'   itempool.fatigue, itemattrib.fatigue
#' )
#'
#' write.csv(itemcontent.fatigue.raw, "itemcontent.fatigue.csv", row.names = FALSE)
#' write.table(resp.fatigue.raw, "resp.fatigue.csv", row.names = FALSE, col.names = FALSE, sep = ",")
#' }
NULL
