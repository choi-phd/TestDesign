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
#'   \item \code{item.params.science.raw} Item parameters.
#'   \item \code{item.attrib.science.raw} Item attributes.
#'   \item \code{constraints.science.raw} Constraints.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset.science
#' @name dataset.science
#' @aliases item.params.science.raw item.attrib.science.raw constraints.science.raw itempool.science itemattrib.science constraints.science
#' @examples
#' \dontrun{
#' write.csv(item.params.science.raw, "item.params.science.raw.csv", row.names = F)
#' itempool.science = LoadItemPool("item.params.science.raw.csv")
#' 
#' write.csv(item.attrib.science.raw, "item.attrib.science.raw.csv", row.names = F)
#' itemattrib.science = LoadItemAttrib("item.attrib.science.raw.csv", itempool.science)
#' 
#' write.csv(constraints.science.raw, "constraints.science.raw.csv", row.names = F)
#' constraints.science = LoadConstraints("constraints.science.raw.csv",
#'     itempool.science, itemattrib.science)
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
#'   \item \code{item.params.reading.raw} Item parameters.
#'   \item \code{item.attrib.reading.raw} Item attributes.
#'   \item \code{stim.attrib.reading.raw} Item attributes.
#'   \item \code{constraints.reading.raw} Constraints.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset.reading
#' @name dataset.reading
#' @aliases item.params.reading.raw item.attrib.reading.raw stim.attrib.reading.raw constraints.reading.raw itempool.reading itemattrib.reading stimattrib.reading constraints.reading 
#' @examples
#' \dontrun{
#' write.csv(item.params.reading.raw, "item.params.reading.raw.csv", row.names = F)
#' itempool.reading = LoadItemPool("item.params.reading.raw.csv")
#' 
#' write.csv(item.attrib.reading.raw, "item.attrib.reading.raw.csv", row.names = F)
#' itemattrib.reading = LoadItemAttrib("item.attrib.reading.raw.csv", itempool.reading)
#' 
#' write.csv(stim.attrib.reading.raw, "stim.attrib.reading.raw.csv", row.names = F)
#' stimattrib.reading = LoadStAttrib("stim.attrib.reading.raw.csv", itemattrib.reading)
#' 
#' write.csv(constraints.reading.raw, "constraints.reading.raw.csv", row.names = F)
#' constraints.reading = LoadConstraints("constraints.reading.raw.csv",
#'     itempool.reading, itemattrib.reading, stimattrib.reading)
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
#'   \item \code{item.params.fatigue.raw} Item parameters.
#'   \item \code{item.attrib.fatigue.raw} Item attributes.
#'   \item \code{constraints.fatigue.raw} Constraints.
#'   \item \code{item.content.fatigue.raw} Item contents.
#'   \item \code{resp.fatigue.raw} Raw response data.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset.fatigue
#' @name dataset.fatigue
#' @aliases item.params.fatigue.raw item.attrib.fatigue.raw item.content.fatigue.raw constraints.fatigue.raw resp.fatigue.raw itempool.fatigue itemattrib.fatigue constraints.fatigue
#' @examples 
#' \dontrun{
#' write.csv(item.params.fatigue.raw, "item.params.fatigue.raw.csv", row.names = F)
#' itempool.fatigue = LoadItemPool("item.params.fatigue.raw.csv")
#' 
#' write.csv(item.attrib.fatigue, "item.attrib.fatigue.raw", row.names = F)
#' itemattrib.fatigue = LoadItemAttrib("item.attrib.fatigue.raw", itempool.fatigue)
#' 
#' write.csv(constraints.fatigue.raw, "constraints.fatigue.raw.csv", row.names = F)
#' constraints.fatigue = LoadConstraints("constraints.fatigue.raw.csv",
#'           itempool.fatigue, itemattrib.fatigue)
#' 
#' write.csv(item.content.fatigue.raw, "item.content.fatigue.raw.csv", row.names = F)
#' write.table(resp.fatigue.raw, "resp.fatigue.raw.csv", row.names = F, col.names = F, sep = ",")
#' }
NULL
