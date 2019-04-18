#' par_science
#'
#' Item-based example item pool (1000 items).
#' 
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{par_science} Item parameters.
#'   \item \code{item_attrib_science} Item attributes.
#'   \item \code{constraints_science} A constraint set (36 constraints).
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_science
#' @name par_science
#' @examples
#' \dontrun{
#' write.csv(par_science, "par_science.csv", row.names = F)
#' itempool.science = LoadItemPool("par_science.csv")
#' 
#' write.csv(item_attrib_science, "item_attrib_science.csv", row.names = F)
#' itemattrib.science = LoadItemAttrib("item_attrib_science.csv", itempool.science)
#' 
#' write.csv(constraints_science, "constraints_science.csv", row.names = F)
#' constraints.science = LoadConstraints("constraints_science.csv",
#'           itempool.science, itemattrib.science)
#' }
NULL

#' item_attrib_science
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_science
#' @name item_attrib_science
NULL

#' constraints_science
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_science
#' @name constraints_science
NULL



#' par_reading
#'
#' Stimulus-based example item pool.
#' 
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{par_reading} Item parameters.
#'   \item \code{item_attrib_reading} Item attributes.
#'   \item \code{stimulus_attrib_reading} Stimulus attributes.
#'   \item \code{constraints_reading} A constraint set (18 constraints).
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_reading
#' @name par_reading
#' @examples
#' \dontrun{
#' write.csv(par_reading, "par_reading.csv", row.names = F)
#' itempool.reading = LoadItemPool("par_reading.csv")
#' 
#' write.csv(item_attrib_reading, "item_attrib_reading.csv", row.names = F)
#' itemattrib.reading = LoadItemAttrib("item_attrib_reading.csv", itempool.reading)
#' 
#' write.csv(stimulus_attrib_reading, "stimulus_attrib_reading.csv", row.names = F)
#' stimattrib.reading = LoadStAttrib("stimulus_attrib_reading.csv", itemattrib.reading)
#' 
#' write.csv(constraints_reading, "constraints_reading.csv", row.names = F)
#' constraints.reading = LoadConstraints("constraints_reading.csv",
#'           itempool.reading, itemattrib.reading, stimattrib.reading)
#' }
NULL

#' item_attrib_reading
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_reading
#' @name item_attrib_reading
NULL

#' stimulus_attrib_reading
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_reading
#' @name stimulus_attrib_reading
NULL

#' constraints_reading
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_reading
#' @name constraints_reading
NULL




#' par_fatigue
#'
#' An item-based example pool with item contents.
#' 
#' This pool is associated with the following objects:
#' \itemize{
#'   \item \code{par_fatigue} Item parameters.
#'   \item \code{item_attrib_fatigue} Item attributes.
#'   \item \code{item_content_fatigue} Item contents.
#'   \item \code{constraints_fatigue} A constraint set (111 constraints).
#'   \item \code{raw_fatigue} Raw response data.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_fatigue
#' @name par_fatigue
#' @examples 
#' \dontrun{
#' write.csv(par_fatigue, "par_fatigue.csv", row.names = F)
#' itempool.fatigue = LoadItemPool("par_fatigue.csv")
#' 
#' write.csv(item_attrib_fatigue, "item_attrib_fatigue.csv", row.names = F)
#' itemattrib.fatigue = LoadItemAttrib("item_attrib_fatigue.csv", itempool.fatigue)
#' 
#' write.csv(constraints_fatigue, "constraints_fatigue.csv", row.names = F)
#' constraints.fatigue = LoadConstraints("constraints_fatigue.csv",
#'           itempool.fatigue, itemattrib.fatigue)
#' }
NULL

#' item_attrib_fatigue
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_fatigue
#' @name item_attrib_fatigue
NULL

#' item_content_fatigue
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_fatigue
#' @name item_content_fatigue
NULL

#' constraints_fatigue
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_fatigue
#' @name constraints_fatigue
NULL

#' raw_fatigue
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_fatigue
#' @name raw_fatigue
NULL
