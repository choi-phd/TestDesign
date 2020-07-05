#' @include item_functions.R
NULL

#' Load item paramaters
#'
#' \code{\link{loadItemPool}} is a data loading function to create an \linkS4class{item_pool} class.
#' \code{\link{loadItemPool}} can read item parameters and standard errors from a data.frame or a .csv file.
#'
#' @param ipar Item parameters. Can be a data.frame or the file path of a .csv file. The content should at least include columns 'ID' and 'MODEL'.
#' @param ipar_se (Optional) Standard errors. Can be a data.frame or file path of a .csv file.
#' @param file (Deprecated) Use 'ipar' above.
#' @param se_file (Deprecated) Use 'ipar_se' above.
#'
#' @return An \code{\linkS4class{item_pool}} object.
#'
#' @examples
#' ## Read from data.frame:
#' itempool_science <- loadItemPool(itempool_science_data)
#'
#' ## Read from file: write to tempdir() for illustration and clean afterwards
#' f <- file.path(tempdir(), "itempool_science.csv")
#' write.csv(itempool_science_data, f, row.names = FALSE)
#' itempool_science <- loadItemPool(f)
#' file.remove(f)
#'
#' ## TestDesign 1.1.0 - Deprecated arguments
#' \dontrun{
#' loadItemPool(ipar = "ipar.csv", ipar_se = "se.csv") # is equivalent to
#' loadItemPool(file = "ipar.csv", se_file = "se.csv") # pre 1.1.0
#' }
#'
#' @seealso \link{dataset_science} for example usage.
#'
#' @export
loadItemPool <- function(ipar, ipar_se = NULL, file = NULL, se_file = NULL) {

  if (!missing("se_file")){
    warning("Argument deprecated. Use 'ipar_se' instead.")
    ipar_se <- se_file
  }
  if (!missing("file")){
    warning("Argument deprecated. Use 'ipar' instead.")
    ipar <- file
  }

  if (!is.null(ipar)) {
    if (inherits(ipar, "data.frame")) {
      ipar <- ipar
    } else if (inherits(ipar, "character")) {
      ipar <- read.csv(ipar, header = TRUE, as.is = TRUE)
    } else if (inherits(ipar, "SingleGroupClass")) {
      if (requireNamespace("mirt", quietly = TRUE)) {
        if (ipar@Model$nfact > 1) {
          stop(sprintf("model is not unidimensional: %s factors", ipar@Model$nfact))
        }
        tmp     <- mirt::coef(ipar, IRTpars = TRUE, simplify = TRUE)$items
        item_id <- rownames(tmp)
        item_m  <- ipar@Model$itemtype
        item_m[which(item_m == "2PL")]    <- "2PL"
        item_m[which(item_m == "3PL")]    <- "3PL"
        item_m[which(item_m == "graded")] <- "GR"
        item_m[which(item_m == "gpcm")]   <- "GPC"
        ipar <- data.frame(ID = item_id, MODEL = item_m, tmp, row.names = NULL)
        unsupported <- item_m[!item_m %in% c("2PL", "3PL", "graded", "gpcm")]
        if (length(unsupported) > 0) {
          stop(sprintf(
            "unrecognized itemtype: %s",
            paste0(unsupported, collapse = " ")))
        }
      } else {
        stop("'mirt' package required to read SingleGroupClass objects")
      }
    }
  }

  pool       <- new("item_pool")
  pool@raw   <- ipar
  ni         <- nrow(ipar)
  pool@index <- 1:ni
  pool@id    <- as.character(ipar[[1]])
  model      <- ipar[[2]]
  NCAT       <- numeric(ni)
  parms      <- vector(mode = "list", length = ni)
  nfields    <- rowSums(!is.na(ipar))
  valid      <- logical(ni)
  pool@ipar  <- matrix(NA, nrow = ni, ncol = max(nfields) - 2)

  load_se <- FALSE

  if (!is.null(ipar_se)) {
    if (inherits(ipar_se, "data.frame")) {
      ipar_se <- ipar_se
      load_se <- TRUE
    } else if (inherits(ipar_se, "character")) {
      ipar_se <- read.csv(ipar_se, header = TRUE, as.is = TRUE)
      load_se <- TRUE
    }
  }

  if (load_se) {
    se <- matrix(NA, nrow = ni, ncol = max(nfields) - 2)
  }

  for (i in 1:ni) {
    if (model[i] == 1 | model[i] == "1PL") {
      NCAT[i] <- 2
      b <- ipar[[3]][i]
      pool@model[i] <- "item_1PL"
      parms[[i]] <- new("item_1PL", difficulty = b)
      valid[i] <- TRUE

      j <- 1
      pool@ipar[i, j] <- b
      if (load_se) {
        se[i, j] <- as.matrix(ipar_se[i, 2 + j])
      }
    } else if (model[i] == 2 | model[i] == "2PL") {
      NCAT[i] <- 2
      a <- ipar[[3]][i]
      b <- ipar[[4]][i]
      if (a > 0) {
        pool@model[i] <- "item_2PL"
        parms[[i]] <- new("item_2PL", slope = a, difficulty = b)
        valid[i] <- TRUE

        j <- 1:2
        pool@ipar[i, j] <- c(a, b)
        if (load_se) {
          se[i, j] <- as.matrix(ipar_se[i, 2 + j])
        }
      }
    } else if (model[i] == 3 | model[i] == "3PL") {
      NCAT[i] <- 2
      a <- ipar[[3]][i]
      b <- ipar[[4]][i]
      c <- ipar[[5]][i]
      if (a > 0 && c >= 0 && c < 1) {
        pool@model[i] <- "item_3PL"
        parms[[i]] <- new("item_3PL", slope = a, difficulty = b, guessing = c)
        valid[i] <- TRUE

        j <- 1:3
        pool@ipar[i, j] <- c(a, b, c)
        if (load_se) {
          se[i, j] <- as.matrix(ipar_se[i, 2 + j])
        }
      }
    } else if (model[i] == 4 | model[i] == "PC") {
      NCAT[i] <- nfields[i] - 1
      b <- as.numeric(ipar[i, 3:nfields[i]])
      pool@model[i] <- "item_PC"
      parms[[i]] <- new("item_PC", threshold = b, ncat = NCAT[i])
      valid[i] <- TRUE

      j <- 1:(NCAT[i] - 1)
      pool@ipar[i, j] <- b
      if (load_se) {
        se[i, j] <- as.matrix(ipar_se[i, 2 + j])
      }
    } else if (model[i] == 5 | model[i] == "GPC") {
      NCAT[i] <- nfields[i] - 2
      a <- as.numeric(ipar[[3]][i])
      b <- as.numeric(ipar[i, 4:nfields[i]])
      if (a > 0) {
        pool@model[i] <- "item_GPC"
        parms[[i]] <- new("item_GPC", slope = a, threshold = b, ncat = NCAT[i])
        valid[i] <- TRUE

        j <- 1:NCAT[i]
        pool@ipar[i, j] <- c(a, b)
        if (load_se) {
          se[i, j] <- as.matrix(ipar_se[i, 2 + j])
        }
      }
    } else if (model[i] == 6 | model[i] == "GR") {
      NCAT[i] <- nfields[i] - 2
      a <- as.numeric(ipar[[3]][i])
      b <- as.numeric(ipar[i, 4:nfields[i]])
      if (a > 0 && (!is.unsorted(b))) {
        pool@model[i] <- "item_GR"
        parms[[i]] <- new("item_GR", slope = a, category = b, ncat = NCAT[i])
        valid[i] <- TRUE

        j <- 1:NCAT[i]
        pool@ipar[i, j] <- c(a, b)
        if (load_se) {
          se[i, j] <- as.matrix(ipar_se[i, 2 + j])
        }
      }
    } else {
      stop(paste("Item", i, ": unknown IRT model specified - valid models are 1: 1PL, 2: 2PL, 3: 3PL, 4: PC, 5: GPC, or 6: GR"))
    }
  }
  if (sum(!valid) > 0) {
    stop(paste("Check the parameters for the following item(s):", paste((1:ni)[!valid], collapse = ", "), "\n"))
  }

  pool@ni <- ni
  pool@max_cat <- max(NCAT)
  pool@NCAT <- NCAT
  pool@parms <- parms

  if (load_se) {
    pool@se <- se
  } else {
    pool@se <- pool@ipar * 0
  }
  if (max(rowSums(!is.na(pool@ipar))) != max(nfields) - 2) {
    pool@ipar <- pool@ipar[, 1:max(rowSums(!is.na(pool@ipar)))]
  }

  tmp <- pool@raw
  tmp[, 3:max(nfields)] <- pool@se
  pool@raw_se <- tmp

  if (validObject(pool)) {
    return(pool)
  }
}

#' @rdname loadItemAttrib
setClass("item_attrib",
  slots = c(
    data = "data.frame"
  ),
  prototype = list(
    data = data.frame()
  ),
  validity = function(object) {
    if (!("ID" %in% names(object@data))) {
      stop("The 'ID' column must be present.")
    }
    if (any(object@data[["ID"]] %in% c("", " ", "NA", "N/A"))) {
      stop("The 'ID' column in must not include empty or NA values.")
    }
    if (length(unique(object@data[["ID"]])) != nrow(object@data)) {
      stop("The 'ID' column in must not have any duplicate values.")
    }
    if (!identical(object@data[["INDEX"]], 1:length(object@data[["INDEX"]]))) {
      stop(sprintf("The 'INDEX' column must be equal to 1:%s.", length(object@data[["INDEX"]])))
    }
    return(TRUE)
  }
)

#' Load item attributes
#'
#' Read item attributes from specified file.
#'
#' @param object Item attributes. Can be a data.frame or the file path of a .csv file. The content should at least include column 'ID' that matches the item pool.
#' @param pool An \code{\linkS4class{item_pool}} object. Use \code{\link{loadItemPool}} for this.
#' @param file (Deprecated) Use 'object' above.
#'
#' @return An \code{\linkS4class{item_attrib}} object.
#'
#' @examples
#' ## Read from data.frame:
#' itempool_science   <- loadItemPool(itempool_science_data)
#' itemattrib_science <- loadItemAttrib(itemattrib_science_data, itempool_science)
#'
#' ## Read from file: write to tempdir() for illustration and clean afterwards
#' f <- file.path(tempdir(), "itemattrib_science.csv")
#' write.csv(itemattrib_science_data, f, row.names = FALSE)
#' itemattrib_science <- loadItemAttrib(f, itempool_science)
#' file.remove(f)
#'
#' ## TestDesign 1.1.0 - Deprecated arguments
#' \dontrun{
#' loadItemAttrib(object = "iatt.csv", pool) # is equivalent to
#' loadItemAttrib(file   = "iatt.csv", pool) # pre 1.1.0
#' }
#'
#' @seealso \link{dataset_science} for example usage.
#'
#' @export
loadItemAttrib <- function(object, pool, file = NULL) {

  if (is.null(pool) || !inherits(pool, "item_pool")) {
    stop("'pool' is missing or is not an 'item_pool' object.")
  }

  if (!missing("file")){
    warning("Argument deprecated. Use 'object' instead.")
    object <- file
  }
  if (!is.null(object)) {
    if (inherits(object, "data.frame")) {
      item_attrib <- object
    } else if (inherits(object, "character")) {
      item_attrib <- read.csv(object, header = TRUE, as.is = TRUE)
    }
  }

  names(item_attrib) <- toupper(names(item_attrib))

  if (is.numeric(item_attrib[["ID"]])) {
    item_attrib[["ID"]] <- as.character(item_attrib[["ID"]])
  }

  if (!all(sort(pool@id) == sort(item_attrib[["ID"]]))) {
    stop("The 'ID' values must match pool@id.")
  } else if (!all(pool@id == item_attrib[["ID"]])) {
    item_attrib <- merge(data.frame(ID = pool@id), item_attrib, by = "ID")[, names(item_attrib)] # re-ordering cols in attrib
  }

  if ("INDEX" %in% names(item_attrib)) {
    warning("The 'INDEX' column was ignored and replaced with valid values.")
  }
  item_attrib <- data.frame(cbind(INDEX = 1:nrow(item_attrib), item_attrib))

  if (nrow(item_attrib) != pool@ni) {
    stop("The number of rows must match pool@ni.")
  }

  if ("STID" %in% names(item_attrib)) {
    idx <- item_attrib[["STID"]] %in% c("", " ", "N/A")
    if (any(idx)) {
      item_attrib[["STID"]][idx] <- NA
    }
    if (is.numeric(item_attrib[["STID"]])) {
      item_attrib[["STID"]] <- as.character(item_attrib[["STID"]])
    }
  }

  out <- new("item_attrib")
  out@data <- item_attrib

  if (validObject(out)) {
    return(out)
  }
}

#' @rdname loadStAttrib
setClass("st_attrib",
  slots = c(
    data = "data.frame"
  ),
  prototype = list(
    data = data.frame()
  ),
  validity = function(object) {
    if (!("STID" %in% names(object@data))) {
      stop("The 'STID' column must be present.")
    }
    if (any(object@data[["STID"]] %in% c("", " ", NA))) {
      stop("The 'STID' column in must not include empty or NA values.")
    }
    if (length(unique(object@data[["STID"]])) != nrow(object@data)) {
      stop("The 'STID' column in must not have any duplicate values.")
    }
    if (!identical(object@data[["STINDEX"]], 1:length(object@data[["STINDEX"]]))) {
      stop(sprintf("The 'STINDEX' column must be equal to 1:%s.", length(object@data[["STINDEX"]])))
    }
    return(TRUE)
  }
)

setClassUnion("stattrib_or_null", c("st_attrib", "NULL"))

#' Load set/stimulus/passage attributes
#'
#' Read set, stimulus, or passage attributes from specified file.
#'
#' @param object Set attributes. Can be a file path of a .csv file, or a data.frame. The content should at least include the column 'STID' referring to the same column in item attributes.
#' @param item_attrib An \code{\linkS4class{item_attrib}} object containing item attributes. Use \code{\link{loadItemAttrib}} for this.
#' @param file (Deprecated) Use 'object' above.
#'
#' @return A \code{\linkS4class{st_attrib}} object containing stimulus attributes.
#'
#' @examples
#' ## Read from data.frame:
#' itempool_reading   <- loadItemPool(itempool_reading_data)
#' itemattrib_reading <- loadItemAttrib(itemattrib_reading_data, itempool_reading)
#' stimattrib_reading <- loadStAttrib(stimattrib_reading_data, itemattrib_reading)
#'
#' ## Read from file: write to tempdir() for illustration and clean afterwards
#' f <- file.path(tempdir(), "stimattrib_reading.csv")
#' write.csv(stimattrib_reading_data, f, row.names = FALSE)
#' stimattrib_reading <- loadStAttrib(f, itemattrib_reading)
#' file.remove(f)
#'
#' ## TestDesign 1.1.0 - Deprecated arguments
#' \dontrun{
#' loadStAttrib(object = "satt.csv", item_attrib) # is equivalent to
#' loadStAttrib(file   = "satt.csv", item_attrib) # pre 1.1.0
#' }
#'
#' @seealso \link{dataset_reading} for example usage.
#'
#' @export
loadStAttrib <- function(object, item_attrib, file = NULL) {

  if (is.null(item_attrib) || !inherits(item_attrib, "item_attrib")) {
    stop("'item_attrib' is missing or is not an 'item_attrib' object.")
  }

  if (!missing("file")){
    warning("Argument deprecated. Use 'object' instead.")
    object <- file
  }

  if (!is.null(object)) {
    if (inherits(object, "data.frame")) {
      st_attrib <- object
    } else if (inherits(object, "character")) {
      st_attrib <- read.csv(object, header = TRUE, as.is = TRUE)
    }
  }

  names(st_attrib) <- toupper(names(st_attrib))

  if (is.numeric(st_attrib[["STID"]])) {
    st_attrib[["STID"]] <- as.character(st_attrib[["STID"]])
  }
  if ("STINDEX" %in% names(st_attrib)) {
    warning("The 'STINDEX' column was ignored and replaced with valid values.")
  }
  st_attrib <- data.frame(cbind(STINDEX = 1:nrow(st_attrib), st_attrib))

  if (!("STID" %in% names(item_attrib@data))) {
    stop("'item_attrib' must have 'STID' column.")
  }
  if (!all(unique(na.omit(item_attrib@data[["STID"]])) %in% st_attrib[["STID"]])) {
    stop("The 'STID' column in 'st_attrib' content must have all unique values in the 'STID' column of 'item_attrib' content.")
  }

  out <- new("st_attrib")
  out@data <- st_attrib

  if (validObject(out)) {
    return(out)
  }
}

#' An S4 class to represent a single constraint
#'
#' An S4 class to represent a single constraint.
#'
#' @slot constraint Character. The index of the constraint.
#' @slot mat A matrix representing the left-hand side weights. Has nc rows.
#' @slot dir A vector of length nc. Each entry represents a logical operator relating the left-hand side to the right-hand side.
#' @slot rhs A vector of length nc. Each entry represents the right-hand side of the constraint.
#' @slot nc Numeric. The number of constraints represented in the constraint set.
#' @slot suspend \code{TRUE} if the constraint is to be turned off.
setClass("constraint",
  slots = c(
    constraint = "character",
    mat        = "matrix",
    dir        = "character",
    rhs        = "numeric",
    nc         = "numeric",
    suspend    = "logical"
  ),
  prototype = list(
    constraint = character(0),
    mat        = matrix(NA, 0, 0),
    dir        = character(0),
    rhs        = numeric(0),
    nc         = 0,
    suspend    = FALSE
  ),
  validity = function(object) {
    err <- c()
    if (dim(object@mat)[1] != object@nc) {
      msg <- sprintf(
        "constraint %s: nrow(@mat) does not match @nc",
        object@constraint
      )
      err <- c(err, msg)
    }
    if (length(object@dir) != object@nc) {
      msg <- sprintf(
        "constraint %s: length(@dir) does not match @nc",
        object@constraint
      )
      err <- c(err, msg)
    }
    if (length(object@rhs) != object@nc) {
      msg <- sprintf(
        "constraint %s: length(@rhs) does not match @nc",
        object@constraint
      )
      err <- c(err, msg)
    }
    tmp <- !object@dir %in% c("==", "<=", ">=")
    if (any(tmp)) {
      msg <- sprintf(
        "constraint %s @dir: unexpected logical operator '%s'",
        object@constraint,
        object@dir[tmp]
      )
      err <- c(err, msg)
    }
    if (length(err) == 0) {
      return(TRUE)
    } else {
      return(err)
    }
  }
)

#' An S4 class to represent a set of constraints
#'
#' An S4 class to represent a set of constraints.
#'
#' @slot slope Numeric. A slope parameter value.
#' @slot difficulty Numeric. A difficulty parameter value.
setClass("constraints",
  slots = c(
    constraints = "data.frame",
    list_constraints = "list",
    pool = "item_pool",
    item_attrib = "item_attrib",
    st_attrib = "stattrib_or_null",
    test_length = "numeric",
    nv = "numeric",
    ni = "numeric",
    ns = "numeric",
    id = "character",
    index = "character",
    mat = "matrix",
    dir = "character",
    rhs = "numeric",
    set_based = "logical",
    item_order = "numeric_or_null",
    item_order_by = "character_or_null",
    stim_order = "numeric_or_null",
    stim_order_by = "character_or_null",
    item_index_by_stimulus = "list_or_null",
    stimulus_index_by_item = "numeric_or_null"
  ),
  prototype = list(
    constraints = data.frame(0),
    list_constraints = list(0),
    pool = new("item_pool"),
    item_attrib = new("item_attrib"),
    st_attrib = NULL,
    test_length = numeric(0),
    nv = numeric(0),
    ni = numeric(0),
    ns = numeric(0),
    id = character(0),
    index = character(0),
    mat = matrix(0),
    dir = character(0),
    rhs = numeric(0),
    set_based = logical(0),
    item_order = NULL,
    item_order_by = NULL,
    stim_order = NULL,
    stim_order_by = NULL,
    item_index_by_stimulus = list(0),
    stimulus_index_by_item = numeric(0)
  ),
  validity = function(object) {
    if (!all(names(object@constraints) %in% c("CONSTRAINT", "TYPE", "WHAT", "CONDITION", "LB", "UB", "ONOFF"))) {
      stop("Column names must include all of CONSTRAINT, TYPE, WHAT, CONDITION, LB, UB, and ONOFF. See vignette('constraints') for details.")
    }
    if (!any(is.na(object@constraints[["LB"]]) | is.na(object@constraints[["UB"]]))) {
      if (any(object@constraints[["LB"]] > object@constraints[["UB"]])) {
        stop("Specified UBs must be equal to or larger than LBs.")
      }
    } else if (any(is.na(object@constraints[["LB"]]) + is.na(object@constraints[["UB"]]) == 1)) {
      stop("In each constraint, LB and UB must be specified together or omitted together.")
    }
    if (!all(object@constraints[["TYPE"]] %in% c(
      "NUMBER", "COUNT", "ALLORNONE", "ALL OR NONE", "IIF", "MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE",
      "XOR", "ENEMY", "SUM", "AVERAGE", "MEAN", "INCLUDE", "EXCLUDE", "NOT", "ORDER"))) {
      stop("The 'TYPE' values must be from accepted values. See vignette('constraints') for details.")
    }
    return(TRUE)
  }
)

#' Load constraints
#'
#' Read constraints from specified file.
#'
#' Use \code{vignette("constraints")} for instructions on how to create the constraint file.
#'
#' @param object Constraint specifications. Can be a file path of a .csv file, or a data.frame. See vignette for the expected format.
#' @param pool An \code{item_pool} object. Use \code{\link{loadItemPool}} for this.
#' @param item_attrib An \code{item_attrib} object containing item attributes. Use \code{\link{loadItemAttrib}} for this.
#' @param st_attrib (Optional) An \code{st_attrib} object containing stimulus attributes. Use \code{\link{loadStAttrib}} for this.
#' @param file (Deprecated) Use 'object' above.
#'
#' @return A \code{constraints} object containing the parsed constraints, to be used in \code{\link{Static}} and \code{\link{Shadow}}.
#'
#' @examples
#' ## Read from data.frame:
#' itempool_science    <- loadItemPool(itempool_science_data)
#' itemattrib_science  <- loadItemAttrib(itemattrib_science_data, itempool_science)
#' constraints_science <- loadConstraints(constraints_science_data,
#'   itempool_science, itemattrib_science)
#'
#' ## Read from file: write to tempdir() for illustration and clean afterwards
#' f <- file.path(tempdir(), "constraints_science.csv")
#' write.csv(constraints_science_data, f, row.names = FALSE)
#' constraints_science <- loadConstraints(f,
#'   itempool_science, itemattrib_science)
#' file.remove(f)
#'
#' ## TestDesign 1.1.0 - Deprecated arguments
#' \dontrun{
#' loadConstraints(object = "consts.csv", pool, item_attrib) # is equivalent to
#' loadConstraints(file   = "consts.csv", pool, item_attrib) # pre 1.1.0
#' }
#'
#' @seealso \link{dataset_science} for example usage.
#'
#' @export
loadConstraints <- function(object, pool, item_attrib, st_attrib = NULL, file = NULL) {

  if (!inherits(pool, "item_pool")) {
    stop("'pool' must be an 'item_pool' object.")
  }
  if (!inherits(item_attrib, "item_attrib")) {
    stop("'item_attrib' must be an 'item_attrib' object.")
  }
  if (!is.null(st_attrib)) {
    if (!inherits(st_attrib, "st_attrib")) {
      stop("'st_attrib' must be a 'st_attrib' object.")
    }
  }

  if (!missing("file")){
    warning("Argument deprecated. Use 'object' instead.")
    object <- file
  }

  if (!is.null(object)) {
    if (inherits(object, "data.frame")) {
      constraints <- object
    } else if (inherits(object, "character")) {
      constraints <- read.csv(object,
        header = TRUE, as.is = TRUE,
        colClasses = c(
          "character", "character", "character", "character",
          "numeric", "numeric", "character"
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  names(constraints)     <- toupper(names(constraints))
  constraints[["TYPE"]]  <- toupper(constraints[["TYPE"]])
  constraints[["WHAT"]]  <- toupper(constraints[["WHAT"]])
  constraints[["COUNT"]] <- NA
  constraints[["ONOFF"]] <- toupper(constraints[["ONOFF"]])
  constraints[["ONOFF"]][is.na(constraints[["ONOFF"]])] <- ""
  ONOFF <- TRUE

  if ("CONSTRAINT" %in% names(constraints)) {
    if (any(constraints[["CONSTRAINT"]] != as.character(1:dim(constraints)[1])) |
      !inherits(constraints[["CONSTRAINT"]], "character")) {
      constraints[["CONSTRAINT"]] <- as.character(1:dim(constraints)[1])
      warning("The 'CONSTRAINT' column was ignored and replaced with valid values.")
    }
  }


  # Validation: Pool
  ni <- pool@ni
  ns <- 0
  x  <- numeric(ni)
  nc <- nrow(constraints)

  if (nrow(item_attrib@data) != ni) {
    stop("Number of rows of 'item_attrib' must match pool@ni.")
  }
  if (!all(pool@id == item_attrib@data[["ID"]])) {
    stop("'ID' entries in 'item_attrib' must match pool@id.")
  }
  list_constraints <- vector(mode = "list", length = nc)
  item_constraints <- which(constraints[["WHAT"]] == "ITEM")
  stim_constraints <- which(constraints[["WHAT"]] %in% c("STIMULUS", "PASSAGE", "SET", "TESTLET"))
  item_order <- item_order_by <- NULL
  stim_order <- stim_order_by <- NULL

  if (length(stim_constraints) > 0) {
    if (is.null(st_attrib)) {
      stop("'st_attrib' must be supplied to load stimulus-based constraints.")
    }
    if (!("STID" %in% names(item_attrib@data))) {
      stop("'item_attrib' content must include 'STID' column to load stimulus-based constraints.")
    }

    set_based <- TRUE
    constraints[["ST_COUNT"]] <- NA

    id <- c(item_attrib@data[["ID"]], st_attrib@data[["STID"]])
    ns <- nrow(st_attrib@data)
    nv <- ni + ns
    item_id_by_stimulus <- split(item_attrib@data[["ID"]], as.factor(item_attrib@data[["STID"]]))
    item_index_by_stimulus <- lapply(item_id_by_stimulus, function(x) which(item_attrib@data[["ID"]] %in% x))
    item_index_by_stimulus <- lapply(st_attrib@data[["STID"]], function(x) item_index_by_stimulus[[x]])
    if (any(item_attrib@data[["STID"]] %in% c("", " ", "N/A", "n/a"))) {
      item_attrib@data[["STID"]][item_attrib@data[["STID"]] %in% c("", " ", "N/A", "n/a")] <- NA
    }
    stimulus_id_by_item <- item_attrib@data[["STID"]]
    stimulus_index_by_item <- sapply(stimulus_id_by_item, function(x) { if (!is.na(x)) { which(st_attrib@data[["STID"]] == x) } else { NA } } )
    if (any(toupper(constraints[["CONDITION"]]) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET"))) {
      common_stimulus_length <- TRUE
    } else if (all(c("LB", "UB") %in% names(st_attrib@data))) {
      common_stimulus_length <- FALSE
      stimulus_length_LB <- st_attrib@data[["LB"]]
      stimulus_length_UB <- st_attrib@data[["UB"]]
      if (any(is.na(c(stimulus_length_LB, stimulus_length_UB))) || any(stimulus_length_LB > stimulus_length_UB) || any(c(stimulus_length_LB, stimulus_length_UB) < 0)) {
        stop("'st_attrib' contains missing or invalid entries in 'LB' and 'UB' columns.")
      }
    } else {
      stop("'st_attrib' should contain 'LB' and 'UB' columns. Otherwise, 'CONDITION' entries of 'file' content should include 'PER STIMULUS' entry.")
    }

  } else if (length(item_constraints) > 0) {
    set_based <- FALSE
    nv <- ni
    id <- item_attrib@data[["ID"]]
    item_index_by_stimulus <- NULL
    stimulus_index_by_item <- NULL
  } else {
    stop("'file' content must include at least one 'ITEM' entry in 'WHAT' column. For stimulus-based constraints, 'LB' and 'UB' should have an entry.")
  }

  for (index in item_constraints) {
    list_constraints[[index]] <- new("constraint")
    list_constraints[[index]]@constraint <- constraints[["CONSTRAINT"]][index]
    constraint_type_is_valid <- FALSE
    list_constraints[[index]]@suspend <- constraints[["ONOFF"]][index] == "OFF"

    if (constraints[["TYPE"]][index] %in% c("NUMBER", "COUNT")) {
      constraint_type_is_valid <- TRUE

      if (toupper(constraints[["CONDITION"]][index]) %in% c("", " ", "PER TEST", "TEST")) {

        constraints[["COUNT"]][index] <- dim(item_attrib@data)[1]

        test_length_LB <- round(constraints[["LB"]][index])
        test_length_UB <- round(constraints[["UB"]][index])
        if (any(c(test_length_LB, test_length_UB) < 0) || test_length_LB > test_length_UB) {
          stop(sprintf("constraint %s has invalid LB/UB", index))
        } else if (test_length_LB == test_length_UB) {
          test_length <- test_length_UB
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, 1:ni] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- test_length_UB
        } else {
          stop("LB and UB for ITEM NUMBER must be set equal.")
        }
        if (set_based && !common_stimulus_length) {
          n_LB_eq_UB <- sum(stimulus_length_LB == stimulus_length_UB)
          n_LB_ne_UB <- sum(stimulus_length_LB != stimulus_length_UB)
          tmp_mat <- matrix(0, nrow = ns + n_LB_ne_UB, ncol = nv)
          tmp_dir <- character(ns + n_LB_ne_UB)
          tmp_rhs <- rep(0, ns + n_LB_ne_UB)
          tmp_index <- 1
          for (s in 1:ns) {
            if (stimulus_length_LB[s] == stimulus_length_UB[s]) {
              tmp_mat[tmp_index, item_index_by_stimulus[[s]]] <- 1
              tmp_mat[tmp_index, ni + s] <- -stimulus_length_UB[s]
              tmp_dir[tmp_index] <- "=="
              tmp_index <- tmp_index + 1
            } else if (stimulus_length_LB[s] < stimulus_length_UB[s]) {
              tmp_mat[c(tmp_index, tmp_index + 1), item_index_by_stimulus[[s]]] <- 1
              tmp_mat[tmp_index, ni + s] <- -stimulus_length_LB[s]
              tmp_mat[tmp_index + 1, ni + s] <- -stimulus_length_UB[s]
              tmp_dir[tmp_index] <- ">="
              tmp_dir[tmp_index + 1] <- "<="
              tmp_index <- tmp_index + 2
            } else {
              stop(sprintf("stimulus %s contains improper LB/UB (LB > UB)", s))
            }
          }
          list_constraints[[index]]@mat <- rbind(list_constraints[[index]]@mat, tmp_mat)
          list_constraints[[index]]@dir <- c(list_constraints[[index]]@dir, tmp_dir)
          list_constraints[[index]]@rhs <- c(list_constraints[[index]]@rhs, tmp_rhs)
        }
      } else if (toupper(constraints[["CONDITION"]][index]) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {

        if (!set_based) {
          stop(sprintf("Constraints must include at least one 'STIMULUS' under WHAT for CONDITION: %s", toupper(constraints[["CONDITION"]][index])))
        }
        stimulus_length_LB <- round(constraints[["LB"]][index])
        stimulus_length_UB <- round(constraints[["UB"]][index])
        if (any(c(stimulus_length_LB, stimulus_length_UB) < 0) || stimulus_length_LB > stimulus_length_UB) {
          stop(sprintf("Constraint %s has invalid LB/UB", index))
        } else if (stimulus_length_LB == stimulus_length_UB) {
          list_constraints[[index]]@mat <- matrix(0, nrow = ns, ncol = nv)
          list_constraints[[index]]@dir <- rep("==", ns)
          list_constraints[[index]]@rhs <- rep(0, ns)
          for (s in 1:ns) {
            list_constraints[[index]]@mat[s, item_index_by_stimulus[[s]]] <- 1
            list_constraints[[index]]@mat[s, ni + s] <- -stimulus_length_UB
          }
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = ns * 2, ncol = nv)
          list_constraints[[index]]@dir <- rep(c(">=", "<="), ns)
          list_constraints[[index]]@rhs <- rep(0, ns * 2)
          for (s in 1:ns) {
            list_constraints[[index]]@mat[c(s * 2 - 1, s * 2), item_index_by_stimulus[[s]]] <- 1
            list_constraints[[index]]@mat[c(s * 2 - 1), ni + s] <- -stimulus_length_LB
            list_constraints[[index]]@mat[c(s * 2), ni + s] <- -stimulus_length_UB
          }
        }

      } else if (constraints[["CONDITION"]][index] %in% names(item_attrib@data)) {

        condition <- item_attrib@data[constraints[["CONDITION"]][index]]
        constraints[["COUNT"]][index] <- sum(!is.na(condition))
        condition <- condition[!is.na(unique(condition))]

        if (length(condition) == 0) {
          stop(sprintf("Constraint %s has 0 items meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
        }
        if (any(c(constraints[["LB"]][index], constraints[["UB"]][index]) < 0) || constraints[["LB"]][index] > constraints[["UB"]][index]) {
          stop(sprintf("Constraint %s has invalid LB/UB", index))
        } else if (constraints[["LB"]][index] == constraints[["UB"]][index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = length(condition), ncol = nv)
          list_constraints[[index]]@dir <- rep("==", length(condition))
          list_constraints[[index]]@rhs <- rep(constraints[["UB"]][index], length(condition))
          for (m in 1:length(condition)) {
            condition_met <- which(item_attrib@data[constraints[["CONDITION"]][index]] == condition[m])
            list_constraints[[index]]@mat[m, condition_met] <- 1
          }
        } else if (constraints[["LB"]][index] <= constraints[["UB"]][index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = 2 * length(condition), ncol = nv)
          list_constraints[[index]]@dir <- rep(c(">=", "<="), length(condition))
          list_constraints[[index]]@rhs <- rep(c(constraints[["LB"]][index], constraints[["UB"]][index]), length(condition))
          for (m in 1:length(condition)) {
            condition_met <- which(item_attrib@data[constraints[["CONDITION"]][index]] == condition[m])
            list_constraints[[index]]@mat[c(m * 2 - 1, m * 2), condition_met] <- 1
          }
        }

      } else {

        match_vec       <- with(item_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
        condition_met   <- which(match_vec)
        n_condition_met <- sum(match_vec)
        constraints[["COUNT"]][index] <- n_condition_met

        if (length(condition_met) == 0) {
          stop(sprintf("Constraint %s has 0 items meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
        }
        if (any(c(constraints[["LB"]][index], constraints[["UB"]][index]) < 0) || constraints[["LB"]][index] > constraints[["UB"]][index]) {
          stop(sprintf("Constraint %s has invalid LB/UB", index))
        } else if (constraints[["LB"]][index] == constraints[["UB"]][index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, condition_met] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- constraints[["UB"]][index]
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
          list_constraints[[index]]@mat[, condition_met] <- 1
          list_constraints[[index]]@dir <- c(">=", "<=")
          list_constraints[[index]]@rhs <- c(constraints[["LB"]][index], constraints[["UB"]][index])
        }
      }
    }

    if (constraints[["TYPE"]][index] %in% c("SUM", "AVERAGE", "MEAN")) {

      constraint_type_is_valid <- TRUE

      if (!(constraints[["CONDITION"]][index] %in% names(item_attrib@data))) {
        stop(sprintf("constraint %s: %s not found in item_attrib:", index, constraints[["CONDITION"]][index]))
      } else {
        if (any(is.na(item_attrib@data[[constraints[["CONDITION"]][index]]]))) {
          stop(sprintf("constraint %s: %s must not have a missing value", index, constraints[["CONDITION"]][index]))
        }
        if (any(c(constraints[["LB"]][index], constraints[["UB"]][index]) < 0) || constraints[["LB"]][index] > constraints[["UB"]][index]) {
          stop(sprintf("constraint %s has invalid LB/UB", index))
        } else if (constraints[["LB"]][index] == constraints[["UB"]][index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@dir <- "<="
          list_constraints[[index]]@rhs <- constraints[["UB"]][index]
          if (constraints[["TYPE"]][index] == "SUM") {
            list_constraints[[index]]@mat[1, 1:ni] <- item_attrib@data[[constraints[["CONDITION"]][index]]]
          } else if (constraints[["TYPE"]][index] %in% c("AVERAGE", "MEAN")) {
            list_constraints[[index]]@mat[1, 1:ni] <- item_attrib@data[[constraints[["CONDITION"]][index]]] / test_length_UB
          }
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
          list_constraints[[index]]@dir <- c(">=", "<=")
          list_constraints[[index]]@rhs <- c(constraints[["LB"]][index], constraints[["UB"]][index])
          if (constraints[["TYPE"]][index] == "SUM") {
            list_constraints[[index]]@mat[, 1:ni] <- item_attrib@data[[constraints[["CONDITION"]][index]]]
          } else if (constraints[["TYPE"]][index] %in% c("AVERAGE", "MEAN")) {
            list_constraints[[index]]@mat[1, 1:ni] <- item_attrib@data[[constraints[["CONDITION"]][index]]] / test_length_UB
            list_constraints[[index]]@mat[2, 1:ni] <- item_attrib@data[[constraints[["CONDITION"]][index]]] / test_length_LB
          }
        }
      }
    }

    if (constraints[["TYPE"]][index] == "INCLUDE") {

      constraint_type_is_valid <- TRUE
      match_vec       <- with(item_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
      condition_met   <- which(match_vec)
      n_condition_met <- sum(match_vec)
      constraints[["COUNT"]][index] <- n_condition_met

      if (length(condition_met) == 0) {
        stop(sprintf("Constraint %s has 0 items meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
        list_constraints[[index]]@mat[1, condition_met] <- 1
        list_constraints[[index]]@dir <- "=="
        if (set_based) {
          stimulus_to_include <- unique(stimulus_index_by_item[condition_met])
          stimulus_to_include <- stimulus_to_include[!is.na(stimulus_to_include)]
          list_constraints[[index]]@mat[1, ni + stimulus_to_include] <- 1
          list_constraints[[index]]@rhs <- length(condition_met) + length(stimulus_to_include)
        } else {
          list_constraints[[index]]@rhs <- length(condition_met)
        }
      }
    }
    if (constraints[["TYPE"]][index] %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {

      constraint_type_is_valid <- TRUE
      match_vec       <- with(item_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
      condition_met   <- which(match_vec)
      n_condition_met <- sum(match_vec)
      constraints[["COUNT"]][index] <- n_condition_met

      if (length(condition_met) == 0) {
        stop(sprintf("Constraint %s has 0 items meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
        list_constraints[[index]]@mat[1, condition_met] <- 1
        list_constraints[[index]]@dir <- "=="
        list_constraints[[index]]@rhs <- 0
      }
    }

    if (constraints[["TYPE"]][index] %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {

      constraint_type_is_valid <- TRUE
      match_vec       <- with(item_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
      condition_met   <- which(match_vec)
      n_condition_met <- sum(match_vec)
      constraints[["COUNT"]][index] <- n_condition_met

      n_met <- n_condition_met
      if (n_met < 2) {
        stop(sprintf("Constraint %s has < 2 items meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = (n_met * (n_met - 1)) / 2, ncol = nv)
        list_constraints[[index]]@dir <- rep("==", (n_met * (n_met - 1)) / 2)
        list_constraints[[index]]@rhs <- rep(0, (n_met * (n_met - 1)) / 2)
        tmp_index <- 0
        for (i in condition_met) {
          for (j in condition_met) {
            if (i < j) {
              tmp_index <- tmp_index + 1
              list_constraints[[index]]@mat[tmp_index, c(i, j)] <- c(1, -1)
            }
          }
        }
      }
    }

    if (constraints[["TYPE"]][index] %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {

      constraint_type_is_valid <- TRUE
      match_vec       <- with(item_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
      condition_met   <- which(match_vec)
      n_condition_met <- sum(match_vec)
      constraints[["COUNT"]][index] <- n_condition_met

      if (length(condition_met) < 2) {
        stop(sprintf("Constraint %s has < 2 items meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
        list_constraints[[index]]@mat[1, condition_met] <- 1
        list_constraints[[index]]@dir <- "<="
        list_constraints[[index]]@rhs <- 1
      }
    }

    if (constraints[["TYPE"]][index] == "ORDER") {

      constraint_type_is_valid <- TRUE

      if (!list_constraints[[index]]@suspend) {
        if (constraints[["CONDITION"]][index] %in% names(item_attrib@data)) {
          if (any(is.na(item_attrib@data[[constraints[["CONDITION"]][index]]]))) {
            stop(sprintf("Constraint %s: %s must not have a missing value", index, constraints[["CONDITION"]][index]))
          }
          item_order <- item_attrib@data[[constraints[["CONDITION"]][index]]]
          item_order_by <- constraints[["CONDITION"]][index]
        } else {
          stop(sprintf("Constraint %s is invalid: column '%s' not found in 'item_attrib'", index, constraints[["CONDITION"]][index]))
        }
      }
    }
    if (!constraint_type_is_valid) {
      stop(sprintf("Constraint %s has an invalid 'TYPE': %s", index, constraints[["TYPE"]][index]))
    }
    list_constraints[[index]]@nc <- nrow(list_constraints[[index]]@mat)
  }

  if (set_based) {
    for (index in stim_constraints) {
      list_constraints[[index]] <- new("constraint")
      list_constraints[[index]]@constraint <- constraints[["CONSTRAINT"]][index]
      constraint_type_is_valid <- FALSE
      list_constraints[[index]]@suspend <- constraints[["ONOFF"]][index] == "OFF"

      if (constraints[["TYPE"]][index] %in% c("NUMBER", "COUNT")) {

        constraint_type_is_valid <- TRUE
        constraints[["ST_COUNT"]][index] <- dim(st_attrib@data)[1]

        if (toupper(constraints[["CONDITION"]][index]) %in% c("", " ", "PER TEST")) {
          number_stimulus_LB <- round(constraints[["LB"]][index])
          number_stimulus_UB <- round(constraints[["UB"]][index])
          if (number_stimulus_LB == number_stimulus_UB) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
            list_constraints[[index]]@mat[1, (ni + 1):nv] <- 1
            list_constraints[[index]]@dir <- "=="
            list_constraints[[index]]@rhs <- number_stimulus_UB
          } else if (number_stimulus_LB < number_stimulus_UB) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
            list_constraints[[index]]@mat[, (ni + 1):nv] <- 1
            list_constraints[[index]]@dir <- c(">=", "<=")
            list_constraints[[index]]@rhs <- c(number_stimulus_LB, number_stimulus_UB)
          } else {
            stop(sprintf("constraint %s has invalid LB/UB", index))
          }
        } else if (constraints[["CONDITION"]][index] %in% names(st_attrib@data)) {

          condition <- st_attrib@data[constraints[["CONDITION"]][index]]
          constraints[["ST_COUNT"]][index] <- sum(!is.na(condition))
          condition <- condition[!is.na(unique(condition))]

          if (length(condition) == 0) {
            stop(sprintf("Constraint %s has 0 stimuli meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
          }
          if (any(c(constraints[["LB"]][index], constraints[["UB"]][index]) < 0) || constraints[["LB"]][index] > constraints[["UB"]][index]) {
            stop(sprintf("constraint %s has invalid LB/UB", index))
          } else if (constraints[["LB"]][index] == constraints[["UB"]][index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = length(condition), ncol = nv)
            list_constraints[[index]]@dir <- rep("==", length(condition))
            list_constraints[[index]]@rhs <- rep(constraints[["UB"]][index], length(condition))
            for (m in 1:length(condition)) {
              condition_met <- which(st_attrib@data[constraints[["CONDITION"]][index]] == condition[m])
              list_constraints[[index]]@mat[m, ni + condition_met] <- 1
            }
          } else if (constraints[["LB"]][index] <= constraints[["UB"]][index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2 * length(condition), ncol = nv)
            list_constraints[[index]]@dir <- rep(c(">=", "<="), length(condition))
            list_constraints[[index]]@rhs <- rep(c(constraints[["LB"]][index], constraints[["UB"]][index]), length(condition))
            for (m in 1:length(condition)) {
              condition_met <- which(item_attrib@data[constraints[["CONDITION"]][index]] == condition[m])
              list_constraints[[index]]@mat[c(m * 2 - 1, m * 2), ni + condition_met] <- 1
            }
          }

        } else {

          match_vec       <- with(st_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
          condition_met   <- which(match_vec)
          n_condition_met <- sum(match_vec)
          constraints[["ST_COUNT"]][index] <- n_condition_met

          if (length(condition_met) == 0) {
            stop(sprintf("Constraint %s has 0 stimuli meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
          }
          if (any(c(constraints[["LB"]][index], constraints[["UB"]][index]) < 0) || constraints[["LB"]][index] > constraints[["UB"]][index]) {
            stop(sprintf("Constraint %s has invalid LB/UB", index))
          } else if (constraints[["LB"]][index] == constraints[["UB"]][index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
            list_constraints[[index]]@mat[1, ni + condition_met] <- 1
            list_constraints[[index]]@dir <- "=="
            list_constraints[[index]]@rhs <- constraints[["UB"]][index]
          } else {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
            list_constraints[[index]]@mat[, ni + condition_met] <- 1
            list_constraints[[index]]@dir <- c(">=", "<=")
            list_constraints[[index]]@rhs <- c(constraints[["LB"]][index], constraints[["UB"]][index])
          }
        }
      }

      if (constraints[["TYPE"]][index] %in% c("SUM", "AVERAGE", "MEAN")) {

        constraint_type_is_valid <- TRUE

        if (!(constraints[["CONDITION"]][index] %in% names(st_attrib@data))) {
          stop(sprintf("Constraint %s has invalid: %s not found in item_attrib:", index, constraints[["CONDITION"]][index]))
        } else {
          if (any(is.na(st_attrib@data[[constraints[["CONDITION"]][index]]]))) {
            stop(sprintf("Constraint %s: %s must not have a missing value", index, constraints[["CONDITION"]][index]))
          }
          if (any(c(constraints[["LB"]][index], constraints[["UB"]][index]) < 0) || constraints[["LB"]][index] > constraints[["UB"]][index]) {
            stop(sprintf("Constraint %s has invalid LB/UB", index))
          } else if (constraints[["LB"]][index] == constraints[["UB"]][index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
            list_constraints[[index]]@dir <- "<="
            list_constraints[[index]]@rhs <- constraints[["UB"]][index]
            if (constraints[["TYPE"]][index] == "SUM") {
              list_constraints[[index]]@mat[1, (ni + 1):nv] <- st_attrib@data[[constraints[["CONDITION"]][index]]]
            } else if (constraints[["TYPE"]][index] %in% c("AVERAGE", "MEAN")) {
              list_constraints[[index]]@mat[1, (ni + 1):nv] <- st_attrib@data[[constraints[["CONDITION"]][index]]] / number_stimulus_UB
            }
          } else {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
            list_constraints[[index]]@dir <- c(">=", "<=")
            list_constraints[[index]]@rhs <- c(constraints[["LB"]][index], constraints[["UB"]][index])
            if (constraints[["TYPE"]][index] == "SUM") {
              list_constraints[[index]]@mat[, (ni + 1):nv] <- st_attrib@data[[constraints[["CONDITION"]][index]]]
            } else if (constraints[["TYPE"]][index] %in% c("AVERAGE", "MEAN")) {
              list_constraints[[index]]@mat[1, (ni + 1):nv] <- st_attrib@data[[constraints[["CONDITION"]][index]]] / number_stimulus_UB
              list_constraints[[index]]@mat[2, (ni + 1):nv] <- st_attrib@data[[constraints[["CONDITION"]][index]]] / number_stimulus_LB
            }
          }
        }
      }

      if (constraints[["TYPE"]][index] == "INCLUDE") {

        constraint_type_is_valid <- TRUE
        match_vec       <- with(st_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
        condition_met   <- which(match_vec)
        n_condition_met <- sum(match_vec)
        constraints[["ST_COUNT"]][index] <- n_condition_met

        if (length(condition_met) == 0) {
          stop(sprintf("Constraint %s has 0 stimuli meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, ni + condition_met] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- length(condition_met)
        }
      }

      if (constraints[["TYPE"]][index] %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {

        constraint_type_is_valid <- TRUE
        match_vec       <- with(st_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
        condition_met   <- which(match_vec)
        n_condition_met <- sum(match_vec)
        constraints[["ST_COUNT"]][index] <- n_condition_met

        if (length(condition_met) == 0) {
          stop(sprintf("Constraint %s has 0 stimuli meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, ni + condition_met] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- 0
          for (s in condition_met) {
            list_constraints[[index]]@mat[1, item_index_by_stimulus[[s]]] <- 1
          }
        }
      }
      if (constraints[["TYPE"]][index] %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {

        constraint_type_is_valid <- TRUE
        match_vec       <- with(st_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
        condition_met   <- which(match_vec)
        n_condition_met <- sum(match_vec)
        constraints[["ST_COUNT"]][index] <- n_condition_met

        n_met <- n_condition_met
        if (n_met < 2) {
          stop(sprintf("Constraint %s has < 2 stimuli meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = (n_met * (n_met - 1)) / 2, ncol = nv)
          list_constraints[[index]]@dir <- rep("==", (n_met * (n_met - 1)) / 2)
          list_constraints[[index]]@rhs <- rep(0, (n_met * (n_met - 1)) / 2)
          tmp_index <- 0
          for (i in condition_met) {
            for (j in condition_met) {
              if (i < j) {
                tmp_index <- tmp_index + 1
                list_constraints[[index]]@mat[tmp_index, ni + c(i, j)] <- c(1, -1)
              }
            }
          }
        }
      }
      if (constraints[["TYPE"]][index] %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {

        constraint_type_is_valid <- TRUE
        match_vec       <- with(st_attrib@data, eval(parse(text = constraints[["CONDITION"]][index])))
        condition_met   <- which(match_vec)
        n_condition_met <- sum(match_vec)
        constraints[["ST_COUNT"]][index] <- n_condition_met

        if (length(condition_met) < 2) {
          stop(sprintf("Constraint %s has < 2 stimuli meeting CONDITION: %s", index, constraints[["CONDITION"]][index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, ni + condition_met] <- 1
          list_constraints[[index]]@dir <- "<="
          list_constraints[[index]]@rhs <- 1
        }
      }
      if (constraints[["TYPE"]][index] == "ORDER") {

        constraint_type_is_valid <- TRUE

        if (!list_constraints[[index]]@suspend) {
          if (constraints[["CONDITION"]][index] %in% names(st_attrib@data)) {
            if (any(is.na(st_attrib@data[[constraints[["CONDITION"]][index]]]))) {
              stop(sprintf("Constraint %s: %s must not have a missing value", index, constraints[["CONDITION"]][index]))
            }
            stim_order <- st_attrib@data[[constraints[["CONDITION"]][index]]]
            stim_order_by <- constraints[["CONDITION"]][index]
          } else {
            stop(sprintf("Constraint %s is invalid: %s not found in st_attrib", index, constraints[["CONDITION"]][index]))
          }
        }
      }
      if (!constraint_type_is_valid) {
        stop(sprintf("Constraint %s, %s is not a valid constraint type", index, constraints[["TYPE"]][index]))
      }
      list_constraints[[index]]@nc <- nrow(list_constraints[[index]]@mat)
    }
  }

  index <- NULL
  mat   <- NULL
  dir   <- NULL
  rhs   <- NULL
  for (i in 1:nc) {
    if (constraints[["TYPE"]][i] != "ORDER" && !list_constraints[[i]]@suspend) {
      list_constraints[[i]]@nc <- nrow(list_constraints[[i]]@mat)
      mat   <- rbind(mat, list_constraints[[i]]@mat)
      dir   <- c(dir, list_constraints[[i]]@dir)
      rhs   <- c(rhs, list_constraints[[i]]@rhs)
      index <- c(index, rep(constraints[["CONSTRAINT"]][i], list_constraints[[i]]@nc))
    }
  }

  out <- new("constraints")
  out@constraints      <- constraints
  out@list_constraints <- list_constraints
  out@pool             <- pool
  out@item_attrib      <- item_attrib
  out@st_attrib        <- st_attrib
  out@test_length      <- test_length
  out@nv    <- nv
  out@ni    <- ni
  out@ns    <- ns
  out@id    <- id
  out@index <- index
  out@mat   <- mat
  out@dir   <- dir
  out@rhs   <- rhs
  out@set_based              <- set_based
  out@item_order             <- item_order
  out@item_order_by          <- item_order_by
  out@stim_order             <- stim_order
  out@stim_order_by          <- stim_order_by
  out@item_index_by_stimulus <- item_index_by_stimulus
  out@stimulus_index_by_item <- stimulus_index_by_item

  return(out)
}

#' Toggle constraints
#'
#' \code{\link{toggleConstraints}} is a function to toggle individual constraints in a \code{\linkS4class{constraints}} object.
#'
#' @param object a \code{\linkS4class{constraints}} object from \code{\link{loadConstraints}}.
#' @param on constraint indices to mark as active.
#' @param off constraint indices to mark as inactive.
#'
#' @return \code{\link{toggleConstraints}} returns the updated \code{\linkS4class{constraints}} object.
#'
#' @examples
#' constraints_science2 <- updateConstraints(constraints_science, off = 32:36)
#' constraints_science3 <- updateConstraints(constraints_science, on = 32:36)
#'
#' @export
toggleConstraints <- function(object, on = NULL, off = NULL) {
  nc <- nrow(object@constraints)
  if (length(intersect(on, off)) > 0) {
    stop("'on' and 'off' must not have a common value.")
  }
  if (!"ONOFF" %in% names(object@constraints)) {
    object@constraints[["ONOFF"]] <- ""
  }
  if (!is.null(on)) {
    if (any(!is.element(on, 1:nc))) {
      stop("'on' values should refer to constraint indices in 'object'.")
    }
    for (index in on) {
      object@list_constraints[[index]]@suspend <- FALSE
      object@constraints[index, "ONOFF"] <- ""
    }
  }
  if (!is.null(off)) {
    if (any(!is.element(off, 1:nc))) {
      stop("'off' values should refer to constraint indices in 'object'.")
    }
    for (index in off) {
      object@list_constraints[[index]]@suspend <- TRUE
      object@constraints[index, "ONOFF"] <- "OFF"
    }
  }

  index <- NULL
  mat   <- NULL
  dir   <- NULL
  rhs   <- NULL

  for (i in 1:nc) {
    if (object@constraints[["TYPE"]][i] != "ORDER" && !object@list_constraints[[i]]@suspend) {
      object@list_constraints[[i]]@nc <- nrow(object@list_constraints[[i]]@mat)
      mat   <- rbind(mat, object@list_constraints[[i]]@mat)
      dir   <- c(dir, object@list_constraints[[i]]@dir)
      rhs   <- c(rhs, object@list_constraints[[i]]@rhs)
      index <- c(index, rep(object@constraints[["CONSTRAINT"]][i], object@list_constraints[[i]]@nc))
    }
  }

  object@index <- index
  object@mat   <- mat
  object@dir   <- dir
  object@rhs   <- rhs

  return(object)
}

#' @rdname toggleConstraints
#' @export
updateConstraints <- function(object, on = NULL, off = NULL) {
  .Deprecated("toggleConstraints", msg = "updateConstraints() is deprecated. Use toggleConstraints() instead.")
  o <- toggleConstraints(object, on, off)
  return(o)
}

#' Build constraints (shortcut to other loading functions)
#'
#' \code{\link{buildConstraints}} is a data loading function to create a \code{\linkS4class{constraints}} object.
#' \code{\link{buildConstraints}} is a shortcut that calls other data loading functions.
#' The constraints must be in the expected format; see the vignette in \code{vignette("constraints")}.
#'
#' @param object constraint specifications. Can be a data.frame or the file path of a .csv file. See the vignette for the expected format.
#' @param item_pool item parameters. Can be a \code{\linkS4class{item_pool}} object, a data.frame or the file path of a .csv file.
#' @param item_attrib item attributes. Can be an \code{\linkS4class{item_attrib}} object, a data.frame or the file path of a .csv file.
#' @param st_attrib (optional) stimulus attributes. Can be an \code{\linkS4class{st_attrib}} object, a data.frame or the file path of a .csv file.
#' @param pool (deprecated) use \code{item_pool} argument instead.
#' @param constraints (deprecated) use \code{object} argument instead.
#'
#' @return \code{\link{buildConstraints}} returns a \code{\linkS4class{constraints}} object. This object is used in \code{\link{Static}} and \code{\link{Shadow}}.
#'
#' @examples
#' ## Read from objects:
#' constraints_science <- buildConstraints(constraints_science_data,
#'   itempool_science, itemattrib_science)
#' constraints_reading <- buildConstraints(constraints_reading_data,
#'   itempool_reading, itemattrib_reading, stimattrib_reading)
#'
#' ## Read from data.frame:
#' constraints_science <- buildConstraints(constraints_science_data,
#'   itempool_science_data, itemattrib_science_data)
#' constraints_reading <- buildConstraints(constraints_reading_data,
#'   itempool_reading_data, itemattrib_reading_data, stimattrib_reading_data)
#'
#' ## Read from file: write to tempdir() for illustration and clean afterwards
#' f1 <- file.path(tempdir(), "constraints_science.csv")
#' f2 <- file.path(tempdir(), "itempool_science.csv")
#' f3 <- file.path(tempdir(), "itemattrib_science.csv")
#' write.csv(constraints_science_data, f1, row.names = FALSE)
#' write.csv(itempool_science_data   , f2, row.names = FALSE)
#' write.csv(itemattrib_science_data , f3, row.names = FALSE)
#' constraints_science <- buildConstraints(f1, f2, f3)
#' file.remove(f1)
#' file.remove(f2)
#' file.remove(f3)
#' @export
buildConstraints <- function(object, item_pool, item_attrib, st_attrib = NULL, pool = NULL, constraints = NULL) {

  if (!missing("pool")){
    warning("argument 'pool' is deprecated. Use 'item_pool' instead.")
    item_pool <- pool
  }
  if (!missing("constraints")){
    warning("argument 'constraints' is deprecated. Use 'object' instead.")
    object <- constraints
  }

  if (!inherits(item_pool, "item_pool")) {
    item_pool <- loadItemPool(item_pool)
  }
  if (!inherits(item_attrib, "item_attrib")) {
    item_attrib <- loadItemAttrib(item_attrib, item_pool)
  }
  if (!is.null(st_attrib)) {
    if (!inherits(st_attrib, "st_attrib")) {
      st_attrib <- loadStAttrib(st_attrib, item_attrib)
    }
  } else {
    st_attrib <- NULL
  }
  constraints <- loadConstraints(object, item_pool, item_attrib, st_attrib)
  return(constraints)
}
