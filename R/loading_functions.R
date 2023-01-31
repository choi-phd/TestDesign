#' @include calc_prob_functions.r
#' @include calc_escore_functions.r
#' @include calc_location_functions.r
#' @include calc_fisher_functions.r
#' @include calc_loglikelihood_functions.r
#' @include calc_jacobian_functions.r
#' @include calc_hessian_functions.r
#' @include sim_resp_functions.r
NULL

#' Load item pool
#'
#' \code{\link{loadItemPool}} is a data loading function for creating an \code{\linkS4class{item_pool}} object.
#' \code{\link{loadItemPool}} can read item parameters and standard errors from a \code{\link{data.frame}} or a .csv file.
#'
#' @param ipar item parameters. Can be a \code{\link{data.frame}} or the file path of a .csv file. The content should at least include columns 'ID' and 'MODEL'.
#' @param ipar_se (optional) standard errors. Can be a \code{\link{data.frame}} or the file path of a .csv file.
#' @param unique if \code{TRUE}, item IDs must be unique to create a valid \code{\linkS4class{item_pool}} object. (default = \code{FALSE})
#'
#' @return \code{\link{loadItemPool}} returns an \code{\linkS4class{item_pool}} object.
#'
#' \itemize{
#'   \item{\code{ni}} the number of items in the pool.
#'   \item{\code{max_cat}} the maximum number of response categories across all items in the pool.
#'   \item{\code{index}} the numeric item index of each item.
#'   \item{\code{id}} the item ID string of each item.
#'   \item{\code{model}} the object class names of each item representing an item model type.
#'   Can be \code{\linkS4class{item_1PL}}, \code{\linkS4class{item_2PL}}, \code{\linkS4class{item_3PL}},
#'   \code{\linkS4class{item_PC}}, \code{\linkS4class{item_GPC}}, or \code{\linkS4class{item_GR}}.
#'   \item{\code{NCAT}} the number of response categories of each item.
#'   \item{\code{parms}} a list containing the item object of each item.
#'   \item{\code{ipar}} a matrix containing all item parameters.
#'   \item{\code{se}} a matrix containing all item parameter standard errors. The values will be 0 if the argument \code{ipar_se} was not supplied.
#'   \item{\code{raw}} the original input \code{\link{data.frame}} used to create this object.
#' }
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
#' @seealso \code{\link{dataset_science}}, \code{\link{dataset_reading}}, \code{\link{dataset_fatigue}}, \code{\link{dataset_bayes}} for examples.
#'
#' @export
loadItemPool <- function(ipar, ipar_se = NULL, unique = FALSE) {

  if (!is.null(ipar)) {
    while(TRUE) {
      if (inherits(ipar, "data.frame")) {
        ipar <- ipar
        break
      }
      if (inherits(ipar, "character")) {
        if (length(ipar) != 1) {
          stop("the 'ipar' argument expects only one filepath; but it was not one.")
        }
        if (!file.exists(ipar)) {
          stop(
            sprintf(
              "the file specified in the 'ipar' argument does not exist: %s",
              ipar
            )
          )
        }
        ipar <- read.csv(ipar, header = TRUE, as.is = TRUE)
        break
      }
      if (inherits(ipar, "SingleGroupClass")) {
        if (!requireNamespace("mirt", quietly = TRUE)) {
          stop("'mirt' package is required to read SingleGroupClass objects.")
        }
        if (ipar@Model$nfact > 1) {
          stop(sprintf("item model is not unidimensional: %s factors", ipar@Model$nfact))
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
        break
      }
      stop("the 'ipar' argument could not be used to read data; it was not a data frame or a valid filepath, or a mirt SingleGroupClass object.")
    }
  }

  item_pool       <- new("item_pool")
  item_pool@raw   <- ipar
  ni              <- nrow(ipar)
  item_pool@index <- 1:ni
  item_pool@id    <- as.character(ipar[[1]])
  model           <- ipar[[2]]
  NCAT            <- numeric(ni)
  parms           <- vector(mode = "list", length = ni)
  n_values        <- rowSums(!is.na(ipar))
  n_nonpars       <- 2
  n_pars          <- n_values - n_nonpars
  valid           <- logical(ni)
  item_pool@ipar  <- matrix(NA, nrow = ni, ncol = max(n_pars))
  item_pool@se    <- matrix(NA, nrow = ni, ncol = max(n_pars))

  # parse parameter SEs
  while (!is.null(ipar_se)) {
    if (inherits(ipar_se, "data.frame")) {
      ipar_se <- ipar_se
      break
    }
    if (inherits(ipar_se, "character")) {
      if (length(ipar_se) != 1) {
          stop("the 'ipar_se' argument expects only one filepath; but it was not one.")
        }
        if (!file.exists(ipar_se)) {
          stop(
            sprintf(
              "the file specified in the 'ipar_se' argument does not exist: %s",
              ipar_se
            )
          )
        }
      ipar_se <- read.csv(ipar_se, header = TRUE, as.is = TRUE)
      break
    }
    stop("the 'ipar_se' argument could not be used to read data; it was not a data frame or a valid filepath.")
  }

  if (is.null(ipar_se)) {
    ipar_se <- ipar
    for (j in 1:dim(ipar)[2]) {
      if (inherits(ipar_se[, j], "numeric")) {
        ipar_se[, j] <- 0
      }
    }
  }

  for (i in 1:ni) {

    if (model[i] == 1 | model[i] == "1PL") {

      NCAT[i] <- 2
      b    <- ipar[   i, n_nonpars + 1]
      b_se <- ipar_se[i, n_nonpars + 1]

      valid[i] <- TRUE

      item_pool@model[i] <- "item_1PL"
      parms[[i]] <- new("item_1PL", difficulty = b)

      item_pool@ipar[i, 1] <- b
      item_pool@se[  i, 1] <- b_se

      next

    }

    if (model[i] == 2 | model[i] == "2PL") {

      NCAT[i] <- 2
      a    <- ipar[   i, n_nonpars + 1]
      b    <- ipar[   i, n_nonpars + 2]
      a_se <- ipar_se[i, n_nonpars + 1]
      b_se <- ipar_se[i, n_nonpars + 2]

      if (a <= 0) { valid[i] <- FALSE; next }
      valid[i] <- TRUE

      item_pool@model[i] <- "item_2PL"
      parms[[i]] <- new("item_2PL", slope = a, difficulty = b)

      item_pool@ipar[i, 1:2] <- c(a   , b)
      item_pool@se[  i, 1:2] <- c(a_se, b_se)

      next

    }

    if (model[i] == 3 | model[i] == "3PL") {

      NCAT[i] <- 2
      a    <- ipar[   i, n_nonpars + 1]
      b    <- ipar[   i, n_nonpars + 2]
      c    <- ipar[   i, n_nonpars + 3]
      a_se <- ipar_se[i, n_nonpars + 1]
      b_se <- ipar_se[i, n_nonpars + 2]
      c_se <- ipar_se[i, n_nonpars + 3]

      if (a <= 0) { valid[i] <- FALSE; next }
      if (c <  0) { valid[i] <- FALSE; next }
      if (c >= 1) { valid[i] <- FALSE; next }
      valid[i] <- TRUE

      item_pool@model[i] <- "item_3PL"
      parms[[i]] <- new("item_3PL", slope = a, difficulty = b, guessing = c)

      item_pool@ipar[i, 1:3] <- c(a   , b   , c)
      item_pool@se[  i, 1:3] <- c(a_se, b_se, c_se)

      next

    }

    if (model[i] == 4 | model[i] == "PC") {

      NCAT[i] <- n_pars[i] + 1
      b    <- as.numeric(ipar[   i, n_nonpars + 1:n_pars[i]])
      b_se <- as.numeric(ipar_se[i, n_nonpars + 1:n_pars[i]])

      valid[i] <- TRUE

      item_pool@model[i] <- "item_PC"
      parms[[i]] <- new("item_PC", threshold = b, ncat = NCAT[i])

      item_pool@ipar[i, 1:n_pars[i]] <- b
      item_pool@se[  i, 1:n_pars[i]] <- b_se

      next

    }

    if (model[i] == 5 | model[i] == "GPC") {

      NCAT[i] <- (n_pars[i] - 1) + 1
      a    <- as.numeric(ipar[   i, n_nonpars + 1])
      b    <- as.numeric(ipar[   i, n_nonpars + 2:n_pars[i]])
      a_se <- as.numeric(ipar_se[i, n_nonpars + 1])
      b_se <- as.numeric(ipar_se[i, n_nonpars + 2:n_pars[i]])

      if (a <= 0) { valid[i] <- FALSE; next }
      valid[i] <- TRUE

      item_pool@model[i] <- "item_GPC"
      parms[[i]] <- new("item_GPC", slope = a, threshold = b, ncat = NCAT[i])

      item_pool@ipar[i, 1:n_pars[i]] <- c(a   , b)
      item_pool@se[  i, 1:n_pars[i]] <- c(a_se, b_se)

      next

    }

    if (model[i] == 6 | model[i] == "GR") {

      NCAT[i] <- (n_pars[i] - 1) + 1
      a    <- as.numeric(ipar[   i, n_nonpars + 1])
      b    <- as.numeric(ipar[   i, n_nonpars + 2:n_pars[i]])
      a_se <- as.numeric(ipar_se[i, n_nonpars + 1])
      b_se <- as.numeric(ipar_se[i, n_nonpars + 2:n_pars[i]])

      if (a <= 0)         { valid[i] <- FALSE; next }
      if (is.unsorted(b)) { valid[i] <- FALSE; next }
      valid[i] <- TRUE

      item_pool@model[i] <- "item_GR"
      parms[[i]] <- new("item_GR", slope = a, category = b, ncat = NCAT[i])

      item_pool@ipar[i, 1:n_pars[i]] <- c(a   , b)
      item_pool@se[  i, 1:n_pars[i]] <- c(a_se, b_se)

      next

    }

    stop(
      sprintf(
        "Item %s (%s): unexpected IRT model '%s' (valid models are 1PL, 2PL, 3PL, PC, GPC, GR)",
        i, ipar[["ID"]][i], model[i]
      )
    )

  }
  if (sum(!valid) > 0) {
    invalid_items <- which(!valid)
    invalid_items <- sprintf("row %s (%s)", invalid_items, ipar[["ID"]][invalid_items])
    stop(sprintf(
      "some items had invalid item parameters; check the following item(s): %s",
      paste(invalid_items, collapse = ", ")
    ))
  }

  item_pool@ni <- ni
  item_pool@max_cat <- max(NCAT)
  item_pool@NCAT <- NCAT
  item_pool@parms <- parms

  tmp <- item_pool@raw
  tmp[, 2 + 1:max(n_pars)] <- item_pool@se
  item_pool@raw_se <- tmp

  item_pool@unique <- unique

  if (validObject(item_pool)) {
    return(item_pool)
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
      stop("the 'ID' column does not exist; it must be present.")
    }
    if (any(object@data[["ID"]] %in% c("", " ", "NA", "N/A"))) {
      stop("the 'ID' column has empty or NA values; it must not have any.")
    }
    if (length(unique(object@data[["ID"]])) != nrow(object@data)) {
      stop("the 'ID' column has duplicate values; it must be all unique.")
    }
    if (!identical(object@data[["INDEX"]], 1:length(object@data[["INDEX"]]))) {
      stop(sprintf(
        "the 'INDEX' column must be equal to 1:%s.",
        length(object@data[["INDEX"]])
      ))
    }
    return(TRUE)
  }
)

#' Load item attributes
#'
#' \code{\link{loadItemAttrib}} can read item attributes a \code{\link{data.frame}} or a .csv file.
#' \code{\link{loadItemAttrib}} is a data loading function for creating an \code{\linkS4class{item_attrib}} object.
#'
#' @param object item attributes. Can be a \code{\link{data.frame}} or the file path of a .csv file. The content should at least include column 'ID' that matches with the \code{\linkS4class{item_pool}} object.
#' @template pool_param
#'
#' @return \code{\link{loadItemAttrib}} returns an \code{\linkS4class{item_attrib}} object.
#'
#' \itemize{
#'   \item{\code{data}} a \code{\link{data.frame}} containing item attributes.
#' }
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
#' @seealso \code{\link{dataset_science}}, \code{\link{dataset_reading}}, \code{\link{dataset_fatigue}}, \code{\link{dataset_bayes}} for examples.
#'
#' @export
loadItemAttrib <- function(object, pool) {

  if (is.null(pool)) {
    stop("the 'pool' argument is missing.")
  }

  if (!inherits(pool, "item_pool")) {
    stop("the 'pool' argument is not an 'item_pool' object.")
  }

  if (!validObject(pool)) {
    stop("the 'pool' argument is not a valid 'item_pool' object.")
  }

  if (!is.null(object)) {
    while (TRUE) {
      if (inherits(object, "data.frame")) {
        item_attrib <- object
        break
      }
      if (inherits(object, "character")) {
        if (length(object) != 1) {
          stop("the 'object' argument expects only one filepath; but it was not one.")
        }
        if (!file.exists(object)) {
          stop(
            sprintf(
              "the file specified in the 'object' argument does not exist: %s",
              object
            )
          )
        }
        item_attrib <- read.csv(object, header = TRUE, as.is = TRUE)
        break
      }
      stop("the 'object' argument could not be used to read data; it was not a data frame or a valid filepath.")
    }
  }

  names(item_attrib) <- toupper(names(item_attrib))

  if (is.numeric(item_attrib[["ID"]])) {
    item_attrib[["ID"]] <- as.character(item_attrib[["ID"]])
  }

  # consistency check with item pool ID values ---------------------------------

  if (length(pool@id) != length(item_attrib[["ID"]])) {
    stop("the 'ID' column values of supplied item attributes must exactly match pool@id; they do not have the same # of IDs.")
  }

  if (!all(sort(pool@id) == sort(item_attrib[["ID"]]))) {
    stop("the 'ID' column values of supplied item attributes must exactly match pool@id; they do not match after sorting.")
  }

  if (!all(pool@id == item_attrib[["ID"]])) {
    # IDs match but are in different row orders
    # attempt to match row orders here
    known_column_order <- names(item_attrib)
    item_attrib <- merge(
      data.frame(ID = pool@id), item_attrib, by = "ID"
    )
    item_attrib <- item_attrib[, known_column_order]
  }

  if ("STID" %in% names(item_attrib)) {
    # parse stimulus IDs
    idx <- item_attrib[["STID"]] %in% c("", " ", "N/A")
    if (any(idx)) {
      item_attrib[["STID"]][idx] <- NA
    }
    if (is.numeric(item_attrib[["STID"]])) {
      item_attrib[["STID"]] <- as.character(item_attrib[["STID"]])
    }
  }

  if ("INDEX" %in% names(item_attrib)) {
    item_attrib$INDEX <- NULL
    warning("the 'INDEX' column was ignored because it is reserved for internal use.")
  }
  item_attrib <- data.frame(cbind(INDEX = 1:nrow(item_attrib), item_attrib))

  o <- new("item_attrib")
  o@data <- item_attrib

  if (validObject(o)) {
    return(o)
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
      stop("the 'STID' column does not exist; it must be present.")
    }
    if (any(object@data[["STID"]] %in% c("", " ", "NA", "N/A"))) {
      stop("the 'STID' column has empty or NA values; it must not have any.")
    }
    if (length(unique(object@data[["STID"]])) != nrow(object@data)) {
      stop("the 'STID' column has duplicate values; it must be all unique.")
    }
    if (!identical(object@data[["STINDEX"]], 1:length(object@data[["STINDEX"]]))) {
      stop(sprintf(
        "the 'STINDEX' column must be equal to 1:%s.",
        length(object@data[["STINDEX"]])
      ))
    }
    return(TRUE)
  }
)

setClassUnion("stattrib_or_null", c("st_attrib", "NULL"))

#' Load set/stimulus/passage attributes
#'
#' \code{\link{loadStAttrib}} is a data loading function for creating an \code{\linkS4class{st_attrib}} object.
#' \code{\link{loadStAttrib}} can read stimulus attributes a \code{\link{data.frame}} or a .csv file.
#'
#' @param object set attributes. Can be a \code{\link{data.frame}} or the file path of a .csv file. The content should at least include the column 'STID' referring to the column 'STID' in the \code{data} slot of the \code{\linkS4class{item_attrib}} object.
#' @template item_attrib_param
#'
#' @return \code{\link{loadStAttrib}} returns a \code{\linkS4class{st_attrib}} object.
#'
#' \itemize{
#'   \item{\code{data}} a \code{\link{data.frame}} containing stimulus attributes.
#' }
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
#' @seealso \code{\link{dataset_reading}} for examples.
#'
#' @export
loadStAttrib <- function(object, item_attrib) {

  if (is.null(item_attrib)) {
    stop("the 'item_attrib' argument is missing.")
  }

  if (!inherits(item_attrib, "item_attrib")) {
    stop("the 'item_attrib' argument is not an 'item_attrib' object.")
  }

  if (!validObject(item_attrib)) {
    stop("the 'item_attrib' argument is not a valid 'item_attrib' object.")
  }

  if (!is.null(object)) {
    while (TRUE) {
      if (inherits(object, "data.frame")) {
        st_attrib <- object
        break
      }
      if (inherits(object, "character")) {
        if (length(object) != 1) {
          stop("the 'object' argument expects only one filepath; but it was not one.")
        }
        if (!file.exists(object)) {
          stop(
            sprintf(
              "the file specified in the 'object' argument does not exist: %s",
              object
            )
          )
        }
        st_attrib <- read.csv(object, header = TRUE, as.is = TRUE)
        break
      }
      stop("the 'object' argument could not be used to read data; it was not a data frame or a valid filepath.")
    }
  }

  names(st_attrib) <- toupper(names(st_attrib))

  if (is.numeric(st_attrib[["STID"]])) {
    st_attrib[["STID"]] <- as.character(st_attrib[["STID"]])
  }
  if ("STINDEX" %in% names(st_attrib)) {
    st_attrib$STINDEX <- NULL
    warning("the 'STINDEX' column was ignored because it is reserved for internal use.")
  }
  st_attrib <- data.frame(cbind(STINDEX = 1:nrow(st_attrib), st_attrib))

  if (!("STID" %in% names(item_attrib@data))) {
    stop("the 'item_attrib' argument does not have an 'STID' column; it must have one.")
  }
  expected_ids  <- unique(na.omit(item_attrib@data[["STID"]]))
  actual_ids    <- st_attrib[["STID"]]
  undefined_ids <- setdiff(expected_ids, actual_ids)
  if (length(undefined_ids) > 0) {
    stop(sprintf(
      "%s %s %s",
      "all unique 'STID's in the 'item_attrib' object must appear in the 'STID' column of the 'object' argument;",
      "this condition was not met. see these STIDs:",
      paste(undefined_ids, collapse = ", ")
    ))
  }

  o <- new("st_attrib")
  o@data <- st_attrib

  if (validObject(o)) {
    return(o)
  }
}

#' Class 'constraint': a single constraint
#'
#' \code{\linkS4class{constraint}} is an S4 class for representing a single constraint.
#'
#' @slot constraint the numeric index of the constraint.
#' @slot constraint_id the character ID of the constraint.
#' @slot nc the number of MIP-format constraints translated from this constraint.
#' @slot mat,dir,rhs these represent MIP-format constraints. A single MIP-format constraint is associated with a row in \code{mat}, a value in \code{rhs}, and a value in \code{dir}.
#' \itemize{
#'    \item{the \emph{i}-th row of \code{mat} represents LHS coefficients to use on decision variables in the \emph{i}-th MIP-format constraint.}
#'    \item{the \emph{i}-th value of \code{rhs} represents RHS values to use in the \emph{i}-th MIP-format constraint.}
#'    \item{the \emph{i}-th value of \code{dir} represents the imposed constraint between LHS and RHS.}
#' }
#' @slot suspend \code{TRUE} if the constraint is not to be imposed.
#'
#' @export
setClass("constraint",
  slots = c(
    constraint    = "numeric",
    constraint_id = "character",
    nc            = "numeric",
    mat           = "matrix",
    dir           = "character",
    rhs           = "numeric",
    suspend       = "logical"
  ),
  prototype = list(
    constraint    = numeric(0),
    constraint_id = character(0),
    nc            = 0,
    mat           = matrix(NA, 0, 0),
    dir           = character(0),
    rhs           = numeric(0),
    suspend       = FALSE
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

#' Class 'constraints': a set of constraints
#'
#' \code{\linkS4class{constraints}} is an S4 class for representing a set of constraints and its associated objects.
#'
#' See \code{\link{constraints-operators}} for object manipulation functions.
#'
#' @slot constraints a \code{\link{data.frame}} containing the constraint specifications.
#' @slot list_constraints a list containing the \code{\linkS4class{constraint}} object representation of each constraint.
#' @slot pool the \code{\linkS4class{item_pool}} object associated with the constraints.
#' @slot item_attrib the \code{\linkS4class{item_attrib}} object associated with the constraints.
#' @slot st_attrib the \code{\linkS4class{st_attrib}} object associated with the constraints.
#' @slot test_length the test length specified in the constraints.
#' @slot nv the number of decision variables. Equals \code{ni + ns}.
#' @slot ni the number of items to search from.
#' @slot ns the number of stimulus to search from.
#' @slot id the item/stimulus ID string of each item/stimulus.
#' @slot index,mat,dir,rhs these represent MIP-format constraints. A single MIP-format constraint is associated with a value in \code{index}, a row in \code{mat}, a value in \code{rhs}, and a value in \code{dir}.
#' \itemize{
#'    \item{the \emph{i}-th value of \code{index} represents which constraint specification in the \code{constraints} argument it was translated from.}
#'    \item{the \emph{i}-th row of \code{mat} represents LHS coefficients to use on decision variables in the \emph{i}-th MIP-format constraint.}
#'    \item{the \emph{i}-th value of \code{rhs} represents RHS values to use in the \emph{i}-th MIP-format constraint.}
#'    \item{the \emph{i}-th value of \code{dir} represents the imposed constraint between LHS and RHS.}
#' }
#' @slot set_based \code{TRUE} if the constraint is set-based. \code{FALSE} otherwise.
#' @slot item_order the item attribute of each item to use in imposing an item order constraint, if any.
#' @slot item_order_by the name of the item attribute to use in imposing an item order constraint, if any.
#' @slot stim_order the stimulus attribute of each stimulus to use in imposing a stimulus order constraint, if any.
#' @slot stim_order_by the name of the stimulus attribute to use in imposing a stimulus order constraint, if any.
#' @slot item_index_by_stimulus a list containing item indices of each stimulus.
#' @slot stimulus_index_by_item the stimulus indices of each item.
#'
#' @export
setClass("constraints",
  slots = c(
    constraints            = "data.frame",
    list_constraints       = "list",
    pool                   = "item_pool",
    item_attrib            = "item_attrib",
    st_attrib              = "stattrib_or_null",
    test_length            = "numeric",
    nv                     = "numeric",
    ni                     = "numeric",
    ns                     = "numeric",
    id                     = "character",
    index                  = "numeric",
    mat                    = "matrix",
    dir                    = "character",
    rhs                    = "numeric",
    set_based              = "logical",
    item_order             = "numeric_or_null",
    item_order_by          = "character_or_null",
    stim_order             = "numeric_or_null",
    stim_order_by          = "character_or_null",
    item_index_by_stimulus = "list_or_null",
    stimulus_index_by_item = "numeric_or_null"
  ),
  prototype = list(
    constraints            = data.frame(0),
    list_constraints       = list(0),
    pool                   = new("item_pool"),
    item_attrib            = new("item_attrib"),
    st_attrib              = NULL,
    test_length            = numeric(0),
    nv                     = numeric(0),
    ni                     = numeric(0),
    ns                     = numeric(0),
    id                     = character(0),
    index                  = numeric(0),
    mat                    = matrix(0),
    dir                    = character(0),
    rhs                    = numeric(0),
    set_based              = logical(0),
    item_order             = NULL,
    item_order_by          = NULL,
    stim_order             = NULL,
    stim_order_by          = NULL,
    item_index_by_stimulus = list(0),
    stimulus_index_by_item = numeric(0)
  ),
  validity = function(object) {

    tmp <- object
    tmp@constraints$CONSTRAINT <- NULL

    recreated <- try(
      loadConstraints(
        tmp@constraints,
        tmp@pool,
        tmp@item_attrib,
        tmp@st_attrib
      ),
      silent = TRUE
    )

    if (inherits(recreated, "try-error")) {

      err <- as.character(recreated)

    } else {

      recreated@constraints$CONSTRAINT <- NULL

      err <- c()

      for (x in slotNames(recreated)) {
        slot_recreated <- slot(recreated, x)
        slot_origin    <- slot(tmp      , x)
        if (inherits(slot_recreated, "integer")) {
          slot_recreated <- as.numeric(slot_recreated)
        }
        if (inherits(slot_origin, "integer")) {
          slot_origin <- as.numeric(slot_origin)
        }
        if (!identical(slot_recreated, slot_origin)) {
          err <- c(
            err,
            sprintf("constraints: slot '%s' recreated from @constraints does not match @%s", x, x)
          )
        }
      }

      if (!identical(
        recreated@constraints$CONSTRAINT_ID,
        unique(recreated@constraints$CONSTRAINT_ID)
      )) {
        err <- c(
          err,
          sprintf("constraints: the 'CONSTRAINT_ID' column must have unique values")
        )
      }

    }

    if (length(err) == 0) {
      return(TRUE)
    } else {
      return(err)
    }
  }
)

#' Load constraints
#'
#' \code{\link{loadConstraints}} is a data loading function for creating a \code{\linkS4class{constraints}} object.
#' \code{\link{loadConstraints}} can read constraints from a data.frame or a .csv file.
#' The contents must be in the expected format; see the vignette in \code{vignette("constraints")} for a documentation.
#'
#' @param object constraint specifications. Can be a \code{\link{data.frame}} or the file path of a .csv file. See the vignette for a description of the expected format.
#' @template pool_param
#' @template item_attrib_param
#' @template st_attrib_param
#'
#' @return \code{\link{loadConstraints}} returns a \code{\linkS4class{constraints}} object. This object is used in \code{\link{Static}} and \code{\link{Shadow}}.
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
#' @seealso \code{\link{dataset_science}}, \code{\link{dataset_reading}}, \code{\link{dataset_fatigue}}, \code{\link{dataset_bayes}} for examples.
#'
#' @export
loadConstraints <- function(object, pool, item_attrib, st_attrib = NULL) {

  if (!inherits(pool, "item_pool")) {
    stop("'pool' argument must be an 'item_pool' object")
  }
  if (!inherits(item_attrib, "item_attrib")) {
    stop("'item_attrib' argument must be an 'item_attrib' object")
  }
  if (!is.null(st_attrib)) {
    if (!inherits(st_attrib, "st_attrib")) {
      stop("'st_attrib' argument must be a 'st_attrib' object")
    }
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

  constraints <- sanitizeConstraintsData(constraints)

  # Validation: Pool
  ni <- pool@ni
  ns <- 0
  x  <- numeric(ni)
  nc <- nrow(constraints)

  if (nrow(item_attrib@data) != ni) {
    stop("item_attrib@data: nrow() must match pool@ni")
  }
  if (!all(pool@id == item_attrib@data[["ID"]])) {
    stop("item_attrib@data: 'ID' must match pool@id")
  }
  list_constraints <- vector(mode = "list", length = nc)
  item_constraints <- which(constraints[["WHAT"]] == "ITEM")
  stim_constraints <- which(constraints[["WHAT"]] %in% c("STIMULUS", "PASSAGE", "SET", "TESTLET"))
  item_order <- item_order_by <- NULL
  stim_order <- stim_order_by <- NULL

  if (length(stim_constraints) > 0) {
    if (is.null(st_attrib)) {
      stop("constraints: stimulus-based constraints require 'st_attrib' argument")
    }
    if (!("STID" %in% names(item_attrib@data))) {
      stop("constraints: stimulus-based constraints require 'STID' column in item_attrib@data")
    }

    group_by_stimulus <- TRUE
    constraints[["ST_COUNT"]] <- NA

    id <- c(item_attrib@data[["ID"]], st_attrib@data[["STID"]])
    ns <- nrow(st_attrib@data)
    nv <- ni + ns
    item_id_by_stimulus    <- split(item_attrib@data[["ID"]], as.factor(item_attrib@data[["STID"]]))
    item_index_by_stimulus <- lapply(item_id_by_stimulus, function(x) which(item_attrib@data[["ID"]] %in% x))
    item_index_by_stimulus <- item_index_by_stimulus[st_attrib@data[["STID"]]]
    if (any(item_attrib@data[["STID"]] %in% c("", " ", "N/A", "n/a"))) {
      item_attrib@data[["STID"]][item_attrib@data[["STID"]] %in% c("", " ", "N/A", "n/a")] <- NA
    }
    stimulus_id_by_item    <- item_attrib@data[["STID"]]
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
    group_by_stimulus <- FALSE
    nv <- ni
    id <- item_attrib@data[["ID"]]
    item_index_by_stimulus <- NULL
    stimulus_index_by_item <- NULL
  } else {
    stop("'file' content must include at least one 'ITEM' entry in 'WHAT' column. For stimulus-based constraints, 'LB' and 'UB' should have an entry.")
  }

  for (index in item_constraints) {
    x <- constraints[index, ]
    validate_constraints <- validateConstraintData(x, item_attrib)
  }
  if (group_by_stimulus) {
    for (index in stim_constraints) {
      x <- constraints[index, ]
      validate_constraints <- validateConstraintData(x, st_attrib)
    }
  }

  constants                   <- list()
  constants$ni                <- ni
  constants$ns                <- ns
  constants$nv                <- nv
  constants$group_by_stimulus <- group_by_stimulus
  constants$i_by_s            <- item_index_by_stimulus
  constants$s_by_i            <- stimulus_index_by_item
  constants <- getLBUBInConstraintData(constants, constraints, item_constraints, stim_constraints)

  for (index in item_constraints) {

    list_constraints[[index]] <- parseConstraintData(constraints[index, ], item_attrib, constants)
    constraints[index, ]      <- addCountsToConstraintData(constraints[index, ], item_attrib)

    if (constraints[["TYPE"]][index] %in% c("NUMBER", "COUNT")) {

      if (toupper(constraints[["CONDITION"]][index]) %in% c("", " ", "PER TEST", "TEST")) {

        if (group_by_stimulus && !common_stimulus_length) {
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

        if (!group_by_stimulus) {
          stop(sprintf("Constraints must include at least one 'STIMULUS' under WHAT for CONDITION: %s", toupper(constraints[["CONDITION"]][index])))
        }

      }

    }

    if (constraints[["TYPE"]][index] == "ORDER") {

      if (!list_constraints[[index]]@suspend) {
        item_order <- item_attrib@data[[constraints[["CONDITION"]][index]]]
        item_order_by <- constraints[["CONDITION"]][index]
      }

    }

    list_constraints[[index]]@nc <- nrow(list_constraints[[index]]@mat)

  }

  if (group_by_stimulus) {
    for (index in stim_constraints) {

      list_constraints[[index]] <- parseConstraintData(constraints[index, ], st_attrib, constants)
      constraints[index, ]      <- addCountsToConstraintData(constraints[index, ], st_attrib)

      if (constraints[["TYPE"]][index] == "ORDER") {

        if (!list_constraints[[index]]@suspend) {
            stim_order <- st_attrib@data[[constraints[["CONDITION"]][index]]]
            stim_order_by <- constraints[["CONDITION"]][index]
        }
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

  o <- new("constraints")
  o@constraints      <- constraints
  o@list_constraints <- list_constraints
  o@pool             <- pool
  o@item_attrib      <- item_attrib
  o@st_attrib        <- st_attrib
  o@test_length      <- constants$i_count$LB
  o@nv    <- nv
  o@ni    <- ni
  o@ns    <- ns
  o@id    <- id
  o@index <- index
  o@mat   <- mat
  o@dir   <- dir
  o@rhs   <- rhs
  o@set_based              <- group_by_stimulus
  o@item_order             <- item_order
  o@item_order_by          <- item_order_by
  o@stim_order             <- stim_order
  o@stim_order_by          <- stim_order_by
  o@item_index_by_stimulus <- item_index_by_stimulus
  o@stimulus_index_by_item <- stimulus_index_by_item

  return(o)
}

#' Build constraints (shortcut to other loading functions)
#'
#' \code{\link{buildConstraints}} is a data loading function for creating a \code{\linkS4class{constraints}} object.
#' \code{\link{buildConstraints}} is a shortcut that calls other data loading functions.
#' The constraints must be in the expected format; see the vignette in \code{vignette("constraints")}.
#'
#' @param object constraint specifications. Can be a data.frame or the file path of a .csv file. See the vignette for the expected format.
#' @param item_pool item parameters. Can be a \code{\linkS4class{item_pool}} object, a data.frame or the file path of a .csv file.
#' @param item_attrib item attributes. Can be an \code{\linkS4class{item_attrib}} object, a data.frame or the file path of a .csv file.
#' @param st_attrib (optional) stimulus attributes. Can be an \code{\linkS4class{st_attrib}} object, a data.frame or the file path of a .csv file.
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
buildConstraints <- function(object, item_pool, item_attrib, st_attrib = NULL) {

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
