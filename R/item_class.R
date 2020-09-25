#' @include extensions.R
NULL

#' @noRd
validateDifficulty <- function(object) {
  if (length(object@difficulty) == 0) {
    return("@difficulty must not be empty")
  }
}
#' @noRd
validateSlope <- function(object) {
  if (object@slope <= 0) {
    return("@slope must be non-negative")
  }
}

#' @noRd
validateGuessing <- function(object) {
  if (object@guessing < 0 || object@guessing >= 1.0) {
    return("@guessing must be in [0.0, 1.0) range")
  }
}

#' @noRd
validateNcat <- function(object) {
  if (length(object@ncat) == 0) {
    return("@ncat must be supplied")
  }
}

#' @noRd
validateNthr <- function(object) {
  if (object@ncat != length(object@threshold) + 1) {
    return("length(@threshold) must be equal to @ncat - 1")
  }
}

#' @noRd
validateCategory <- function(object) {
  if (object@ncat != length(object@category) + 1) {
    return("length(@category) must be equal to @ncat - 1")
  }
}

#' @noRd
validateOrder <- function(object) {
  if (is.unsorted(object@category)) {
    return("@category must be in ascending order")
  }
}

#' @noRd
returnErrors <- function(errors) {
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}

#' Item classes
#'
#' \itemize{
#'   \item{\code{\linkS4class{item_1PL}} class represents a 1PL item.}
#'   \item{\code{\linkS4class{item_2PL}} class represents a 2PL item.}
#'   \item{\code{\linkS4class{item_3PL}} class represents a 3PL item.}
#'   \item{\code{\linkS4class{item_PC}} class represents a partial credit item.}
#'   \item{\code{\linkS4class{item_GPC}} class represents a generalized partial credit item.}
#'   \item{\code{\linkS4class{item_GR}} class represents a graded response item.}
#' }
#'
#' @slot slope a slope parameter value
#' @slot difficulty a difficulty parameter value
#' @slot guessing a guessing parameter value
#' @slot threshold a vector of threshold parameter values
#' @slot category a vector of category boundary values
#' @slot ncat the number of response categories
#'
#' @examples
#' item_1 <- new("item_1PL", difficulty = 0.5)
#' item_2 <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' item_3 <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' item_4 <- new("item_PC", threshold = c(-0.5, 0.5), ncat = 3)
#' item_5 <- new("item_GPC", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
#' item_6 <- new("item_GR", slope = 1.0, category = c(-2.0, -1.0, 0, 1.0, 2.0), ncat = 6)
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name item-classes
#' @aliases item
NULL

#' @rdname item-classes
setClass("item_1PL",
  slots = c(
    difficulty = "numeric"
  ),
  prototype = list(
    difficulty = numeric(0)
  ),
  validity = function(object) {
    e <- character()
    e <- c(e, validateDifficulty(object))
    return(returnErrors(e))
  }
)

#' @rdname item-classes
setClass("item_2PL",
  slots = c(
    slope      = "numeric",
    difficulty = "numeric"
  ),
  prototype = list(
    slope      = numeric(0),
    difficulty = numeric(0)
  ),
  validity = function(object) {
    e <- character()
    e <- c(e, validateDifficulty(object))
    e <- c(e, validateSlope(object))
    return(returnErrors(e))
  }
)

#' @rdname item-classes
setClass("item_3PL",
  slots = c(
    slope      = "numeric",
    difficulty = "numeric",
    guessing   = "numeric"
  ),
  prototype = list(
    slope      = numeric(0),
    difficulty = numeric(0),
    guessing   = numeric(0)
  ),
  validity = function(object) {
    e <- character()
    e <- c(e, validateDifficulty(object))
    e <- c(e, validateSlope(object))
    e <- c(e, validateGuessing(object))
    return(returnErrors(e))
  }
)

#' @rdname item-classes
setClass("item_PC",
  slots = c(
    threshold = "numeric",
    ncat      = "numeric"
  ),
  prototype = list(
    threshold = numeric(0),
    ncat      = numeric(0)
  ),
  validity = function(object) {
    e <- character()
    e <- c(e, validateNcat(object))
    e <- c(e, validateNthr(object))
    return(returnErrors(e))
  }
)

#' @rdname item-classes
setClass("item_GPC",
  slots = c(
    slope     = "numeric",
    threshold = "numeric",
    ncat      = "numeric"
  ),
  prototype = list(
    slope     = numeric(0),
    threshold = numeric(0),
    ncat      = numeric(0)
  ),
  validity = function(object) {
    e <- character()
    e <- c(e, validateNcat(object))
    e <- c(e, validateSlope(object))
    return(returnErrors(e))
  }
)


#' @rdname item-classes
setClass("item_GR",
  slots = c(
    slope    = "numeric",
    category = "numeric",
    ncat     = "numeric"
  ),
  prototype = list(
    slope    = numeric(0),
    category = numeric(0),
    ncat     = numeric(0)
  ),
  validity = function(object) {
    e <- character()
    e <- c(e, validateNcat(object))
    e <- c(e, validateSlope(object))
    e <- c(e, validateCategory(object))
    e <- c(e, validateOrder(object))
    return(returnErrors(e))
  }
)

#' Class 'item_pool': an item pool
#'
#' \code{\linkS4class{item_pool}} is an S4 class to represent an item pool.
#'
#' See \code{\link{item_pool-operators}} for object manipulation functions.
#'
#' @slot ni the number of items in the pool.
#' @slot max_cat the maximum number of response categories across the pool.
#' @slot index the numeric index of each item.
#' @slot id the ID string of each item.
#' @slot model the item class name of each item. See \code{\link{item-classes}}.
#' @slot NCAT the number of response categories of each item.
#' @slot parms a list containing item class objects. See \code{\link{item-classes}}.
#' @slot ipar a matrix containing item parameters.
#' @slot se a matrix containing item parameter standard errors.
#' @slot raw the raw input \code{\link{data.frame}} used in \code{\link{loadItemPool}} to create this object.
#' @slot raw_se the raw input \code{\link{data.frame}} used in \code{\link{loadItemPool}} to create this object.
#'
#' @export
setClass("item_pool",
  slots = c(
    ni      = "numeric",
    max_cat = "numeric",
    index   = "numeric",
    id      = "character",
    model   = "character",
    NCAT    = "numeric",
    parms   = "list",
    ipar    = "matrix",
    se      = "matrix",
    raw     = "data.frame",
    raw_se  = "dataframe_or_null"
  ),
  prototype = list(
    ni      = numeric(0),
    max_cat = numeric(0),
    index   = numeric(0),
    id      = character(0),
    model   = character(0),
    NCAT    = numeric(0),
    parms   = list(0),
    ipar    = matrix(0),
    se      = matrix(0),
    raw     = data.frame(),
    raw_se  = NULL
  ),
  validity = function(object) {
    if (length(unique(object@id)) != length(object@id)) {
      stop("Entries in @id must be unique")
    }
    if (dim(object@raw)[1] != object@ni) {
      stop("Number of items in @raw does not match @ni. Change @ni to match @raw.")
    }
    return(TRUE)
  }
)

#' Class 'item_pool_cluster': an item pool
#'
#' \code{\linkS4class{item_pool_cluster}} is an S4 class to represent a group of item pools.
#'
#' @slot np the number of item pools.
#' @slot pools a list of \code{\linkS4class{item_pool}} objects.
#' @slot names a vector containing item pool names.
setClass("item_pool_cluster",
  slots = c(
    np = "numeric",
    pools = "list",
    names = "character"
  ),
  prototype = list(
    np = numeric(0),
    pools = list(0),
    names = character(0)
  ),
  validity = function(object) {
    errors <- NULL
    if (length(object@pools) != object@np) {
      errors <- c(errors, "@np must match length(@pools). Change @np to match length(@pools).")
    }
    if (length(object@names) != object@np) {
      errors <- c(errors, "@np must match length(@names). Change @np to match length(@names).")
    }
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)
