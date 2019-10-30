#' @include RcppExports.R import.R
NULL

#' @noRd
validateSlope <- function(object) {
  if (object@slope <= 0) {
    return("@slope must be non-negative.")
  }
}

#' @noRd
validateGuessing <- function(object) {
  if (object@guessing < 0 || object@guessing >= 1.0) {
    return("@guessing must be in [0.0, 1.0) range.")
  }
}

#' @noRd
validateNcat <- function(object) {
  if (length(object@ncat) == 0) {
    return("@ncat must be supplied.")
  }
}

#' @noRd
validateNthr <- function(object) {
  if (object@ncat != length(object@threshold) + 1) {
    return("length(@threshold) must be equal to @ncat - 1.")
  }
}

#' @noRd
validateCategory <- function(object) {
  if (object@ncat != length(object@category) + 1) {
    return("length(@category) must be equal to @ncat - 1.")
  }
}

#' @noRd
validateOrder <- function(object) {
  if (is.unsorted(object@category)) {
    return("@category must be in ascending order.")
  }
}

#' Show
#'
#' @name show-method
#' @rdname show-methods
#' @docType methods
#' @noRd
NULL

#' An S4 class to represent a 1PL item
#'
#' An S4 class to represent a 1PL item.
#'
#' @slot difficulty Numeric. A difficulty parameter value.
#'
#' @examples
#' item_1 <- new("item_1PL", difficulty = 0.5)
#' @template 1pl-ref
setClass("item_1PL",
  slots = c(difficulty = "numeric"),
  prototype = list(difficulty = numeric(0))
)

#' @name show-method
#' @aliases show,item_1PL-method
#' @docType methods
#' @noRd
setMethod("show", "item_1PL", function(object) {
  cat("One-parameter logistic or Rasch model (item_1PL)\n")
  cat("  Difficulty     :", object@difficulty, "\n")
})

#' An S4 class to represent a 2PL item
#'
#' An S4 class to represent a 2PL item.
#'
#' @slot slope Numeric. A slope parameter value.
#' @slot difficulty Numeric. A difficulty parameter value.
#'
#' @examples
#' item_2 <- new("item_2PL", slope = 1.0, difficulty = 0.5)
#' @template 2pl-ref
setClass("item_2PL",
  slots = c(
    slope = "numeric",
    difficulty = "numeric"
  ),
  prototype = list(
    slope = numeric(0),
    difficulty = numeric(0)
  ),
  validity = function(object) {
    errors <- character()
    errors <- c(errors, validateSlope(object))
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' @name show-method
#' @aliases show,item_2PL-method
#' @docType methods
#' @noRd
setMethod("show", "item_2PL", function(object) {
  cat("Two-parameter logistic model (item_2PL) \n")
  cat("  Slope          :", object@slope, "\n")
  cat("  Difficulty     :", object@difficulty, "\n")
})

#' An S4 class to represent a 3PL item
#'
#' An S4 class to represent a 3PL item.
#'
#' @slot slope Numeric. A slope parameter value.
#' @slot difficulty Numeric. A difficulty parameter value.
#' @slot guessing Numeric. A guessing parameter value.
#'
#' @examples
#' item_3 <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' @template 3pl-ref
setClass("item_3PL",
  slots = c(
    slope = "numeric",
    difficulty = "numeric",
    guessing = "numeric"
  ),
  prototype = list(
    slope = numeric(0),
    difficulty = numeric(0),
    guessing = numeric(0)
  ),
  validity = function(object) {
    errors <- character()
    errors <- c(errors, validateSlope(object))
    errors <- c(errors, validateGuessing(object))
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' @name show-method
#' @aliases show,item_3PL-method
#' @docType methods
#' @noRd
setMethod("show", "item_3PL", function(object) {
  cat("Three-parameter logistic model (item_3PL)\n")
  cat("  Slope          :", object@slope, "\n")
  cat("  Difficulty     :", object@difficulty, "\n")
  cat("  Guessing       :", object@guessing, "\n")
})

#' An S4 class to represent a partial credit item
#'
#' An S4 class to represent a partial credit item.
#'
#' @slot threshold Numeric. A vector of threshold parameter values.
#' @slot ncat Numeric. The number of response categories.
#'
#' @examples
#' item_4 <- new("item_PC", threshold = c(-0.5, 0.5), ncat = 3)
#' @template pc-ref
setClass("item_PC",
  slots = c(
    threshold = "numeric",
    ncat = "numeric"
  ),
  prototype = list(
    threshold = numeric(0),
    ncat = numeric(0)
  ),
  validity = function(object) {
    errors <- character()
    errors <- c(errors, validateNcat(object))
    errors <- c(errors, validateNthr(object))
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' @name show-method
#' @aliases show,item_PC-method
#' @docType methods
#' @noRd
setMethod("show", "item_PC", function(object) {
  cat("Partial credit model (item_PC)\n")
  cat("  Threshold     :", object@threshold, "\n")
  cat("  N categories  :", object@ncat, "\n")
})

#' An S4 class to represent a generalized partial credit item
#'
#' An S4 class to represent a generalized partial credit item.
#'
#' @slot slope Numeric. A slope parameter value.
#' @slot threshold Numeric. A vector of threshold parameter values.
#' @slot ncat Numeric. The number of response categories.
#'
#' @examples
#' item_5 <- new("item_GPC", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
#' @template gpc-ref
setClass("item_GPC",
  slots = c(
    slope = "numeric",
    threshold = "numeric",
    ncat = "numeric"
  ),
  prototype = list(
    slope = numeric(0),
    threshold = numeric(0),
    ncat = numeric(0)
  ),
  validity = function(object) {
    errors <- character()
    errors <- c(errors, validateNcat(object))
    errors <- c(errors, validateSlope(object))
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' @name show-method
#' @aliases show,item_GPC-method
#' @docType methods
#' @noRd
setMethod("show", "item_GPC", function(object) {
  cat("Generalized partial credit model (item_GPC)\n")
  cat("  Slope         :", object@slope, "\n")
  cat("  Threshold     :", object@threshold, "\n")
  cat("  N categories  :", object@ncat, "\n")
})

#' An S4 class to represent a graded response item
#'
#' An S4 class to represent a graded response item.
#'
#' @slot slope Numeric. A slope parameter value.
#' @slot category Numeric. A vector of category boundary values.
#' @slot ncat Numeric. The number of response categories.
#'
#' @examples
#' item_6 <- new("item_GR", slope = 1.0, category = c(-2.0, -1.0, 0, 1.0, 2.0), ncat = 6)
#' @template gr-ref
setClass("item_GR",
  slots = c(
    slope = "numeric",
    category = "numeric",
    ncat = "numeric"
  ),
  prototype = list(
    slope = numeric(0),
    category = numeric(0),
    ncat = numeric(0)
  ),
  validity = function(object) {
    errors <- character()
    errors <- c(errors, validateNcat(object))
    errors <- c(errors, validateSlope(object))
    errors <- c(errors, validateCategory(object))
    errors <- c(errors, validateOrder(object))
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' @name show-method
#' @aliases show,item_GR-method
#' @docType methods
#' @noRd
setMethod("show", "item_GR", function(object) {
  cat("Graded response model (item_GR)\n")
  cat("  Slope         :", object@slope, "\n")
  cat("  Category b    :", object@category, "\n")
  cat("  N categories  :", object@ncat, "\n")
})

#' An S4 class to represent an item pool
#'
#' An S4 class to represent an item pool.
#'
#' @slot ni Numeric. The number of items in the item pool.
#' @slot max_cat Numeric. The maximum number of response categories across all items.
#' @slot index Numeric. A vector of item indices.
#' @slot id Character. A vector of item ids.
#' @slot model Numeric. A vector of item model codes (1: item.1pl, 2: item_2PL, 3: item_3PL, 4: item_PC, 5: item_GPC, 6: item_GR).
#' @slot NCAT Numeric. A vector of the number of response categories for each item.
#' @slot parms A list of item parameters in the pool.
#' @slot ipar A matrix of item parameters in the pool.
#' @slot se A matrix representing standard errors of the item parameters.
#' @slot raw A data.frame containing raw input data.

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
    raw     = "data.frame"
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
    raw     = data.frame()
  ),
  validity = function(object) {
    if (length(unique(object@id)) != length(object@id)) {
      stop("Entries in @id must be unique.")
    }
    if (dim(object@raw)[1] != object@ni) {
      stop("Number of items in @raw does not match @ni. Change @ni to match @raw.")
    }
    return(TRUE)
  }
)

#' @name show-method
#' @aliases show,item_pool-method
#' @docType methods
#' @noRd
setMethod("show", "item_pool", function(object) {
  if (length(object@ni) > 0) {
    cat("@ni    :", object@ni, "\n")
    cat("@max_cat :", object@max_cat, "\n\n")
    print(data.frame(index = object@index, id = object@id, model = object@model, NCAT = object@NCAT))
    for (i in 1:object@ni) {
      cat("\n", paste0(object@index[i], ". "))
      show(object@parms[[i]])
    }
    cat("\n")
  } else {
    cat("The 'item_pool' object is empty.")
  }
})

#' Create a subset of an item pool object
#'
#' Create a subset of an \code{\linkS4class{item_pool}} object.
#'
#' @param pool An \code{\linkS4class{item_pool}} object.
#' @param select A vector of indices identifying the items to subset.
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:100)
#' @export

subsetItemPool <- function(pool, select = NULL) {
  if (class(pool) != "item_pool") {
    stop("'pool' must be an 'item_pool' object.")
  }
  if (!validObject(pool)) {
    stop("'pool' is not a valid 'item_pool' object.")
  }
  if (is.null(select)) {
    return(pool)
  } else if (all(select %in% 1:pool@ni)) {
    select           <- unique(select)
    n_select         <- length(select)
    sub_pool         <- new("item_pool")
    sub_pool@ni      <- n_select
    sub_pool@index   <- 1:n_select
    sub_pool@id      <- pool@id[select]
    sub_pool@model   <- pool@model[select]
    sub_pool@NCAT    <- pool@NCAT[select]
    sub_pool@parms   <- pool@parms[select]
    sub_pool@max_cat <- max(sub_pool@NCAT)
    sub_pool@ipar    <- pool@ipar[select, , drop = FALSE]
    sub_pool@se      <- pool@se[select, , drop = FALSE]
    sub_pool@raw     <- pool@raw[select, , drop = FALSE]
    return(sub_pool)
  } else {
    stop("'select' contains item indices not defined in 'pool'.")
  }
}

#' @name extract-methods
#' @aliases [,item_pool,ANY,ANY,ANY-method
#' @docType methods

setMethod(
  f = "[",
  signature = "item_pool",
  definition = function(x, i, j, ...) {
    if (i == "ni") {
      return(x@ni)
    }
    if (i == "max_cat") {
      return(x@max_cat)
    }
    if (i == "index") {
      return(x@index)
    }
    if (i == "id") {
      return(x@id)
    }
    if (i == "model") {
      return(x@model)
    }
    if (i == "NCAT") {
      return(x@NCAT)
    }
    if (i == "parms") {
      return(x@parms)
    }
    if (i == "ipar") {
      return(x@ipar)
    }
    if (i == "se") {
      return(x@se)
    }
  }
)

#' @name item_pool.operators
#' @title Item pool and pool cluster operators
#'
#' @description \code{pool1 + pool2} combines two \code{\linkS4class{item_pool}} objects.
#'
#' @param pool1 An \code{\linkS4class{item_pool}} object.
#' @param pool2 An \code{\linkS4class{item_pool}} object.
#'
#' @examples
#' itempool <- itempool_science + itempool_reading
#'
#' @rdname item_pool.operators
#' @export

`+.item_pool` <- function(pool1, pool2) {
  if (class(pool1) != "item_pool" || class(pool2) != "item_pool") stop("Operands must be 'item_pool' objects.")

  if (!validObject(pool1)) {
    stop("'pool1' is not a valid 'item_pool' object.")
  }
  if (!validObject(pool2)) {
    stop("'pool2' is not a valid 'item_pool' object.")
  }

  combined_pool <- new("item_pool")
  id        <- c(pool1@id, pool2@id)
  model     <- c(pool1@model, pool2@model)
  NCAT      <- c(pool1@NCAT, pool2@NCAT)
  parms     <- c(pool1@parms, pool2@parms)

  nfield1   <- dim(pool1@ipar)[2]
  nfield2   <- dim(pool2@ipar)[2]
  nfield    <- max(nfield1, nfield2)

  ipar1              <- matrix(NA, dim(pool1@ipar)[1], nfield)
  ipar1[, 1:nfield1] <- pool1@ipar
  ipar2              <- matrix(NA, dim(pool2@ipar)[1], nfield)
  ipar2[, 1:nfield2] <- pool2@ipar
  ipar               <- rbind(ipar1, ipar2)

  se1                <- matrix(NA, dim(pool1@se)[1], nfield)
  se1[, 1:nfield1]   <- pool1@se
  se2                <- matrix(NA, dim(pool2@se)[1], nfield)
  se2[, 1:nfield2]   <- pool2@se
  se                 <- rbind(se1, se2)

  is_unique <- which(!duplicated(id))

  combined_pool@ni      <- length(is_unique)
  combined_pool@max_cat <- max(NCAT[is_unique])
  combined_pool@index   <- 1:combined_pool@ni
  combined_pool@id      <- id[is_unique]
  combined_pool@model   <- model[is_unique]
  combined_pool@NCAT    <- NCAT[is_unique]
  combined_pool@parms   <- parms[is_unique]
  combined_pool@ipar    <- ipar[is_unique, , drop = FALSE]
  combined_pool@se      <- se[is_unique, , drop = FALSE]

  combined_pool@raw     <-
    data.frame(
      ID    = combined_pool@id,
      MODEL = combined_pool@model
    )

  combined_pool@raw     <- cbind(combined_pool@raw, combined_pool@ipar)

  if (sum(duplicated(id)) > 0) {
    warning(sprintf("Duplicate items were found and removed : %s", paste0(id[duplicated(id)], collapse = ", ")))
  }
  return(combined_pool)
}

#' @description \code{pool1 - pool2} excludes the items in the second item pool from the first. The two \code{\linkS4class{item_pool}} objects must overlap for this to be performed.
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:500)
#' itempool <- itempool_science - subitempool
#'
#' @rdname item_pool.operators
#' @export

`-.item_pool` <- function(pool1, pool2) {
  if (class(pool1) != "item_pool" || class(pool2) != "item_pool") stop("Operands must be 'item_pool' objects.")
  if (any(pool2@id %in% pool1@id)) {
    left <- which(!(pool1@id %in% pool2@id))
    if (length(left) > 0) {
      pool1@ni      <- length(left)
      pool1@max_cat <- max(pool1@NCAT[left])
      pool1@index   <- 1:length(left)
      pool1@id      <- pool1@id[left]
      pool1@model   <- pool1@model[left]
      pool1@NCAT    <- pool1@NCAT[left]
      pool1@parms   <- pool1@parms[left]
      pool1@ipar    <- pool1@ipar[left, , drop = FALSE]
      pool1@se      <- pool1@se[left, , drop = FALSE]
      pool1@raw     <- pool1@raw[left, , drop = FALSE]
    } else {
      warning("The resulting 'item_pool' object is empty.")
      return(pool1)
    }
  }
  return(pool1)
}

#' @description \code{pool1 == pool2} tests equality of the two item_pool objects.
#' @examples
#' itempool <- subsetItemPool(itempool_science, 1:500)
#' subitempool1 <- itempool_science - itempool
#' subitempool2 <- subsetItemPool(itempool_science, 501:1000)
#' subitempool1 == subitempool2  ## TRUE
#'
#' @rdname item_pool.operators
#' @export

`==.item_pool` <- function(pool1, pool2) {
  if (class(pool1) != "item_pool" || class(pool2) != "item_pool") stop("Operands must be 'item_pool' objects.")
  return(identical(pool1, pool2))
}

#' An S4 class to represent a cluster of item pools
#'
#' An S4 class to represent a cluster of item pools.
#'
#' @slot np A scalar to indicate the number of item pools in the cluster.
#' @slot pools A list of \code{item_pool} objects.
#' @slot names A character vector of item pool names of length np.
setClass("pool_cluster",
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

#' @name show-method
#' @aliases show,pool_cluster-method
#' @docType methods
#' @noRd
setMethod("show", "pool_cluster", function(object) {
  if (length(object@np) > 0) {
    cat("@np    :", object@np, "\n")
    cat("@names :", paste0(object@names, collapse = ", "), "\n\n")
    for (i in 1:object@np) {
      cat("pool   :", object@names[i], "\n")
      show(object@pools[[i]])
    }
  } else {
    cat("The 'pool_cluster' object is empty.")
  }
})
