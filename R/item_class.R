#' @noRd
validateSlope <- function(object) {
  if (object@slope <= 0) {
    return("A non-positive value for the slope parameter is not permissible.")
  }
}

#' @noRd
validateGuessing <- function(object) {
  if (object@guessing < 0 || object@guessing >= 1.0) {
    return("The supplied value for the guessing parameter is out of bounds. The value needs to be in between 0.0 and 1.0.")
  }
}

#' @noRd
validateNcat <- function(object) {
  if (length(object@ncat) == 0) {
    return("Number of categories is missing.")
  }
}

#' @noRd
validateNthr <- function(object) {
  if (object@ncat != length(object@threshold) + 1) {
    return("The number of thresholds needs to be equal to ncat - 1.")
  }
}

#' @noRd
validateCategory <- function(object) {
  if (object@ncat != length(object@category) + 1) {
    return("The number of category values does not match ncat.")
  }
}

#' @noRd
validateOrder <- function(object) {
  if (is.unsorted(object@category)) {
    return("The category values are not in an ascending order.")
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
#' item_1 <- new("item.1pl", difficulty = 0.5)
#' @template 1pl-ref
setClass("item.1pl",
  slots = c(difficulty = "numeric"),
  prototype = list(difficulty = numeric(0))
)

#' @name show-method
#' @aliases show,item.1pl-method
#' @docType methods
#' @noRd
setMethod("show", "item.1pl", function(object) {
  cat("One-parameter logistic or Rasch model (item.1pl)\n")
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
#' item_2 <- new("item.2pl", slope = 1.0, difficulty = 0.5)
#' @template 2pl-ref
setClass("item.2pl",
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
#' @aliases show,item.2pl-method
#' @docType methods
#' @noRd
setMethod("show", "item.2pl", function(object) {
  cat("Two-parameter logistic model (item.2pl) \n")
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
#' item_3 <- new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
#' @template 3pl-ref
setClass("item.3pl",
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
#' @aliases show,item.3pl-method
#' @docType methods
#' @noRd
setMethod("show", "item.3pl", function(object) {
  cat("Three-parameter logistic model (item.3pl)\n")
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
#' item_4 <- new("item.pc", threshold = c(-0.5, 0.5), ncat = 3)
#' @template pc-ref
setClass("item.pc",
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
#' @aliases show,item.pc-method
#' @docType methods
#' @noRd
setMethod("show", "item.pc", function(object) {
  cat("Partial credit model (item.pc)\n")
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
#' item_5 <- new("item.gpc", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
#' @template gpc-ref
setClass("item.gpc",
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
#' @aliases show,item.gpc-method
#' @docType methods
#' @noRd
setMethod("show", "item.gpc", function(object) {
  cat("Generalized partial credit model (item.gpc)\n")
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
#' item_6 <- new("item.gr", slope = 1.0, category = c(-2.0, -1.0, 0, 1.0, 2.0), ncat = 6)
#' @template gr-ref
setClass("item.gr",
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
#' @aliases show,item.gr-method
#' @docType methods
#' @noRd
setMethod("show", "item.gr", function(object) {
  cat("Graded response model (item.gr)\n")
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
#' @slot model Numeric. A vector of item model codes (1: item.1pl, 2: item.2pl, 3: item.3pl, 4: item.pc, 5: item.gpc, 6: item.gr).
#' @slot NCAT Numeric. A vector of the number of response categories for each item.
#' @slot parms A list of item parameters in the pool.
#' @slot ipar A matrix of item parameters in the pool.
#' @slot se A matrix representing standard errors of the item parameters.

setClass("item.pool",
  slots = c(
    ni = "numeric",
    max_cat = "numeric",
    index = "numeric",
    id = "character",
    model = "character",
    NCAT = "numeric",
    parms = "list",
    ipar = "matrix",
    se = "matrix"
  ),
  prototype = list(
    ni = numeric(0),
    max_cat = numeric(0),
    index = numeric(0),
    id = character(0),
    model = character(0),
    NCAT = numeric(0),
    parms = list(0),
    ipar = matrix(0),
    se = matrix(0)
  ),
  validity = function(object) {
    if (length(unique(object@id)) != object@ni) stop("entries in @id are not unique.")
    return(TRUE)
  }
)

#' @name show-method
#' @aliases show,item.pool-method
#' @docType methods
#' @noRd
setMethod("show", "item.pool", function(object) {
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
    cat("item pool is empty.")
  }
})

#' An S4 class to represent a cluster of item pools
#'
#' An S4 class to represent a cluster of item pools.
#'
#' @slot np A scalar to indicate the number of item pools in the cluster.
#' @slot pools A list of \code{item.pool} objects.
#' @slot names A character vector of item pool names of length np.
setClass("pool.cluster",
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
      errors <- c(errors, "length(pools) is not equal to np.")
    } 
    if (length(object@names) != object@np) {
      errors <- c(errors, "length(names) is not equal to np.")
    }
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

#' @name show-method
#' @aliases show,pool.cluster-method
#' @docType methods
#' @noRd
setMethod("show", "pool.cluster", function(object) {
  if (length(object@np) > 0) {
    cat("@np    :", object@np, "\n")
    cat("@names :", paste0(object@names, collapse = ", "), "\n\n")
    for (i in 1:object@np) {
      cat("pool   :", object@names[i], "\n")
      show(object@pools[[i]])
    }
  } else {
    cat("item pool cluster is empty.")
  }
})
