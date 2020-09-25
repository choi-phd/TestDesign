#' @include shadow_class.R
NULL

#' @title Basic operators for item pool objects
#'
#' @description
#'
#' Create a subset of an \code{\linkS4class{item_pool}} object:
#'
#' \itemize{
#'   \item{\code{pool[i]}}
#'   \item{\code{subsetItemPool(pool, i)}}
#' }
#'
#' Combine two \code{\linkS4class{item_pool}} objects:
#'
#' \itemize{
#'   \item{\code{c(pool1, pool2)}}
#'   \item{\code{combineItemPool(pool1, pool2)}}
#'   \item{\code{pool1 + pool2}}
#' }
#'
#' \code{pool1 - pool2} excludes items in \code{pool2} from \code{pool1}.
#'
#' \code{pool1 == pool2} tests whether two \code{\linkS4class{item_pool}} objects are identical.
#'
#' @param x,x1,x2 an \code{\linkS4class{item_pool}} object.
#' @param i item indices to use in subsetting.
#' @param j,drop,... not used, exists for compatibility.
#'
#' @examples
#' p1 <- itempool_science[1:100]
#' p2 <- c(itempool_science, itempool_reading)
#' p3 <- p2 - p1
#'
#' p1 <- itempool_science[1:500]
#' p2 <- itempool_science - p1
#' p3 <- itempool_science[501:1000]
#' identical(p2, p3)  ## TRUE
#'
#' p <- p1 + p3
#' p == itempool_science ## TRUE
#'
#' @name item_pool-operators
NULL

#' @rdname item_pool-operators
#' @export
subsetItemPool <- function(x, i = NULL) {

  if (!inherits(x, "item_pool")) {
    stop("'x' must be an 'item_pool' object")
  }
  if (!validObject(x)) {
    stop("'x' is not a valid 'item_pool' object")
  }

  if (is.null(i)) {
    return(x)
  }

  if (!all(i %in% 1:x@ni)) {
    stop("'i' contains item indices not defined in 'x'")
  }

  i      <- unique(i)
  raw    <- x@raw[i, ]
  raw_se <- x@raw_se[i, ]
  new_p  <- loadItemPool(raw, raw_se)

  return(new_p)

}

#' @noRd
combineItemPoolData <- function(raw1, raw2) {

  tmp       <- setdiff(names(raw1), names(raw2))
  raw2[tmp] <- NA
  tmp       <- setdiff(names(raw2), names(raw1))
  raw1[tmp] <- NA

  raw       <- rbind(raw1, raw2)

  idx       <- which(!duplicated(raw$ID))
  raw       <- raw[idx, ]

  return(raw)

}

#' @rdname item_pool-operators
#' @export
combineItemPool <- function(x1, x2) {

  if (!inherits(x1, "item_pool") || !inherits(x2, "item_pool")) {
    stop("operands must be 'item_pool' objects")
  }
  if (!validObject(x1)) {
    stop("'x1' is not a valid 'item_pool' object")
  }
  if (!validObject(x2)) {
    stop("'x2' is not a valid 'item_pool' object")
  }

  raw    <- combineItemPoolData(x1@raw   , x2@raw   )
  raw_se <- combineItemPoolData(x1@raw_se, x2@raw_se)
  o      <- loadItemPool(raw, raw_se)

  id     <- c(x1@raw$ID, x2@raw$ID)
  if (sum(duplicated(id)) > 0) {
    warning(sprintf("duplicate item IDs were removed: %s", paste0(id[duplicated(id)], collapse = ", ")))
  }

  return(o)

}

#' @aliases [,item_pool,numeric,ANY,ANY-method
#' @docType methods
#' @rdname item_pool-operators
setMethod(
  f = "[",
  signature = c("item_pool", "numeric"),
  definition = function(x, i, j, ...) {
    return(subsetItemPool(x, i))
  }
)

#' @aliases c,item_pool-method
#' @docType methods
#' @rdname item_pool-operators
setMethod(
  f = "c",
  signature = "item_pool",
  definition = function(x, ...) {
    arg  <- list(...)
    pool <- x
    for (p in arg) {
      pool <- combineItemPool(pool, p)
    }
    return(pool)
  }
)

#' @rdname item_pool-operators
#' @export
`+.item_pool` <- function(x1, x2) {
  if (!inherits(x1, "item_pool") || !inherits(x2, "item_pool")) {
    stop("operands must be 'item_pool' objects")
  }
  new_p <- combineItemPool(x1, x2)
  return(new_p)
}

#' @rdname item_pool-operators
#' @export
`-.item_pool` <- function(x1, x2) {
  if (!inherits(x1, "item_pool") || !inherits(x2, "item_pool")) {
    stop("operands must be 'item_pool' objects")
  }
  if (any(x2@id %in% x1@id)) {
    idx <- which(!(x1@id %in% x2@id))
    if (length(idx) > 0) {
      o <- subsetItemPool(x1, idx)
      return(o)
    } else {
      warning("subset not performed: the resulting 'item_pool' object is empty")
      return(x1)
    }
  }
  return(x1)
}

#' @rdname item_pool-operators
#' @export
`==.item_pool` <- function(x1, x2) {
  if (!inherits(x1, "item_pool") || !inherits(x2, "item_pool")) {
    stop("operands must be 'item_pool' objects")
  }
  return(identical(x1, x2))
}
