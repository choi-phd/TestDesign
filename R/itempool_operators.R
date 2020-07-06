#' @include item_class.R
NULL

#' Basic operators for item pool objects
#'
#' @param select,i,j item indices to use in subsetting.
#' @param drop not used, exists for compatibility.
#'
#' @name itempool-operators
NULL

#' Create a subset of an item pool object
#'
#' \code{\link{subsetItemPool}} is a function to create a subset of an \code{\linkS4class{item_pool}} object.
#'
#' @param pool An \code{\linkS4class{item_pool}} object.
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:100)
#'
#' @rdname itempool-operators
#' @export
subsetItemPool <- function(pool, select = NULL) {
  if (!inherits(pool, "item_pool")) {
    stop("'pool' must be an 'item_pool' object")
  }
  if (!validObject(pool)) {
    stop("'pool' is not a valid 'item_pool' object")
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
    stop("'select' contains item indices not defined in 'pool'")
  }
}

#' @title Item pool and pool cluster operators
#'
#' @param pool1,pool2 an \code{\linkS4class{item_pool}} object.
#'
#' @description \code{pool1 + pool2} combines two \code{\linkS4class{item_pool}} objects.
#'
#' @examples
#' itempool <- combineItemPool(itempool_science, itempool_reading)
#'
#' @rdname itempool-operators
#' @export
combineItemPool <- function(pool1, pool2) {
  if (!inherits(pool1, "item_pool") || !inherits(pool2, "item_pool")) {
    stop("operands must be 'item_pool' objects")
  }
  if (!validObject(pool1)) {
    stop("'pool1' is not a valid 'item_pool' object")
  }
  if (!validObject(pool2)) {
    stop("'pool2' is not a valid 'item_pool' object")
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
    warning(sprintf("duplicate items were found and removed: %s", paste0(id[duplicated(id)], collapse = ", ")))
  }
  return(combined_pool)
}

#' @description \code{pool1 - pool2} excludes the items in \code{pool2} from \code{pool1}.
#'
#' @examples
#' subitempool <- subsetItemPool(itempool_science, 1:500)
#' itempool <- itempool_science - subitempool
#'
#' @rdname itempool-operators
#' @export
`-.item_pool` <- function(pool1, pool2) {
  if (!inherits(pool1, "item_pool") || !inherits(pool2, "item_pool")) {
    stop("operands must be 'item_pool' objects")
  }
  if (any(pool2@id %in% pool1@id)) {
    left <- which(!(pool1@id %in% pool2@id))
    if (length(left) > 0) {
      pool <- subsetItemPool(pool1, left)
      return(pool)
    } else {
      warning("subset not performed: the resulting 'item_pool' object is empty")
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
#' @rdname itempool-operators
#' @export
`==.item_pool` <- function(pool1, pool2) {
  if (!inherits(pool1, "item_pool") || !inherits(pool2, "item_pool")) {
    stop("operands must be 'item_pool' objects")
  }
  return(identical(pool1, pool2))
}

#' @aliases [,item_pool,numeric,ANY,ANY-method
#' @docType methods
#' @rdname itempool-operators
setMethod(
  f = "[",
  signature = c("item_pool", "numeric"),
  definition = function(x, i, j, ...) {
    return(subsetItemPool(x, i))
  }
)

#' @aliases c,item_pool-method
#' @docType methods
#' @rdname itempool-operators
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

#' Create an item pool cluster object
#'
#' Create a \code{\linkS4class{pool_cluster}} object.
#'
#' @param x,... \code{\linkS4class{item_pool}} objects.
#' @param names (optional) names to use for \code{\linkS4class{item_pool}}.
#' @examples
#'
#' cluster <- makeItemPoolCluster(itempool_science, itempool_reading)
#' @export
#' @docType methods
#' @rdname itempool-operators
setGeneric(
  name = "makeItemPoolCluster",
  def = function(x, ..., names = NULL) {
    standardGeneric("makeItemPoolCluster")
  }
)

#' @docType methods
#' @rdname itempool-operators
setMethod(
  f = "makeItemPoolCluster",
  signature = "item_pool",
  definition = function(x, ..., names = NULL) {

    pools <- list(x, ...)

    np <- length(pools)

    if (is.null(names)) {
      names <- paste0("Pool_", 1:np)
    } else {
      if (length(names) != np) stop("makeItemPoolCluster: length(names) does not match length(...)")
    }

    pool_cluster       <- new("pool_cluster")
    pool_cluster@np    <- np
    pool_cluster@pools <- vector(mode = "list", length = np)
    pool_cluster@names <- names

    for (i in 1:np) {
      if (!inherits(pools[[i]], "item_pool")) {
        stop(paste0("pool.list[[", i, "]] is not of class \"item_pool\""))
      }
      pool_cluster@pools[[i]] <- pools[[i]]
    }

    if (validObject(pool_cluster)) {
      return(pool_cluster)
    }

  }
)

#' @description \code{pool_cluster1 == pool_cluster2} tests equality of the two pool_cluster objects.
#'
#' @param pool_cluster1 a \code{\linkS4class{pool_cluster}} object.
#' @param pool_cluster2 a \code{\linkS4class{pool_cluster}} object.
#'
#' @examples
#' cluster1 <- makeItemPoolCluster(itempool_science, itempool_reading)
#' cluster2 <- makeItemPoolCluster(cluster1@pools[[1]], cluster1@pools[[2]])
#' cluster1 == cluster2  ## TRUE
#'
#' @rdname itempool-operators
#' @export
`==.pool_cluster` <- function(pool_cluster1, pool_cluster2) {
  if (!inherits(pool_cluster1, "pool_cluster") || !inherits(pool_cluster2, "pool_cluster")) stop("Operands must be 'pool_cluster' objects.")
  return(identical(pool_cluster1, pool_cluster2))
}
