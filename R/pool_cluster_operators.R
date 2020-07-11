#' @include item_class.R
NULL

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
#' @rdname pool_cluster-operators
setGeneric(
  name = "makeItemPoolCluster",
  def = function(x, ..., names = NULL) {
    standardGeneric("makeItemPoolCluster")
  }
)

#' @docType methods
#' @rdname pool_cluster-operators
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
#' @rdname pool_cluster-operators
#' @export
`==.pool_cluster` <- function(pool_cluster1, pool_cluster2) {
  if (!inherits(pool_cluster1, "pool_cluster") || !inherits(pool_cluster2, "pool_cluster")) stop("Operands must be 'pool_cluster' objects.")
  return(identical(pool_cluster1, pool_cluster2))
}
