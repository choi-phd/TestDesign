#' @include constraints_operators.R
NULL

#' Create an item pool cluster object
#'
#' Create a \code{\linkS4class{item_pool_cluster}} object.
#'
#' @param x,... \code{\linkS4class{item_pool}} objects.
#' @param names (optional) names to use for \code{\linkS4class{item_pool}}.
#' @examples
#'
#' cluster <- makeItemPoolCluster(itempool_science, itempool_reading)
#' @export
#' @docType methods
#' @rdname item_pool_cluster-operators
setGeneric(
  name = "makeItemPoolCluster",
  def = function(x, ..., names = NULL) {
    standardGeneric("makeItemPoolCluster")
  }
)

#' @docType methods
#' @rdname item_pool_cluster-operators
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

    item_pool_cluster       <- new("item_pool_cluster")
    item_pool_cluster@np    <- np
    item_pool_cluster@pools <- vector(mode = "list", length = np)
    item_pool_cluster@names <- names

    for (i in 1:np) {
      if (!inherits(pools[[i]], "item_pool")) {
        stop(paste0("pool.list[[", i, "]] is not of class \"item_pool\""))
      }
      item_pool_cluster@pools[[i]] <- pools[[i]]
    }

    if (validObject(item_pool_cluster)) {
      return(item_pool_cluster)
    }

  }
)

#' @description \code{item_pool_cluster1 == item_pool_cluster2} tests equality of two item_pool_cluster objects.
#'
#' @param item_pool_cluster1 an \code{\linkS4class{item_pool_cluster}} object.
#' @param item_pool_cluster2 an \code{\linkS4class{item_pool_cluster}} object.
#'
#' @examples
#' cluster1 <- makeItemPoolCluster(itempool_science, itempool_reading)
#' cluster2 <- makeItemPoolCluster(cluster1@pools[[1]], cluster1@pools[[2]])
#' cluster1 == cluster2  ## TRUE
#'
#' @rdname item_pool_cluster-operators
#' @export
`==.item_pool_cluster` <- function(item_pool_cluster1, item_pool_cluster2) {
  if (!inherits(item_pool_cluster1, "item_pool_cluster") || !inherits(item_pool_cluster2, "item_pool_cluster")) stop("Operands must be 'item_pool_cluster' objects.")
  return(identical(item_pool_cluster1, item_pool_cluster2))
}
