# Documentation progress
# Phase 2. Add simple descriptions: COMPLETE

#' An S4 class to represent a cluster of item pools
#' 
#' @slot np A scalar to indicate the number of item pools in the cluster.
#' @slot pools A list of \code{item.pool} objects.
#' @slot names A character vector of item pool names of length np.
setClass("pool.cluster",
         slots = c(np = "numeric",
                   pools = "list",
                   names = "character"),
         prototype = list(np = numeric(0),
                          pools = list(0),
                          names = character(0)),
         validity = function(object){
           if (length(object@pools) != object@np) stop("length(pools) is not equal to np")
           if (length(object@names) != object@np) stop("length(names) is not equal to np")
           return(TRUE)
         }
)

