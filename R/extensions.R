#' @include RcppExports.R import.R
NULL

#' @name show
#' @title Extension of show() for objects in TestDesign package
#'
#' @description Extension of show() for objects in TestDesign package
#'
#' @param object an object to display.
#'
#' @docType methods
#' @rdname show-methods
#' @export
NULL

#' @name print
#' @title Extension of print() for objects in TestDesign package
#'
#' @description Extension of print() for objects in TestDesign package
#'
#' @param x an object to print.
#'
#' @docType methods
#' @rdname print-methods
#' @export
NULL

#' @name plot
#' @title Extension of plot() for objects in TestDesign package
#'
#' @description Extension of plot() for objects in TestDesign package
#'
#' @details The base \code{plot()} does not allow directly storing the plot as a object. \code{TestDesign::plot()} calls \code{recordPlot()} internally to allow this. This adds convenience, but also introduces a caveat when using with the 'knitr' package.
#' The caveat is that using \code{plot()} alone will not render the plot. This issue can be resolved by using \code{p <- plot()} and \code{print(p)} in two separate blocks in the markdown document.
#'
#' @docType methods
#' @rdname plot-methods
#' @export
NULL

#' @name summary
#' @title Extension of summary() for objects in TestDesign package
#'
#' @description Extension of summary() for objects in TestDesign package
#'
#' @param object an object to summarize.
#' @param simple if \code{TRUE}, do not print constraints. (default = \code{FALSE})
#'
#' @examples
#' summary(itempool_science)
#' summary(itemattrib_science)
#'
#' cfg <- createStaticTestConfig()
#' solution <- Static(cfg, constraints_science)
#' summary(solution)
#' summary(solution, simple = TRUE)
#'
#' cfg <- createShadowTestConfig()
#' solution <- Shadow(cfg, constraints_science, true_theta = seq(-1, 1, 1))
#' summary(solution)
#' summary(solution, simple = TRUE)
#'
#' @docType methods
#' @rdname summary-methods
#' @export
NULL

#' Summary classes
#'
#' @name summary-classes
NULL
