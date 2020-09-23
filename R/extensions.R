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
#'
#' @examples
#' summary(itempool_science)
#' summary(itemattrib_science)
#'
#' cfg <- createStaticTestConfig()
#' solution <- Static(cfg, constraints_science)
#' summary(solution)
#'
#' cfg <- createShadowTestConfig()
#' solution <- Shadow(cfg, constraints_science, true_theta = seq(-1, 1, 1))
#' summary(solution)
#'
#' @docType methods
#' @rdname summary-methods
#' @export
NULL

#' Summary classes
#'
#' @name summary-classes
NULL
