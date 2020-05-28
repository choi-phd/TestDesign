#' @include datasets.R
NULL

#' Open TestDesign app
#'
#' \code{\link{TestDesign}} is a caller function to open the Shiny interface of TestDesign package.
#'
#' @examples
#'
#' \dontrun{
#' if (interactive()) {
#'   TestDesign()
#' }
#' }
#'
#' @rdname TestDesign
#' @export
TestDesign <- function() {
  app_dir <- system.file("shiny", package = "TestDesign")
  if (app_dir == "") {
    stop("Could not find the application directory. Try re-installing `TestDesign`.", call. = FALSE)
  }

  pkgs <- c("shiny", "shinythemes", "shinyWidgets", "shinyjs", "DT")
  tmp <- NULL

  for (pkg in pkgs) {
    if (length(find.package(pkg, quiet = TRUE)) == 0) {
      tmp <- c(tmp, sprintf("'%s'", pkg))
    }
  }

  if (!is.null(tmp)) {
    tmp <- paste(tmp, collapse = ", ")
    message("Shiny application requires additional packages.")
    message("Run the following code to install:")
    message("")
    tmp <- paste0("install.packages(c(", tmp, "))")
    message(tmp)
  } else {
    if (!isNamespaceLoaded("shiny")) {
      attachNamespace("shiny")
    }
    shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
  }
}

#' Open TestDesign app
#'
#' \code{\link{app}} and \code{\link{OAT}} are aliases of \code{\link{TestDesign}}.
#'
#' \code{\link{TestDesign}} is a caller function to open the Shiny interface of TestDesign package.
#'
#' @examples
#'
#' \dontrun{
#' if (interactive()) {
#'   TestDesign()
#' }
#' }
#'
#' @rdname TestDesign_alias
#' @export
app <- function() {
  TestDesign()
}

#' @rdname TestDesign_alias
#' @export
OAT <- function() {
  TestDesign()
}
