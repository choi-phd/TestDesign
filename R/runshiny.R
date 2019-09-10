#' @include datasets.R
NULL

#' Launch Shiny app
#'
#' Launch Shiny app locally.
#'
#' @examples
#' if (interactive()) {
#'   OAT()
#'   ## or
#'   app()
#' }
#'
#' @aliases app
#' @rdname OAT
#' @export

OAT <- function() {
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

#' @rdname OAT
#' @export

app <- function() {
  OAT()
}
