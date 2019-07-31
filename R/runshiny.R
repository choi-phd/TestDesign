#' Launch Shiny app
#'
#' Launch Shiny app locally.
#'
#' @export
#' @aliases app
#' @rdname OAT
OAT <- function() {
  app_dir <- system.file("shiny", package = "TestDesign")
  if (app_dir == "") {
    stop("Could not find the application directory. Try re-installing `TestDesign`.", call. = FALSE)
  }

  tmp <- NULL
  if (!requireNamespace("shiny")) {
    tmp <- c(tmp, "'shiny'")
  }
  if (!requireNamespace("shinythemes")) {
    tmp <- c(tmp, "'shinythemes'")
  }
  if (!requireNamespace("shinyWidgets")) {
    tmp <- c(tmp, "'shinyWidgets'")
  }
  if (!requireNamespace("shinyjs")) {
    tmp <- c(tmp, "'shinyjs'")
  }
  if (!requireNamespace("DT")) {
    tmp <- c(tmp, "'DT'")
  }

  if (!is.null(tmp)) {
    tmp <- paste(tmp, collapse = ", ")
    message("Shiny application requires additional packages.")
    message("Run the following code to install:")
    message("")
    tmp <- paste0("install.packages(c(", tmp, "))")
    message(tmp)
  } else {
    shiny::runApp(app_dir, display.mode = "normal")
  }
}

#' @export
#' @rdname OAT

app <- function() {
  OAT()
}
