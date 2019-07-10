#' Launch Shiny app
#' 
#' Launch Shiny app locally.
#' 
#' @export
#' @aliases app
#' @rdname guiShadow
guiShadow = function() {
  appDir = system.file("shiny", package = "Shadow")
  if (appDir == "") {
    stop("Could not find application directory. Try re-installing `Shadow`.", call. = FALSE)
  }
  
  tmp = NULL
  if (!requireNamespace("shiny")) tmp = c(tmp, "'shiny'")
  if (!requireNamespace("shinythemes")) tmp = c(tmp, "'shinythemes'")
  if (!requireNamespace("shinyWidgets")) tmp = c(tmp, "'shinyWidgets'")
  if (!requireNamespace("shinyjs")) tmp = c(tmp, "'shinyjs'")
  if (!requireNamespace("DT")) tmp = c(tmp, "'DT'")

  if (!is.null(tmp)){
    tmp = paste(tmp, collapse = ", ")
    message("Shiny application requires additional packages.")
    message("Run the following code to install:")
    message("")
    tmp = paste0("install.packages(", tmp, ")")
    message(tmp)
  } else {
    shiny::runApp(appDir, display.mode = "normal")
  }
}

#' @export
#' @rdname guiShadow

app = function() {
  guiShadow()
}