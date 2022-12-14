library(shiny, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE, warn.conflicts = FALSE))
library(DT, quietly = TRUE, warn.conflicts = FALSE)
library(TestDesign, quietly = TRUE)

solvers <- c("lpSolve", "Rsymphony", "gurobi", "Rglpk")
accepted_files <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")
css_y <- "overflow-y:scroll; max-height: 65vh"

parseText <- function(arg_text) {
  # Limit text to only have legit values
  txt <- gsub("[^0-9\\., \\-]", "", arg_text)
  return(txt == arg_text)
}

parseObject <- function(arg_object) {
  if (is.null(arg_object)) {
    return(NULL)
  }
  return(arg_object)
}

first_obj <- TRUE

assignObject <- function(obj, objname, desc) {
  if (first_obj) {
    first_obj <<- FALSE
    message("\nRefresh the environment tab to see the objects in the list.")
  }
  assign(objname, obj, envir = .GlobalEnv)
  tmp <- sprintf("%-48s assigned to : %s", desc, objname)
  message(tmp)
}

updateLogs <- function(v, newlog) {
  v$logs      <- c(v$logs, newlog)
  v$logs_text <- paste0(v$logs, collapse = "\n")
  return(v)
}

getTempFilePath <- function(fname) {
  return(file.path(tempdir(), fname))
}
