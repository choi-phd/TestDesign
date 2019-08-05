library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(TestDesign)
library(DT)

accepted_files <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")
css_y <- "overflow-y:scroll; max-height: 65vh"

ui <- fluidPage(
  theme = shinytheme("lumen"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      HTML("
        h3 { font-size: 125%; }
        i { display: inline-block; margin-right: 0.2em; }
        label, .form-group, .progress { margin-bottom: 0px; }
        .btn { width: 100%; }"
      )
    )
  ),

  titlePanel("TestDesign: Optimal Test Assembly"),

  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type = "text/css", "select { min-width: 100%; max-width: 100%; }"),
        tags$style(type = "text/css", ".span4 { min-width: 100%; max-width: 100%; }"),
        tags$style(type = "text/css", ".well { min-width: 100%; max-width: 100%; }")
      ),

      dropdownButton(
        h3(""),
        label = "Load files",
        fileInput("itempool_file",   buttonLabel = "Item parameters",                 label = NULL, accept = accepted_files),
        fileInput("itemse_file",     buttonLabel = "Item standard errors (optional)", label = NULL, accept = accepted_files),
        fileInput("itemattrib_file", buttonLabel = "Item attributes",                 label = NULL, accept = accepted_files),
        fileInput("stimattrib_file", buttonLabel = "Stimulus attributes (optional)",  label = NULL, accept = accepted_files),
        fileInput("const_file",      buttonLabel = "Constraints",                     label = NULL, accept = accepted_files),
        fileInput("content_file",    buttonLabel = "Item contents (optional)",        label = NULL, accept = accepted_files),
        checkboxGroupButtons(
          inputId = "clear_files", justified = TRUE,
          choices = c("Clear files"), checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass"))
        ),
        circle = FALSE, status = "primary", icon = icon("file-import"), width = "100%"
      ),

      h3(""),

      radioGroupButtons(
        inputId = "problemtype", justified = TRUE,
        choiceNames = c("Static", "Adaptive"), choiceValues = 1:2, selected = 1
      ),
      radioGroupButtons(
        inputId = "solvertype", justified = TRUE,
        choices = c("lpSolve", "Symphony", "Gurobi", "GLPK"), checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass"))
      ),
      radioGroupButtons(
        inputId = "objtype", justified = TRUE, label = h3("Objective type:"),
        choices = c("TCC", "TIF", "MAXINFO")
      ),

      textInput("thetas", label = h3("Theta values (comma-separated)"), value = "0, 1"),
      textInput("targets", label = h3("Target values (comma-separated)"), value = "15, 20"),

      checkboxGroupButtons(
        inputId = "maxinfo_button", justified = TRUE,
        choices = c("Check obtainable info range"), checkIcon = list(yes = icon("less-than-equal"), no = icon("less-than-equal"))
      ),

      dropdownButton(
        inputId = "simulation_dropdown", label = "Simulation settings",
        radioGroupButtons(
          inputId = "simulee_theta_distribution", justified = TRUE,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("simulee_theta_params", label = "True theta distribution parameters", value = "0, 1"),
        textInput("n_simulees", label = h3("# of simulees"), value = "1"),
        textInput("simulee_id", label = h3("Display plots for simulee:"), value = "1"),
        circle = FALSE, icon = icon("database"), width = "100%", status = "danger"
      ),

      dropdownButton(
        inputId = "exposure_dropdown", label = "Exposure control settings",
        radioGroupButtons(
          inputId = "exposure_method", justified = TRUE,
          choices = c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")
        ),
        sliderInput("exposure_fading_factor", label = h3("Fading factor"), min = .9, max = 1.0, value = 1.0, step = .01),
        textInput("exposure_acc_factor", label = h3("Acceleration factor"), value = "2"),
        circle = FALSE, icon = icon("cog"), width = "100%"
      ),

      dropdownButton(
        inputId = "theta_settings", label = "Estimation Settings for interim & final thetas",
        h3("Interim theta"),
        radioGroupButtons(
          inputId = "interim_method", justified = TRUE,
          choices = c("EAP", "FB", "EB")
        ),
        radioGroupButtons(
          inputId = "interim_prior", justified = TRUE,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("interim_prior_par", label = "Prior distribution parameters", value = "0, 1"),
        h3("Final theta"),
        radioGroupButtons(
          inputId = "final_method", justified = TRUE,
          choices = c("EAP", "FB", "EB")
        ),
        radioGroupButtons(
          inputId = "final_prior", justified = TRUE,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("final_prior_par", label = "Prior distribution parameters", value = "0, 1"),
        circle = FALSE, icon = icon("cog"), width = "100%"
      ),

      radioGroupButtons(
        inputId = "item_selection_method", justified = TRUE, label = h3("Item selection method"),
        choices = c("MFI", "MPWI", "FB", "EB")
      ),
      radioGroupButtons(
        inputId = "content_balancing_method", justified = TRUE, label = h3("Content balancing method"),
        choices = c("NONE", "STA")
      ),

      dropdownButton(
        inputId = "refresh_policy_dropdown", label = "Refresh policy",
        radioGroupButtons(
          inputId = "refresh_policy", justified = TRUE, direction = "vertical",
          choices = c("ALWAYS", "POSITION", "INTERVAL", "THRESHOLD", "INTERVAL-THRESHOLD", "SET")
        ),
        textInput("refresh_threshold", label = h3("Refresh when theta change exceeds"), value = "0.0"),
        textInput("refresh_position", label = h3("Refresh at item positions"), value = "1"),
        textInput("refresh_interval", label = h3("Refresh at item intervals (1 = always)"), value = "1"),
        circle = FALSE, icon = icon("cog"), width = "100%"
      ),

      downloadButton("export_data", "Export data")
    ),

    mainPanel(
      tags$head(
        tags$style(type = "text/css", "#text_output {background-color: rgba(64,64,64,1); color: cyan;}")
      ),
      disabled(
        checkboxGroupButtons(
          inputId = "run_solver", justified = TRUE,
          choices = c("Run Solver"), checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")), status = "primary"
        )
      ),
      verbatimTextOutput("text_output", placeholder = TRUE),
      progressBar(id = "pb", value = 0, total = 1, display_pct = TRUE),
      hr(),
      tabsetPanel(
        id = "tabs",
        tabPanel("Main",                value = 1, plotOutput("plot_output", width = "100%", height = "65vh")),
        tabPanel("Output",              value = 2, plotOutput("shadow_chart", width = "100%", height = "65vh")),
        tabPanel("Output",              value = 3, DTOutput("results"), style = css_y),
        tabPanel("Item parameters",     value = 4, DTOutput("table_itempool"), style = css_y),
        tabPanel("Item attributes",     value = 5, DTOutput("table_itemattrib"), style = css_y),
        tabPanel("Stimulus attributes", value = 6, DTOutput("table_stimattrib"), style = css_y),
        tabPanel("Constraints",         value = 7, DTOutput("table_constraints"), style = css_y)
      )
    )
  )
)

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

server <- function(input, output, session) {
  v <- reactiveValues(
    itempool_exists = FALSE,
    itemse_exists = FALSE,
    const_exists = FALSE,
    content_exists = FALSE,
    stimattrib_exists = FALSE,
    problemtype = 0
  )

  observeEvent(input$itempool_file, {
    if (!is.null(input$itempool_file)) {
      v$itempool <- try(loadItemPool(input$itempool_file$datapath))
      if (class(v$itempool) == "item_pool") {
        v$itempool_exists <- TRUE
        v$text <- "Step 1. Item parameter file: OK"
        v$ipar <- v$itempool@ipar
        assignObject(v$itempool,
          "shiny_itempool",
          "Item parameters (full object)")
        assignObject(v$ipar,
          "shiny_itempool_ipar",
          "Item parameters (matrix)")
      } else {
        v$itempool_exists <- FALSE
        v$text <- "Step 1 Error: Item parameters are not in the correct format."
      }
    }
  })

  observeEvent(input$itemse_file, {
    if (!is.null(input$itemse_file) & v$itempool_exists) {
      v$itempool <- try(loadItemPool(input$itempool_file$datapath, se.file.csv = input$itemse_file$datapath))
      if (class(v$itempool) == "item_pool") {
        v$itemse_exists <- TRUE
        v$text <- "Optional Step. Item standard error file: OK"
        assignObject(v$itempool,
          "shiny_itempool",
          "Item parameters (full object)")
      } else {
        v$itemse_exists <- FALSE
        v$text <- "Optional Step Error: Item standard errors are not in the correct format."
      }
    }
  })

  observeEvent(input$itemattrib_file, {
    if (!is.null(input$itemattrib_file) & v$itempool_exists) {
      v$itemattrib <- try(loadItemAttrib(input$itemattrib_file$datapath, v$itempool))
      if (class(v$itemattrib) == "data.frame") {
        v$itemattrib_exists <- TRUE
        v$text <- "Step 2. Item attributes: OK"
        assignObject(v$itemattrib,
          "shiny_itemattrib",
          "Item attributes")
      } else {
        v$itemattrib_exists <- FALSE
        v$text <- "Step 2 Error: Item attributes are not in the correct format."
      }
    }
  })

  observeEvent(input$stimattrib_file, {
    if (!is.null(input$stimattrib_file) & v$itempool_exists & v$itemattrib_exists) {
      v$stimattrib <- try(loadStAttrib(input$stimattrib_file$datapath, v$itemattrib))
      if (class(v$stimattrib) == "data.frame") {
        v$stimattrib_exists <- TRUE
        v$text <- "Optional Step. Stimulus attributes: OK"
        assignObject(v$stimattrib,
          "shiny_stimattrib",
          "Stimulus attributes")
      } else {
        v$stimattrib_exists <- FALSE
        v$text <- "Optional Step Error: Stimulus attributes are not in the correct format."
      }
    }
  })

  observeEvent(input$const_file, {
    if (!is.null(input$const_file) & v$itempool_exists & v$itemattrib_exists) {
      if (v$stimattrib_exists) {
        v$const <- try(loadConstraints(input$const_file$datapath, v$itempool, v$itemattrib, v$stimattrib))
      } else {
        v$const <- try(loadConstraints(input$const_file$datapath, v$itempool, v$itemattrib))
      }
      if (class(v$const) == "list") {
        v$const_exists <- TRUE
        v$text <- "Step 3. Constraints: OK. Press button to run solver."
        v$constraints <- v$const$constraints
        assignObject(v$const,
          "shiny_const",
          "Constraints (full object)")
        assignObject(v$constraints,
          "shiny_constraints",
          "Constraints (data.frame)")
      } else {
        v$const_exists <- FALSE
        v$text <- "Step 3 Error: Constraints are not in the correct format."
      }
      shinyjs::toggleState("run_solver", v$const_exists)
    }
  })

  observeEvent(input$content_file, {
    if (!is.null(input$content_file)) {
      v$content <- try(read.csv(input$content_file$datapath))
      if (class(v$content) == "data.frame") {
        v$content_exists <- TRUE
        v$text <- "Optional Step. Item contents: OK."
        assignObject(v$content,
          "shiny_content",
          "Item contents")
      } else {
        v$content_exists <- FALSE
        v$text <- "Optional Step Error: Item contents are not in the correct format."
      }
    }
  })

  observeEvent(input$clear_files, {
    shinyjs::reset("itempool_file")
    shinyjs::reset("itemse_file")
    shinyjs::reset("itemattrib_file")
    shinyjs::reset("stimattrib_file")
    shinyjs::reset("const_file")
    shinyjs::reset("content_file")
    v$itempool   <- NULL
    v$itemattrib <- NULL
    v$stimattrib <- NULL
    v$const      <- NULL
    v$content    <- NULL
    v$itempool_exists   <- FALSE
    v$itemattrib_exists <- FALSE
    v$stimattrib_exists <- FALSE
    v$const_exists      <- FALSE
    v$content_exists    <- FALSE
    v$text <- "Files cleared."
    updateCheckboxGroupButtons(
      session = session,
      inputId = "clear_files",
      selected = character(0)
    )
  })

  observeEvent(input$problemtype, {
    shinyjs::toggle("objtype",                  condition = input$problemtype == 1)
    shinyjs::toggle("thetas",                   condition = input$problemtype == 1)
    shinyjs::toggle("targets",                  condition = input$problemtype == 1)
    shinyjs::toggle("maxinfo_button",           condition = input$problemtype == 1)
    shinyjs::toggle("exposure_dropdown",        condition = input$problemtype == 2)
    shinyjs::toggle("theta_settings",           condition = input$problemtype == 2)
    shinyjs::toggle("item_selection_method",    condition = input$problemtype == 2)
    shinyjs::toggle("content_balancing_method", condition = input$problemtype == 2)
    shinyjs::toggle("refresh_policy_dropdown",  condition = input$problemtype == 2)
    shinyjs::toggle("simulation_dropdown",      condition = input$problemtype == 2)
    if (input$problemtype == 2) {
      showTab("tabs", target = "2")
      hideTab("tabs", target = "3")
    } else {
      hideTab("tabs", target = "2")
      showTab("tabs", target = "3")
    }
  })

  observeEvent(input$objtype, {
    shinyjs::toggleState("targets", input$objtype != "MAXINFO")
  })

  observeEvent(input$refresh_policy, {
    shinyjs::toggle("refresh_threshold", condition = input$refresh_policy %in% c("THRESHOLD", "INTERVAL-THRESHOLD"))
    shinyjs::toggle("refresh_interval",  condition = input$refresh_policy %in% c("INTERVAL", "INTERVAL-THRESHOLD"))
    shinyjs::toggle("refresh_position",  condition = input$refresh_policy %in% c("POSITION"))
  })

  observeEvent(input$simulee_id, {
    if (v$problemtype == 2) {
      if (!is.null(v$fit)) {
        if (parseText(input$simulee_id)) {
          eval(parse(text = sprintf("simulee_id = c(%s)[1]", input$simulee_id)))
          v$simulee_id <- min(simulee_id, v$n_simulees)
        } else {
          v$text <- "Number of simulees should be an integer."
          break
        }
        v$plot_output <- plotCAT(v$fit, v$simulee_id)
        assignObject(v$plot_output,
          sprintf("shiny_audit_plot_%i", v$simulee_id),
          sprintf("Audit trail plot for simulee %i", v$simulee_id)
        )
        v$shadow_chart <- plotShadow(v$fit, v$const, v$simulee_id)
        assignObject(v$shadow_chart,
          sprintf("shiny_shadow_chart_%i", v$simulee_id),
          sprintf("Shadow test chart for simulee %i", v$simulee_id)
        )
      }
    }
  })

  observeEvent(input$maxinfo_button, {
    if (v$itempool_exists & v$const_exists) {
      v$info_range_plot <- plotMaxInfo(v$itempool, v$const)
      v$plot_output <- v$info_range_plot
      assignObject(v$info_range_plot,
        "shiny_info_range_plot",
        "Obtainable info range plot"
      )
    }
    updateCheckboxGroupButtons(
      session = session,
      inputId = "maxinfo_button",
      selected = character(0)
    )
  })


  observeEvent(input$run_solver, {
    shinyjs::disable("run_solver")

    for (do in 1) {
      if (input$problemtype == 1 & v$const_exists) {
        v$problemtype <- 1

        conf <- new("config_ATA")
        conf@MIP$solver <- input$solvertype

        if (parseText(input$thetas)) {
          eval(parse(text = sprintf("conf@item_selection$target_location <- c(%s)", input$thetas)))
        } else {
          v$text <- "Theta values should be comma-separated numbers."
          break
        }

        conf@item_selection$method <- input$objtype

        if (conf@item_selection$method != "MAXINFO") {
            if (parseText(input$targets)) {
            eval(parse(text = sprintf("conf@item_selection$target_value <- c(%s)", input$targets)))
          } else {
            v$text <- "Target values should be comma-separated numbers."
            break
          }
        } else {
          conf@item_selection$target_value <- NULL
        }

        conf@item_selection$target_weight <- rep(1, length(conf@item_selection$target_location))

        assignObject(conf,
          "shiny_config_ATA",
          "config_ATA object"
        )

        v$text <- "Solving.."

        v$fit <- ATA(conf, v$const, T)
        assignObject(v$fit,
          "shiny_ATA",
          "ATA solution object"
        )

        if (is.null(v$fit$MIP)) {
          v$text <- "Solver did not find a solution. Try relaxing the target values."
        } else {
          v$plot_output <- v$fit$plot

          v$text <- sprintf("%-10s : solved in %3.3fs", conf@MIP$solver, v$fit$solve_time[3])
          v$selected_index <- which(v$fit$MIP$solution == 1)
          v$selected_index <- v$selected_index[v$selected_index <= v$itempool@ni]

          v$selected_item_names <- v$itemattrib[v$selected_index, ][["ID"]]
          v$selected_item_attribs <- v$itemattrib[v$selected_index, ]
          assignObject(v$selected_item_attribs,
            "shiny_selected_item_attribs",
            "Selected item attributes"
          )
          if (v$content_exists) {
            v$index_from_content <- v$content$ID %in% v$selected_item_names
            v$selected_item_contents <- v$content[v$index_from_content, ]
            assignObject(v$selected_item_contents,
              "shiny_selected_item_contents",
              "Selected item contents"
            )
            v$results <- v$selected_item_contents
          } else {
            v$results <- v$selected_item_attribs
          }
        }
      }

      if (input$problemtype == 2 & v$const_exists) {
        v$problemtype <- 2

        if (input$exposure_method == "BIGM-BAYESIAN") {
          if (!input$interim_method %in% c("FB", "EB")) {
            v$text <- "BIGM-BAYESIAN requires interim methods FB or EB."
            break
          }
        }
        if (input$exposure_method == "BIGM-BAYESIAN") {
          if (!input$item_selection_method %in% c("FB", "EB")) {
            v$text <- "BIGM-BAYESIAN requires item selection method FB or EB."
            break
          }
        }

        if (parseText(input$n_simulees)) {
          eval(parse(text = sprintf("v$n_simulees <- c(%s)[1]", input$n_simulees)))
          v$n_simulees <- min(100, v$n_simulees)
          updateProgressBar(session = session, id = "pb", value = 0, total = v$n_simulees)
        } else {
          v$text <- "Number of simulees should be an integer."
          break
        }

        if (parseText(input$simulee_id)) {
          eval(parse(text = sprintf("v$simulee_id <- c(%s)[1]", input$simulee_id)))
        } else {
          v$text <- "Number of simulees should be an integer."
          break
        }

        if (parseText(input$simulee_theta_params)) {
          eval(parse(text = sprintf("v$simulee_theta_params <- c(%s)[1:2]", input$simulee_theta_params)))
        } else {
          v$text <- "Theta distribution parameters should be two numbers."
          break
        }

        if (input$simulee_theta_distribution == "NORMAL") {
          true_theta <- rnorm(v$n_simulees, v$simulee_theta_params[1], v$simulee_theta_params[2])
        }
        if (input$simulee_theta_distribution == "UNIF") {
          true_theta <- runif(v$n_simulees, min = v$simulee_theta_params[1], max = v$simulee_theta_params[2])
        }
        assignObject(true_theta,
          "shiny_truetheta",
          "Simulation: true theta values"
        )

        theta_grid <- seq(-3, 3, 1)
        resp_data <- makeTest(v$itempool, theta_grid, info_type = "FISHER", true_theta = true_theta)@data
        assignObject(resp_data,
          "shiny_respdata",
          "Simulation: response data"
        )

        conf <- new("config_Shadow")

        # parse exposure control settings

        conf@exposure_control$fading_factor <- input$exposure_fading_factor

        if (parseText(input$exposure_acc_factor)) {
          eval(parse(text = sprintf("conf@exposure_control$acceleration_factor <- c(%s)[1]", input$exposure_acc_factor)))
          if (conf@exposure_control$acceleration_factor < 1) {
            v$text <- "Acceleration factor should be a number at least 1."
            break
          }
        } else {
          v$text <- "Acceleration factor should be a number at least 1."
          break
        }

        conf@exposure_control$method <- input$exposure_method
        conf@exposure_control$diagnostic_stats <- TRUE

        conf@interim_theta$method <- input$interim_method
        conf@interim_theta$prior_dist <- input$interim_prior
        conf@final_theta$method <- input$final_method
        conf@final_theta$prior_dist <- input$final_prior
        conf@item_selection$method <- input$item_selection_method

        if (conf@interim_theta$method == "FB" & v$itemse_exists == F) {
          v$text <- "FB interim method requires the standard errors associated with the item parameters."
          break
        }
        if (parseText(input$interim_prior_par)) {
          eval(parse(text = paste0("conf@interim_theta$prior_par = c(", input$interim_prior_par, ")")))
          if (length(conf@interim_theta$prior_par) != 2) {
            v$text <- "Interim prior parameters should be two numeric values."
            break
          }
        } else {
          v$text <- "Interim prior parameters should be two values."
          break
        }
        if (parseText(input$final_prior_par)) {
          eval(parse(text = sprintf("conf@final_theta$prior_par = c(%s)", input$final_prior_par)))
          if (length(conf@final_theta$prior_par) != 2) {
            v$text <- "Final prior parameters should be two values."
            break
          }
        } else {
          v$text <- "Final prior parameters should be two values."
          break
        }


        if (conf@item_selection$method == "FB") {
          if (conf@interim_theta$method != "FB") {
            v$text <- "FB item selection method requires FB interim method."
            break
          }
        }

        conf@refresh_policy$method <- input$refresh_policy

        if (parseText(input$refresh_interval)) {
          eval(parse(text = sprintf("conf@refresh_policy$interval <- c(%s)[1]", input$refresh_interval)))
          if (conf@refresh_policy$interval < 1 |
              all(conf@refresh_policy$interval != as.integer(conf@refresh_policy$interval))) {
            v$text <- "Refresh interval should be an integer of at least 1."
            break
          }
        }
        if (parseText(input$refresh_position)) {
          eval(parse(text = sprintf("conf@refresh_policy$position <- c(%s)", input$refresh_position)))
          if (conf@refresh_policy$position < 1 |
              all(conf@refresh_policy$position != as.integer(conf@refresh_policy$position))) {
            v$text <- "Refresh positions should be comma-separated integers of at least 1."
            break
          }
        }
        if (parseText(input$refresh_threshold)) {
          eval(parse(text = sprintf("conf@refresh_policy$threshold <- c(%s)[1]", input$refresh_threshold)))
          if (conf@refresh_policy$threshold < 0) {
            v$text <- "Refresh threshold should be a positive value."
            break
          }
        }

        eval(parse(text = sprintf("conf@item_selection$target_location <- c(%s)", input$thetas)))

        conf@MIP$solver <- input$solvertype

        assignObject(conf,
          "shiny_config_Shadow",
          "config_Shadow object"
        )

        v$text <- "Simulating.."
        v$time <- Sys.time()
        v$fit <- Shadow(v$itempool, conf, true_theta, constraints = v$const, prior = NULL, prior_par = c(0, 1), data = resp_data, session = session)
        message("\n")
        assignObject(v$fit,
          "shiny_Shadow",
          "Simulation result"
        )

        if (v$simulee_id > v$n_simulees) {
          v$simulee_id <- 1
        }

        v$plot_output <- plotCAT(v$fit, v$simulee_id)
        assignObject(v$plot_output,
          sprintf("shiny_thetaplot_%i", v$simulee_id),
          sprintf("Theta plot for simulee %i", v$simulee_id)
        )
        v$shadow_chart <- plotShadow(v$fit, v$const, v$simulee_id)
        assignObject(v$shadow_chart,
          sprintf("shiny_shadow_chart_%i", v$simulee_id),
          sprintf("Shadow test chart for simulee %i", v$simulee_id)
        )

        v$time <- Sys.time() - v$time

        v$text <- sprintf("%-10s: simulation complete in %3.3fs", conf@MIP$solver, v$time)
      }
    }

    updateCheckboxGroupButtons(
      session = session,
      inputId = "run_solver",
      selected = character(0)
    )
    shinyjs::enable("run_solver")
  })

  output$table_itempool    <- renderDT(parseObject(v$ipar), options = list(pageLength = 100))
  output$table_itemattrib  <- renderDT(parseObject(v$itemattrib), options = list(pageLength = 100))
  output$table_stimattrib  <- renderDT(parseObject(v$stimattrib), options = list(pageLength = 100))
  output$table_constraints <- renderDT(parseObject(v$constraints), options = list(pageLength = 100))
  output$results      <- renderDT(parseObject(v$results), options = list(pageLength = 100))
  output$text_output  <- renderText(parseObject(v$text))
  output$plot_output  <- renderPlot(parseObject(v$plot_output))
  output$shadow_chart <- renderPlot(parseObject(v$shadow_chart))

  output$export_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep = "")
    },
    content = function(fname) {
      fs <- c()
      setwd(tempdir())

      if (!is.null(v$ipar)) {
        path <- "raw_data_item_params.csv"
        fs <- c(fs, path)
        write.csv(v$ipar, path, row.names = F)
      }
      if (!is.null(v$itemattrib)) {
        path <- "raw_data_item_attribs.csv"
        fs <- c(fs, path)
        write.csv(v$itemattrib, path, row.names = F)
      }
      if (!is.null(v$stimattrib)) {
        path <- "raw_data_stim_attribs.csv"
        fs <- c(fs, path)
        write.csv(v$stimattrib, path, row.names = F)
      }
      if (!is.null(v$constraints)) {
        path <- "raw_data_constraints.csv"
        fs <- c(fs, path)
        write.csv(v$constraints, path, row.names = F)
      }
      if (!is.null(v$content)) {
        path <- "raw_data_content.csv"
        fs <- c(fs, path)
        write.csv(v$content, path, row.names = F)
      }

      if (v$problemtype == 1) {
        if (!is.null(v$plot_output)) {
          path <- "plot.pdf"
          fs <- c(fs, path)
          pdf(path)
          print(v$plot_output)
          dev.off()
        }
        if (v$content_exists) {
          if (!is.null(v$selected_item_contents)) {
            path <- "selected_item_contents.csv"
            fs <- c(fs, path)
            write.csv(v$selected_item_contents, path, row.names = F)
          }
        }
        if (!is.null(v$selected_item_attribs)) {
          if (!is.null(v$selected_index)) {
            path <- "selected_item_attribs.csv"
            fs <- c(fs, path)
            write.csv(v$selected_item_attribs, path, row.names = F)
          }
        }
      }

      if (v$problemtype == 2) {
        if (!is.null(v$fit)) {
          path <- sprintf("audit_plot_%i.pdf", v$simulee_id)
          fs <- c(fs, path)
          pdf(path)
          print(v$plot_output)
          dev.off()
          path <- sprintf("shadow_chart_%i.pdf", v$simulee_id)
          fs <- c(fs, path)
          pdf(path)
          print(v$shadow_chart)
          dev.off()
        }
      }
      zip(zipfile = fname, files = fs, flags = "-j")
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)
