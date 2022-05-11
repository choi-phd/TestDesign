server <- function(input, output, session) {
  v <- reactiveValues(
    itempool_exists = FALSE,
    itemse_exists = FALSE,
    const_exists = FALSE,
    content_exists = FALSE,
    stimattrib_exists = FALSE,
    problemtype = 0,
    solvers = solvers
  )

  # test each solver and filter
  for (s in solvers) {
    if (testSolver(s) != "") {
      v$solvers <- setdiff(solvers, s)
    }
  }
  observe({
    updateRadioGroupButtons(
      session, "solvertype",
      choices = v$solvers
    )
  })

  observeEvent(input$itempool_file, {
    if (!is.null(input$itempool_file)) {
      v$itempool <- try(loadItemPool(input$itempool_file$datapath))
      if (class(v$itempool) == "item_pool") {
        v$itempool_exists <- TRUE
        v <- updateLogs(v, "Step 1. Item parameter file: OK")
        v$ipar <- v$itempool@raw
        assignObject(v$itempool,
          "shiny_itempool",
          "Item parameters (full object)")
        assignObject(v$ipar,
          "shiny_itempool_ipar",
          "Item parameters (matrix)")
      } else {
        v$itempool_exists <- FALSE
        v <- updateLogs(v, "Error: Item parameters are not in the correct format. See ?dataset_science for details.")
      }
    }
  })

  observeEvent(input$itemse_file, {
    if (!is.null(input$itemse_file) & v$itempool_exists) {
      v$itempool <- try(loadItemPool(input$itempool_file$datapath, se_file = input$itemse_file$datapath))
      if (class(v$itempool) == "item_pool") {
        v$itemse_exists <- TRUE
        v <- updateLogs(v, "Item standard error file: OK")
        assignObject(v$itempool,
          "shiny_itempool",
          "Item parameters (full object)")
      } else {
        v$itemse_exists <- FALSE
        v <- updateLogs(v, "Error: Item standard errors are not in the correct format.")
      }
    }
  })

  observeEvent(input$itemattrib_file, {
    if (!is.null(input$itemattrib_file) & v$itempool_exists) {
      v$itemattrib <- try(loadItemAttrib(input$itemattrib_file$datapath, v$itempool))
      if (class(v$itemattrib) == "item_attrib") {
        v$itemattrib_exists <- TRUE
        v <- updateLogs(v, "Step 2. Item attributes: OK")
        assignObject(v$itemattrib,
          "shiny_itemattrib",
          "Item attributes")
      } else {
        v$itemattrib_exists <- FALSE
        v <- updateLogs(v, "Step 2 Error: Item attributes are not in the correct format. See ?dataset_science for details.")
      }
    }
  })

  observeEvent(input$stimattrib_file, {
    if (!is.null(input$stimattrib_file) & v$itempool_exists & v$itemattrib_exists) {
      v$stimattrib <- try(loadStAttrib(input$stimattrib_file$datapath, v$itemattrib))
      if (class(v$stimattrib) == "st_attrib") {
        v$stimattrib_exists <- TRUE
        v <- updateLogs(v, "Stimulus attributes: OK")
        assignObject(v$stimattrib,
          "shiny_stimattrib",
          "Stimulus attributes")
      } else {
        v$stimattrib_exists <- FALSE
        v <- updateLogs(v, "Error: Stimulus attributes are not in the correct format. See ?dataset_reading for details.")
      }
    }
  })

  observeEvent(input$constraints_file, {
    if (!is.null(input$constraints_file) & v$itempool_exists & v$itemattrib_exists) {
      if (v$stimattrib_exists) {
        v$constraints <- try(loadConstraints(input$constraints_file$datapath, v$itempool, v$itemattrib, v$stimattrib))
      } else {
        v$constraints <- try(loadConstraints(input$constraints_file$datapath, v$itempool, v$itemattrib))
      }
      if (class(v$constraints) == "constraints") {

        v$constraints_exists <- TRUE
        v <- updateLogs(v, "Step 3. Constraints: OK.")

        v$constraints_data <- v$constraints@constraints
        assignObject(v$constraints,
          "shiny_constraints",
          "Constraints (full object)")
        assignObject(v$constraints_data,
          "shiny_constraints_data",
          "Constraints (raw data.frame)")

        if (isolate(v$constraints@set_based & !input$solvertype %in% c("lpsymphony", "Rsymphony", "gurobi"))) {
          v <- updateLogs(v, "Warning: set-based assembly requires 'lpsymphony', 'Rsymphony' or 'gurobi'.")
        }

      } else {
        v$constraints_exists <- FALSE
        v <- updateLogs(v, "Error: Constraints are not in the expected format. See ?dataset_science for details.")
      }

      v <- updateLogs(v, "Press the button to run solver.")
      shinyjs::toggleState("run_solver", v$constraints_exists)

    }
  })

  observeEvent(input$content_file, {
    if (!is.null(input$content_file)) {
      v$content <- try(read.csv(input$content_file$datapath))
      if (class(v$content) == "data.frame") {
        v$content_exists <- TRUE
        v <- updateLogs(v, "Item contents: OK.")
        assignObject(v$content,
          "shiny_content",
          "Item contents")
      } else {
        v$content_exists <- FALSE
        v <- updateLogs(v, "Error: Item contents are not in the expected format. See ?dataset_fatigue for details.")
      }
    }
  })

  observeEvent(input$clear_files, {
    shinyjs::reset("itempool_file")
    shinyjs::reset("itemse_file")
    shinyjs::reset("itemattrib_file")
    shinyjs::reset("stimattrib_file")
    shinyjs::reset("constraints_file")
    shinyjs::reset("content_file")
    v$itempool    <- NULL
    v$itemattrib  <- NULL
    v$stimattrib  <- NULL
    v$constraints <- NULL
    v$content     <- NULL
    v$itempool_exists    <- FALSE
    v$itemse_exists      <- FALSE
    v$itemattrib_exists  <- FALSE
    v$stimattrib_exists  <- FALSE
    v$constraints_exists <- FALSE
    v$content_exists     <- FALSE
    v <- updateLogs(v, "Files cleared.")
    updateCheckboxGroupButtons(
      session = session,
      inputId = "clear_files",
      selected = character(0)
    )
  })

  observeEvent(input$problemtype, {
    shinyjs::toggle("objtype",                        condition = input$problemtype == 1)
    shinyjs::toggle("item_selection_target_location", condition = input$problemtype == 1)
    shinyjs::toggle("item_selection_target_value",    condition = input$problemtype == 1)
    shinyjs::toggle("maxinfo_button",                 condition = input$problemtype == 1)
    shinyjs::toggle("exposure_dropdown",              condition = input$problemtype == 2)
    shinyjs::toggle("theta_settings",                 condition = input$problemtype == 2)
    shinyjs::toggle("item_selection_method",          condition = input$problemtype == 2)
    shinyjs::toggle("refresh_policy_dropdown",        condition = input$problemtype == 2)
    shinyjs::toggle("simulation_dropdown",            condition = input$problemtype == 2)
    if (input$problemtype == 2) {
      showTab("tabs", target = "2")
      hideTab("tabs", target = "3")
    } else {
      hideTab("tabs", target = "2")
      showTab("tabs", target = "3")
    }
  })

  observeEvent(input$objtype, {
    shinyjs::toggleState("item_selection_target_value", input$objtype != "MAXINFO")
  })

  observeEvent(input$refresh_policy_method, {
    shinyjs::toggle("refresh_policy_threshold", condition = input$refresh_policy_method %in% c("THRESHOLD", "INTERVAL-THRESHOLD"))
    shinyjs::toggle("refresh_policy_interval",  condition = input$refresh_policy_method %in% c("INTERVAL", "INTERVAL-THRESHOLD"))
    shinyjs::toggle("refresh_policy_position",  condition = input$refresh_policy_method %in% c("POSITION"))
  })

  observeEvent(input$simulee_id, {
    if (v$problemtype == 2) {
      if (!is.null(v$fit)) {
        if (parseText(input$simulee_id)) {
          eval(parse(text = sprintf("simulee_id <- c(%s)[1]", input$simulee_id)))
          if (is.null(simulee_id)) {
            simulee_id <- 1
          }
          v$simulee_id <- min(simulee_id, v$n_simulees)
          updateTextInput(session, "simulee_id", value = as.character(v$simulee_id))
        } else {
          v <- updateLogs(v, "The index of simulee to plot should be an integer.")
          break
        }

        if (input$simulee_id != "") {
          v <- updateLogs(v, sprintf("Created plots for simulee %i", v$simulee_id))

          plot(v$fit, v$simulee_id, type = "audit")
          p <- recordPlot()
          dev.off()

          v$plot_output <- p
          assignObject(v$plot_output,
            sprintf("shiny_audit_plot_%i", v$simulee_id),
            sprintf("Audit trail plot for simulee %i", v$simulee_id)
          )

          plot(v$fit, v$simulee_id, type = "shadow", simple = TRUE)
          p <- recordPlot()
          dev.off()

          v$shadow_chart <- p
          assignObject(v$shadow_chart,
            sprintf("shiny_shadow_chart_%i", v$simulee_id),
            sprintf("Shadow test chart for simulee %i", v$simulee_id)
          )
        }
      }
    }
  })

  observeEvent(input$maxinfo_button, {
    if (v$itempool_exists & v$constraints_exists) {

      plot(v$constraints)
      p <- recordPlot()
      dev.off()

      v$info_range_plot <- p
      v$plot_output <- v$info_range_plot
      assignObject(v$info_range_plot,
        "shiny_info_range_plot",
        "Obtainable info range plot"
      )
      v <- updateLogs(v, "Info range plot is printed on the 'Main' tab.")

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
      if (input$problemtype == 1 & v$constraints_exists) {
        v$problemtype <- 1

        cfg <- try(createStaticTestConfig(
          MIP = list(solver = input$solvertype),
          item_selection = list(
            method = input$objtype,
            target_location = eval(parse(text = sprintf("c(%s)", input$item_selection_target_location))),
            target_value    = eval(parse(text = sprintf("c(%s)", input$item_selection_target_value)))
          )
        ))

        if (inherits(cfg, "try-error")) {
          v <- updateLogs(v, "Error: the config is not valid. See the console for details.")
          break
        }

        assignObject(cfg,
          "shiny_config_Static",
          "config_Static object"
        )

        progress <- Progress$new(session)
        on.exit(progress$close())
        progress$set(
          message = "Computing..",
          detail = "This may take a while."
        )

        v$fit <- Static(cfg, v$constraints)
        assignObject(v$fit,
          "shiny_Static",
          "Static() solution object"
        )

        if (is.null(v$fit@MIP)) {
          v <- updateLogs(v, "Solver did not find a solution. Try relaxing the target values.")
        } else {
          plot(v$fit)
          p <- recordPlot()
          dev.off()

          v$plot_output <- p

          v <- updateLogs(v, sprintf("%-10s: solved in %3.3fs", cfg@MIP$solver, v$fit@solve_time))
          v$selected_index <- which(v$fit@MIP[[1]]$solution == 1)
          v$selected_index <- v$selected_index[v$selected_index <= v$itempool@ni]

          v$selected_item_names <- v$itemattrib@data[v$selected_index, ][["ID"]]
          v$selected_item_attribs <- v$itemattrib@data[v$selected_index, ]
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

      if (input$problemtype == 2 & v$constraints_exists) {
        v$problemtype <- 2

        if (input$exposure_control_method == "BIGM-BAYESIAN") {
          if (!input$interim_theta_method %in% c("FB", "EB")) {
            v <- updateLogs(v, "BIGM-BAYESIAN requires interim methods FB or EB.")
            break
          }
        }
        if (input$exposure_control_method == "BIGM-BAYESIAN") {
          if (!input$item_selection_method %in% c("FB", "EB")) {
            v <- updateLogs(v, "BIGM-BAYESIAN requires item selection method FB or EB.")
            break
          }
        }

        if (parseText(input$n_simulees)) {
          eval(parse(text = sprintf("v$n_simulees <- c(%s)[1]", input$n_simulees)))
          v$n_simulees <- min(100, v$n_simulees)
          updateProgressBar(session = session, id = "pb", value = 0, total = v$n_simulees)
        } else {
          v <- updateLogs(v, "Number of simulees should be an integer.")
          break
        }

        if (parseText(input$simulee_id)) {
          v$simulee_id <- eval(parse(text = sprintf("c(%s)[1]", input$simulee_id)))
        } else {
          v <- updateLogs(v, "Number of simulees should be an integer.")
          break
        }

        if (parseText(input$simulee_theta_params)) {
          v$simulee_theta_params <- eval(parse(text = sprintf("c(%s)[1:2]", input$simulee_theta_params)))
        } else {
          v <- updateLogs(v, "Theta distribution parameters should be two numbers.")
          break
        }

        if (input$simulee_theta_distribution == "NORMAL") {
          true_theta <- rnorm(v$n_simulees, v$simulee_theta_params[1], v$simulee_theta_params[2])
        }
        if (input$simulee_theta_distribution == "UNIFORM") {
          true_theta <- runif(v$n_simulees, min = v$simulee_theta_params[1], max = v$simulee_theta_params[2])
        }
        assignObject(true_theta,
          "shiny_truetheta",
          "Simulation: true theta values"
        )

        resp_data <- simResp(v$itempool, true_theta)
        assignObject(resp_data,
          "shiny_respdata",
          "Simulation: response data"
        )

        # parse config
        cfg <- try(createShadowTestConfig(
          MIP = list(
            solver = input$solvertype
          ),
          item_selection = list(
            method = input$item_selection_method
          ),
          exposure_control = list(
            method = input$exposure_control_method,
            fading_factor = input$exposure_control_fading_factor,
            acceleration_factor = as.numeric(input$exposure_control_acceleration_factor),
            diagnostic_stats = FALSE
          ),
          interim_theta = list(
            method = input$interim_theta_method,
            prior_dist = input$interim_theta_prior_dist
          ),
          final_theta = list(
            method = input$final_theta_method,
            prior_dist = input$final_theta_prior_dist
          ),
          refresh_policy = list(
            method = input$refresh_policy_method,
            interval = input$refresh_policy_interval,
            position = input$refresh_policy_position,
            threshold = input$refresh_policy_threshold
          )
        ))

        if (inherits(cfg, "try-error")) {
          v <- updateLogs(v, "Error: the config is not valid. See the console for details.")
          break
        }

        if (cfg@interim_theta$method == "FB" & v$itemse_exists == FALSE) {
          v <- updateLogs(v, "FB interim method requires the standard errors to be supplied.")
          break
        }
        if (cfg@final_theta$method == "FB" & v$itemse_exists == FALSE) {
          v <- updateLogs(v, "FB final method requires the standard errors to be supplied.")
          break
        }

        if (cfg@refresh_policy$method == "SET" && v$constraints@set_based == FALSE) {
          v <- updateLogs(v, "Set-based refresh policy is only applicable for set-based item pools.")
          break
        }

        assignObject(cfg,
          "shiny_config_Shadow",
          "config_Shadow object"
        )

        progress <- Progress$new(session)
        on.exit(progress$close())
        progress$set(
          message = "Computing..",
          detail = "This may take a while."
        )

        v$time <- Sys.time()
        v$fit <- try(
          Shadow(
            config = cfg,
            constraints = v$constraints,
            true_theta = true_theta,
            data = resp_data,
            prior = NULL,
            prior_par = NULL,
            session = session
          )
        )

        message("\n")
        assignObject(v$fit,
          "shiny_Shadow",
          "Simulation result"
        )

        if (inherits(v$fit, "try-error")) {
          v <- updateLogs(v, sprintf("Error: see the console for details."))
          break
        }

        v$time <- difftime(Sys.time(), v$time, units = "secs")

        v <- updateLogs(v, sprintf("%-10s: simulation complete in %3.3fs", cfg@MIP$solver, v$time))

        updateTextInput(session, "simulee_id", value = "")

      }
    }

    updateCheckboxGroupButtons(
      session = session,
      inputId = "run_solver",
      selected = character(0)
    )
    shinyjs::enable("run_solver")
  })

  output$table_itempool    <- renderDT(
    parseObject(v$ipar),
    options = list(pageLength = 100),
    rownames = FALSE
  )
  output$table_itemattrib  <- renderDT(
    parseObject(if(!is.null(v$itemattrib)) v$itemattrib@data else NULL),
    options = list(pageLength = 100),
    rownames = FALSE
  )
  output$table_stimattrib  <- renderDT(
    parseObject(if(!is.null(v$stimattrib)) v$stimattrib@data else NULL),
    options = list(pageLength = 100),
    rownames = FALSE
  )
  output$table_constraints <- renderDT(
    parseObject(v$constraints_data),
    options = list(pageLength = 100),
    rownames = FALSE
  )
  output$results      <- renderDT(
    parseObject(v$results),
    options = list(pageLength = 100),
    rownames = FALSE
  )
  output$text_output  <- renderText(parseObject(v$logs_text))
  output$plot_output  <- renderPlot(parseObject(v$plot_output))
  output$shadow_chart <- renderPlot(parseObject(v$shadow_chart))

  output$export_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep = "")
    },
    content = function(fname) {

      fs <- c()

      if (!is.null(v$itempool)) {
        path <- getTempFilePath("raw_data_item_params.csv")
        fs <- c(fs, path)
        write.csv(v$itempool@raw, path, row.names = FALSE)
      }
      if (!is.null(v$itemattrib)) {
        path <- getTempFilePath("raw_data_item_attribs.csv")
        fs <- c(fs, path)
        write.csv(v$itemattrib@data, path, row.names = FALSE)
      }
      if (!is.null(v$stimattrib)) {
        path <- getTempFilePath("raw_data_stim_attribs.csv")
        fs <- c(fs, path)
        write.csv(v$stimattrib@data, path, row.names = FALSE)
      }
      if (!is.null(v$constraints_data)) {
        path <- getTempFilePath("raw_data_constraints.csv")
        fs <- c(fs, path)
        write.csv(v$constraints_data, path, row.names = FALSE)
      }
      if (!is.null(v$content)) {
        path <- getTempFilePath("raw_data_content.csv")
        fs <- c(fs, path)
        write.csv(v$content, path, row.names = FALSE)
      }

      if (v$problemtype == 1) {
        if (!is.null(v$plot_output)) {
          path <- getTempFilePath("plot.pdf")
          fs <- c(fs, path)
          pdf(path)
          print(v$plot_output)
          dev.off()
        }
        if (v$content_exists) {
          if (!is.null(v$selected_item_contents)) {
            path <- getTempFilePath("selected_item_contents.csv")
            fs <- c(fs, path)
            write.csv(v$selected_item_contents, path, row.names = FALSE)
          }
        }
        if (!is.null(v$selected_item_attribs)) {
          if (!is.null(v$selected_index)) {
            path <- getTempFilePath("selected_item_attribs.csv")
            fs <- c(fs, path)
            write.csv(v$selected_item_attribs, path, row.names = FALSE)
          }
        }
      }

      if (v$problemtype == 2) {
        if (!is.null(v$fit)) {
          path <- getTempFilePath(sprintf("audit_plot_%i.pdf", v$simulee_id))
          fs <- c(fs, path)
          pdf(path)
          print(v$plot_output)
          dev.off()
          path <- getTempFilePath(sprintf("shadow_chart_%i.pdf", v$simulee_id))
          fs <- c(fs, path)
          pdf(path)
          print(v$shadow_chart)
          dev.off()
        }
      }

      if (length(fs) > 0) {
        zip(zipfile = fname, files = fs, flags = "-j")
        file.remove(fs)
      }
    },
    contentType = "application/zip"
  )

  observeEvent(input$closeapp, {
    if ("Yes" %in% input$closeapp) {
      stopApp()
    }
    if ("No" %in% input$closeapp) {
      updateCheckboxGroupButtons(
        session = session,
        inputId = "closeapp",
        selected = character(0)
      )
      toggleDropdownButton(inputId = "closeapp_dropdown")
    }
  })

  session$onSessionEnded(function() {
    stopApp()
  })

}
