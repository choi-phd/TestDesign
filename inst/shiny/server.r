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

  observeEvent(input$const_file, {
    if (!is.null(input$const_file) & v$itempool_exists & v$itemattrib_exists) {
      if (v$stimattrib_exists) {
        v$const <- try(loadConstraints(input$const_file$datapath, v$itempool, v$itemattrib, v$stimattrib))
      } else {
        v$const <- try(loadConstraints(input$const_file$datapath, v$itempool, v$itemattrib))
      }
      if (class(v$const) == "constraints") {

        v$const_exists <- TRUE
        v <- updateLogs(v, "Step 3. Constraints: OK.")

        v$constraints <- v$const@constraints
        assignObject(v$const,
          "shiny_const",
          "Constraints (full object)")
        assignObject(v$constraints,
          "shiny_constraints",
          "Constraints (raw data.frame)")

        if (isolate(v$const@set_based & !input$solvertype %in% c("lpsymphony", "Rsymphony", "gurobi"))) {
          v <- updateLogs(v, "Warning: set-based assembly requires 'lpsymphony', 'Rsymphony' or 'gurobi'.")
        }

      } else {
        v$const_exists <- FALSE
        v <- updateLogs(v, "Error: Constraints are not in the expected format. See ?dataset_science for details.")
      }

      v <- updateLogs(v, "Press the button to run solver.")
      shinyjs::toggleState("run_solver", v$const_exists)

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
    shinyjs::reset("const_file")
    shinyjs::reset("content_file")
    v$itempool   <- NULL
    v$itemattrib <- NULL
    v$stimattrib <- NULL
    v$const      <- NULL
    v$content    <- NULL
    v$itempool_exists   <- FALSE
    v$itemse_exists     <- FALSE
    v$itemattrib_exists <- FALSE
    v$stimattrib_exists <- FALSE
    v$const_exists      <- FALSE
    v$content_exists    <- FALSE
    v <- updateLogs(v, "Files cleared.")
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
    if (v$itempool_exists & v$const_exists) {

      plot(v$const)
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
      if (input$problemtype == 1 & v$const_exists) {
        v$problemtype <- 1

        cfg <- try(createStaticTestConfig(
          MIP = list(solver = input$solvertype),
          item_selection = list(
            method = input$objtype,
            target_location = eval(parse(text = sprintf("c(%s)", input$thetas))),
            target_value    = eval(parse(text = sprintf("c(%s)", input$targets)))
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

        v$fit <- Static(conf, v$const)
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

      if (input$problemtype == 2 & v$const_exists) {
        v$problemtype <- 2

        if (input$exposure_method == "BIGM-BAYESIAN") {
          if (!input$interim_method %in% c("FB", "EB")) {
            v <- updateLogs(v, "BIGM-BAYESIAN requires interim methods FB or EB.")
            break
          }
        }
        if (input$exposure_method == "BIGM-BAYESIAN") {
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
        if (input$simulee_theta_distribution == "UNIF") {
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

        conf <- new("config_Shadow")

        # parse exposure control settings

        conf@exposure_control$fading_factor <- input$exposure_fading_factor

        if (parseText(input$exposure_acc_factor)) {
          eval(parse(text = sprintf("conf@exposure_control$acceleration_factor <- c(%s)[1]", input$exposure_acc_factor)))
          if (conf@exposure_control$acceleration_factor < 1) {
            v <- updateLogs(v, "Acceleration factor should be a number larger than or equal to 1.0.")
            break
          }
        } else {
          v <- updateLogs(v, "Acceleration factor should be a number larger than or equal to 1.0.")
          break
        }

        conf@exposure_control$method <- input$exposure_method
        conf@exposure_control$diagnostic_stats <- TRUE
        conf@exposure_control$max_exposure_rate <-
          rep(0.25, conf@exposure_control$n_segment)

        # parse theta estimation settings

        conf@interim_theta$method <- input$interim_method
        conf@interim_theta$prior_dist <- input$interim_prior
        conf@final_theta$method <- input$final_method
        conf@final_theta$prior_dist <- input$final_prior
        conf@item_selection$method <- input$item_selection_method

        if (conf@interim_theta$method == "FB" & v$itemse_exists == F) {
          v <- updateLogs(v, "FB interim method requires the standard errors to be supplied.")
          break
        }
        if (conf@final_theta$method == "FB" & v$itemse_exists == F) {
          v <- updateLogs(v, "FB final method requires the standard errors to be supplied.")
          break
        }

        if (parseText(input$interim_prior_par)) {
          eval(parse(text = sprintf("conf@interim_theta$prior_par = c(%s)", input$interim_prior_par)))
          if (length(conf@interim_theta$prior_par) != 2) {
            v <- updateLogs(v, "Interim prior parameters should be two values.")
            break
          }
        } else {
          v <- updateLogs(v, "Interim prior parameters should be two values.")
          break
        }
        if (parseText(input$final_prior_par)) {
          eval(parse(text = sprintf("conf@final_theta$prior_par = c(%s)", input$final_prior_par)))
          if (length(conf@final_theta$prior_par) != 2) {
            v <- updateLogs(v, "Final prior parameters should be two values.")
            break
          }
        } else {
          v <- updateLogs(v, "Final prior parameters should be two values.")
          break
        }


        if (conf@item_selection$method == "FB") {
          if (conf@interim_theta$method != "FB") {
            v <- updateLogs(v, "FB item selection method requires FB interim method.")
            break
          }
        }
        if (conf@item_selection$method == "EB") {
          if (conf@interim_theta$method != "EB") {
            v <- updateLogs(v, "EB item selection method requires EB interim method.")
            break
          }
        }

        # parse refresh policy settings

        conf@refresh_policy$method <- input$refresh_policy

        if (conf@refresh_policy$method == "SET" && v$const@set_based == FALSE) {
          v <- updateLogs(v, "Set-based refresh policy is only applicable for set-based item pools.")
          break
        }

        if (parseText(input$refresh_interval)) {
          eval(parse(text = sprintf("conf@refresh_policy$interval <- c(%s)[1]", input$refresh_interval)))
          if (conf@refresh_policy$interval < 1 |
              all(conf@refresh_policy$interval != as.integer(conf@refresh_policy$interval))) {
            v <- updateLogs(v, "Refresh interval should be an integer larger than or equal to 1.")
            break
          }
        }
        if (parseText(input$refresh_position)) {
          eval(parse(text = sprintf("conf@refresh_policy$position <- c(%s)", input$refresh_position)))
          if (any(conf@refresh_policy$position < 1) |
            all(conf@refresh_policy$position != as.integer(conf@refresh_policy$position))) {
            v <- updateLogs(v, "Refresh positions should be comma-separated integers larger than or equal to 1.")
            break
          }
        }
        if (parseText(input$refresh_threshold)) {
          eval(parse(text = sprintf("conf@refresh_policy$threshold <- c(%s)[1]", input$refresh_threshold)))
          if (conf@refresh_policy$threshold < 0) {
            v <- updateLogs(v, "Refresh threshold should be a positive value.")
            break
          }
        }

        eval(parse(text = sprintf("conf@item_selection$target_location <- c(%s)", input$thetas)))

        conf@MIP$solver <- input$solvertype

        assignObject(conf,
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
        v$fit <- Shadow(conf, v$const, true_theta, resp_data, prior = NULL, prior_par = NULL, session = session)
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
    parseObject(v$constraints),
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
        write.csv(v$itempool@raw, path, row.names = F)
      }
      if (!is.null(v$itemattrib)) {
        path <- getTempFilePath("raw_data_item_attribs.csv")
        fs <- c(fs, path)
        write.csv(v$itemattrib@data, path, row.names = F)
      }
      if (!is.null(v$stimattrib)) {
        path <- getTempFilePath("raw_data_stim_attribs.csv")
        fs <- c(fs, path)
        write.csv(v$stimattrib@data, path, row.names = F)
      }
      if (!is.null(v$constraints)) {
        path <- getTempFilePath("raw_data_constraints.csv")
        fs <- c(fs, path)
        write.csv(v$constraints, path, row.names = F)
      }
      if (!is.null(v$content)) {
        path <- getTempFilePath("raw_data_content.csv")
        fs <- c(fs, path)
        write.csv(v$content, path, row.names = F)
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
            write.csv(v$selected_item_contents, path, row.names = F)
          }
        }
        if (!is.null(v$selected_item_attribs)) {
          if (!is.null(v$selected_index)) {
            path <- getTempFilePath("selected_item_attribs.csv")
            fs <- c(fs, path)
            write.csv(v$selected_item_attribs, path, row.names = F)
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
