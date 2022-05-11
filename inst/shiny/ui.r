ui <- fluidPage(
  theme = shinytheme("lumen"),
  shinyjs::useShinyjs(),
  includeCSS("styles.css"),

  titlePanel("TestDesign: Optimal Test Assembly"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      dropdownButton(
        h3(""),
        label = "Load files",
        fileInput("itempool_file",    buttonLabel = "Item parameters",                 label = NULL, accept = accepted_files),
        fileInput("itemse_file",      buttonLabel = "Item standard errors (optional)", label = NULL, accept = accepted_files),
        fileInput("itemattrib_file",  buttonLabel = "Item attributes",                 label = NULL, accept = accepted_files),
        fileInput("stimattrib_file",  buttonLabel = "Stimulus attributes (optional)",  label = NULL, accept = accepted_files),
        fileInput("constraints_file", buttonLabel = "Constraints",                     label = NULL, accept = accepted_files),
        fileInput("content_file",     buttonLabel = "Item contents (optional)",        label = NULL, accept = accepted_files),
        checkboxGroupButtons(
          inputId = "clear_files", justified = TRUE,
          choices = c("Clear files"), checkIcon = list(yes = icon("trash-alt"), no = icon("trash-alt"))
        ),
        circle = FALSE, icon = icon("file-import"), width = "100%"
      ),

      dropdownButton(
        inputId = "solvertype_dropdown", label = "Solver settings",
        radioGroupButtons(
          inputId = "solvertype", justified = TRUE, direction = "vertical",
          choices = solvers
        ),
        circle = FALSE, icon = icon("drafting-compass"), width = "100%"
      ),

      disabled(
        checkboxGroupButtons(
          inputId = "run_solver", justified = TRUE,
          choices = c("Run Solver"), checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")), status = "primary"
        )
      ),

      progressBar(id = "pb", value = 0, total = 1, display_pct = TRUE),

      radioGroupButtons(
        inputId = "problemtype", justified = TRUE,
        choiceNames = c("Static", "Adaptive"), choiceValues = 1:2, selected = 1
      ),
      radioGroupButtons(
        inputId = "objtype", justified = TRUE, label = h3("Objective type:"),
        choices = c("TCC", "TIF", "MAXINFO")
      ),

      textInput("thetas", label = h3("Theta values (comma-separated)"), value = "0, 1"),
      textInput("targets", label = h3("Target values (comma-separated)"), value = "15, 20"),

      h3(""),

      checkboxGroupButtons(
        inputId = "maxinfo_button", justified = TRUE,
        choices = c("Obtainable info range"), checkIcon = list(yes = icon("less-than-equal"), no = icon("less-than-equal"))
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
        circle = FALSE, icon = icon("database"), width = "100%"
      ),

      dropdownButton(
        inputId = "exposure_dropdown", label = "Exposure control settings",
        radioGroupButtons(
          inputId = "exposure_control_method", justified = TRUE,
          choices = c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")
        ),
        sliderInput("exposure_control_fading_factor", label = h3("Fading factor"), min = .9, max = 1.0, value = 1.0, step = .01),
        textInput("exposure_control_acceleration_factor", label = h3("Acceleration factor"), value = "2"),
        circle = FALSE, icon = icon("cog"), width = "100%"
      ),

      dropdownButton(
        inputId = "theta_settings", label = "Estimation Settings for interim & final thetas",
        h3("Interim theta"),
        radioGroupButtons(
          inputId = "interim_theta_method", justified = TRUE,
          choices = c("EAP", "FB", "EB")
        ),
        radioGroupButtons(
          inputId = "interim_theta_prior_dist", justified = TRUE,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("interim_theta_prior_par", label = "Prior distribution parameters", value = "0, 1"),
        h3("Final theta"),
        radioGroupButtons(
          inputId = "final_theta_method", justified = TRUE,
          choices = c("EAP", "FB", "EB")
        ),
        radioGroupButtons(
          inputId = "final_theta_prior_dist", justified = TRUE,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("final_theta_prior_par", label = "Prior distribution parameters", value = "0, 1"),
        circle = FALSE, icon = icon("cog"), width = "100%"
      ),

      radioGroupButtons(
        inputId = "item_selection_method", justified = TRUE, label = h3("Item selection method"),
        choices = c("MFI", "MPWI", "FB", "EB")
      ),

      dropdownButton(
        inputId = "refresh_policy_dropdown", label = "Refresh policy",
        radioGroupButtons(
          inputId = "refresh_policy_method", justified = TRUE, direction = "vertical",
          choices = c("ALWAYS", "POSITION", "INTERVAL", "THRESHOLD", "INTERVAL-THRESHOLD", "SET")
        ),
        textInput("refresh_policy_threshold", label = h3("Refresh when theta change exceeds"), value = "0.1"),
        textInput("refresh_policy_position", label = h3("Refresh at item positions (comma-separated)"), value = "1, 10"),
        textInput("refresh_policy_interval", label = h3("Refresh at item intervals (1 = always)"), value = "2"),
        circle = FALSE, icon = icon("cog"), width = "100%"
      ),

      downloadButton("export_data", "Export data"),

      dropdownButton(
        label = "Close app", inputId = "closeapp_dropdown",
        circle = FALSE, width = "100%", icon = icon("times-circle"),
        h3("Are you sure?"),
        checkboxGroupButtons(
          inputId = "closeapp",
          choices = c("Yes", "No"),
          justified = TRUE
        )
      )

    ),

    mainPanel(
      width = 9,
      verbatimTextOutput("text_output", placeholder = TRUE),
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
