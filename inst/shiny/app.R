library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(IRTclass)
library(Shadow)
library(DT)
library(readxl)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML("
h3 {
  font-size: 125%;
}
i {
  display: inline-block;
  margin-right: 0.2em;
}
label, .form-group, .progress {
  margin-bottom: 0px;
}
.btn {
  width: 100%;
}

"))),
  titlePanel("Shadow"),

  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", "select { min-width: 100%; max-width: 100%; }"),
        tags$style(type="text/css", ".span4 { min-width: 100%; max-width: 100%; }"),
        tags$style(type="text/css", ".well { min-width: 100%; max-width: 100%; }")
      ),
      helpText("This is a demo of Adaptive Test Assembly (ATA)."),

      dropdownButton(

        h3(""),

        fileInput("itempool_file", buttonLabel = "Item parameters", label = NULL),
        fileInput("itemattrib_file", buttonLabel = "Item attributes", label = NULL),
        fileInput("stimattrib_file", buttonLabel = "Stimulus attributes (optional)", label = NULL),
        fileInput("const_file", buttonLabel = "Solver constraints", label = NULL),
        fileInput("content_file", buttonLabel = "Item contents (optional)", label = NULL),

        circle = FALSE, status = "primary",
        icon = icon("file-import"), width = "100%",

        label = "Load files"
      ),

      h3(""),

      radioGroupButtons(
        inputId = "problemtype",
        choiceNames = c("Fixed-length", "Shadow"),
        choiceValues = 1:2,
        selected = 1,
        justified = TRUE
      ),
      radioGroupButtons(
        inputId = "solvertype",
        choices = c("Symphony", "Gurobi", "GLPK", "lpSolve"),
        checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")),
        justified = TRUE
      ),
      radioGroupButtons(
        inputId = "objtype",
        label = h3("Objective type:"),
        choices = c("TCC", "TIF", "MAXINFO"),
        justified = TRUE
      ),
      textInput("thetas", label = h3("Theta values (comma-separated)"), value = "0,1"),
      textInput("targets", label = h3("Target values (comma-separated)"), value = "15,20"),

      checkboxGroupButtons(
        inputId = "maxinfo_button",
        choices = c("Check allowed info range"),
        checkIcon = list(yes = icon("less-than-equal"), no = icon("less-than-equal")),
        justified = TRUE
      ),

      dropdownButton(
        inputId = "simulation_dropdown",
        radioGroupButtons(
          inputId = "simulee_theta_distribution",
          choices = c("NORMAL", "UNIF"),
          justified = TRUE
        ),
        textInput("simulee_theta_params", label = "True theta distribution parameters", value = "0, 1"),

        textInput("n_simulees", label = h3("# of simulees"), value = "1"),
        textInput("simulee_id", label = h3("Display plots for simulee:"), value = "1"),

        circle = FALSE,
        icon = icon("database"), width = "100%",
        status = "danger",
        label = "Simulation settings"
      ),

      dropdownButton(
        inputId = "exposure_dropdown",
        radioGroupButtons(
          inputId = "exposure_method",
          choices = c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN"),
          justified = TRUE
        ),
        sliderInput("exposure_ff", label = h3("Fading factor"), min = .9, max = 1.0, value = 1.0, step = .01),
        textInput("exposure_af", label = h3("Acceleration factor"), value = "2"),
        circle = FALSE,
        icon = icon("cog"), width = "100%",
        label = "Exposure control settings"
      ),

      dropdownButton(

        inputId = "theta_settings",

        h3("Interim theta"),

        radioGroupButtons(
          inputId = "interim_method",
          choices = c("EAP", "FB", "EB"),
          justified = TRUE
        ),
        radioGroupButtons(
          inputId = "interim_prior",
          choices = c("NORMAL", "UNIF"),
          justified = TRUE
        ),
        textInput("interim_priorpar", label = "Prior distribution parameters", value = "0, 1"),

        h3("Final theta"),

        radioGroupButtons(
          inputId = "final_method",
          choices = c("EAP", "FB", "EB"),
          justified = TRUE
        ),
        radioGroupButtons(
          inputId = "final_prior",
          choices = c("NORMAL", "UNIF"),
          justified = TRUE
        ),
        textInput("final_priorpar", label = "Prior distribution parameters", value = "0, 1"),

        circle = FALSE,
        icon = icon("cog"), width = "100%",
        label = "Estimation Settings for interim & final thetas"
      ),

      radioGroupButtons(
        inputId = "itemselection_method",
        label = h3("Item selection method"),
        choices = c("MFI", "MPWI", "FB", "EB"),
        justified = TRUE
      ),
      radioGroupButtons(
        inputId = "contentbalancing_method",
        label = h3("Content balancing method"),
        choices = c("NONE", "STA"),
        justified = TRUE
      ),

      dropdownButton(
        inputId = "refreshpolicy_dropdown",
        radioGroupButtons(
          inputId = "refreshpolicy",
          choices = c("ALWAYS", "POSITION", "INTERVAL", "THRESHOLD", "INTERVAL-THRESHOLD", "STIMULUS", "SET", "PASSAGE"),
          justified = TRUE,
          direction = "vertical"   # SET is the generic option, condense last 3 options
        ),
        circle = FALSE,
        icon = icon("cog"), width = "100%",
        label = "Refresh policy"
      )
    ),


    mainPanel(
      tags$head(
        tags$style(type='text/css', '#textoutput {background-color: rgba(64,64,64,1); color: cyan;}')
      ),
      disabled(
        checkboxGroupButtons(
          inputId = "runsolver",
          choices = c("Run Solver"),
          checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")),
          status = "primary",
          justified = TRUE
        )
      ),
      verbatimTextOutput("textoutput", placeholder = T),
      progressBar(id = "pb", value = 0, total = 1, display_pct = TRUE),
      hr(),
      tabsetPanel(id = "tabs",
        tabPanel("Main",
                 plotOutput("plotoutput", width = "100%", height = "700px"),
                 value = 1),
        tabPanel("Result items",
                 plotOutput("shadowplot", width = "100%", height = "700px"),
                 value = 2),
        tabPanel("Result items",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("results"),
                 value = 3),
        tabPanel("Item parameters",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("table_itempool"),
                 value = 4),
        tabPanel("Item attributes",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("table_itemattrib"),
                 value = 5),
        tabPanel("Stimulus attributes",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("table_stimattrib"),
                 value = 6),
        tabPanel("Constraints",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("table_constraints"),
                 value = 7)

      )
    )
  )
)

is_text_parsable = function(arg.text){
  txt = gsub("[^0-9\\., \\-]", "", arg.text) # Limits eval to only accept legit inputs
  return(txt == arg.text)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v = reactiveValues(itempool.exists = F, const.exists = F, content.exists = F, stimattrib.exists = F)

  observeEvent(input$itempool_file, {
    if (!is.null(input$itempool_file)){
      v$itempool = try(LoadItemPool(input$itempool_file$datapath))
      if (class(v$itempool) == "item.pool"){
        v$itempool.exists = TRUE
        v$text = "Step 1. Item parameter file: OK"
      } else {
        v$itempool.exists = FALSE
        v$text = "Step 1 Error: Item parameters are not in the correct format."
      }
    }
  })

  observeEvent(input$itemattrib_file, {
    if (!is.null(input$itemattrib_file) & v$itempool.exists){
      v$itemattrib = try(LoadItemAttrib(input$itemattrib_file$datapath, v$itempool))
      if (class(v$itemattrib) == "data.frame"){
        v$itemattrib.exists = TRUE
        v$text = "Step 2. Item attributes: OK"
      } else {
        v$itemattrib.exists = FALSE
        v$text = "Step 2 Error: Item attributes are not in the correct format."
      }
    }
  })

  observeEvent(input$stimattrib_file, {
    if (!is.null(input$stimattrib_file) & v$itemattrib.exists){
      v$stimattrib = try(LoadStAttrib(input$stimattrib_file$datapath, v$itemattrib))
      if (class(v$stimattrib) == "data.frame"){
        v$stimattrib.exists = TRUE
        v$text = "Optional Step. Stimulus attributes: OK"
      } else {
        v$stimattrib.exists = FALSE
        v$text = "Optional Step Error: Stimulus attributes are not in the correct format."
      }
    }
  })

  observeEvent(input$const_file, {
    if (!is.null(input$itempool_file) & v$itemattrib.exists){
      if ( v$stimattrib.exists) v$const = try(LoadConstraints(input$const_file$datapath, v$itempool, v$itemattrib, v$stimattrib))
      if (!v$stimattrib.exists) v$const = try(LoadConstraints(input$const_file$datapath, v$itempool, v$itemattrib))
      if (class(v$const) == "list"){
        v$const.exists = TRUE
        v$text = "Step 3. Constraints: OK. Press button to run solver."
        enable("runsolver")
      } else {
        v$const.exists = FALSE
        v$text = "Step 3 Error: Constraints are not in the correct format."
        shinyjs::disable("runsolver")
      }
    }
  })

  observeEvent(input$content_file, {
    v$content = try(read_excel(input$content_file$datapath))
    if (class(v$content)[1] == "tbl_df"){
      v$content.exists = TRUE
      v$text = "Optional Step. Item contents: OK."
    } else {
      v$content.exists = FALSE
      v$text = "Optional Step Error: Item contents are not in the correct format."
    }
  })

  observeEvent(input$problemtype, {
    if (input$problemtype == 2){
      shinyjs::hide("objtype")
      shinyjs::hide("thetas")
      shinyjs::hide("targets")
      shinyjs::hide("maxinfo_button")
      shinyjs::show("exposure_dropdown")
      shinyjs::show("theta_settings")
      shinyjs::show("itemselection_method")
      shinyjs::show("contentbalancing_method")
      shinyjs::show("refreshpolicy_dropdown")
      shinyjs::show("simulation_dropdown")
      showTab("tabs", target = "2")
      hideTab("tabs", target = "3")
    } else {
      shinyjs::show("objtype")
      shinyjs::show("thetas")
      shinyjs::show("targets")
      shinyjs::show("maxinfo_button")
      shinyjs::hide("exposure_dropdown")
      shinyjs::hide("theta_settings")
      shinyjs::hide("itemselection_method")
      shinyjs::hide("contentbalancing_method")
      shinyjs::hide("refreshpolicy_dropdown")
      shinyjs::hide("simulation_dropdown")
      hideTab("tabs", target = "2")
      showTab("tabs", target = "3")
    }
  })

  observeEvent(input$objtype, {
    if (input$objtype == 3){
      shinyjs::disable("targets")
    } else {
      shinyjs::enable("targets")
    }
  })

  observeEvent(input$simulee_id, {

    if (!is.null(v$fit)){
      if (is_text_parsable(input$simulee_id)){
        eval(parse(text = paste0("v$simulee_id = c(", input$simulee_id, ")[1]")))
      } else {
        v$text = "Number of simulees should be an integer."
        break
      }

      v$plotoutput = plotCAT(v$fit$output[[v$simulee_id]])
      v$shadowplot = plotShadow(v$fit$output[[v$simulee_id]], v$const)
    }
  })

  observeEvent(input$maxinfo_button, {
    if (v$itempool.exists & v$const.exists){
      v$plotoutput = maxinfo_plot(v$itempool, v$const)
    }
    updateCheckboxGroupButtons(
      session = session,
      inputId = "maxinfo_button",
      selected = character(0)
    )
  })

  observeEvent(input$runsolver, {
    if (input$problemtype == 1 & v$const.exists){

      shinyjs::disable("runsolver")

      conf = new("ATA.config")

      conf@itemSelection$method = input$objtype
      conf@MIP$solver = input$solvertype

      if (is_text_parsable(input$thetas)){
        eval(parse(text = paste0("conf@itemSelection$targetLocation = c(", input$thetas, ")")))
      } else {
        v$text = "Theta values should be comma-separated numbers."
        shinyjs::enable("runsolver")
        break
      }

      if (is_text_parsable(input$targets)){
        eval(parse(text = paste0("conf@itemSelection$targetValue = c(", input$targets, ")")))
      } else {
        v$text = "Target values should be comma-separated numbers."
        shinyjs::enable("runsolver")
        break
      }

      conf@itemSelection$targetWeight = rep(1, length(conf@itemSelection$targetLocation))

      v$text = "Solving.."

      v$fit = ATA(conf, v$const, T)

      if (is.null(v$fit$MIP)){
        v$text = "Solver did not find a solution. Try relaxing the target values."
      } else {
        v$plotoutput = v$fit$plot

        v$text = paste0(conf@MIP$solver, ": solved in ", sprintf("%3.3f", v$fit$solve.time[3]), "s")
        selected.idx = which(v$fit$MIP$solution == 1)
        selected.item.names = v$itemattrib[selected.idx,][['ID']]
        if (v$content.exists){
          idx.from.content = v$content$ID %in% selected.item.names
          v$results = v$content[idx.from.content,c(1:4)]
        } else {
          v$results = v$itemattrib[selected.idx,]
        }
      }

      updateCheckboxGroupButtons(
        session = session,
        inputId = "runsolver",
        selected = character(0)
      )
      shinyjs::enable("runsolver")
    }


    if (input$problemtype == 2 & v$const.exists){

      shinyjs::disable("runsolver")

      if (input$exposure_method == "BIGM-BAYESIAN"){
        if (!input$interim_method %in% c("EB", "FB")){
          exposure_method_legit = F
          v$text = "BIGM-BAYESIAN requires interim methods EB or FB."
          shinyjs::enable("runsolver")
          break
      }}

      if (is_text_parsable(input$n_simulees)){
        eval(parse(text = paste0("n_simulees = c(", input$n_simulees, ")[1]")))
        updateProgressBar(session = session, id = "pb", value = 0, total = n_simulees)
      } else {
        v$text = "Number of simulees should be an integer."
        shinyjs::enable("runsolver")
        break
      }

      if (is_text_parsable(input$simulee_id)){
        eval(parse(text = paste0("v$simulee_id = c(", input$simulee_id, ")[1]")))
      } else {
        v$text = "Number of simulees should be an integer."
        shinyjs::enable("runsolver")
        break
      }

      if (is_text_parsable(input$simulee_theta_params)){
        eval(parse(text = paste0("v$simulee_theta_params = c(", input$simulee_theta_params, ")[1:2]")))
      } else {
        v$text = "Theta distribution parameters should be two numbers."
        shinyjs::enable("runsolver")
        break
      }

      if(input$simulee_theta_distribution == "NORMAL"){
        trueTheta = rnorm(n_simulees, v$simulee_theta_params[1], v$simulee_theta_params[2])
      }
      if(input$simulee_theta_distribution == "UNIF"){
        trueTheta = runif(n_simulees, min = v$simulee_theta_params[1], max = v$simulee_theta_params[2])
      }

      thetaGrid = seq(-3, 3, 1)
      testData = MakeTest(v$itempool, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
      respData = testData@Data

      conf = new("Shadow.config")

      # parse exposure control settings

      conf@exposureControl$fadingFactor = input$exposure_ff

      if (is_text_parsable(input$exposure_af)){
        eval(parse(text = paste0("conf@exposureControl$accelerationFactor = c(", input$targets, ")[1]")))
        if (conf@exposureControl$accelerationFactor < 1){
          v$text = "Acceleration factor should be at least 1."
          shinyjs::enable("runsolver")
          break
        }
      } else {
        v$text = "Acceleration factor should be a number."
        shinyjs::enable("runsolver")
        break
      }

      conf@exposureControl$method = input$exposure_method
      conf@exposureControl$diagnosticStats = TRUE

      conf@interimTheta$method    = input$interim_method
      conf@interimTheta$priorDist = input$interim_prior
      if (is_text_parsable(input$interim_priorpar)){
        eval(parse(text = paste0("conf@interimTheta$priorPar = c(", input$interim_priorpar, ")")))
        if (length(conf@interimTheta$priorPar) != 2){
          v$text = "Interim prior parameters should be two values."
          shinyjs::enable("runsolver")
          break
        }
      } else {
        v$text = "Interim prior parameters should be two values."
        shinyjs::enable("runsolver")
        break
      }

      conf@finalTheta$method      = input$final_method
      conf@finalTheta$priorDist   = input$final_prior

      if (is_text_parsable(input$final_priorpar)){
        eval(parse(text = paste0("conf@finalTheta$priorPar = c(", input$final_priorpar, ")")))
        if (length(conf@finalTheta$priorPar) != 2){
          v$text = "Final prior parameters should be two values."
          shinyjs::enable("runsolver")
          break
        }
      } else {
        v$text = "Final prior parameters should be two values."
        shinyjs::enable("runsolver")
        break
      }

      conf@refreshPolicy$method = input$refreshpolicy
      conf@MIP$solver = input$solvertype

      v$text = "Simulating.."
      v$time = Sys.time()
      v$fit <- Shadow(v$itempool, conf, trueTheta, Constraints = v$const, prior = NULL, priorPar = c(0, 1), Data = respData, session = session)

      v$plotoutput = plotCAT(v$fit$output[[v$simulee_id]])
      v$shadowplot = plotShadow(v$fit$output[[v$simulee_id]], v$const)

      v$time = Sys.time() - v$time

      v$text = paste0(conf@MIP$solver, ": simulation complete in ", sprintf("%3.3f", v$time), "s")

      updateCheckboxGroupButtons(
        session = session,
        inputId = "runsolver",
        selected = character(0)
      )

      shinyjs::enable("runsolver")
    }
  })



  output$table_itempool <- renderDT({
    if (is.null(v$itempool)) return()
    v$itempool@ipar},
    server = F,
    selection = list(selected = c(0)),
    options = list(pageLength = 100)
  )
  output$table_itemattrib <- renderDT({
    if (is.null(v$itemattrib)) return()
    v$itemattrib},
    options = list(pageLength = 100)
  )
  output$table_stimattrib <- renderDT({
    if (is.null(v$stimattrib)) return()
    v$stimattrib},
    options = list(pageLength = 100)
  )
  output$table_constraints <- renderDT({
    if (is.null(v$const)) return()
    v$const$Constraints},
    options = list(pageLength = 100)
  )
  output$results <- renderDT({
    if (is.null(v$results)) return()
    v$results},
    options = list(pageLength = 100)
  )


  output$textoutput <- renderText({
    if (is.null(v$text)) return()
    v$text
  })
  output$plotoutput <- renderPlot({
    if (is.null(v$plotoutput)) return()
    v$plotoutput
  })
  output$shadowplot <- renderPlot({
    if (is.null(v$shadowplot)) return()
    v$shadowplot
  })
}

# Run the application
shinyApp(ui = ui, server = server)
