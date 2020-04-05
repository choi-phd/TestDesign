library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(Shadow)
library(DT)

acceptedfiles = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
css.y = "overflow-y:scroll; max-height: 65vh"

ui = fluidPage(
  theme = shinytheme("lumen"),
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML("
h3 { font-size: 125%; }
i { display: inline-block; margin-right: 0.2em; }
label, .form-group, .progress { margin-bottom: 0px; }
.btn { width: 100%; }
"))),
  titlePanel("Shadow"),

  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", "select { min-width: 100%; max-width: 100%; }"),
        tags$style(type="text/css", ".span4 { min-width: 100%; max-width: 100%; }"),
        tags$style(type="text/css", ".well { min-width: 100%; max-width: 100%; }")
      ),
      helpText("This is a demo of Optimal Test Assembly."),

      dropdownButton(
        h3(""),
        label = "Load files",
        fileInput("itempool.file"  , buttonLabel = "Item parameters"                , label = NULL, accept = acceptedfiles),
        fileInput("itemse.file"    , buttonLabel = "Item standard errors (optional)", label = NULL, accept = acceptedfiles),
        fileInput("itemattrib.file", buttonLabel = "Item attributes"                , label = NULL, accept = acceptedfiles),
        fileInput("stimattrib.file", buttonLabel = "Stimulus attributes (optional)" , label = NULL, accept = acceptedfiles),
        fileInput("const.file"     , buttonLabel = "Constraints"                    , label = NULL, accept = acceptedfiles),
        fileInput("content.file"   , buttonLabel = "Item contents (optional)"       , label = NULL, accept = acceptedfiles),
        circle = F, status = "primary", icon = icon("file-import"), width = "100%"
      ),

      h3(""),

      radioGroupButtons(
        inputId = "problemtype", justified = T,
        choiceNames = c("Fixed", "Adaptive"), choiceValues = 1:2, selected = 1
      ),
      radioGroupButtons(
        inputId = "solvertype", justified = T,
        choices = c("Symphony", "Gurobi", "GLPK", "lpSolve"), checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass"))
      ),
      radioGroupButtons(
        inputId = "objtype", justified = T, label = h3("Objective type:"),
        choices = c("TCC", "TIF", "MAXINFO")
      ),

      textInput("thetas", label = h3("Theta values (comma-separated)"), value = "0, 1"),
      textInput("targets", label = h3("Target values (comma-separated)"), value = "15, 20"),

      checkboxGroupButtons(
        inputId = "maxinfo.button", justified = T,
        choices = c("Check obtainable info range"), checkIcon = list(yes = icon("less-than-equal"), no = icon("less-than-equal"))
      ),

      dropdownButton(
        inputId = "simulation.dropdown", label = "Simulation settings",
        radioGroupButtons(
          inputId = "simulee.theta.distribution", justified = T,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("simulee.theta.params", label = "True theta distribution parameters", value = "0, 1"),
        textInput("n.simulees", label = h3("# of simulees"), value = "1"),
        textInput("simulee.id", label = h3("Display plots for simulee:"), value = "1"),
        circle = F, icon = icon("database"), width = "100%", status = "danger"
      ),

      dropdownButton(
        inputId = "exposure.dropdown", label = "Exposure control settings",
        radioGroupButtons(
          inputId = "exposure.method", justified = TRUE,
          choices = c("ELIGIBILITY", "BIGM", "BIGM-BAYESIAN")
        ),
        sliderInput("exposure.ff", label = h3("Fading factor"), min = .9, max = 1.0, value = 1.0, step = .01),
        textInput("exposure.af", label = h3("Acceleration factor"), value = "2"),
        circle = F, icon = icon("cog"), width = "100%"
      ),

      dropdownButton(
        inputId = "theta.settings", label = "Estimation Settings for interim & final thetas",
        h3("Interim theta"),
        radioGroupButtons(
          inputId = "interim.method", justified = T,
          choices = c("EAP", "FB", "EB")
        ),
        radioGroupButtons(
          inputId = "interim.prior", justified = T,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("interim.priorpar", label = "Prior distribution parameters", value = "0, 1"),
        h3("Final theta"),
        radioGroupButtons(
          inputId = "final.method", justified = T,
          choices = c("EAP", "FB", "EB")
        ),
        radioGroupButtons(
          inputId = "final.prior", justified = T,
          choices = c("NORMAL", "UNIF")
        ),
        textInput("final.priorpar", label = "Prior distribution parameters", value = "0, 1"),
        circle = F, icon = icon("cog"), width = "100%"
      ),

      radioGroupButtons(
        inputId = "itemselection.method", justified = T, label = h3("Item selection method"),
        choices = c("MFI", "MPWI", "FB", "EB")
      ),
      radioGroupButtons(
        inputId = "contentbalancing.method", justified = T, label = h3("Content balancing method"),
        choices = c("NONE", "STA")
      ),

      dropdownButton(
        inputId = "refreshpolicy.dropdown", label = "Refresh policy",
        radioGroupButtons(
          inputId = "refreshpolicy", justified = T, direction = "vertical",
          choices = c("ALWAYS", "POSITION", "INTERVAL", "THRESHOLD", "INTERVAL-THRESHOLD", "SET")
        ),
        circle = FALSE, icon = icon("cog"), width = "100%"
      ),

      downloadButton("export.data", "Export data")
    ),

    mainPanel(
      tags$head(
        tags$style(type='text/css', '#textoutput {background-color: rgba(64,64,64,1); color: cyan;}')
      ),
      disabled(
        checkboxGroupButtons(
          inputId = "runsolver", justified = T,
          choices = c("Run Solver"), checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")), status = "primary"
        )
      ),
      verbatimTextOutput("textoutput", placeholder = T),
      progressBar(id = "pb", value = 0, total = 1, display_pct = T),
      hr(),
      tabsetPanel(id = "tabs",
        tabPanel("Main",                value = 1, plotOutput("plotoutput", width = "100%", height = "65vh")),
        tabPanel("Output",              value = 2, plotOutput("shadowchart", width = "100%", height = "65vh")),
        tabPanel("Output",              value = 3, DTOutput("results")          , style = css.y),
        tabPanel("Item parameters",     value = 4, DTOutput("table.itempool")   , style = css.y),
        tabPanel("Item attributes",     value = 5, DTOutput("table.itemattrib") , style = css.y),
        tabPanel("Stimulus attributes", value = 6, DTOutput("table.stimattrib") , style = css.y),
        tabPanel("Constraints",         value = 7, DTOutput("table.constraints"), style = css.y)
      )
    )
  )
)

is.text.parsable = function(arg.text){
  txt = gsub("[^0-9\\., \\-]", "", arg.text) # Limits eval to only accept legit inputs
  return(txt == arg.text)
}

return.object.or.null = function(arg.object){
  if (is.null(arg.object)) return(NULL)
  return(arg.object)
}

assign.object.first = TRUE

assign.object = function(objname, obj, desc){
  if (assign.object.first){
    assign.object.first <<- FALSE
    message("\nRefresh the environment tab to see the objects in the list.")
  }
  assign(objname, obj, envir = .GlobalEnv)
  pad = paste0(rep(" ", 48 - nchar(desc)), collapse = "")
  tmp = paste0(desc, pad, "assigned to : ", objname)
  message(tmp)
}

server = function(input, output, session) {
  v = reactiveValues(itempool.exists = F,
    itemse.exists = F,
    const.exists = F,
    content.exists = F,
    stimattrib.exists = F,
    problemtype = 0)

  observeEvent(input$itempool.file, {
    if (!is.null(input$itempool.file)){
      v$itempool = try(LoadItemPool(input$itempool.file$datapath))
      if (class(v$itempool) == "item.pool"){
        v$itempool.exists = T; v$text = "Step 1. Item parameter file: OK"
        v$ipar = v$itempool@ipar
        assign.object("shiny.itempool"     , v$itempool, "Item parameters (full object)")
        assign.object("shiny.itempool.ipar", v$ipar    , "Item parameters (matrix)")
      } else {
        v$itempool.exists = F; v$text = "Step 1 Error: Item parameters are not in the correct format."
      }
    }
  })

  observeEvent(input$itemse.file, {
    if (!is.null(input$itemse.file) & v$itempool.exists){
      v$itempool = try(LoadItemPool(input$itempool.file$datapath, se.file.csv = input$itemse.file$datapath))
      if (class(v$itempool) == "item.pool"){
        v$itemse.exists = T; v$text = "Optional Step. Item standard error file: OK"
        assign.object("shiny.itempool"     , v$itempool, "Item parameters (full object)")
      } else {
        v$itemse.exists = F; v$text = "Optional Step Error: Item standard errors are not in the correct format."
      }
    }
  })

  observeEvent(input$itemattrib.file, {
    if (!is.null(input$itemattrib.file) & v$itempool.exists){
      v$itemattrib = try(LoadItemAttrib(input$itemattrib.file$datapath, v$itempool))
      if (class(v$itemattrib) == "data.frame"){
        v$itemattrib.exists = T; v$text = "Step 2. Item attributes: OK"
        assign.object("shiny.itemattrib", v$itemattrib, "Item attributes")
      } else {
        v$itemattrib.exists = F; v$text = "Step 2 Error: Item attributes are not in the correct format."
      }
    }
  })

  observeEvent(input$stimattrib.file, {
    if (!is.null(input$stimattrib.file) & v$itempool.exists & v$itemattrib.exists){
      v$stimattrib = try(LoadStAttrib(input$stimattrib.file$datapath, v$itemattrib))
      if (class(v$stimattrib) == "data.frame"){
        v$stimattrib.exists = T; v$text = "Optional Step. Stimulus attributes: OK"
        assign.object("shiny.stimattrib", v$stimattrib, "Stimulus attributes")
      } else {
        v$stimattrib.exists = F; v$text = "Optional Step Error: Stimulus attributes are not in the correct format."
      }
    }
  })

  observeEvent(input$const.file, {
    if (!is.null(input$const.file) & v$itempool.exists & v$itemattrib.exists){
      if ( v$stimattrib.exists) v$const = try(LoadConstraints(input$const.file$datapath, v$itempool, v$itemattrib, v$stimattrib))
      if (!v$stimattrib.exists) v$const = try(LoadConstraints(input$const.file$datapath, v$itempool, v$itemattrib))
      if (class(v$const) == "list"){
        v$const.exists = T; v$text = "Step 3. Constraints: OK. Press button to run solver."
        v$constraints = v$const$Constraints
        assign.object("shiny.const"      , v$const      , "Constraints (full object)")
        assign.object("shiny.constraints", v$constraints, "Constraints (data.frame)")
      } else {
        v$const.exists = F; v$text = "Step 3 Error: Constraints are not in the correct format."
      }
      shinyjs::toggleState("runsolver", v$const.exists)
    }
  })

  observeEvent(input$content.file, {
    if (!is.null(input$content.file)){
      v$content = try(read.csv(input$content.file$datapath))
      if (class(v$content) == "data.frame"){
        v$content.exists = T; v$text = "Optional Step. Item contents: OK."
        assign.object("shiny.content", v$content, "Item contents")
      } else {
        v$content.exists = F; v$text = "Optional Step Error: Item contents are not in the correct format."
      }
    }
  })

  observeEvent(input$problemtype, {
    shinyjs::toggle("objtype"                , condition = input$problemtype == 1)
    shinyjs::toggle("thetas"                 , condition = input$problemtype == 1)
    shinyjs::toggle("targets"                , condition = input$problemtype == 1)
    shinyjs::toggle("maxinfo.button"         , condition = input$problemtype == 1)
    shinyjs::toggle("exposure.dropdown"      , condition = input$problemtype == 2)
    shinyjs::toggle("theta.settings"         , condition = input$problemtype == 2)
    shinyjs::toggle("itemselection.method"   , condition = input$problemtype == 2)
    shinyjs::toggle("contentbalancing.method", condition = input$problemtype == 2)
    shinyjs::toggle("refreshpolicy.dropdown" , condition = input$problemtype == 2)
    shinyjs::toggle("simulation.dropdown"    , condition = input$problemtype == 2)
    if (input$problemtype == 2){
      showTab("tabs", target = "2")
      hideTab("tabs", target = "3")
    } else {
      hideTab("tabs", target = "2")
      showTab("tabs", target = "3")
    }
  })

  observeEvent(input$objtype, { shinyjs::toggleState("targets", input$objtype != "MAXINFO") })

  observeEvent(input$simulee.id, {
    if (v$problemtype == 2){
      if (!is.null(v$fit)){
        if (is.text.parsable(input$simulee.id)){
          eval(parse(text = paste0("simulee.id = c(", input$simulee.id, ")[1]")))
          v$simulee.id = min(simulee.id, v$n.simulees)
        } else {
          v$text = "Number of simulees should be an integer."; break
        }
        v$plotoutput = plotCAT(v$fit$output[[v$simulee.id]])
        assign.object(paste0("shiny.thetaplot.", v$simulee.id), v$plotoutput, paste0("Theta plot for simulee ", v$simulee.id))
        v$shadowchart = plotShadow(v$fit$output[[v$simulee.id]], v$const)
        assign.object(paste0("shiny.shadowchart.", v$simulee.id), v$shadowchart, paste0("Shadow test chart for simulee ", v$simulee.id))
      }
    }
  })

  observeEvent(input$maxinfo.button, {
    if (v$itempool.exists & v$const.exists){
      v$inforangeplot = maxinfoplot(v$itempool, v$const)
      v$plotoutput = v$inforangeplot
      assign.object("shiny.inforangeplot", v$inforangeplot, "Obtainable info range plot")
    }
    updateCheckboxGroupButtons(
      session = session,
      inputId = "maxinfo.button",
      selected = character(0)
    )
  })

  observeEvent(input$runsolver, {

    shinyjs::disable("runsolver")

    for (do in 1){

      if (input$problemtype == 1 & v$const.exists){

        v$problemtype = 1

        conf = new("ATA.config")

        conf@MIP$solver = input$solvertype

        if (is.text.parsable(input$thetas)){
          eval(parse(text = paste0("conf@itemSelection$targetLocation = c(", input$thetas, ")")))
        } else {
          v$text = "Theta values should be comma-separated numbers."; break
        }

        if (is.text.parsable(input$targets)){
          eval(parse(text = paste0("conf@itemSelection$targetValue = c(", input$targets, ")")))
        } else {
          v$text = "Target values should be comma-separated numbers."; break
        }

        conf@itemSelection$targetWeight = rep(1, length(conf@itemSelection$targetLocation))

        assign.object("shiny.ATA.config", conf, "ATA.config object")

        v$text = "Solving.."

        v$fit = ATA(conf, v$const, T)
        assign.object("shiny.ATA", v$fit, "ATA solution object")

        if (is.null(v$fit$MIP)){
          v$text = "Solver did not find a solution. Try relaxing the target values."
        } else {
          v$plotoutput = v$fit$plot

          v$text = paste0(conf@MIP$solver, ": solved in ", sprintf("%3.3f", v$fit$solve.time[3]), "s")
          v$selected.idx = which(v$fit$MIP$solution == 1)
          v$selected.item.names = v$itemattrib[v$selected.idx,][['ID']]
          v$selected.item.attribs = v$itemattrib[v$selected.idx,]
          assign.object("shiny.selected.item.attribs", v$selected.item.attribs, "Selected item attributes")
          if (v$content.exists){
            v$idx.from.content = v$content$ID %in% v$selected.item.names
            v$selected.item.contents = v$content[v$idx.from.content,]
            assign.object("shiny.selected.item.contents", v$selected.item.contents, "Selected item contents")
            v$results = v$selected.item.contents
          } else {
            v$results = v$selected.item.attribs
          }
        }
      }

      if (input$problemtype == 2 & v$const.exists){

        v$problemtype = 2

        if (input$exposure.method == "BIGM-BAYESIAN"){
          if (!input$interim.method %in% c("EB", "FB")){
            v$text = "BIGM-BAYESIAN requires interim methods EB or FB."; break
          }
        }

        if (is.text.parsable(input$n.simulees)){
          eval(parse(text = paste0("v$n.simulees = c(", input$n.simulees, ")[1]")))
          v$n.simulees = min(100, v$n.simulees)
          updateProgressBar(session = session, id = "pb", value = 0, total = v$n.simulees)
        } else {
          v$text = "Number of simulees should be an integer."; break
        }

        if (is.text.parsable(input$simulee.id)){
          eval(parse(text = paste0("v$simulee.id = c(", input$simulee.id, ")[1]")))
        } else {
          v$text = "Number of simulees should be an integer."; break
        }

        if (is.text.parsable(input$simulee.theta.params)){
          eval(parse(text = paste0("v$simulee.theta.params = c(", input$simulee.theta.params, ")[1:2]")))
        } else {
          v$text = "Theta distribution parameters should be two numbers."; break
        }

        if (input$simulee.theta.distribution == "NORMAL"){
          trueTheta = rnorm(v$n.simulees, v$simulee.theta.params[1], v$simulee.theta.params[2])
        }
        if (input$simulee.theta.distribution == "UNIF"){
          trueTheta = runif(v$n.simulees, min = v$simulee.theta.params[1], max = v$simulee.theta.params[2])
        }
        assign.object("shiny.truetheta", trueTheta, "Simulation: true theta values")

        thetaGrid = seq(-3, 3, 1)
        testData = MakeTest(v$itempool, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
        respData = testData@Data
        assign.object("shiny.respdata", respData, "Simulation: response data")

        conf = new("Shadow.config")

        # parse exposure control settings

        conf@exposureControl$fadingFactor = input$exposure.ff

        if (is.text.parsable(input$exposure.af)){
          eval(parse(text = paste0("conf@exposureControl$accelerationFactor = c(", input$exposure.af, ")[1]")))
          if (conf@exposureControl$accelerationFactor < 1){
            v$text = "Acceleration factor should be a number at least 1."; break
          }
        } else {
          v$text = "Acceleration factor should be a number at least 1."; break
        }

        conf@exposureControl$method = input$exposure.method
        conf@exposureControl$diagnosticStats = TRUE

        conf@interimTheta$method    = input$interim.method
        conf@interimTheta$priorDist = input$interim.prior
        conf@finalTheta$method      = input$final.method
        conf@finalTheta$priorDist   = input$final.prior
        conf@itemSelection$method   = input$itemselection.method

        if (conf@interimTheta$method == "FB" & v$itemse.exists == F) {
          v$text = "FB interim method requires the standard errors associated with the item parameters."; break
        }
        if (is.text.parsable(input$interim.priorpar)){
          eval(parse(text = paste0("conf@interimTheta$priorPar = c(", input$interim.priorpar, ")")))
          if (length(conf@interimTheta$priorPar) != 2){
            v$text = "Interim prior parameters should be two numeric values."; break
          }
        } else {
          v$text = "Interim prior parameters should be two values."; break
        }
        if (is.text.parsable(input$final.priorpar)){
          eval(parse(text = paste0("conf@finalTheta$priorPar = c(", input$final.priorpar, ")")))
          if (length(conf@finalTheta$priorPar) != 2){
            v$text = "Final prior parameters should be two values."; break
          }
        } else {
          v$text = "Final prior parameters should be two values."; break
        }


        if (conf@itemSelection$method == "FB"){
          if (conf@interimTheta$method != "FB"){
            v$text = "FB item selection method requires FB interim method."; break
          }
        }

        conf@refreshPolicy$method = input$refreshpolicy
        conf@MIP$solver = input$solvertype

        assign.object("shiny.Shadow.config", conf, "Shadow.config object")

        v$text = "Simulating.."
        v$time = Sys.time()
        v$fit  = Shadow(v$itempool, conf, trueTheta, Constraints = v$const, prior = NULL, priorPar = c(0, 1), Data = respData, session = session)
        assign.object("shiny.Shadow", v$fit, "Simulation result")

        if (v$simulee.id > v$n.simulees) v$simulee.id = 1

        v$plotoutput = plotCAT(v$fit$output[[v$simulee.id]])
        assign.object(paste0("shiny.thetaplot.", v$simulee.id), v$plotoutput, paste0("Theta plot for simulee ", v$simulee.id))
        v$shadowchart = plotShadow(v$fit$output[[v$simulee.id]], v$const)
        assign.object(paste0("shiny.shadowchart.", v$simulee.id), v$shadowchart, paste0("Shadow test chart for simulee ", v$simulee.id))

        v$time = Sys.time() - v$time

        v$text = paste0(conf@MIP$solver, ": simulation complete in ", sprintf("%3.3f", v$time), "s")
      }
    }

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runsolver",
      selected = character(0)
    )
    shinyjs::enable("runsolver")
  })

  output$table.itempool    = renderDT(return.object.or.null(v$ipar)       , options = list(pageLength = 100))
  output$table.itemattrib  = renderDT(return.object.or.null(v$itemattrib) , options = list(pageLength = 100))
  output$table.stimattrib  = renderDT(return.object.or.null(v$stimattrib) , options = list(pageLength = 100))
  output$table.constraints = renderDT(return.object.or.null(v$constraints), options = list(pageLength = 100))
  output$results           = renderDT(return.object.or.null(v$results), options = list(pageLength = 100))
  output$textoutput        = renderText(return.object.or.null(v$text))
  output$plotoutput        = renderPlot(return.object.or.null(v$plotoutput))
  output$shadowchart       = renderPlot(return.object.or.null(v$shadowchart))

  output$export.data = downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
    content = function(fname) {
      fs = c()
      setwd(tempdir())

      if (!is.null(v$ipar)){
        path = "raw.data.item.params.csv"
        fs = c(fs, path)
        write.csv(v$ipar, path, row.names = F)
      }
      if (!is.null(v$itemattrib)){
        path = "raw.data.item.attribs.csv"
        fs = c(fs, path)
        write.csv(v$itemattrib, path, row.names = F)
      }
      if (!is.null(v$stimattrib)){
        path = "raw.data.stim.attribs.csv"
        fs = c(fs, path)
        write.csv(v$stimattrib, path, row.names = F)
      }
      if (!is.null(v$constraints)){
        path = "raw.data.constraints.csv"
        fs = c(fs, path)
        write.csv(v$constraints, path, row.names = F)
      }
      if (!is.null(v$content)){
        path = "raw.data.content.csv"
        fs = c(fs, path)
        write.csv(v$content, path, row.names = F)
      }

      if (v$problemtype == 1){
        if (!is.null(v$plotoutput)){
          path = "plot.pdf"
          fs = c(fs, path)
          pdf(path)
          print(v$plotoutput)
          dev.off()
        }
        if (v$content.exists){
          if (!is.null(v$selected.item.contents)){
            path = "selected.item.contents.csv"
            fs = c(fs, path)
            write.csv(v$selected.item.contents, path, row.names = F)
          }
        }
        if (!is.null(v$selected.item.attribs)){
          if (!is.null(v$selected.idx)){
            path = "selected.item.attribs.csv"
            fs = c(fs, path)
            write.csv(v$selected.item.attribs, path, row.names = F)
          }
        }
      }

      if (v$problemtype == 2){
        if (!is.null(v$fit)){
          path = paste0("theta.plot.", v$simulee.id, ".pdf")
          fs = c(fs, path)
          pdf(path)
          print(v$plotoutput)
          dev.off()
          path = paste0("shadowchart.", v$simulee.id, ".pdf")
          fs = c(fs, path)
          pdf(path)
          print(v$shadowchart)
          dev.off()
        }
      }
      zip(zipfile = fname, files = fs, flags = "-j")
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)