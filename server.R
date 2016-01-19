#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# http://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb

library(shiny)
# library(lazyeval)
library(ggplot2)


shinyServer(function(input, output, session) {
  
  # Reactives ---------------------------------------------------------------
  
  Data <- reactive({
    inFile <- input$inputCSV
    
    if (is.null(inFile)){
      
      inFile <- input$inputTSV
    
      if (is.null(inFile))
      return(NULL)
      
      read_tsv(inFile$datapath, skip = 2) %>% 
        filter(row_number() != nrow(.))
      
      } else {
        read_csv(inFile$datapath)
      }
  })
  
  PlotData <- reactive({
    if(input$UniqueID == "" || is.null(input$UniqueID)) {
      Data <-  
        Data()
    } else {
      Data <- 
        workingValues$workingData %>% 
        filter(sampleID == input$ID) %>% 
        .[workingValues$keeprows,]}
    
    Model <- 
      lm(get(input$Y) ~ get(input$X), data = Data)
    
    Plot <- 
      ggplot(aes(x = get(input$X),
                 y = get(input$Y)),
             data = Data) +
      xlab(input$X) +
      ylab(input$Y) +
      geom_point(color = "darkgray") +
      geom_abline(slope = coef(Model)[2],
                  intercept = coef(Model[1]),
                  color = "black") +
      theme_bw()
    
    info <- list(Model = Model, Data = Data, Plot = Plot)
    return(info)
  })
  
  IndexID <- reactive(grep(input$ID, workingValues$allIDs))
    
  
  # Reactive Values ---------------------------------------------------------
  
  workingValues <- 
    reactiveValues(workingData = NULL,
                   allIDs = list(),
                   sampledIDs = list(),
                   outputData = NULL,
                   keeprows = logical(),
                   fittedEfllux = tbl_df(data.frame(
                     sampleID = character(), 
                     intercept = numeric(), 
                     slope = numeric(), 
                     adjustedRSquared = numeric(), 
                     residualStandardError = numeric(), 
                     degreesFreedom = numeric(), 
                     fStatistic = numeric())),
                   processedPlots = list())
  
  # displayConditions <- 
  #   reactiveValues(idSelected = input$UniqueID == "" || is.null(input$UniqueID))
  # 
  # Output Variables --------------------------------------------------------
  
  output$fileUploaded <- reactive({
    return(is.null(Data()))
  })
  
  
  output$variablesSelected <- reactive({
    check <- input$X != "" & input$Y != ""
    return(check)
  })
  
  
  # output$idSelected <- reactive({
  #   return(input$UniqueID == "" || is.null(input$UniqueID))
  # })
  # 

  # Observers ---------------------------------------------------------------
  observe({
    updateSelectizeInput(session,
                         inputId = "X",
                         choices = c(Choose = "", 
                                     names(Data())[sapply(Data(),is.numeric)]))
    
    updateSelectizeInput(session,
                         inputId = "Y",
                         choices = c(Choose = "", 
                                     names(Data())[sapply(Data(),is.numeric)]))
    
    updateSelectizeInput(session,
                         inputId = "UniqueID",
                         choices = c(Choose = "", 
                                     names(Data())))
  })
  
  observeEvent(input$ID,{
    
    if(length(input$UniqueID) == 0){NULL}
    
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID()])
  })
  
  
  
  
  observeEvent(input$nextID,{
    
    workingValues$sampledIDs[[input$ID]] <- input$ID
    
    workingValues$processedPlots[[input$ID]] <- PlotData()$Plot
      
    workingValues$fittedEfflux <- 
      bind_rows(workingValues$fittedEfflux,
                data.frame(
                  sampleID = input$ID, 
                  intercept = coef(PlotData()$Model)[2], 
                  slope = coef(PlotData()$Model)[1], 
                  adjustedRSquared = summary(PlotData()$Model)$adj.r.squared, 
                  residualStandardError = summary(PlotData()$Model)$sigma, 
                  degreesFreedom = summary(PlotData()$Model)$fstatistic[3], 
                  fStatistic = summary(PlotData()$Model)$fstatistic[1]))
    
    # workingValues$outputData <- 
    #   bind_rows(
    #   workingValues$workingData %>% 
    #     filter(sampleID == input$ID),
    #   workingValues$outputData
    # )
    
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID() + 1 ])})
  
  
  
  observeEvent(input$previousID,{
    
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID() - 1])})
  
  observeEvent(input$undoEdit,{
    
    workingValues$sampledIDs[[input$ID]] <- NULL
    
    workingValues$keeprows <-  rep(TRUE, nrow(workingValues$workingData))
    
    workingValues$processedPlot[[input$ID]] <- NULL
    
    workingValues$fittedEfflux <- 
      workingValues$fittedEfflux %>% 
      filter(sampleID != input$ID)
    
    # workingvalues$outputData <- 
    #   bind_rows(
    #     workingValues$workingData %>% 
    #       filter(sampleID == input$ID),
    #     workingValues$outputData %>% 
    #       filter(sampleID != input$ID))
  })
  
  observeEvent(
      input$UniqueID,{
        workingValues$workingData <-
          Data() %>%
          unite_("sampleID", unlist(input$UniqueID), sep = "_") %>% 
          arrange_("sampleID", input$X)

        workingValues$allIDs <-
          workingValues$workingData %>%
          distinct(sampleID) %>%
          arrange(sampleID) %>% 
          .$sampleID 
          
        workingValues$outputData <- workingValues$workingData
        
        workingValues$keeprows <-  rep(TRUE, nrow(workingValues$workingData))
      }
    )
  
  
  observeEvent(input$plot_click, {
    res <- nearPoints(PlotData()$Data, 
                      input$plot_click,
                      xvar = input$X,
                      yvar = input$Y,
                      allRows = TRUE)
    
    workingValues$keeprows <- xor(workingValues$keeprows, res$selected_)
  })
  
  observeEvent(input$plot_brush, {
    res <- brushedPoints(PlotData()$Data,
                         input$plot_brush, 
                         xvar = input$X,
                         yvar = input$Y,
                         allRows = TRUE)
    
    workingValues$keeprows <- xor(workingValues$keeprows, res$selected_)
  })
  
  # Renders -----------------------------------------------------------------
  
  output$plotConc <- renderPlot({
    
    if(is.null(input$X) || 
       is.null(input$Y) ||
       input$X == "" ||
       input$Y == "") {return(NULL)}

    PlotData()$Plot
    
  }, height = 600)
  
  output$idSelection <- renderUI({
    if(length(input$UniqueID) == 0) {return(NULL)}
    p("Clicking 'Previous' or 'Next' will cause mark the selected probe as editted")
    fluidRow(
      selectizeInput("ID",
                     label = NULL,
                     choices = workingValues$allIDs,
                     multiple = F),              
      actionButton("previousID",
                   label = "Previous"),
      
      actionButton("undoEdit",
                   label = "Reset Probe"),
      actionButton("nextID",
                   label = "Save & Next")
    )
  })
  
  output$uploadedData <- renderDataTable({
    Data()
  })
  
  output$plottedData <- renderDataTable({
    if(input$UniqueID == "" || is.null(input$UniqueID)) {return(NULL)}
    PlotData()$Data
    # %>% 
    #   select_("sampleID", 
    #           paste("`", input$X, "`", sep = ""), 
    #           paste("`", input$Y, "`", sep = ""))
  })
  
  output$edittedData <- renderDataTable({
    #if(length(edittedSamples) == 0) {return(NULL)}
    workingValues$outputData %>% 
      filter(sampleID %in% workingValues$sampledIDs)
  })
  
  output$outputData <- renderDataTable({
    workingValues$outputData
  })
  
  output$regressionData <- renderDataTable({
    workingValues$fittedEfflux
    })
  
  output$plots <- renderUI({
    selectizeInput("plotID",
                   label = NULL,
                   choices = c(unlist(workingValues$sampledIDs)),
                   multiple = F
                )
    plotOutput(workingValues$processedPlots[[input$plotID]])
  })
  
  output$test <- renderPrint(workingValues$processedPlots)
  
  
  # Output Options ----------------------------------------------------------
  
  # outputOptions(output, 'idSelected', suspendWhenHidden = F)
  outputOptions(output, 'variablesSelected', suspendWhenHidden = F)
  outputOptions(output, 'fileUploaded', suspendWhenHidden = F)
  outputOptions(output, "plotConc", suspendWhenHidden = F)
  
})
