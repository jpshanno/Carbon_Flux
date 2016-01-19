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
library(Cairo)
options(DT.options = list(pageLength = 50))


shinyServer(function(input, output, session) {
  
  
  
  # Reactives ---------------------------------------------------------------
  
  Data <- reactive({
    inFile <- input$File
    
    if (is.null(inFile))
      return(NULL)
    
    filename <- as.character(inFile$name)
    extension <- substr(filename, nchar(filename)-3, nchar(filename))
    
    # if (!(extension %in% c(".dat", ".csv"))){
    #   session$sendCustomMessage(message = list("Please select a *.csv or *.dat file"))
    #   return(NULL)
    # }
    
    if(extension == ".dat"){
      read_tsv(inFile$datapath, skip = 2) %>% 
        filter(row_number() != nrow(.))
    } else {
      read_csv(inFile$datapath)
    }
    
  })
  
  PlotData <- reactive({
    
    
    if(displayConditions$idNotSelected) {
      Data <-  
        Data()
    } else {
      Data <- 
        workingValues$workingData %>% 
        filter(sampleID == input$ID)
    }
    
    
    if(displayConditions$idNotSelected){
      plottingData <- Data
    } else {
      plottingData <- Data[workingValues$keeprows[[input$ID]],]
    }
    
    
    Model <- 
      lm(get(input$Y) ~ get(input$X), data = Data)
    
    Plot <- 
      ggplot(aes(x = get(input$X),
                 y = get(input$Y)),
             data =  plottingData) +
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
                   keeprows = list(),
                   fittedEfflux = tbl_df(data.frame(
                     sampleID = character(), 
                     nPointsUsed = numeric(),
                     intercept = numeric(), 
                     slope = numeric(), 
                     adjustedRSquared = numeric(), 
                     residualStandardError = numeric(), 
                     degreesFreedom = numeric(), 
                     fStatistic = numeric())),
                   processedPlots = list())
  
  displayConditions <-
    reactiveValues(
      idNotSelected = TRUE,
      variablesNotSelected = TRUE)

  # Output Variables --------------------------------------------------------
  
  output$fileUploaded <- reactive({
    return(is.null(Data()))
  })
  
  
  output$variablesSelected <- reactive({
    check <- input$X != "" & input$Y != ""
    return(check)
  })
  
  output$showPlot <- reactive({
    return(!displayConditions$variablesNotSelected)
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
  
  
  observe({
    if(!is.null(input$X) && 
       !is.null(input$Y) && 
       input$X != "" && 
       input$Y != ""){
      displayConditions$variablesNotSelected <- FALSE
    } else {
        displayConditions$variablesNotSelected <- TRUE
      }
    
    if(!is.null(input$UniqueID) && 
       input$UniqueID != ""){
      displayConditions$idNotSelected <- FALSE
    } else {
        displayConditions$idNotSelected <- TRUE
    }
  })
  
  observeEvent(input$ID,{
    
    if(displayConditions$idNotSelected){NULL}
    
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID()])
  })
  
  
  
  
  observeEvent(input$nextID,{
    
    workingValues$sampledIDs[[input$ID]] <- input$ID
    
    workingValues$processedPlots[[input$ID]] <- PlotData()$Plot
      
    workingValues$fittedEfflux <- 
      bind_rows(workingValues$fittedEfflux %>% 
                  filter(sampleID != input$ID),
                data.frame(
                  sampleID = input$ID, 
                  nPointsUsed = nrow(PlotData()$Data[workingValues$keeprows[[input$ID]],]),
                  intercept = coef(PlotData()$Model)[2], 
                  slope = coef(PlotData()$Model)[1], 
                  adjustedRSquared = summary(PlotData()$Model)$adj.r.squared, 
                  residualStandardError = summary(PlotData()$Model)$sigma, 
                  degreesFreedom = summary(PlotData()$Model)$fstatistic[3], 
                  fStatistic = summary(PlotData()$Model)$fstatistic[1]))
    
    
    workingValues$outputData <-
      bind_rows(
      workingValues$outputData %>%
        filter(sampleID != input$ID),
      PlotData()$Data[workingValues$keeprows[[input$ID]],]) %>% 
      arrange_("sampleID")
    
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID() + 1 ])})
  
  
  
  observeEvent(input$previousID,{
    
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID() - 1])})
  
  
  observeEvent(input$undoEdit,{
    
    workingValues$sampledIDs[[input$ID]] <- NULL
    
    workingValues$keeprows[[input$ID]] <- rep(TRUE, 
                                              nrow(
                                                filter(
                                                  workingValues$workingData, 
                                                  sampleID == input$ID)))
    
    workingValues$processedPlot[[input$ID]] <- NULL
    
    if(!is.null(workingValues$fittedEfflux)) {
      workingValues$fittedEfflux <- 
        workingValues$fittedEfflux %>% 
        filter(sampleID != input$ID)}
    
    workingValues$outputData <-
      bind_rows(
        workingValues$workingData %>%
          filter(sampleID == input$ID),
        workingValues$outputData %>%
          filter(sampleID != input$ID)) %>% 
      arrange_("sampleID")
  })
  
  
  observeEvent(
    input$UniqueID,{
        
        if(displayConditions$idNotSelected){
          workingValues$workingData = NULL
          workingValues$allIDs = list()
          workingValues$sampledIDs = list()
          workingValues$keeprows = list()
          workingValues$outputData = NULL
          workingValues$fittedEfflux = tbl_df(data.frame(
            sampleID = character(), 
            nPointsUsed = numeric(),
            intercept = numeric(), 
            slope = numeric(), 
            adjustedRSquared = numeric(), 
            residualStandardError = numeric(), 
            degreesFreedom = numeric(), 
            fStatistic = numeric()))
          workingValues$processedPlots = list()
        }
        
        workingValues$workingData <-
          Data() %>%
          unite_("sampleID", unlist(input$UniqueID), sep = "_") %>% 
          arrange_("sampleID")

        workingValues$allIDs <-
          workingValues$workingData %>%
          distinct(sampleID) %>%
          arrange(sampleID) %>% 
          .$sampleID 

        workingValues$outputData <- workingValues$workingData
        
        for(SAMPLE in workingValues$allIDs){
          workingValues$keeprows[[SAMPLE]] <- rep(TRUE, nrow(filter(workingValues$workingData, sampleID == SAMPLE)))
        }
          # rep(TRUE, nrow(filter(workingValues$workingData, sampleID == input$ID)))
      }
    )
  
  
  observeEvent(input$plot_click, {
    if(displayConditions$idNotSelected) {return(NULL)}
    res <- nearPoints(PlotData()$Data, 
                      input$plot_click,
                      xvar = input$X,
                      yvar = input$Y,
                      maxpoints = 1,
                      allRows = TRUE)
    
    workingValues$keeprows[[input$ID]] <- workingValues$keeprows[[input$ID]] & !res$selected_
  })
  
  observeEvent(input$plot_brush, {
    if(displayConditions$idNotSelected) {return(NULL)}
    res <- brushedPoints(PlotData()$Data,
                         input$plot_brush, 
                         xvar = input$X,
                         yvar = input$Y,
                         allRows = TRUE)
    
    workingValues$keeprows[[input$ID]] <- workingValues$keeprows[[input$ID]] & !res$selected_
  })
  
  # Renders -----------------------------------------------------------------
  
  output$plotConc <- renderPlot({
    
    if(displayConditions$variablesNotSelected) {return(NULL)}

    PlotData()$Plot
    
  }, height = 600)
  
  output$idSelection <- renderUI({
    if(length(input$UniqueID) == 0) {return(NULL)}
      column(width = 12,
             fluidRow(
               actionButton("previousID",
                            label = "Previous",
                            width = "30%"),
               actionButton("undoEdit",
                            label = "Reset Probe",
                            width = "30%"),
               actionButton("nextID",
                            label = "Save & Next",
                            width = "30%")
             ),
             br(),
             fluidRow(
               selectizeInput("ID",
                              label = NULL,
                              choices = workingValues$allIDs,
                              multiple = F)
               )
      )
  })
  
  # output$uploadedData <- renderDataTable({
  #   Data()
  # })
  
  output$plottedData <- renderDataTable({
    if(displayConditions$variablesNotSelected || displayConditions$idNotSelected) {return(NULL)}
        PlotData()$Data[workingValues$keeprows[[input$ID]],] %>%
          select_("sampleID",
                  paste("`", input$X, "`", sep = ""),
                  paste("`", input$Y, "`", sep = ""))
        # options = list(paging = FALSE, searching = FALSE),
        # rownames = FALSE
    })
  
  output$edittedData <- renderDataTable({
    if(length(workingValues$sampledIDs) ==0) {return(NULL)}
    DT::datatable(
      workingValues$outputData %>% 
        filter(sampleID %in% workingValues$sampledIDs),
      rownames = F
    )
  })
  
  output$outputData <- renderDataTable({
    if(length(workingValues$sampledIDs) == 0){
      DT::datatable(
        Data(),
      rownames = F
      )
    } else {
    DT::datatable(
      workingValues$outputData,
      rownames = F
    )
    }
  })
  
  output$regressionData <- renderDataTable({
    if(length(workingValues$sampledIDs) ==0) {return(NULL)}
    DT::datatable(
      workingValues$fittedEfflux,
      rownames = F
    )
    })
  
  
  output$downloadbutton <- renderUI({
    if(length(workingValues$sampledIDs)== 0) {return(NULL)}
    downloadButton("download",
                   label = "Download all data and plots")
  })
  
  output$download <-
    downloadHandler(
      filename = "Processed_Data.zip",
      content = function(file){
        write_csv(workingValues$outputData %>% 
                    filter(sampleID %in% workingValues$sampledIDs),
                  "Editted_Samples.csv")
        write_csv(workingValues$outputData, "All_Samples.csv")
        write_csv(workingValues$fittedEfflux, "Model_Fits.csv")
        write_csv(workingValues$fittedEfflux %>% 
                    select(sampleID, slope) %>% 
                    rename_("Efflux (ppm)" = "slope"),
                  "Efflux_Summary.csv")
        
        plots <- list()
        for(i in workingValues$sampledIDs){
          plots[[i]] <- paste("Plots/", i, ".jpeg", sep = "")
          ggsave(workingValues$processedPlots[[i]], filename = plots[[i]])
        }
        
        zip(file, files = c("All_Samples.csv", 
                    "Editted_Samples.csv", 
                    "Efflux_Summary.csv", 
                    "Model_Fits.csv",
                    unlist(plots)
                    ))
      }
    )
  
  output$instructions <- renderUI({
    
    if (is.null(Data())) {
      
       p("Please select either a raw *.dat file from a PP Systems EGM infrared gas analyzer or a *.csv. If selecting a *.csv please be sure your file contains one or more column that can be used as a unique sample ID.")
      
    } else {
      if (!displayConditions$idNotSelected & !displayConditions$variablesNotSelected) {
        # renderOutput({
          p("Select points to remove from the data by clicking on them. Remove multiple points by clicking and dragging a box around the points you wish to remove")
        # })
      } else{
        # renderOutput({
          p("Please select your time and concentration variables then select the unique identifier column(s) for your data")
        # })
      }
    }
  })
  
  # output$test <- renderPrint({
  #  PlotData()$Data[workingValues$keeprows[[input$ID]],]
  # })
  # # # 
  # output$test2 <- renderPrint({
  #   extension
  # })

  # Output Options ----------------------------------------------------------
  
  # outputOptions(output, 'idSelected', suspendWhenHidden = F)
  outputOptions(output, 'variablesSelected', suspendWhenHidden = F)
  outputOptions(output, 'fileUploaded', suspendWhenHidden = F)
  outputOptions(output, "plotConc", suspendWhenHidden = F)
  outputOptions(output, "showPlot", suspendWhenHidden = F)
})
