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
library(lazyeval)


shinyServer(function(input, output, session) {
  
  # Reactives ---------------------------------------------------------------
  
  Data <- reactive({
    inFile <- input$inputFileGas
    
    if (is.null(inFile))
      return(NULL)
    
    read_csv(inFile$datapath)
  })
  
  PlotData <- reactive({
    if(is.null(input$inputUniqueID) || input$inputUniqueID == "") {
      return(Data())}
    
    NewData() %>% 
      filter(sampleID == input$ID)
  })
  
  IDs <- eventReactive(
    input$inputUniqueID,{
      NewData() %>% 
        distinct(sampleID) %>% 
        .$sampleID
    }
  )
  
  IndexID <- reactive(grep(input$ID, IDs()))
  
  # Output Variables --------------------------------------------------------
  
  output$fileUploaded <- reactive({
    return(is.null(Data()))
  })
  
  
  output$variablesSelected <- reactive({
    check <- input$inputX != "" & input$inputY != ""
    return(check)
  })
  
  
  output$idSelected <- reactive({
    return(length(input$inputUniqueID) != 0)
  })
  
  NewData <- 
    reactive({
      Data() %>% 
        unite_("sampleID", unlist(input$inputUniqueID), sep = "-")
    })
  
  edittedSamples <- reactiveValues(ID = list())

  
  # Observers ---------------------------------------------------------------
  observe({
    updateSelectizeInput(session,
                         inputId = "inputUniqueID",
                         choices = c(Choose = "", names(Data())))
    updateSelectizeInput(session,
                         inputId = "inputY",
                         choices = c(Choose = "", names(Data())[sapply(Data(),is.numeric)]))
    updateSelectizeInput(session,
                         inputId = "inputX",
                         choices = c(Choose = "", names(Data())[sapply(Data(),is.numeric)]))
  })
  
  observeEvent(input$ID,{
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = IDs()[IndexID()])})
  
  observeEvent(input$nextID,{
    edittedSamples$ID[[IDs()[IndexID()]]] <- IDs()[IndexID()]
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = IDs()[IndexID() + 1 ])})
  
  observeEvent(input$previousID,{
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = IDs()[IndexID() - 1])})
  
  observeEvent(input$undoEdit,{
    edittedSamples$ID[[IDs()[IndexID()]]] <- NULL
  })
  
  # Renders -----------------------------------------------------------------
  
  output$plotConc <- renderPlot({
    
    Y <- PlotData()[[input$inputY]]
    X <- PlotData()[[input$inputX]]
    
    plot(Y ~ X, 
         col = 'darkgray', 
         #border = 'white', 
         xlab = input$inputX,
         ylab = input$inputY)
    
  }, height = 600)
  
  output$idSelection <- renderUI({
    fluidRow(
      selectizeInput("ID",
                     label = NULL,
                     choices = IDs(),
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
    if(is.null(input$inputUniqueID) || input$inputUniqueID == "") {return(NULL)}
    PlotData() %>% 
      select_("sampleID", 
              paste("`", input$inputX, "`", sep = ""), 
              paste("`", input$inputY, "`", sep = ""))
  })
  
  # output$test <- renderPrint(edittedSamples$ID)
  
  output$edittedData <- renderDataTable({
    #if(length(edittedSamples) == 0) {return(NULL)}
    NewData() %>% 
      filter(sampleID %in% unlist(edittedSamples$ID))
  })
  
  output$outputData <- renderDataTable({
    if(length(edittedSamples) == 0) {Data()}
  })
  
  # Output Options ----------------------------------------------------------
  
  outputOptions(output, 'idSelected', suspendWhenHidden = F)
  outputOptions(output, 'variablesSelected', suspendWhenHidden = F)
  outputOptions(output, 'fileUploaded', suspendWhenHidden = F)
  outputOptions(output, "plotConc", suspendWhenHidden = F)
  
})
