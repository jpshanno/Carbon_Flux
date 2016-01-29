# Author: Joe Shannon 
# Purpose: This shiny application provides an easy interactive method to trim
# sample data and fit linear regressions to each sample's data. It is
# specifically designed for soil and stem efflux.  It will work with any type of
# data that has a unique sample ID and dependent/independent variables.

library(shiny)
library(ggplot2)
library(Cairo)
library(DT)
options(DT.options = list(pageLength = 50))


shinyServer(function(input, output, session) {
  

# Reactives ---------------------------------------------------------------
  
  # Load selected file
  Data <- reactive({
    
    inFile <- input$File
      
      if(input$csvExample > 0) {
        return(read_csv("./www/examples/Multi-Column_ID.csv"))}
      if(input$datExample > 0) {
        return(read_tsv("./www/examples/EGM4_Output.dat", skip = 2) %>% 
                 filter(row_number() != nrow(.)))}
      
      if (is.null(inFile)){
        return(NULL)}

      filename <- as.character(inFile$name)
      extension <- substr(filename, nchar(filename)-3, nchar(filename))
      
      if(extension == ".dat"){
        read_tsv(inFile$datapath, skip = 2) %>% 
          filter(row_number() != nrow(.))
      } else {
        read_csv(inFile$datapath)
      }
    })
  
  # This reactive is the workhorse of the data processing. It selects data from 
  # only the selected sample, fits a model, and creates a plot. The data, model,
  # and plot are all returned.
  PlotData <- reactive({
    Data <- 
      data$plotting
    
    xCoord <- min(Data[[input$X]], na.rm = T)
    yDiff <- max(Data[[input$Y]], na.rm = T) - min(Data[[input$Y]], na.rm = T)
    yCoord <- max(Data[[input$Y]], na.rm = T) - 0.05*yDiff
    
    
    # Fit basic linear model with only unselected rows
    if(nrow(data$plotting) == 0){
      Model <- NULL
    } else {
      Model <- lm(get(input$Y) ~ get(input$X), data = Data)}
    
    # Build plots with points and model
    if(nrow(data$plotting) != 0){
      Plot <- 
        ggplot(data = Data,
               aes(x = get(input$X),
                   y = get(input$Y))) +
        xlab(input$X) +
        ylab(input$Y) +
        geom_point(color = "darkgray") +
        geom_abline(slope = coef(Model)[2],
                    intercept = coef(Model)[1],
                    color = "black") +
        annotate("text", 
                 label = paste("Slope =", 
                               round(coef(Model)[2],2), 
                               sep = " "), 
                 x = xCoord, 
                 y = yCoord, 
                 hjust = "inward") +
        theme_bw()
      if(input$zeroLimit == T){
        upperY <- ggplot_build(Plot)$panel$ranges[[1]]$y.range[2]
        Plot <- Plot + coord_cartesian(ylim = c(0, upperY))
      }
    } else {
      Plot <- 
        ggplot(aes(x = get(input$X),
                   y = get(input$Y)),
               data =  Data) +
        xlab(input$X) +
        ylab(input$Y) +
        geom_blank()
    }
    
    # Return all function values
    info <- list(Model = Model, 
                 Data = Data, 
                 Plot = Plot)
    return(info)
  })
  
  # Translate the selected ID to an index number of the available IDs for subsetting
  # IndexID <- reactive(grep(input$ID, workingValues$allIDs))
    
  

# Reactive Values ---------------------------------------------------------
# Create value that can be updated and used on the server
  # 
  # workingValues <- 
  #   reactiveValues(workingData = NULL,
  #                  allIDs = list(),
  #                  sampledIDs = list(),
  #                  outputData = NULL,
  #                  keepRows = list(),
  #                  fittedEfflux = tbl_df(data.frame(
  #                    sampleID = character(), 
  #                    nPointsUsed = numeric(),
  #                    intercept = numeric(), 
  #                    slope = numeric(), 
  #                    adjustedRSquared = numeric(), 
  #                    residualStandardError = numeric(), 
  #                    degreesFreedom = numeric(), 
  #                    fStatistic = numeric())),
  #                  processedPlots = list())
  
  displayConditions <-
    reactiveValues(
      idNotSelected = TRUE,
      variablesNotSelected = TRUE)

  data <- reactiveValues(
    input = NULL,

    editted = NULL,
      
    plotting = 
      NULL,
    regressionInfo = 
      tbl_df(data.frame(
        sampleID = character(), 
        nPointsUsed = numeric(),
        intercept = numeric(), 
        slope = numeric(), 
        adjustedRSquared = numeric(), 
        residualStandardError = numeric(), 
        degreesFreedom = numeric(), 
        fStatistic = numeric())),
    plots =
      list()
  )
  
  
  IDs <- reactiveValues(
    all = character(),
    processed = list()
  ) 
  
  sample <- reactiveValues(
    index = 1,
    name = character()
  )
  

# General Observers ------
  
  # Populate the drop down menus with the column names from the uploaded data
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
  
  # Flag whether or not variables have been selected
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

  
  # Set working values when ID is selcted
  observe({
    if(displayConditions$idNotSelected){return(NULL)}
      # Create a unique ID using the columns selected for ID
      data$input <-
        Data() %>%
        unite_("sampleID", unlist(input$UniqueID), sep = "_") %>% 
        arrange_("sampleID")
      
      data$editted <-
        Data() %>%
        unite_("sampleID", unlist(input$UniqueID), sep = "_") %>%
        slice(0)
      
      # Generate a string of all the sample IDs
      IDs$all <- 
        data$input  %>%
        distinct(sampleID) %>%
        arrange(sampleID) %>% 
        .$sampleID
  })
  
  observe({
    if(displayConditions$idNotSelected){return(NULL)}
    sample$name <- IDs$all[sample$index]})    
  

      # Generate a list of logical vectors named with each ID. This is used in
      # the plotting to indicate which rows are selected
      # for(SAMPLE in workingValues$allIDs){
      #   workingValues$keepRows[[SAMPLE]] <- 
      #     rep(TRUE, 
      #         nrow(
      #           filter(
      #             workingValues$workingData, sampleID == SAMPLE)))
      # }

  observe({
    if(displayConditions$idNotSelected){return(NULL)}
      if(sample$name %in% IDs$processed){
        data$plotting <- 
          data$editted %>% 
          filter(sampleID == sample$name)
      } else {
        data$plotting <- 
          data$input %>% 
          filter(sampleID == sample$name)
      }
  })

  

# Event Observers -----
  
  # Run on click on the plot
  observeEvent(input$plot_click, {
    data$plotting <- anti_join(data$plotting, 
                               nearPoints(PlotData()$Data, 
                                          input$plot_click,
                                          xvar = input$X,
                                          yvar = input$Y,
                                          maxpoints = 1))
    # if(nrow(data$plotting) == 0){sample$index <- sample$index + 1}
  })
  
  # Run on dragging a box on the plot Return a data frame of points with a
  # column (selected_) indicating which point was selected. Max points ensures
  # that only the closest point is returned
  observeEvent(input$plot_brush, {
    data$plotting <- anti_join(data$plotting, 
                               brushedPoints(PlotData()$Data,
                                             input$plot_brush, 
                                             xvar = input$X,
                                             yvar = input$Y))
    # if(nrow(data$plotting) == 0){sample$index <- sample$index + 1}
  })
  
  observeEvent(input$plot_dblclick, {
                      data$plotting <- 
                        filter(data$plotting, is.na(sampleID))
  })
  
  # Runs when you press Save & Next
  observeEvent(input$nextID,{
    
    # Add the value in the ID dropdown menu to list of processed samples
    IDs$processed[[sample$name]] <- sample$name
    
    # Add the sample plot to a list of generated plots - overwrites the plot as
    # data is selected and removed
    data$plots[[sample$name]] <- PlotData()$Plot
    
    # Extracted values from the model and merge them into a tbl
    if(!is.null(PlotData()$Model)){
      data$regressionInfo <-
        bind_rows(data$regressionInfo %>%
                           filter(sampleID != sample$name),
                         data.frame(
                           sampleID =
                             sample$name,
                           nPointsUsed =
                             nrow(PlotData()$Data),
                           intercept =
                             coef(PlotData()$Model)[1],
                           slope =
                             coef(PlotData()$Model)[2],
                           adjustedRSquared =
                             summary(PlotData()$Model)$adj.r.squared,
                           residualStandardError =
                             summary(PlotData()$Model)$sigma,
                           degreesFreedom =
                             summary(PlotData()$Model)$fstatistic[3],
                           fStatistic =
                             summary(PlotData()$Model)$fstatistic[1]))}
    
    data$editted <- 
      bind_rows(
        filter(data$editted, sampleID != sample$name),
        data$plotting) %>% 
      arrange(sampleID)
    
    if(sample$index != length(IDs$all)) {sample$index <- sample$index + 1}
    
    # Update the ID dropdown menu to the next sample
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = sample$name)
  })

  # Runs when you press Reset Probe
  observeEvent(input$undoEdit,{
    
    # Remove the current sample from the list of processed samples
    IDs$processed[[sample$name]] <- NULL
    
    # Display all points again. keepRows is a logical that indicates with sample
    # points to keep in the data & display
    data$plotting <- 
      data$input %>% 
      filter(sampleID == sample$name)
    
    # Remove the processed plot for the current sample from the list of plots
    data$plots[[sample$name]] <- NULL
    
    # Remove the model information for the current sample
    if(nrow(data$regressionInfo)) {
      data$regressionInfo <- 
        data$regressionInfo %>% 
        filter(sampleID != sample$name)}
    
    # Remove the data ponits from the current probe from the editted dataset
    data$editted <-
      filter(data$editted, sampleID != sample$name)
  })  
  
  # Runs when you press previous
  observeEvent(input$previousID,{
    if(sample$index != 1) {sample$index <- sample$index - 1}
    # Update the ID dropdown menu to the previous sample
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = sample$name)
    })
  
  

# Render Output -----
  
  # Display the name of the selected
  output$filename <- renderUI({
    if(!is.null(input$File)){
    return(HTML(
      paste("<strong>Uploaded File:</strong>",
            basename(input$File$name))
    ))}
    if(input$datExample > 0){
      return(HTML("<strong>Uploaded File:</strong> EGM-4 Example"))}
    if(input$csvExample > 0){
      return(HTML("<strong>Uploaded File:</strong> CSV Example"))}
  })
  
  # Create the plot, do not attempt to run if variables are not set
  output$plotConc <- renderPlot({
    
    if(displayConditions$variablesNotSelected) {return(NULL)}

    PlotData()$Plot
    
  }, height = 600)
  
  # Generate the plot controls
  output$idSelection <- renderUI({
    if(length(input$UniqueID) == 0) {return(NULL)}
      column(width = 12,
             fluidRow(
               actionButton("previousID",
                            label = "Previous",
                            width = "30%",
                            class = "btn-info"),
               actionButton("undoEdit",
                            label = "Reset Probe",
                            width = "30%",
                            class = "btn-info"),
               actionButton("nextID",
                            label = "Save & Next",
                            width = "30%",
                            class = "btn-info")
             ),
             br(),
             fluidRow(
               selectizeInput("ID",
                              label = NULL,
                              choices = IDs$all,
                              multiple = F)
               )
      )
  })

  # Generate the table of plotted data
  # output$plottedData <- renderDataTable({
  #   
  #   #Don't run if variables and ID aren't selected
  #   if(displayConditions$variablesNotSelected || 
  #      displayConditions$idNotSelected) {return(NULL)}
  #   DT::datatable(
  #     PlotData()$Data[workingValues$keepRows[[input$ID]],] %>%
  #       select_("sampleID",
  #               paste("`", input$X, "`", sep = ""),
  #               paste("`", input$Y, "`", sep = "")),
  #     options = list(paging = FALSE, searching = FALSE),
  #     rownames = FALSE
  #   )
  # })
  
  # Generate the table of only editted table
  output$edittedData <- DT::renderDataTable({
    if(length(IDs$processed) ==0) {return(NULL)}
    DT::datatable(
      data$editted,
      options = list(paging = FALSE, searching = FALSE),
      rownames = F)
  })
  
  # Generate the table of data that will be downloaded
  output$outputData <- DT::renderDataTable({
    DT::datatable(
      bind_rows(filter(data$input, !(sampleID %in% IDs$processed)),
                       filter(data$editted, sampleID %in% IDs$processed)),
      options = list(paging = FALSE, searching = FALSE),
      rownames = F)
  })

  #Generate the table of model information
  output$regressionData <- DT::renderDataTable({
    if(length(IDs$processed) ==0) {return(NULL)}
    # DT::datatable(
    data$regressionInfo#,
    # options = list(paging = FALSE, searching = FALSE),
    # rownames = F)
  })
  
  # Show the download button after at least 1 sample has been processed
  output$downloadbutton <- renderUI({
    if(length(IDs$processed)== 0) {return(NULL)}
      downloadButton("download",
                     label = "Download all data and plots",
                     class = "btn-info")
  })
  
  # Download the Data
  output$download <-
    downloadHandler(
      filename = "Processed_Data.zip",
      content = function(file){
        
        # Create CSVs
        write_csv(data$editted,
                  "Processed_Samples.csv")
        write_csv(data$regressionInfo, 
                  "Model_Fits.csv")
        write_csv(anti_join(data$input, data$editted) %>% 
                    filter(sampleID %in% IDs$processed),
                  "Removed_Points.csv")
        write_csv(data$input %>%
                    filter(!(sampleID %in% IDs$processed)),
                  "Unprocessed_Samples.csv")
        write_csv(data$regressionInfo %>% 
                    select(sampleID, slope) %>% 
                    rename(`Efflux (ppm)` = slope),
                  "Efflux_Summary.csv")

        # Create a list of plots to save
        plots <- list()
        dir.create("Plots")
        for(i in names(data$plots)){
          plots[[i]] <- paste("Plots/", i, ".jpeg", sep = "")
          ggsave(data$plots[[i]], filename = plots[[i]])
        }
        
        # Place everything in a zip file
        zip(file, files = c("Processed_Samples.csv", 
                            "Unprocessed_Samples.csv",
                            "Removed_Points.csv",
                            "Efflux_Summary.csv", 
                            "Model_Fits.csv",
                            unlist(plots)
        ))
      }
    )
  
  # Instructions that vary based on where the user is in the process
  output$instructions <- renderUI({
    
    if (is.null(Data())) {
      fluidRow(
             h4("Step 1: File Upload"),
             p("Please select either a raw *.dat file from a PP Systems EGM infrared gas analyzer or any *.csv. If selecting a *.csv please be sure your file contains one or more column that can be used as a unique sample ID."),
             br(),
             p("Two example datasets are available. The first is raw output from a PP Systems EGM-4. The second respresents a full season of sampling where each unique sample is represented by its treatment, collar number, month of sample, and day of sample.")
      )
    } else {
      if (!displayConditions$idNotSelected & !displayConditions$variablesNotSelected) {
        fluidRow(
          h4("Step 3: Evaluate Data and Fit Models"),
          p("Select points to remove from the data by clicking on them or remove multiple points by clicking and dragging a box around the points you wish to remove. Please keep in mind that this step is not for removing data you view as 'outliers'. Points should only be removed due to equipment/sampling errors or to remove efflux 'ramp up' at the beginning of sampling."),
          p("After you are finished with a sample press 'Save & Next' to advance to the next sample. This will also save information about the model fit and update the output data, removing the data you selected. You can view any sample using the dropdown mean or return to the previous plot with the 'Previous' button (navigating this way will not save any of your selections). Resetting the probe will delete the saved regression information and add all of the sample points back to your plot and the output data."),
          p("Once you have saved information from at least one plot the updated datset, regression information, and the final plot will all be available for download. I recommend downloading data often to avoid losing work if the app or your browser has a problem."),
          p("Downloaded data will be a zipped file containing the final plots for each sample and five CSVs. 'Processed_Samples.csv' and 'Unprocessed_Samples.csv' contain the samples you viewed and saved and the samples that were not saved, respecitvely. 'Removed_Points.csv' contains all of the data points that you removed. 'Efflux_Summary.csv' contains the sample ID and the slope (assumed to be efflux in ppm), futher information about the models can be found in 'Model_Fits.csv'.")
        )
      } else{
        fluidRow(
          h4("Step 2: Set Variable & ID Columns"),
          p("If you are working with efflux data select the columns that contains sample time steps and sample concentrations. If you are working with any other data the time variable corresponds to the x-axis and the concentration variable corresponds to the y-axis."),
          p("Choose one or more columns that separates your data into unique samples. Changing the ID variables will delete all processed data")
          )
      }
    }
  })
  


# Conditional Panel Controls --------------------------------------------------------
  
  output$fileUploaded <- reactive({
    return(is.null(Data()))
  })

  output$showPlot <- reactive({
    return(!displayConditions$variablesNotSelected & !displayConditions$idNotSelected)
  })

  # Set output options to keep them running in the background
  outputOptions(output, 'fileUploaded', suspendWhenHidden = F)
  outputOptions(output, "showPlot", suspendWhenHidden = F)


# Testing Tools -----

# output$test <- renderPrint({
#  input$csvExample
# })
# # # 
# output$test2 <- renderPrint({
#   extension
# })
})
