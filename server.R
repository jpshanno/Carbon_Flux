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
      
      if (is.null(inFile))
        return(NULL)
      
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
      # If no ID variable has been selected display the plot with all the data
      if(displayConditions$idNotSelected) {
        Data <-  
          Data()
      } else {
        Data <- 
          workingValues$workingData %>% 
          filter(sampleID == input$ID)
      }
      
      # If an ID has been selected create a trimmed dataset removing selected data
      # points. This can probably be combined with the If statement above
      if(displayConditions$idNotSelected){
        plottingData <- Data
      } else {
        plottingData <- Data[workingValues$keepRows[[input$ID]],]
      }
    
      # Fit basic linear model with only unselected rows
      Model <- 
        lm(get(input$Y) ~ get(input$X), data = plottingData)
    
      # Build plots with points and model
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
    
      # Return all function values
      info <- list(Model = Model, 
                   Data = Data, 
                   Plot = Plot)
      return(info)
  })
  
  # Translate the selected ID to an index number of the available IDs for subsetting
  IndexID <- reactive(grep(input$ID, workingValues$allIDs))
    
  

# Reactive Values ---------------------------------------------------------
# Create value that can be updated and used on the server
  
  workingValues <- 
    reactiveValues(workingData = NULL,
                   allIDs = list(),
                   sampledIDs = list(),
                   outputData = NULL,
                   keepRows = list(),
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

  

# Event Observers -----
  
  
  # Run when the ID variable is set
  observeEvent(input$UniqueID,{
      # If the ID variable is cleared or not selected reset all values. DOESN'T
      # CURRENTLY WORK
      if(displayConditions$idNotSelected){
        workingValues$workingData = NULL
        workingValues$allIDs = list()
        workingValues$sampledIDs = list()
        workingValues$keepRows = list()
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
        
        # Create a unique ID using the columns selected for ID
        workingValues$workingData <-
          Data() %>%
          unite_("sampleID", unlist(input$UniqueID), sep = "_") %>% 
          arrange_("sampleID")
        
        # Generate a string of all the sample IDs
        workingValues$allIDs <-
          workingValues$workingData %>%
          distinct(sampleID) %>%
          arrange(sampleID) %>% 
          .$sampleID 
        
        # Populate the output dataset with the full uploaded set
        workingValues$outputData <- workingValues$workingData
        
        # Generate a list of logical vectors named with each ID. This is used in
        # the plotting to indicate which rows are selected
        for(SAMPLE in workingValues$allIDs){
          workingValues$keepRows[[SAMPLE]] <- 
            rep(TRUE, 
                nrow(
                  filter(
                    workingValues$workingData, sampleID == SAMPLE)))
        }
      }
    })
  
  # Run on click on the plot
  observeEvent(input$plot_click, {
    if(displayConditions$idNotSelected) {return(NULL)}
    
    # Return a data frame of points with a column (selected_) indicating which
    # point was selected. Max points ensures that only the closest point is
    # returned
    res <- nearPoints(PlotData()$Data, 
                      input$plot_click,
                      xvar = input$X,
                      yvar = input$Y,
                      maxpoints = 1,
                      allRows = TRUE)
    
    # Compare the selected points with the keepRows vector and update keepRows
    # to be true in both cases
    workingValues$keepRows[[input$ID]] <- 
      workingValues$keepRows[[input$ID]] & !res$selected_
  })
  
  # Run on dragging a box on the plot
  observeEvent(input$plot_brush, {
    if(displayConditions$idNotSelected) {return(NULL)}
    # Return a data frame of points with a column (selected_) indicating which
    # point was selected. Max points ensures that only the closest point is
    # returned
    res <- brushedPoints(PlotData()$Data,
                         input$plot_brush, 
                         xvar = input$X,
                         yvar = input$Y,
                         allRows = TRUE)
    
    # Compare the selected points with the keepRows vector and update keepRows
    # to be true in both cases
    workingValues$keepRows[[input$ID]] <- workingValues$keepRows[[input$ID]] & !res$selected_
  })
  
  # Runs when you press Save & Next
  observeEvent(input$nextID,{
    
    # Add the value in the ID dropdown menu to list of processed samples
    workingValues$sampledIDs[[input$ID]] <- input$ID
    
    # Add the sample plot to a list of generated plots - overwrites the plot as
    # data is selected and removed
    workingValues$processedPlots[[input$ID]] <- PlotData()$Plot
    
    # Extracted values from the model and merge them into a tbl
    workingValues$fittedEfflux <- 
      bind_rows(workingValues$fittedEfflux %>% 
                  filter(sampleID != input$ID),
                data.frame(
                  sampleID = 
                    input$ID, 
                  nPointsUsed = 
                    nrow(PlotData()$Data[workingValues$keepRows[[input$ID]],]),
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
                    summary(PlotData()$Model)$fstatistic[1]))
    
    # Update the output dataset to remove selected points
    workingValues$outputData <-
      bind_rows(
      workingValues$outputData %>%
        filter(sampleID != input$ID),
      PlotData()$Data[workingValues$keepRows[[input$ID]],]) %>% 
      arrange_("sampleID")
    
    # Update the ID dropdown menu to the next sample
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID() + 1 ])
    })

  # Runs when you press Reset Probe
  observeEvent(input$undoEdit,{
    
    # Remove the current sample from the list of processed samples
    workingValues$sampledIDs[[input$ID]] <- NULL
    
    # Display all points again. keepRows is a logical that indicates with sample
    # points to keep in the data & display
    workingValues$keepRows[[input$ID]] <- rep(TRUE, 
                                              nrow(
                                                filter(
                                                  workingValues$workingData, 
                                                  sampleID == input$ID)))
    
    # Remove the processed plot for the current sample from the list of plots
    workingValues$processedPlot[[input$ID]] <- NULL
    
    # Remove the model information for the current sample
    if(!is.null(workingValues$fittedEfflux)) {
      workingValues$fittedEfflux <- 
        workingValues$fittedEfflux %>% 
        filter(sampleID != input$ID)}
    
    # Reset the output dataset to include all of the data points from the
    # current sample
    workingValues$outputData <-
      bind_rows(
        workingValues$workingData %>%
          filter(sampleID == input$ID),
        workingValues$outputData %>%
          filter(sampleID != input$ID)) %>% 
      arrange_("sampleID")
  })  
  
  # Runs when you press previous
  observeEvent(input$previousID,{
    
    # Update the ID dropdown menu to the previous sample
    updateSelectizeInput(session,
                         inputId = "ID",
                         selected = workingValues$allIDs[IndexID() - 1])
    })
  
  

# Render Output -----
  
  # Display the name of the selected
  output$filename <- renderUI({
    HTML(
      paste("<strong>Uploaded File:</strong>",
            basename(input$File$name))
      )
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
  output$edittedData <- renderDataTable({
    
    # Return nothing if no samples have been processed
    if(length(workingValues$sampledIDs) ==0) {return(NULL)}
    
    DT::datatable(
      workingValues$outputData %>% 
        filter(sampleID %in% workingValues$sampledIDs),
      options = list(paging = FALSE, searching = FALSE),
      rownames = F
    )
  })
  
  # Generate the table of data that will be downloaded
  output$outputData <- renderDataTable({
    # If no samples have been processed return the original data
    if(length(workingValues$sampledIDs) == 0){
      DT::datatable(
        Data(),
        options = list(paging = FALSE, searching = FALSE),
        rownames = F
      )
    } else {
      
      # Otherwise return the merged dataset
      DT::datatable(
        workingValues$outputData,
        options = list(paging = FALSE, searching = FALSE),
        rownames = F
      )
    }
  })

  #Generate the table of model information
  output$regressionData <- renderDataTable({
    
    # Return nothing if no samples have been processed
    if(length(workingValues$sampledIDs) ==0) {return(NULL)}
    
    DT::datatable(
      data.frame(workingValues$fittedEfflux),
      options = list(paging = FALSE, searching = FALSE),
      rownames = F
    )
    })
  
  # Show the download button after at least 1 sample has been processed
  output$downloadbutton <- renderUI({
    if(length(workingValues$sampledIDs)== 0) {return(NULL)}
      downloadButton("download",
                     label = "Download all data and plots")
  })
  
  # Download the Data
  output$download <-
    downloadHandler(
      filename = "Processed_Data.zip",
      content = function(file){
        
        # Create CSVs
        write_csv(workingValues$outputData %>% 
                    filter(sampleID %in% workingValues$sampledIDs),
                  "Editted_Samples.csv")
        write_csv(workingValues$outputData, "All_Samples.csv")
        write_csv(workingValues$fittedEfflux, "Model_Fits.csv")
        write_csv(workingValues$fittedEfflux %>% 
                    select(sampleID, slope) %>% 
                    rename(`Efflux (ppm)` = slope),
                  "Efflux_Summary.csv")

        # Create a list of plots to save
        plots <- list()
        for(i in workingValues$sampledIDs){
          plots[[i]] <- paste("Plots/", i, ".jpeg", sep = "")
          ggsave(workingValues$processedPlots[[i]], filename = plots[[i]])
        }
        
        # Place everything in a zip file
        zip(file, files = c("All_Samples.csv", 
                    "Editted_Samples.csv", 
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
             p("Please select either a raw *.dat file from a PP Systems EGM infrared gas analyzer or any *.csv. If selecting a *.csv please be sure your file contains one or more column that can be used as a unique sample ID.")
      )
    } else {
      if (!displayConditions$idNotSelected & !displayConditions$variablesNotSelected) {
        fluidRow(
          h4("Step 3: Evaluate Data and Fit Models"),
          p("Select points to remove from the data by clicking on them or remove multiple points by clicking and dragging a box around the points you wish to remove. Please keep in mind that this step is not for removing data you view as 'outliers'. Points should only be removed due to equipment/sampling errors or to remove efflux 'ramp up' at the beginning of sampling."),
          p("After you are finished with a sample press 'Save & Next' to advance to the next sample. This will also save information about the model fit and update the output data, removing the data you selected. You can view any sample using the dropdown mean or return to the previous plot with the 'Previous' button (navigating this way will not save any of your selections). Resetting the probe will delete the saved regression information and add all of the sample points back to your plot and the output data."),
          p("Once you have saved information from at least one plot the updated datset, regression information, and the final plot will all be available for download. I recommend downloading data often to avoid losing work if the app or your browser has a problem."),
          p("Downloaded data will be a zipped file containing the final plots for each sample and four CSVs.'Editted_Samples.csv' contains only the samples you editted and 'All_Samples.csv' contains your editted samples with uploaded samples you did not edit. 'Efflux_Summary.csv' contains the sample ID and the slope (assumed to be efflux in ppm), futher information about the models can be found in 'Model_Fits.csv'.")
        )
      } else{
        fluidRow(
          h4("Step 2: Set Variable & ID Columns"),
          p("If you are working with efflux data select the columns that contains sample time steps and sample concentrations. If you are working with any other data the time variable corresponds to the x-axis and the concentration variable corresponds to the y-axis."),
          p("Choose one or more columns that separates your data into unique samples.")
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
#  input$UniqueID
# })
# # # 
# output$test2 <- renderPrint({
#   extension
# })
})
