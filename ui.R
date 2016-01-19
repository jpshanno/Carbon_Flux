#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# http://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb

library(shiny)
library(dplyr)
library(readr)
library(tidyr)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    includeCSS("https://bootswatch.com/yeti/bootstrap.css"),
    # Header -------
    titlePanel(title = "Raw Efflux Processing"),
    
    # Sidebar ------
    sidebarPanel(
      width = 3,
      
      conditionalPanel("output.fileUploaded == true",
                       h4("Select an Input File"),
                       p("Select a CSV file to upload"),
                       fileInput(inputId = "inputCSV",
                                 label = NULL),
                       p("Or select a PP Systems *.dat file"),
                       fileInput(inputId = "inputTSV",
                                 label = NULL)
      ),
      
      conditionalPanel(
        "output.fileUploaded != true",
        
        h4("Time"),
        p("Select the column that contains your time steps"),
        
        selectizeInput(inputId = "X",
                       label = NULL,
                       choices = NULL,
                       multiple = F),
        
        h4("Concentrations"),
        p("Select the column that contains gas concentration"),
        
        selectizeInput(inputId = "Y",
                       label = NULL,
                       choices = NULL,
                       multiple = F),
        
        h4("Unique ID"),
        p("Select one or more columns that represent a unique ID for each sample location and event"),
        
        selectizeInput(inputId = "UniqueID",
                       label = NULL,
                       choices = NULL,
                       multiple = T)
      )     
    ),
    
    # Main Panel -----
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Data Selection", style = "padding-top:1%; margin-right:auto; margin-left:auto",
                 # verbatimTextOutput("test"),
                 uiOutput("idSelection"),
                 fluidRow(
                   column(8, plotOutput("plotConc",
                                        click = "plot_click",
                                        dblclick = "plot_dblclick",
                                        hover = "plot_hover",
                                        brush = brushOpts(id = "plot_brush",
                                                  delay = 4000,
                                                  delayType = "debounce",
                                                  resetOnNew = TRUE
                                                  )
                                        )
                          ),
                   column(4, dataTableOutput("plottedData"))
                 )
                 
        ),
        tabPanel("Uploaded Dataset",
                 dataTableOutput("uploadedData")
        ),
        tabPanel("Processed Samples Only",
                 dataTableOutput("edittedData")
        ),
        tabPanel("Output Dataset",
                 dataTableOutput("outputData")
        ),
        tabPanel("Regression Output",
                 dataTableOutput("regressionData")
        ),
        tabPanel("Processed Plots",
                 uiOutput("plots"))
      )
    )
  )
)

