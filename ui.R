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
                       p("At this time the file must be a *.csv file"),
                       fileInput(inputId = "inputFileGas",
                                 label = NULL)
      ),
      
      conditionalPanel(
        "output.fileUploaded != true",
        
        h4("Time"),
        p("Select the column that contains your time steps"),
        
        selectizeInput(inputId = "inputX",
                       label = NULL,
                       choices = NULL,
                       multiple = F),
        
        h4("Concentrations"),
        p("Select the column that contains gas concentration"),
        
        selectizeInput(inputId = "inputY",
                       label = NULL,
                       choices = NULL,
                       multiple = F),
        
        h4("Unique ID"),
        p("Select one or more columns that represent a unique ID for each sample location and event"),
        
        selectizeInput(inputId = "inputUniqueID",
                       label = NULL,
                       choices = NULL,
                       multiple = T)
      )     
    ),
    
    # Main Panel -----
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Data Selection",
                 conditionalPanel("output.idSelected == true",
                                  style = "padding-top:1%; margin-right:auto; margin-left:auto",
                                  # verbatimTextOutput("test"),
                                  p("Clicking 'Previous' or 'Next' will cause mark the selected probe as editted"),
                                  uiOutput("idSelection")
                 ),
                 conditionalPanel("output.variablesSelected == true",
                                  fluidRow(
                                    column(8, plotOutput("plotConc")),
                                    column(4, dataTableOutput("plottedData"))
                                  )
                 )
        ),
        tabPanel("Uploaded Dataset",
                 dataTableOutput("uploadedData")
        ),
        tabPanel("Processed Samples Only",
                 dataTableOutput("edittedData")
        ),
        tabPanel("Full Dataset",
                 dataTableOutput("outputData"))
      )
    )
  )
)

