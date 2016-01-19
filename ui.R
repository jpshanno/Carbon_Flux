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
    includeCSS("http://bootswatch.com/yeti/bootstrap.css"),
    # Header -------
    titlePanel(title = "Raw Efflux Processing"),
    
    # Sidebar ------
    sidebarPanel(
      width = 2,
      
      conditionalPanel("output.fileUploaded == true",
                       h4("Select an Input File"),
                       p("Select a CSV or PP systems *.dat file to upload"),
                       fileInput(inputId = "File",
                                 label = NULL,
                                 accept = c('.csv',
                                            '.dat'))
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
    mainPanel(align = "center",
      width = 10,
      tabsetPanel(
        tabPanel("Data Selection", style = "padding-top:5%; margin-right:auto; margin-left:auto",
                 # verbatimTextOutput("test"),
                 # verbatimTextOutput("test2"),
                 # 
                 fluidRow(
                   column(4,
                          # h3("Instructions", align = "left"),
                          uiOutput("instructions", align = "left"),
                          uiOutput("idSelection"),
                          br(),
                          uiOutput("downloadbutton", width = "100%")
                          
                   ),
                   column(8,
                          plotOutput("plotConc",
                                     click = "plot_click",
                                     dblclick = "plot_dblclick",
                                     hover = "plot_hover",
                                     brush = brushOpts(id = "plot_brush",
                                                       delay = 4000,
                                                       delayType = "debounce",
                                                       resetOnNew = TRUE
                                     ),
                                     height = "100%"
                          )
                          # dataTableOutput("plottedData")
                          )
                 )
        ),
        # tabPanel("Uploaded Dataset",
        #          dataTableOutput("uploadedData")
        # ),
        tabPanel("Processed Samples Only",
                 dataTableOutput("edittedData")
        ),
        tabPanel("Output Dataset",
                 dataTableOutput("outputData")
        ),
        tabPanel("Regression Output",
                 dataTableOutput("regressionData")
        )
      )
    )
  )
)

