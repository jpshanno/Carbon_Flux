# Author: Joe Shannon 
# Purpose: This shiny application provides an easy interactive method to trim
# sample data and fit linear regressions to each sample's data. It is
# specifically designed for soil and stem efflux.  It will work with any type of
# data that has a unique sample ID and dependent/independent variables.


library(shiny)
library(dplyr)
library(readr)
library(tidyr)


# Page Set-up ------
shinyUI(
  fluidPage(
    theme = "efflux.css",
    title = "Interactive Sample Cleaning and Linear Modeling",
    style = "margin:2em",

        
# Header -------

    fluidRow(
      style = "margin-bottom:2em",
      column(6,
             h2(
               "Interactive Trimming and Linear Modeling"
             ),
             h4(
               "Purpose"
             ),
             HTML(
               "This shiny application provides an easy interactive method to trim sample data and fit linear regressions to each sample's data. It is specifically designed for CO<sub>2</sub> and CH<sub>4</sub> efflux.  It will work with any type of data that has a unique sample ID and dependent/independent variables."
             )
      )
    ),    

    
# Sidebar ------

    sidebarPanel(
      width = 2,
      style = "margin-top:5em",
      
      # Hide the file upload panel once a file has been chosen
      conditionalPanel("output.fileUploaded == true",
                       
                       h4("Select an Input File"),
                       p("Select a CSV or PP systems *.dat file to upload"),
                       fileInput(inputId = "File",
                                 label = NULL,
                                 accept = c('text',
                                            '.csv',
                                            '.dat'))
      ),
      
      # Once a file has been chosen show the file name and the variable/id selections
      conditionalPanel(
        "output.fileUploaded != true",
        
        uiOutput("filename"),
        
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

    
# Main Panel Set-up -----

    mainPanel(
      align = "center",
      width = 10,
      tabsetPanel(
        
# Data Processing Tab -----        


        tabPanel("Data Selection", 
                 style = "padding-top:2em; margin-left:2em",
                 fluidRow(
                   
                   # Add the interactive plot & navigation tools
                   conditionalPanel(
                     "output.showPlot == true",
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
                            ),
                            
                            br(),
                            
                            uiOutput("idSelection")
                     )
                   ),
                   
                   # Display the appropriate set up instructions
                   column(4,
                          # verbatimTextOutput("test"),
                          uiOutput("instructions", align = "left"),
                          
                          br(),
                          
                          uiOutput("downloadbutton", align = "left")
                   )
                   
                 )
        ),


# Processed Samples Tab -----     

        tabPanel("Processed Samples Only",
                 dataTableOutput("edittedData")
        ),


# Output Dataset Tab -----     

        tabPanel("Output Dataset",
                 dataTableOutput("outputData")
        ),


# Regression Information Tab -----     

        tabPanel("Regression Output",
                 dataTableOutput("regressionData")
        )
      )
    )
  )
)

