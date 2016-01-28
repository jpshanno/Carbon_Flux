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
      column(8,
             h1("Efflux"),
             h3("Interactive Trimming and Linear Modeling"),
             br(),
             h4("Purpose", style = "margin-left:0.25em"),
             HTML(
               "<p style = 'margin-left:0.25em'>This shiny application provides an easy interactive method to trim sample data and fit linear regressions to each sample's data. It is specifically designed for CO<sub>2</sub> and CH<sub>4</sub> efflux.  It will work with any type of data that has a unique sample ID and dependent/independent variables. The code for this project is available on <a href = 'https://github.com/jpshanno/efflux'>GitHub</a> and is available as a function in <a href = 'https://github.com/jpshanno/ecoFlux'>the ecoFlux package</a> (devtools::install_github('jpshanno/ecoFlux')). Please contact <a href = 'mailto:josephshannon@outlook.com'>Joe Shannon</a> with any questions or problems.</p>")
      )
    ),    

    
# Sidebar ------

    sidebarPanel(
      width = 3,
      style = "margin-top:3em",
      
      # Hide the file upload panel once a file has been chosen
      conditionalPanel("output.fileUploaded == true",
                       
                       h4("Select an Input File"),
                       p("Select a CSV or PP systems *.dat file to upload"),
                       HTML("<span class = 'btn btn-default btn-file btn-info shiny-bound-input'> Select a File<input id = 'File' type='file'></span>"),
                       # fileInput(inputId = "File",
                       #           label = NULL,
                       #           accept = c('text',
                       #                      '.csv',
                       #                      '.dat')),
                       h4("See an Example Dataset"),
                       p("Example output from PP Systems EGM-4"),
                       actionButton("datExample", 
                                    label = "Load Example",
                                    class = "btn-info"),
                       p("Example CSV with multiple ID columns"),
                       actionButton("csvExample", 
                                    label = "Load Example",
                                    class = "btn-info")
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
        checkboxInput("zeroLimit",
                      label = "Set lower y-axis limit to zero?",
                      value = FALSE),
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
      width = 9,
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
                          # verbatimTextOutput("test2"),
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

