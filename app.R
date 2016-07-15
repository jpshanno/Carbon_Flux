library(ecoFlux)
library(DT)
library(devtools)
options(DT.options = list(pageLength = 50))

app <- efflux(run = F)
shinyApp(ui = app$ui, server = app$server)
