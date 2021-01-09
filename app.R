library(shiny)
library(semantic.dashboard)
library(DT)

source('ui.R', local = TRUE)
source('server.R')

shinyApp(ui, server)