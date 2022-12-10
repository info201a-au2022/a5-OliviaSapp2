#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("tidyverse")
library("dplyr")
library("ggplot2")
library("shiny")
library("scales")
library("plotly")
source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
