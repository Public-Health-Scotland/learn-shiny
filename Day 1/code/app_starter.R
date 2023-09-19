# LOAD PACKAGES AND DATA
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(shinythemes)
library(shinycssloaders)

# UI CODE
ui <- fluidPage()

# SERVER CODE
server <- function(input, output) {
}

# CALL TO SHINYAPP FUNCTION
shinyApp(ui = ui, server = server)
