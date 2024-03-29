# GLOBAL
# Shiny training day 
# Create a PHS Shiny dashboard

# LOAD PACKAGES ----

library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)
library(shinyWidgets)
library(tidyr)
library(magrittr)
library(readr)

# LOAD DATA ----

data_allergy <- readRDS("data/allergy_scotland_chart_PRA.rds")

data_asthma <- readRDS("data/asthma_final.rds") %>%
  mutate(rate = round(rate, 1))

# CREATE OBJECTS USED IN OUTPUTS ----

condition_list <- sort(unique(data_allergy$type)) # allergies
diagnosis_list <- sort(unique(data_asthma$diagnosis)) # asthma
data_list_data_tab <- c("Allergies Data" = "data_allergy", "Asthma Data" = "data_asthma")







