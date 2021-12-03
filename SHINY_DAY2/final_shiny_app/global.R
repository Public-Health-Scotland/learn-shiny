##############################################.
# GLOBAL ----
##############################################.

# the global script is useful for loading packages, data, and creating objects from the data which
# might be used later in creation of charts or tables where we want specific drop down options

##############################################.
# LOAD PACKAGES ----
##############################################.
library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)
library(shinyWidgets)
library(tidyr)
library(magrittr)

##############################################.
# LOAD DATA ----
##############################################.
data_allergy <- readRDS("data/allergy_scotland_chart_PRA.rds")

data_asthma <- readRDS("data/asthma_final.rds") %>%
  mutate(rate = round(rate, 1))

##############################################.
# CREATE OBJECTS USED IN OUTPUTS ----
##############################################.
condition_list <- sort(unique(data_allergy$type)) # allergies
diagnosis_list <- sort(unique(data_asthma$diagnosis)) # asthma
data_list_data_tab <- c("Allergies Data" = "data_allergy", "Asthma Data" = "data_asthma")







