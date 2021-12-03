# LOAD PACKAGES AND DATA
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(shinythemes)
library(shinycssloaders)

nyc_dogs <- read_csv("data/nyc_dogs.csv")

# UI CODE
ui <- fluidPage(
  
  titlePanel("NYC DOGS"),
 
  navbarPage("Navigation Bar",
             tabPanel(title = "Table",
                      fluidRow(
                        column(3,
                               radioButtons("gender",
                                            tags$i("Male or Female Dogs?"), 
                                             choices = c("Male", "Female"))
                        ),
                        column(3,
                               selectInput("colour",
                                           tags$i("Which colour?"),
                                           choices = unique(nyc_dogs$colour))
                        ),
                        column(3,
                               selectInput("borough",
                                           tags$i("Which Borough?"),
                                           choices = unique(nyc_dogs$borough))
                        ),
                        
                        column(3,
                               selectInput("breed",
                                           tags$i("Which Breed?"),
                                           choices = unique(nyc_dogs$breed))
                        )
                      ),
                      
                      tableOutput("table_output")),
             
             tabPanel(title = "Plot",
                      fluidRow(
                        column(6,
                            radioButtons("gender_chart",
                                         tags$i("Male or Female Dogs?"),
                              choices = c("Male", "Female"))
                              ),
                        column(6,
                            selectInput("breed_chart",
                                        tags$i("Which Breed?"),
                              choices = unique(nyc_dogs$breed))
                                    )
                              ),
  
                      fluidRow(
                        column(6,
                            plotOutput("colour_barchart")
                              ),
                        column(6,
                            plotOutput("borough_barchart")))
                            ),
                      
            tabPanel(title = "Information",
                     p("This data has been taken from the NYC Department of health and", br(),
                       "shows the popularity of dog names in New York City by gender, colour,", br(),
                       "breed and borough."),
                     p(tags$a("The NYC Department of Health Website", href = "https://www.health.ny.gov/")))
))

# SERVER CODE
server <- function(input, output) {
  
  table_data <- reactive({
    nyc_dogs %>%
    filter(gender == input$gender) %>%
    filter(breed == input$breed) %>%
    filter(colour == input$colour) %>%
    filter(borough == input$borough)
  })
  
  output$table_output <- renderTable({
    table_data()
  })
  
  plot_data <- reactive({
    nyc_dogs %>%
      filter(gender == input$gender_chart) %>%
      filter(breed == input$breed_chart)
})
  
  output$colour_barchart <- renderPlot({
    ggplot(plot_data()) +
      geom_bar(aes(x = colour))
  })
  
  output$borough_barchart <- renderPlot({
    ggplot(plot_data()) +
      geom_bar(aes(x = borough))
  })
}

# CALL TO SHINYAPP FUNCTION
shinyApp(ui = ui, server = server)

