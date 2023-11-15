library(shiny)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)

nyc_dogs <- read_csv("data/nyc_dogs.csv")

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel(tags$h1("NYC DOGS")),
  
  navbarPage("Navigation Bar",
             tabPanel(title = "Table",
                      icon = icon("table"),
                      fluidRow(
                        column(3, 
                               radioButtons(inputId = 'gender',
                                            label = "Male or female dogs?",
                                            choices = c("Male", "Female"))
                               ), # end column
                        column(3, 
                               selectInput(inputId = "colour",
                                           label = "Which colour?",
                                           choices = unique(nyc_dogs$colour))
                               ), # end column
                        column(3,
                               selectInput(inputId = "borough",
                                           label = "Which borough?",
                                           choices = unique(nyc_dogs$borough))
                               ), # end column
                        column(3,
                               selectInput(inputId = "breed",
                                           label = "Which breed?",
                                           choices = unique(nyc_dogs$breed))
                               ) # end column
                        ), # end fluidRow
                      
                      tableOutput("table_output")), # end tabPanel
             
             tabPanel(title = "Plot",
                      icon = icon("chart-area"),
                      fluidRow(
                        column(6,
                               radioButtons(inputId = "gender_chart",
                                            label = tags$i("Male or Female Dogs?"),
                                            choices = c("Male", "Female"))
                        ), # end column
                        column(6,
                               selectInput(inputId = "breed_chart",
                                           label = tags$i("Which Breed?"),
                                           choices = unique(nyc_dogs$breed))
                        ) # end column
                      ), # end fluidRow
                      
                      fluidRow(
                        column(6,
                               plotOutput("colour_barchart")
                        ), # end column
                        column(6,
                               plotOutput("borough_barchart"))
                        ) # end fluidRow
                      ), # end tabPanel
             tabPanel(title = "Information",
                      icon = icon("circle-info"),
                      p("This data has been taken from the NYC Department of health and", br(),
                        "shows the popularity of dog names in New York City by gender, colour,", br(),
                        "breed and borough."),
                      p(tags$a("The NYC Department of Health Website", 
                               href = "https://www.health.ny.gov/"))) # end of tabPanel
             ) # end navbarPage
  ) # end fluidPage
             
server <- function(input, output) {
  
  table_data <- reactive({
    nyc_dogs %>%
      filter(gender == input$gender) %>%
      filter(breed ==  input$breed) %>%
      filter(colour ==  input$colour) %>%
      filter(borough ==  input$borough)
  })
  
  output$table_output <- renderTable({
    table_data()
  })
  
  plot_data <- reactive({
    nyc_dogs %>%
      filter(gender == input$gender_chart) %>%
      filter(breed ==  input$breed_chart)
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

# Run the application 
shinyApp(ui = ui, server = server)


