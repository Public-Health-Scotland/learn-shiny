## LAURA ##

## WORK IN PROGRESS ##

## APP RUNS BUT SOME THINGS ARE GOING HORRIBLY WRONG ##

############################.
## Global ----
############################.
############################.
##Packages 

library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)


# Preparing data - not needed unless new data coming through
library(tidyr)
# 
data_asthma <- readRDS("/PHI_conf/ScotPHO/Profiles/Investigations/asthma_work/data/asthma_final.rds") %>%
    mutate(rate = round(rate, 1)) # round numbers more (one decimal place)

#data folder used
data_folder <- "/PHI_conf/ScotPHO/Profiles/Investigations/asthma_work/data/"

#Use for selection of conditions
diagnosis_list <- sort(unique(data_asthma$diagnosis))

#ScotPHO logo. 
#Needs to be https address or if local in code 64 (the latter does not work with 4.7 plotly)
scotpho_logo <-  list(source ="https://raw.githubusercontent.com/ScotPHO/plotly-charts/master/scotpho.png",
                      xref = "paper", yref = "paper",
                      x= -0.09, y= 1.2, sizex = 0.22, sizey = 0.18, opacity = 1)


############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
#Using divs as issues with classing css 
ui <- fluidPage(h4("Time trend of hospital admissions for asthma"),
                  column(6,
                        selectInput("measure", label = "Select numerator or rates",
                                    choices = c("Numerator", "Rate"), selected = "Rate")),
                        # selectInput("sex", label = "Select sex",
                        #             choices = c("Male", "Female"), selected = "Male")),
                        column(6,
                               # selectizeInput("age_grp", label = "Select age group", 
                               #         choices = c("Over 10", "Under 10", "All"), selected = "All"),
                        selectizeInput("diagnosis", label = "Select one or more diagnosis", 
                                       choices = diagnosis_list, multiple = TRUE, selected = "Status asthmaticus (J46) first position"),
                                       options = list(maxItems =4L)),
                mainPanel(width=12,
                          column(6,
                                plotlyOutput("male_all", width = "100%")),
                          column(6,
                                 plotlyOutput("female_all", width = "100%")),
                          column(6,
                                 plotlyOutput("male_under10", width = "100%")),
                          column(6,
                                 plotlyOutput("female_under10", width = "100%")),
                          column(6,
                                 plotlyOutput("male_over10", width = "100%")),
                          column(6,
                                 plotlyOutput("female_over10", width = "100%")),
                          div(style= "width:100%; float: left;", #Main panel
                              p(div(style = "width: 25%; float: left;", #Footer
                                    HTML("Source: <a href='http://www.isdscotland.org/Health-Topics/Hospital-Care/Diagnoses/'>ISD, SMR 01</a>")),
                                div(style = "width: 25%; float: left;",
                                    downloadLink('download_data', 'Download data')),
                                div(style = "width: 100%; float: left;",
                                    h6("Notes: These statistics are derived from data collected on discharges from hospitals for non-obstetric and 
    non-psychiatric hospitals (SMR01) in Scotland.", tags$br())
                                )
                              ))
                ) #main panel bracket
              ) #fluid page bracket

############################.
## Server ----
############################.
server <- function(input, output) {
  
  # Allowing user to download data
  output$download_data <- downloadHandler( 
    filename =  'asthma_final.csv', content = function(file) { 
      write.csv(data_filtered(), file, row.names=FALSE) })
  
  #reactive data
  data_filtered <- reactive({
    #Data for condition
    data_diagnosis <- data_asthma %>% subset(diagnosis %in% input$diagnosis)
  })
  
  plot_charts <- function(sex_chosen, age_grp_chosen) {
    
    data_plot <- data_asthma %>% subset(diagnosis %in% input$diagnosis & 
                                          sex == sex_chosen &
                                          age_grp == age_grp_chosen)
    
    
    #y axis title
    yaxistitle <- case_when(input$measure == "Numerator" ~ "Number of hospital admissions",
                            input$measure == "Rate" ~ "Hospital admissions <br>per 100,000 population")
    
    plot <- plot_ly(data=data_plot, x=~year, y = ~get(tolower(input$measure)), color = ~diagnosis,
                    colors = c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031'),
                    type = "scatter", mode = 'lines') %>% 
      #Layout
      layout(annotations = list(),
             yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
             xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
             font = list(family = 'Arial, sans-serif'), 
             margin = list(pad = 4, t = 50, r = 30), 
             hovermode = 'false') %>% 
      config(displayModeBar= T, displaylogo = F) 
    
  }
  
  ############################.
  #Visualization
  output$male_all <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "All") %>% layout(title = "Males All Ages")}) 
  output$female_all <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "All") %>% layout(title = "Females All Ages")}) 
  output$male_under10 <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "Under 10") %>% layout(title = "Males Under 10")})
  output$female_under10 <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "Under 10") %>% layout(title = "Females Under 10")}) 
  output$male_over10 <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "Over 10") %>% layout(title = "Males Over 10")}) 
  output$female_over10 <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "Over 10") %>% layout(title = "Females Over 10")}) 
} # end of server part


############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)