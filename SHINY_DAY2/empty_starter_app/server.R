##############################################.
# SERVER ----
##############################################.

function(input, output) {
  
##############################################.
# NEW CONTENT AND FUTURE UPDATES INFO BUTTON ----
##############################################.  
  
  
  
  

  
  
  
 
  
##############################################.
# CREATE ALLERGIES CHART - PLOTLY CODE ----
##############################################. 

  # output$chart <- renderPlotly({ 
  #   
  #   #Data for condition
  #   data_condition <- data_allergy %>% subset(type %in% input$conditions & measure==input$measure)
  #   
  #   #y axis title
  #   yaxistitle <- case_when(input$measure == "Number" ~ "Number of hospital admissions",
  #                           input$measure == "Rate" ~ "Hospital admissions <br>per 100,000 population")
  #   
  #   plot <- plot_ly(data=data_condition, x=~year, y = ~value, color = ~type,
  #                   colors = c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031'),
  #                   type = "scatter", mode = 'lines',
  #                   width = 650, height = 350) %>% 
  #     #Layout
  #     layout(annotations = list(), #It needs this because of a buggy behaviour
  #            yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
  #            xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
  #            font = list(family = 'Arial, sans-serif'), #font
  #            margin = list(pad = 4, t = 50, r = 30), #margin-paddings
  #            hovermode = 'false') %>% # to get hover compare mode as default
  #     config(displayModeBar= T, displaylogo = F) # taking out plotly logo and collaborate button
  # }
  # )

  
##############################################.
# CREATE ASTHMA CHARTS - PLOTLY CODE ----
##############################################. 

  # data_filtered <- reactive({ 
  #   
  #   data_diagnosis <- data_asthma %>% subset(diagnosis %in% input$diagnosis)
  # })
  # 
  # plot_charts <- function(sex_chosen, age_grp_chosen) { 
  #                                                       
  #   data_plot <- data_asthma %>% subset(diagnosis %in% input$diagnosis & 
  #                                         sex == sex_chosen &
  #                                         age_grp == age_grp_chosen)
  #   
  #   #y axis title
  #   yaxistitle <- case_when(input$measure_asthma == "Numerator" ~ "Number of hospital admissions",
  #                           input$measure_asthma == "Rate" ~ "Hospital admissions <br>per 100,000 population")
  #   
  #   plot <- plot_ly(data=data_plot, x=~year, y = ~get(tolower(input$measure_asthma)), color = ~diagnosis,
  #                   colors = c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031'),
  #                   type = "scatter", mode = 'lines') %>% 
  #     #Layout
  #     layout(annotations = list(),
  #            yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
  #            xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
  #            font = list(family = 'Arial, sans-serif'), 
  #            margin = list(pad = 4, t = 50, r = 30), 
  #            hovermode = 'false') %>% 
  #     config(displayModeBar= T, displaylogo = F) 
  #   
  # }
  
# Here, the plot_charts function created above is put into use
  # output$male_all <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "All") %>% layout(title = "Males All Ages")}) 
  # output$female_all <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "All") %>% layout(title = "Females All Ages")}) 
  # output$male_under10 <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "Under 10") %>% layout(title = "Males Under 10")})
  # output$female_under10 <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "Under 10") %>% layout(title = "Females Under 10")}) 
  # output$male_over10 <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "Over 10") %>% layout(title = "Males Over 10")}) 
  # output$female_over10 <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "Over 10") %>% layout(title = "Females Over 10")})
  


##############################################.
# CREATE THE DATA TABLE ----
##############################################.    


  
  
  
  
  
  
  
  
  
##############################################.
# RENDER THE DATA TABLE ----
##############################################.
  
  
  
  
  
  
  
  
  
  
##############################################.
# ENABLE DATA TO BE DOWNLOADED ----
##############################################.

  
  
  
  
  
  
} # END OF SERVER