# SERVER


function(input, output) {
  
# NEW CONTENT AND FUTURE UPDATES INFO BUTTON ----

  
# CREATE ALLERGIES CHART - PLOTLY CODE ----

  #output$chart <- renderPlotly({ 
  #  
  #  #Data for condition
  #  data_condition <- 
  #  
  #  #y axis title
  #  yaxistitle <- case_when(input$measure == "Number" ~ "Number of hospital admissions",
  #                          input$measure == "Rate" ~ "Hospital admissions <br>per 100,000 population")
  #  
  #  plot <- 
  #    
  #    
  #    
  #  
  #  ggplotly(height = 500,
  #           width = 1000) %>%
  #    layout(annotations = list(), #It needs this because of a buggy behaviour
  #           yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
  #           xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
  #           font = list(family = 'Arial, sans-serif'), # font
  #           margin = list(pad = 4, t = 50, r = 30), # margin-paddings
  #           hovermode = 'false') %>% # to get hover compare mode as default
  #    config(displayModeBar= T, displaylogo = F)
  #  #Layout
  #  # taking out plotly logo and collaborate button
  #}
  #)

# CREATE ASTHMA CHARTS - PLOTLY CODE ----


  #plot_charts <- function(sex_chosen, age_grp_chosen) { 
  #  
  #  data_plot <- 
  #  
  #  #y axis title
  #  yaxistitle <- 
  #                          
  #  
  #  plot <- 
  #    
  #    
  #    
  #  
  #  ggplotly() %>%
  #    layout(annotations = list(),
  #           yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
  #           xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
  #           font = list(family = 'Arial, sans-serif'), 
  #           margin = list(pad = 4, t = 50, r = 30), 
  #           hovermode = 'false') %>% 
  #    config(displayModeBar= T, displaylogo = F) 
  #  
  #}
  #
  ## Here, the plot_charts function created above is put into use
  #output$male_all <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "All") %>% layout(title = "Males All Ages")}) 
  #output$female_all <- 
  #output$male_under10 <- 
  #output$female_under10 <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "Under 10") %>% layout(title = "Females Under 10")}) 
  #output$male_over10 <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "Over 10") %>% layout(title = "Males Over 10")}) 
  #output$female_over10 <- 
  


# CREATE THE DATA TABLE ----


# RENDER THE DATA TABLE ----
  
  
# ENABLE DATA TO BE DOWNLOADED ----

  
} # END OF SERVER