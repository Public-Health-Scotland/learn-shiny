# SERVER


function(input, output) {
  
# NEW CONTENT AND FUTURE UPDATES INFO BUTTON ----
  
  observeEvent(input$new_next,
               showModal(modalDialog( # creats a modal: a pop-up box that contains text information
                 title = "New content added and future updates",
                 h4("Future updates"),
                 tags$ul(
                   tags$li("7 July 2023 - Allergies data update."),
                   tags$li("16 June 2025 - Asthma data update.")),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))) # creates the esc button for closing the popup box

  
# CREATE ALLERGIES CHART - PLOTLY CODE ----

  output$chart <- renderPlotly({ 
    
    #Data for condition
    data_condition <- data_allergy %>% 
      subset(type %in% input$conditions & measure==input$measure)
    
    #y axis title
    yaxistitle <- case_when(input$measure == "Number" ~ "Number of hospital admissions",
                            input$measure == "Rate" ~ "Hospital admissions <br>per 100,000 population")
    
    plot <- 
      ggplot(data_condition, aes(x = year, y = value, colour = type)) +
      geom_line(aes(group=1)) +
      theme_minimal()
    
    ggplotly(height = 500,
             width = 1000) %>%
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
             xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
             font = list(family = 'Arial, sans-serif'), # font
             margin = list(pad = 4, t = 50, r = 30), # margin-paddings
             hovermode = 'false') %>% # to get hover compare mode as default
      config(displayModeBar= T, displaylogo = F) # taking out plotly logo and collaborate button
  }
  )

# CREATE ASTHMA CHARTS - PLOTLY CODE ----


  plot_charts <- function(sex_chosen, age_grp_chosen) { 
    
    data_plot <- data_asthma %>% subset(diagnosis %in% input$diagnosis & 
                                          sex == sex_chosen &
                                          age_grp == age_grp_chosen)
    
    #y axis title
    yaxistitle <- case_when(input$measure_asthma == "Numerator" ~ "Number of hospital admissions",
                            input$measure_asthma == "Rate" ~ "Hospital admissions <br>per 100,000 population")
    
    plot <- 
      ggplot(data_plot, aes(x = year, y = get(tolower(input$measure_asthma)), colour = diagnosis)) +
      geom_line(aes(group=1)) +
      theme_minimal()
    
    ggplotly() %>%
      layout(annotations = list(),
             yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
             xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
             font = list(family = 'Arial, sans-serif'), 
             margin = list(pad = 4, t = 50, r = 30), 
             hovermode = 'false') %>% 
      config(displayModeBar= T, displaylogo = F) 
    
  }
  
  # Here, the plot_charts function created above is put into use
  output$male_all <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "All") %>% 
      layout(title = "Males All Ages")}) 
  output$female_all <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "All") %>% 
      layout(title = "Females All Ages")}) 
  output$male_under10 <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "Under 10") %>% 
      layout(title = "Males Under 10")})
  output$female_under10 <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "Under 10") %>% 
      layout(title = "Females Under 10")}) 
  output$male_over10 <- renderPlotly({ plot_charts(sex_chosen = "Male", age_grp_chosen = "Over 10") %>% 
      layout(title = "Males Over 10")}) 
  output$female_over10 <- renderPlotly({ plot_charts(sex_chosen = "Female", age_grp_chosen = "Over 10") %>% 
      layout(title = "Females Over 10")})

# CREATE THE DATA TABLE ----
  
  data_table <- reactive({
    # Change dataset depending on what user selected
    table_data <- switch(input$data_select,
                         "data_allergy" = data_allergy, 
                         "data_asthma" = data_asthma)
    
    if (input$data_select == "data_allergy") { 
      table_data %<>%
        select(year, type, measure, value)
    } else if (input$data_select == "data_asthma") { 
      table_data %<>%
        select(diagnosis, year, sex, age_grp, numerator, rate)
    }
  })

# RENDER THE DATA TABLE ----
  
  output$table_filtered <- DT::renderDataTable({
    
    # Remove the underscore from column names in the table
    table_colnames  <-  gsub("_", " ", colnames(data_table()))
    
    DT::datatable(data_table(), 
                  style = 'bootstrap',
                  class = 'table-bordered table-condensed',
                  rownames = FALSE,
                  options = list(pageLength = 20,
                                 dom = 'tip',
                                 autoWidth = TRUE),
                  filter = "top",
                  colnames = table_colnames)
  })
  
# ENABLE DATA TO BE DOWNLOADED ----
  output$download_table_csv <- downloadHandler(
    filename = function() {
      paste(input$data_select, ".csv", sep = "")
    },
    content = function(file) {
      # This downloads only the data the user has selected using the table filters
      write_csv(data_table()[input[["table_filtered_rows_all"]], ], file) 
    } 
  )
  
} # END OF SERVER