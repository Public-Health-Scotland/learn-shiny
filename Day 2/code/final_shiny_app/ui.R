##############################################.
# UI - USER INTERFACE ----
##############################################.

  navbarPage(title = div(tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"), #bringing in PHS logo and look for dashboard
                                href= "https://www.publichealthscotland.scot/",
                                target = "_blank"),
                         style = "position: relative; top: -10px;"), 
             header = tags$head(includeCSS("www/styles.css"), # CSS styles
                                HTML("<html lang='en'>"),
                                tags$link(rel="shortcut icon", href="favicon_phs.ico")), 

##############################################.
# INFORMATION TAB ----
##############################################.
             tabPanel(title = "Information", icon = icon("info-circle"),
                      fluidRow(
                        column(6,
                             h2("Background Information")),
                        column(6, actionButton("new_next", tags$b("New content and future updates"),
                                             icon = icon('calendar-alt')))
                      ),
                      fluidRow(
                        column(12,
                          h4(tags$b("Allergic Conditions")),
                          p("Allergic conditions arise from an abnormal reaction of the immune system to a typically harmless environmental trigger. 
                           Allergic conditions have a wide variety of impacts on health, ranging from those that generally cause only minor symptoms, 
                           such as hay fever or conjunctivitis, to those that may be chronic and disabling, such as asthma, eczema or urticaria (hives)."), 
                           p("Some allergic conditions may be severe and life-threatening, such as anaphylaxis, although this is uncommon. 
                            Most allergic conditions are treated in primary care; only a minority of people require hospital referral."),
                          h4(tags$b("Asthma")),
                          p("Asthma is a chronic disease of the small airways in the lung. Airway inflammation and associated bronchoconstriction leads to 
                            recurrent attacks of cough, wheezing, breathlessness or chest tightness. The severity and frequency of these episodes varies from 
                            occasional slight wheezing to severe or sometimes, although rarely, life-threatening attacks."),
                          p("The underlying causes of asthma are not fully understood, but attacks are likely caused by an interaction between a susceptible host 
                            and environmental triggers. Environmental triggers may include viral upper respiratory tract infections, common allergens such as pollen, 
                            dust mites and pet dander, or more general exposures such as cold air or physical exercise. Asthma is more common in those with a family 
                            history of asthma or eczema and is also more common among children whose parents smoke. It also often co-exists with hay fever.")))
                      ),

##############################################.
# ALLERGIC CONDITIONS TAB ----
##############################################.
             tabPanel(title = "All Allergic Conditions", icon = icon("hospital"),
                      h2("Hospital admissions for different allergic conditions"),
                      p("Since most people with allergic conditions are managed in primary care, secondary care (hospital) data relate to a smaller group of people, 
                        generally with more severe conditions."),
                      p("The chart shows data from the Scottish Morbidity Record (SMR01) scheme, which records hospital inpatient and day case activity for Scotland."), 
                      p("The chart shows that rates of hospital admissions per 100,000 of the population have remained relatively constant for all allergies across 
                        the past 10 years in Scotland."),
                      
                      fluidRow(
                        column(3,
                               selectInput("measure", label = "Select numbers or rates",
                                           choices = c("Number", "Rate"), selected = "Rate")
                               ),
                        column(3,
                               selectizeInput("conditions", label = "Select one or more allergic conditions (up to four)", 
                                 choices = condition_list, multiple = TRUE, selected = "All allergies",
                                 options = list(maxItems =4L))
                        )
                        ),
                      fluidRow(
                        column(3,
                               plotlyOutput("chart", width = "100%", height = "350px"))
                        )
                      ),

##############################################.
# ASTHMA TAB ----
##############################################.
             tabPanel(title = "Asthma Breakdown", icon = icon("area-chart"),
                      h2("Asthma Exploration Work"),
                      p("The selection of charts below show how hospital admissions for asthma related diagnosis codes have changed over the past 10 years
                        across different age groups."),
                      p("The data is split by sex (all males and all females) and by the following age groups: under 10 years old and over 10 years old."),
                      
                      fluidRow(
                        column(3,
                               selectInput("measure_asthma", label = "Select numerator or rates",
                                           choices = c("Numerator", "Rate"), selected = "Rate")),
                        column(3,
                               selectizeInput("diagnosis", label = "Select one or more diagnosis", 
                                              choices = diagnosis_list, multiple = TRUE, selected = "Status asthmaticus (J46) first position"),
                               options = list(maxItems =4L))
                        ),
                      
                      fluidRow(
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
                               plotlyOutput("female_over10", width = "100%"))
                      )
                    ),

##############################################.
# DATA DOWNLOADS TAB ----
##############################################.
             tabPanel(title = "Data Table Downloads", icon = icon("table"),
                      h2("Select the data you wish to download"),
                      p("This section allows you to view the data in table format.
                        You can use the filters to select the data you are interested in.
                        You can also download the data as a csv using the download button."),
                      
                      fluidRow(
                          column(6, selectInput("data_select", "Select the data you want to explore.",
                                            choices = data_list_data_tab)),
                          column(6, downloadButton('download_table_csv', 'Download data')),
                      
                                DT::dataTableOutput("table_filtered")))
  
) #END OF FLUIDPAGE (USER INTERFACE)
            
             
             
             
             