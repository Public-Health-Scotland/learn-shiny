# UI - USER INTERFACE


# CODE FOR PHS BRANDING PLACED AT BEGINNING OF NAVBARPAGE() 
# NAVBAR ----

 navbarPage(title = div(tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"), #bringing in PHS logo and look for dashboard
                               href= "https://www.publichealthscotland.scot/",
                               target = "_blank"),
                        style = "position: relative; top: -10px;"), 
            header = tags$head(includeCSS("www/styles.css"), # CSS styles
                               HTML("<html lang='en'>"),
                               tags$link(rel="shortcut icon", href="favicon_phs.ico")), 

# INFORMATION TAB ----
            

# ALLERGIC CONDITIONS TAB ----
             

# ASTHMA TAB ----


# DATA DOWNLOADS TAB ----
             

 ) #END OF FLUIDPAGE (USER INTERFACE)
            
             
             
             
             