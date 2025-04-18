# Define the UI using bslib's page_sidebar layout
ui <- page_navbar(
  theme = bs_theme(version = 5),

  navbar_options = navbar_options(collapsible = TRUE),
  
  # Add the canonical link and noindex meta tag inside the head tag
  tags$head(
    tags$link(rel = "canonical", href = "https://climate-insights.netlify.app/ghcnm"),
    tags$meta(name = "robots", content = "noindex,indexifembedded"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),  # Link to custom CSS
  ),
  
  fillable_mobile = T,
  
  nav_panel(
    "Data Explorer",
    #   layout_sidebar(
    #     # Sidebar content
    #     sidebar = sidebar(
    #       open = list(desktop = "open", mobile = "always-above"),
    #       # Slider for selecting a range of years (first_year and last_year)
    #       sliderInput("year_range", "Select Year Range:",
    #                   min = min(stations_data$first_year, na.rm = TRUE),
    #                   max = max(stations_data$last_year, na.rm = TRUE),
    #                   value = c(1961, 
    #                             max(stations_data$last_year, na.rm = TRUE)),
    #                   step = 1,
    #                   sep = ""),
    #       
    #       # Select input for choosing a month
    #       selectInput("month", "Select Month:",
    #                   choices = month.name, selected = month.name[1])
    #     ),
    #     card(
    #       full_screen = TRUE,
    #       card_header(h6(textOutput("map_title"))),
    #       # Main panel content
    #       leafletOutput("station_map", height = "90vh")
    #     )
    #   )
    
    
    leafletOutput("station_map", width = "100%", height = "100%"),
    absolutePanel(
      top = 80, left = 30, right = "auto", bottom = "auto",
      width = 150, height = "auto",
      #Slider for selecting a range of years (first_year and last_year)
      sliderInput("year_range", "Select Year Range:",
                  min = 1750,
                  max = max(stations_data$last_year, na.rm = TRUE),
                  value = c(1961, max(stations_data$last_year, na.rm = TRUE)),
                  step = 1,
                  sep = ""),
      
      # Select input for choosing a month
      selectInput("month", "Select Month:",
                  choices = month.name, selected = month.name[1]),
      
    ),
    
    # Conditionally render the panel with the plot when the plotly output is available
    # Use uiOutput to conditionally render the plot panel
    uiOutput("plot_panel")
    
    
  ),
  nav_panel(
    "Info",
    card(
      includeMarkdown("www/md/info_tavg.md")  # Load external HTML file
    )
  )
)
