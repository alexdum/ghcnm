# Define the UI using bslib's page_sidebar layout
ui <- page_navbar(
  theme = bs_theme(version = 5),
  # Include the external CSS file
  #includeCSS("www/styles.css"),  # Assuming sales.css is in the www folder
  fillable_mobile = T,
  collapsible = T,
  nav_panel(
    "Data Explorer",
    layout_sidebar(
      # Sidebar content
      sidebar = sidebar(
        open = list(desktop = "open", mobile = "always-above"),
        # Slider for selecting a range of years (first_year and last_year)
        sliderInput("year_range", "Select Year Range:",
                    min = min(stations_data$first_year, na.rm = TRUE),
                    max = max(stations_data$last_year, na.rm = TRUE),
                    value = c(1961, 
                              max(stations_data$last_year, na.rm = TRUE)),
                    step = 1,
                    sep = ""),
        
        # Select input for choosing a month
        selectInput("month", "Select Month:",
                    choices = month.name, selected = month.name[1])
      ),
      
      # Main panel content
      leafletOutput("station_map", height = "90vh")
    )
  )
)
