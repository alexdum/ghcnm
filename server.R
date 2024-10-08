
# Define the server logic
shinyServer(function(input, output, session) {
  
  # Reactive expression to filter the Parquet data by year and month
  filtered_parquet_data <- reactive({
    month_number <- match(input$month, month.name)
    
    # Filter the dataset using arrow's dplyr interface
    tavg_dataset %>%
      filter(YEAR >= input$year_range[1],
             YEAR <= input$year_range[2],
             MONTH == month_number) %>%
      group_by(ID) %>%
      summarize(mean_temp = mean(VALUE, na.rm = TRUE)) %>%
      collect() # Collect only the filtered and summarized data
  })
  
  # Reactive expression to filter the stations based on year range and Parquet data
  filtered_stations <- reactive({
    filtered_data <- filtered_parquet_data()
    # Join filtered mean temperature data with station data
    stations_data %>%
      filter(first_year <= input$year_range[1],
             last_year >= input$year_range[2],
             ID %in% filtered_data$ID) %>%
      left_join(filtered_data, by = "ID")
  })
  
  # Set the initial view for the map
  initial_lng <- mean(stations_data$LONGITUDE, na.rm = TRUE)
  initial_lat <- mean(stations_data$LATITUDE, na.rm = TRUE)
  initial_zoom <- 2
  
  # Render the Leaflet map
  output$station_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      # Add a home button
      addEasyButton(
        easyButton(
          icon = "fa-home", 
          title = "Reset View",
          onClick = JS(sprintf("function(btn, map){ map.setView([%f, %f], %d); }", 
                               initial_lat, initial_lng, initial_zoom))
        )
      )
  })
  
  # Observe filtered data and update the map
  observe({
    data <- filtered_stations()
    
    # Define a color scale for temperature values
    color_palette <- colorRampPalette(c("blue", "white", "red"))(100)
    temp_colors <- scales::rescale(data$mean_temp, to = c(1, 100), from = range(data$mean_temp, na.rm = TRUE))
    circle_colors <- color_palette[temp_colors]
    
    leafletProxy("station_map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                       radius = 5, 
                       popup = ~paste("Station:", NAME, "<br>",
                                      "ID:", ID, "<br>",
                                      "Elevation:", STNELEV, "m<br>",
                                      "First Year:", first_year, "<br>",
                                      "Last Year:", last_year, "<br>",
                                      "Mean Temp:", round(mean_temp, 2), "°C"),
                       color = circle_colors, fillOpacity = 0.7)
    
    # Fit map bounds to the filtered data
    if (nrow(data) > 0) {
      leafletProxy("station_map") %>%
        fitBounds(
          lng1 = min(data$LONGITUDE, na.rm = TRUE),
          lat1 = min(data$LATITUDE, na.rm = TRUE),
          lng2 = max(data$LONGITUDE, na.rm = TRUE),
          lat2 = max(data$LATITUDE, na.rm = TRUE)
        )
    }
  })
  
})

