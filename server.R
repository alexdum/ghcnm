
# Define the server logic
shinyServer(function(input, output, session) {
  
  # Reactive expression to filter the Parquet data by year and month
  filtered_parquet_data <- reactive({
    month_number <- match(input$month, month.name)
    
    # Filter the dataset using arrow's dplyr interface
    tavg_dataset %>%
      filter(
        VALUE >= -90,
        YEAR >= input$year_range[1],
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
  
  output$map_title <- renderText({ paste("Multiannual mean:", input$year_range[1], "to", input$year_range[2]) })
  
  # Set the initial view for the map
  initial_lng <- 0 #mean(stations_data$LONGITUDE, na.rm = TRUE)
  initial_lat <- mean(stations_data$LATITUDE, na.rm = TRUE)
  initial_zoom <- 2
  
  # Render the Leaflet map
  output$station_map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%  # Default OpenStreetMap
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topo Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      
      # Create a custom pane for circle markers with a lower zIndex
      addMapPane("markersPane", zIndex = 400) %>%
      addMapPane("labelsPane", zIndex = 500) %>%  # Higher pane for labels
      
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, 
                       group = "CartoDB Labels", 
                       options = providerTileOptions(opacity = 0.8, pane = "labelsPane")) %>%
      
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      addLayersControl(
        baseGroups = c("CartoDB Voyager", "Esri World Imagery", "Esri World Topo Map", "OpenStreetMap"),
        overlayGroups = c("CartoDB Labels"),  # Add CartoDB labels to the overlay control
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
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
  
  observe({
    data <- filtered_stations()
    
    # Define a bin-based color palette for temperature values
    bins <- 6  # Specify the number of bins
    qpal <- colorBin("RdYlBu", domain = data$mean_temp, bins = bins, na.color = "transparent", reverse = FALSE)
    qpal2 <- colorBin("RdYlBu", domain = data$mean_temp, bins = bins, na.color = "transparent", reverse = TRUE)
    
    leafletProxy("station_map", data = data) %>%
      clearMarkers() %>%
      
      # Add markers to the custom pane with lower zIndex
      addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                       radius = 5, 
                       popup = ~paste("Station:", NAME, "<br>",
                                      "ID:", ID, "<br>",
                                      "Elevation:", STNELEV, "m<br>",
                                      "Available years:", first_year, "-", last_year, "<br>",
                                      "Selected years:", input$year_range[1], "-", input$year_range[2], "<br>",
                                      "Mean Temp:", round(mean_temp, 2), "°C"),
                       color = ~qpal2(mean_temp), fillOpacity = 1,
                       options = pathOptions(pane = "markersPane")) %>%
      
      clearControls() %>%
      
      addLegend(position = "bottomright",  # Position the legend at the lower right
                pal = qpal,                # Use the previously defined color palette
                values = data$mean_temp,    # The values to map to the color scale
                title = htmltools::HTML("<div style='text-align: center;'>°C</div>"),  # Align title to the left using HTML
                opacity = 1,             # Set opacity of the legend
                na.label = "No data",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )  # Reverse the order of intervals in the legend
  })
  
  
})

