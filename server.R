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
  initial_lng <- 5 #mean(stations_data$LONGITUDE, na.rm = TRUE)
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
      
      # Move zoom controls to the lower-left corner
      addControl("<div></div>", position = "topright") %>%  # Placeholder to ensure the zoom controls are moved
      htmlwidgets::onRender("function(el, x) {
      var map = this;
      map.zoomControl.setPosition('topright');
    }") %>% 
      
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
                               initial_lat, initial_lng, initial_zoom)),
          position = "topright" # Move the button to the top-right corner
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
                       layerId = ~ID,
                       popup = ~paste("Station:", NAME, "<br>",
                                      "ID:", ID, "<br>",
                                      "Elevation:", STNELEV, "m<br>",
                                      "Available years:", first_year, "-", last_year, "<br>",
                                      "Selected years:", input$year_range[1], "-", input$year_range[2], "<br>",
                                      "Mean Temp:", round(mean_temp, 2), "°C"),
                       color = ~qpal2(mean_temp), fillOpacity = 1,
                       options = pathOptions(pane = "markersPane")) %>%
      
      clearControls() %>%
      
      addLegend(position = "bottomleft",  # Position the legend at the lower right
                pal = qpal,                # Use the previously defined color palette
                values = data$mean_temp,    # The values to map to the color scale
                title = htmltools::HTML("<div style='text-align: center;'>°C</div>"),  # Align title to the left using HTML
                opacity = 1,             # Set opacity of the legend
                na.label = "No data",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )  # Reverse the order of intervals in the legend
  })
  
  # Reactive expression to retrieve time series data for the selected station and year/month inputs
  time_series_data <- reactive({
    req(input$station_map_marker_click$id)  # Ensure a station is clicked
    
    station_id <- input$station_map_marker_click$id
    month <- input$month
    
    # Filter the dataset based on selected station, month, and year range
    time_series_data <-
      tavg_dataset %>%
      filter(
        VALUE >= -90,
        YEAR >= input$year_range[1],
        YEAR <= input$year_range[2],
        MONTH == match(input$month, month.name),
        ID == station_id) %>%
      collect() |>
      mutate(YEAR = as.numeric(YEAR)) 
    
    time_series_data
    
  })
  

  
  # Observer to handle rendering of the time series plot when inputs (month, year) or station selection change
  output$time_series_plot <- renderPlotly({
    data <- time_series_data()  # Get the filtered time series data
    req(nrow(data) > 0)  # Ensure there is data to plot
    
    station_id <- input$station_map_marker_click$id
    month <- input$month
    
    render_time_series_plot(data, station_id, month)
    
  })
  

  # Observer to handle click events on the map markers and update plot accordingly
  observeEvent(input$station_map_marker_click, {
    # Trigger the time series plot update based on the clicked station
    station_id <- input$station_map_marker_click$id
    
    # Update the time series plot when a station is clicked
    output$time_series_plot <- renderPlotly({
      data <- time_series_data()  # Get the filtered time series data
      req(nrow(data) > 0)  # Ensure there is data to plot
      
      month <- input$month
      
      render_time_series_plot(data, station_id, month)
      
    })
  })
  
  # Reactive observer to update the plot when month or year inputs change
  observe({
    req(input$month, input$year_range)  # Ensure month and year range inputs are provided
    
    station_id <- input$station_map_marker_click$id
    month <- input$month
    
    # Update the time series plot when month or year inputs change
    output$time_series_plot <- renderPlotly({
      data <- time_series_data()  # Get the filtered time series data
      req(nrow(data) > 0)  # Ensure there is data to plot
      
     render_time_series_plot(data, station_id, month)

    })
  })
  
  
  # Create a reactive flag indicating whether the plot is available
  plot_available <- reactive({
    req(input$station_map_marker_click$id)
    nrow(time_series_data()) > 0
  })
  
  # Expose the plot_available flag to the client
  output$plot_available <- reactive({
    plot_available()
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)
  
  # Inside your server.R or server function
  
  output$plot_panel <- renderUI({
    if (plot_available()) {
      absolutePanel(
        draggable = TRUE,
        top = 280, left = 30, right = "auto", bottom = "auto",
        width = 450, height = "auto",
        # Add the Download Data button with Font Awesome icon
        plotlyOutput("time_series_plot", height = "200px"),
        downloadButton(
          outputId = "download_data",
          label = NULL,  # No text label
          icon = icon("download"),  # Font Awesome download icon
          class = "custom-download-button",  # Custom CSS class for styling
          title = "Download Data",  # Tooltip text
          `data-toggle` = "tooltip"  # Enable tooltip
        )
      )
    }
  })
  

  
  # Define the download handler for downloading the time series data
  output$download_data <- downloadHandler(
    filename = function() {
      # Create a dynamic filename based on station ID and month
      station_id <- input$station_map_marker_click$id
      month <- input$month
      paste0(tavg_meta$NAME[tavg_meta$ID == station_id],"_", station_id, "_", month)
    },
    content = function(file) {
      # Get the time series data for the clicked station
      data <- time_series_data()
      
      # Write the data to a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
})