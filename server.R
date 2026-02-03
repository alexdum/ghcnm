# Define the server logic
function(input, output, session) {
  # Reactive expression to filter the Parquet data by year and month
  filtered_parquet_data <- reactive({
    month_number <- match(input$month, month.name)

    # Filter the dataset using arrow's dplyr interface
    tavg_dataset %>%
      filter(
        VALUE >= -90,
        YEAR >= input$year_range[1],
        YEAR <= input$year_range[2],
        MONTH == month_number
      ) %>%
      group_by(ID) %>%
      summarize(mean_temp = mean(VALUE, na.rm = TRUE)) %>%
      collect() # Collect only the filtered and summarized data
  })

  # Reactive expression to filter the stations based on year range and Parquet data
  filtered_stations <- reactive({
    filtered_data <- filtered_parquet_data()
    # Join filtered mean temperature data with station data
    stations_data %>%
      filter(
        first_year <= input$year_range[1],
        last_year >= input$year_range[2],
        ID %in% filtered_data$ID
      ) %>%
      left_join(filtered_data, by = "ID")
  })

  output$map_title <- renderText({
    paste("Multiannual mean:", input$year_range[1], "to", input$year_range[2])
  })

  # Set the initial view for the map
  initial_lng <- 5 # mean(stations_data$LONGITUDE, na.rm = TRUE)
  initial_lat <- mean(stations_data$LATITUDE, na.rm = TRUE)
  initial_zoom <- 2


  # Reactive values to store the selected and previous station IDs
  selected_station_id <- reactiveVal(NULL)
  previous_station_id <- reactiveVal(NULL)

  # Render the Leaflet map
  output$station_map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topo Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addMapPane("markersPane", zIndex = 400) %>%
      addMapPane("labelsPane", zIndex = 500) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "CartoDB Labels", options = providerTileOptions(opacity = 0.8, pane = "labelsPane")) %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      addControl("<div></div>", position = "topright") %>%
      htmlwidgets::onRender("function(el, x) { var map = this; map.zoomControl.setPosition('topright'); }") %>%
      addLayersControl(
        baseGroups = c("CartoDB Voyager", "Esri World Imagery", "Esri World Topo Map", "OpenStreetMap"),
        overlayGroups = c("CartoDB Labels"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addEasyButton(
        easyButton(
          icon = "fa-home",
          title = "Reset View",
          onClick = JS(sprintf(
            "function(btn, map){ map.setView([%f, %f], %d); }",
            initial_lat, initial_lng, initial_zoom
          )),
          position = "topright"
        )
      )
  })

  # Initial rendering of all markers
  observeEvent(filtered_stations(), {
    data <- filtered_stations()

    bins <- 6
    qpal <- colorBin("RdYlBu", domain = data$mean_temp, bins = bins, na.color = "transparent", reverse = FALSE)
    qpal2 <- colorBin("RdYlBu", domain = data$mean_temp, bins = bins, na.color = "transparent", reverse = TRUE)

    leafletProxy("station_map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        radius = 5,
        layerId = ~ID,
        label = ~ generateLabel(NAME, ID, STNELEV, first_year, last_year, mean_temp, input$year_range, Country),
        color = "grey",
        weight = 1,
        fillColor = ~ qpal2(mean_temp),
        fillOpacity = 1,
        options = pathOptions(pane = "markersPane")
      ) %>%
      clearControls() %>%
      addLegend(
        position = "bottomleft",
        pal = qpal,
        values = data$mean_temp,
        title = htmltools::HTML("<div style='text-align: center;'>°C</div>"),
        opacity = 1,
        na.label = "No data",
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )
  })

  # Update selected marker
  observeEvent(list(selected_station_id(), input$month, input$year_range), {
    previous_id <- previous_station_id()
    if (!is.null(selected_station_id())) {
      data <- filtered_stations()
      selected_id <- selected_station_id()

      selected_station <- data[data$ID == selected_id, ]
      bins <- 6
      qpal2 <- colorBin("RdYlBu", domain = data$mean_temp, bins = bins, na.color = "transparent", reverse = TRUE)

      if (!is.null(previous_id) && previous_id != selected_id) {
        selected_station_prev <- data[data$ID == previous_id, ]
        leafletProxy("station_map", data = selected_station_prev) %>%
          removeMarker(layerId = previous_id) %>%
          addCircleMarkers(
            lng = ~LONGITUDE, lat = ~LATITUDE,
            radius = 5,
            label = ~ generateLabel(NAME, ID, STNELEV, first_year, last_year, mean_temp, input$year_range, Country),
            layerId = ~ID,
            color = "grey",
            weight = 1,
            fillColor = ~ qpal2(mean_temp),
            fillOpacity = 1,
            options = pathOptions(pane = "markersPane")
          )
      }

      previous_station_id(selected_id)

      leafletProxy("station_map", data = selected_station) %>%
        addCircleMarkers(
          lng = ~LONGITUDE, lat = ~LATITUDE,
          radius = 10,
          layerId = ~ID,
          color = "grey",
          label = ~ generateLabel(NAME, ID, STNELEV, first_year, last_year, mean_temp, input$year_range, Country),
          weight = 1,
          fillColor = ~ qpal2(selected_station$mean_temp),
          fillOpacity = 1,
          options = pathOptions(pane = "markersPane")
        )
    }
  })


  # Reactive expression to retrieve time series data for the selected station and year/month inputs
  time_series_data <- reactive({
    req(input$station_map_marker_click$id) # Ensure a station is clicked

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
        ID == station_id
      ) %>%
      collect() |>
      mutate(YEAR = as.numeric(YEAR))

    time_series_data
  })


  # Observer to handle rendering of the time series plot when inputs (month, year) or station selection change
  output$time_series_plot <- renderPlotly({
    data <- time_series_data() # Get the filtered time series data
    req(nrow(data) > 0) # Ensure there is data to plot

    station_id <- selected_station_id()
    month <- input$month

    render_time_series_plot(data, station_id, month)
  })

  # Observe to handle click events on the map markers and update plot accordingly
  observeEvent(input$station_map_marker_click, {
    selected_station_id(input$station_map_marker_click$id)
  })


  # Observer to handle click events on the map markers and update plot accordingly
  observeEvent(input$station_map_marker_click, {
    # Trigger the time series plot update based on the clicked station
    station_id <- selected_station_id()

    # Update the time series plot when a station is clicked
    output$time_series_plot <- renderPlotly({
      data <- time_series_data() # Get the filtered time series data
      req(nrow(data) > 0) # Ensure there is data to plot

      month <- input$month

      render_time_series_plot(data, station_id, month)
    })
  })

  # Reactive observer to update the plot when month or year inputs change
  observe({
    req(input$month, input$year_range) # Ensure month and year range inputs are provided

    station_id <- selected_station_id()
    month <- input$month

    # Update the time series plot when month or year inputs change
    output$time_series_plot <- renderPlotly({
      data <- time_series_data() # Get the filtered time series data
      req(nrow(data) > 0) # Ensure there is data to plot

      render_time_series_plot(data, station_id, month)
    })
  })


  # Create a reactive flag indicating whether the plot is available
  plot_available <- reactive({
    req(selected_station_id())
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
        draggable = F,
        bottom = 30, # Position from the bottom
        left = "50%", # Start from the middle of the screen
        right = "auto",
        width = 450,
        height = "auto",
        style = "transform: translateX(-50%); background-color: rgba(255, 255, 255, 0.8); border-radius: 10px; padding: 10px;", # Transparent background with some styling

        # Time series plot
        plotlyOutput("time_series_plot", height = "200px"),

        # Download button positioned right below the plot, aligned to the left
        downloadButton(
          outputId = "download_data",
          label = NULL, # No text label
          icon = icon("download"), # Font Awesome download icon
          class = "custom-download-button", # Custom CSS class for styling
          style = "float: left; margin-top: -10px;" # Aligns the button to the left and positions it below the plot
        ) |>
          tooltip("Download data") # bslib data
      )
    }
  })


  # Define the download handler for downloading the time series data
  output$download_data <- downloadHandler(
    filename = function() {
      # Create a dynamic filename based on station ID and month
      station_id <- selected_station_id()
      month <- input$month
      paste0(tavg_meta$NAME[tavg_meta$ID == station_id], "_", station_id, "_", month, ".csv")
    },
    content = function(file) {
      # Get the time series data for the clicked station
      data <- time_series_data()

      # Write the data to a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
}
