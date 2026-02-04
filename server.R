# Define the server logic
function(input, output, session) {
  # Reactive expression to filter the Parquet data by year and month
  filtered_parquet_data <- reactive({
    month_number <- match(input$month, month.name)

    # Determine which dataset to use based on input parameter
    dataset <- if (input$parameter %in% c("Temperature", "Air Temperature")) tavg_dataset else prec_dataset

    # Filter the dataset using arrow's dplyr interface
    dataset %>%
      filter(
        VALUE >= -90,
        YEAR >= input$year_range[1],
        YEAR <= input$year_range[2],
        MONTH == month_number
      ) %>%
      group_by(ID) %>%
      # For precipitation, we might want sum instead of mean if it was daily, but these are likely monthly means/totals?
      # Assuming monthly values are already means for temp and totals for precip.
      # If we aggregate over years (multiannual mean), we want mean of the monthly values.
      summarize(mean_value = mean(VALUE, na.rm = TRUE)) %>%
      collect() # Collect only the filtered and summarized data
  })

  # Reactive expression to filter the stations based on year range and Parquet data
  filtered_stations <- reactive({
    filtered_data <- filtered_parquet_data()
    # Determine which station metadata to use
    stations_info <- if (input$parameter %in% c("Temperature", "Air Temperature")) stations_data else prec_stations_data

    # Join filtered data with station data
    stations_info %>%
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
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
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
  # Initial rendering of all markers
  observeEvent(filtered_stations(), {
    data <- filtered_stations()
    param <- input$parameter

    # Define color palette and units based on parameter
    if (param %in% c("Temperature", "Air Temperature")) {
      palette_domain <- data$mean_value
      # Detailed bins centered on 0, extended to cover extremes (-90 to 60)
      bins <- c(-90, -40, -30, -20, -15, -12.5, -10, -7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30, 35, 40, 60)

      # Custom palette:
      # 10 negative intervals: -90..-40, -40..-30, ... -2.5..0
      # 15 positive intervals: 0..2.5, ... 40..60

      blues <- colorRampPalette(c("#053061", "#4393c3", "#d1e5f0"))(10)
      reds <- colorRampPalette(c("#fff7bc", "#fdae61", "#d73027", "#67001f"))(15)
      palette_name <- c(blues, reds)

      reverse <- FALSE # Custom palette is already ordered Blue -> Red
      units <- "°C"
      value_label_prefix <- "Mean Temp:"
    } else {
      palette_domain <- data$mean_value
      # Use interpolated palette because we have > 9 bins
      palette_name <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(12)
      reverse <- FALSE # Darker for more rain
      units <- "mm"
      value_label_prefix <- "Mean Precip:"
      # Use detailed bins for precipitation to see low values better, extended to Inf
      bins <- c(0, 10, 25, 50, 75, 100, 150, 200, 300, 500, 1000, 2000, Inf)
    }
    # Creating palettes
    # For legend (we want standard direction usually, but let's match the map logic)
    # Actually, leaflet legend usually goes low -> high bottom -> top.
    # We construct qpal for the specific domain and direction

    qpal <- colorBin(palette_name, domain = palette_domain, bins = bins, na.color = "transparent", reverse = FALSE)
    # qpal2 is used for fillColor, often we want to reverse RdYlBu so red is high.
    # For independent control, let's explicitly set reverse map
    qpal_fill <- colorBin(palette_name, domain = palette_domain, bins = bins, na.color = "transparent", reverse = reverse)


    # Generate custom labels for legend
    n_bins <- length(bins)
    labels <- character(n_bins - 1)
    for (i in 1:(n_bins - 1)) {
      if (is.infinite(bins[i + 1])) {
        labels[i] <- paste0("> ", bins[i])
      } else {
        # Optional: format numbers to remove decimals if integers?
        # For now simple paste is fine, R handles printing.
        labels[i] <- paste0(bins[i], " - ", bins[i + 1])
      }
    }

    # Reverse for legend (High values at top)
    legend_colors <- rev(palette_name)
    legend_labels <- rev(labels)

    leafletProxy("station_map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        radius = 5,
        layerId = ~ID,
        label = ~ generateLabel(NAME, ID, STNELEV, first_year, last_year, mean_value, input$year_range, Country, value_label_prefix, units),
        color = "grey",
        weight = 1,
        fillColor = ~ qpal_fill(mean_value),
        fillOpacity = 1,
        options = pathOptions(pane = "markersPane")
      ) %>%
      clearControls() %>%
      addLegend(
        position = "bottomleft",
        colors = legend_colors,
        labels = legend_labels,
        title = htmltools::HTML(paste0("<div style='text-align: center;'>", units, "</div>")),
        opacity = 1
      )
  })

  # Update selected marker
  observeEvent(list(selected_station_id(), input$month, input$year_range, input$parameter), {
    previous_id <- previous_station_id()
    if (!is.null(selected_station_id())) {
      data <- filtered_stations()
      selected_id <- selected_station_id()
      param <- input$parameter

      # Define color palette and units based on parameter (Duplicate logic, ideally refactor into function)
      # Define color palette and units based on parameter (Duplicate logic, ideally refactor into function)
      if (param %in% c("Temperature", "Air Temperature")) {
        palette_domain <- data$mean_value
        # Detailed bins centered on 0 with 2.5 degree steps, extended range
        bins <- c(-90, -40, -30, -20, -15, -12.5, -10, -7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30, 35, 40, 60)
        # Custom palette: 10 blues, 15 reds
        blues <- colorRampPalette(c("#053061", "#4393c3", "#d1e5f0"))(10)
        reds <- colorRampPalette(c("#fff7bc", "#fdae61", "#d73027", "#67001f"))(15)
        palette_name <- c(blues, reds)

        reverse <- FALSE
        units <- "°C"
        value_label_prefix <- "Mean Temp:"
      } else {
        palette_domain <- data$mean_value
        # Use interpolated palette
        palette_name <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(12)
        reverse <- FALSE
        units <- "mm"
        value_label_prefix <- "Mean Precip:"
        bins <- c(0, 10, 25, 50, 75, 100, 150, 200, 300, 500, 1000, 2000, Inf)
      }

      selected_station <- data[data$ID == selected_id, ]
      qpal_fill <- colorBin(palette_name, domain = palette_domain, bins = bins, na.color = "transparent", reverse = reverse)

      if (!is.null(previous_id) && previous_id != selected_id) {
        selected_station_prev <- data[data$ID == previous_id, ]
        if (nrow(selected_station_prev) > 0) {
          leafletProxy("station_map", data = selected_station_prev) %>%
            removeMarker(layerId = previous_id) %>%
            addCircleMarkers(
              lng = ~LONGITUDE, lat = ~LATITUDE,
              radius = 5,
              label = ~ generateLabel(NAME, ID, STNELEV, first_year, last_year, mean_value, input$year_range, Country, value_label_prefix, units),
              layerId = ~ID,
              color = "grey",
              weight = 1,
              fillColor = ~ qpal_fill(mean_value),
              fillOpacity = 1,
              options = pathOptions(pane = "markersPane")
            )
        }
      }

      previous_station_id(selected_id)

      if (nrow(selected_station) > 0) {
        leafletProxy("station_map", data = selected_station) %>%
          addCircleMarkers(
            lng = ~LONGITUDE, lat = ~LATITUDE,
            radius = 10,
            layerId = ~ID,
            color = "grey",
            label = ~ generateLabel(NAME, ID, STNELEV, first_year, last_year, mean_value, input$year_range, Country, value_label_prefix, units),
            weight = 1,
            fillColor = ~ qpal_fill(selected_station$mean_value),
            fillOpacity = 1,
            options = pathOptions(pane = "markersPane")
          )
      }
    }
  })


  # Reactive expression to retrieve time series data for the selected station and year/month inputs
  # Reactive expression to retrieve time series data for the selected station and year/month inputs
  time_series_data <- reactive({
    req(input$station_map_marker_click$id) # Ensure a station is clicked

    station_id <- input$station_map_marker_click$id
    month <- input$month
    dataset <- if (input$parameter == "Temperature") tavg_dataset else prec_dataset

    # Filter the dataset based on selected station, month, and year range
    time_series_data <-
      dataset %>%
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
    param <- input$parameter
    y_label <- if (param == "Temperature") "Temperature (°C)" else "Precipitation (mm)"
    title <- if (param == "Temperature") "Daily Mean Temperature" else "Total Precipitation"

    render_time_series_plot(data, station_id, month, y_label, title)
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
      param <- input$parameter
      y_label <- if (param == "Temperature") "Temperature (°C)" else "Precipitation (mm)"
      title <- if (param == "Temperature") "Daily Mean Temperature" else "Total Precipitation"

      render_time_series_plot(data, station_id, month, y_label, title)
    })
  })

  # --- URL Parameter Parsing ---
  url_initialized <- reactiveVal(FALSE)

  parse_url_params <- function(query) {
    params <- list()
    if (length(query) == 0 || query == "") {
      return(params)
    }
    # Parse query string
    q_str <- sub("^\\?", "", query)
    pairs <- strsplit(q_str, "&")[[1]]
    for (pair in pairs) {
      parts <- strsplit(pair, "=")[[1]]
      if (length(parts) == 2) {
        key <- parts[1]
        val <- URLdecode(parts[2])
        params[[key]] <- val
      }
    }
    params
  }

  # Observer: Apply URL params on app startup
  observe({
    req(!url_initialized())
    query <- session$clientData$url_search
    params <- parse_url_params(query)

    if (length(params) > 0) {
      # 1. Parameter
      if (!is.null(params$parameter)) {
        updateSelectInput(session, "parameter", selected = params$parameter)
      }

      # 2. Month
      if (!is.null(params$month)) {
        updateSelectInput(session, "month", selected = params$month)
      }

      # 3. Year Range
      if (!is.null(params$start) && !is.null(params$end)) {
        updateSliderInput(session, "year_range", value = c(as.numeric(params$start), as.numeric(params$end)))
      }

      # 4. Station Selection
      if (!is.null(params$station)) {
        station_ref <- params$station
        shinyjs::delay(1000, {
          stations_info <- if (input$parameter %in% c("Temperature", "Air Temperature")) stations_data else prec_stations_data
          match <- stations_info %>% filter(ID == station_ref | NAME == station_ref)
          if (nrow(match) > 0) {
            selected_station_id(match$ID[1])
          }
        })
      }
    }
    url_initialized(TRUE)
  })

  # Helper: Broadcast current state to parent page
  broadcast_state <- function() {
    sid <- selected_station_id()
    st_meta <- NULL
    if (!is.null(sid)) {
      stations_info <- if (input$parameter %in% c("Temperature", "Air Temperature")) stations_data else prec_stations_data
      st_meta <- stations_info %>%
        filter(ID == sid) %>%
        head(1)
    }

    station_id <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$ID) else NULL
    station_name <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$NAME) else NULL
    country <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$Country) else NULL

    session$sendCustomMessage("updateParentURL", list(
      station = station_id,
      stationName = station_name,
      country = country,
      parameter = input$parameter,
      month = input$month,
      yearStart = input$year_range[1],
      yearEnd = input$year_range[2]
    ))
  }

  # Observer to broadcast state on any relevant change
  observe({
    # Depend on all state variables that should trigger a broadcast
    sid <- selected_station_id()
    param <- input$parameter
    month <- input$month
    year_range <- input$year_range

    req(url_initialized())

    # Broadcast the current state
    st_meta <- NULL
    if (!is.null(sid)) {
      stations_info <- if (param %in% c("Temperature", "Air Temperature")) stations_data else prec_stations_data
      st_meta <- stations_info %>%
        filter(ID == sid) %>%
        head(1)
    }

    station_id <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$ID) else NULL
    station_name <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$NAME) else NULL
    country <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$Country) else NULL

    session$sendCustomMessage("updateParentURL", list(
      station = station_id,
      stationName = station_name,
      country = country,
      parameter = param,
      month = month,
      yearStart = year_range[1],
      yearEnd = year_range[2]
    ))
  })

  # Reactive observer to update the plot when month or year inputs change
  observe({
    req(input$month, input$year_range) # Ensure month and year range inputs are provided

    station_id <- selected_station_id()
    month <- input$month
    param <- input$parameter
    y_label <- if (param == "Temperature") "Temperature (°C)" else "Precipitation (mm)"
    title <- if (param == "Temperature") "Daily Mean Temperature" else "Total Precipitation"

    # Update the time series plot when month or year inputs change
    output$time_series_plot <- renderPlotly({
      data <- time_series_data() # Get the filtered time series data
      req(nrow(data) > 0) # Ensure there is data to plot

      render_time_series_plot(data, station_id, month, y_label, title)
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
      info <- if (input$parameter %in% c("Temperature", "Air Temperature")) tavg_meta else prec_meta
      paste0(info$NAME[info$ID == station_id], "_", station_id, "_", month, ".csv")
    },
    content = function(file) {
      # Get the time series data for the clicked station
      data <- time_series_data()

      # Write the data to a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
}
