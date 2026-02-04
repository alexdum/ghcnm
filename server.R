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

  # Reactive values for map state
  style_change_trigger <- reactiveVal(0) # Triggers redraw after style change
  stations_before_id <- reactiveVal(NULL) # Layer ID to insert stations before
  current_raster_layers <- reactiveVal(character(0)) # Track raster layers

  # Render the MapLibre map
  output$station_map <- renderMaplibre({
    print("Initializing MapLibre...")
    maplibre(
      style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
      center = c(initial_lng, initial_lat),
      zoom = initial_zoom
    ) %>%
      add_navigation_control(show_compass = FALSE, visualize_pitch = FALSE, position = "top-right")
  })

  # Initialize map bounds / Home Zoom
  observeEvent(input$home_zoom, {
    maplibre_proxy("station_map") %>%
      fly_to(center = c(initial_lng, initial_lat), zoom = initial_zoom)
  })

  # Basemap Switching Logic (Sandwich Method)
  observeEvent(input$basemap, {
    req(input$basemap)
    print(paste("Basemap Change:", input$basemap))
    proxy <- maplibre_proxy("station_map")

    # Remove old raster layers
    old_layers <- isolate(current_raster_layers())
    if (length(old_layers) > 0) {
      for (layer_id in old_layers) {
        proxy %>% clear_layer(layer_id)
      }
      current_raster_layers(character(0))
    }

    if (input$basemap %in% c("carto_positron", "carto_voyager", "esri_imagery", "mapbox_satellite")) {
      # VECTOR STYLES
      style_url <- switch(input$basemap,
        "carto_positron" = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
        "carto_voyager" = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
        "esri_imagery" = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json", # Voyager for labels
        "mapbox_satellite" = paste0("https://api.mapbox.com/styles/v1/mapbox/satellite-streets-v12?access_token=", mapbox_token)
      )

      proxy %>% set_style(style_url)
      stations_before_id("watername_ocean") # Insert stations below labels

      # Esri Imagery Special Case (Raster beneath Vector Labels)
      if (input$basemap == "esri_imagery") {
        session <- shiny::getDefaultReactiveDomain()
        selected_basemap <- input$basemap

        later::later(function() {
          shiny::withReactiveDomain(session, {
            current_basemap <- isolate(input$basemap)
            if (current_basemap != selected_basemap) {
              return()
            }

            unique_suffix <- as.numeric(Sys.time()) * 1000
            source_id <- paste0("esri_source_", unique_suffix)
            layer_id <- paste0("esri_layer_", unique_suffix)
            esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"

            maplibre_proxy("station_map") %>%
              add_raster_source(id = source_id, tiles = c(esri_url), tileSize = 256) %>%
              add_layer(
                id = layer_id,
                type = "raster",
                source = source_id,
                paint = list("raster-opacity" = 1),
                before_id = "watername_ocean"
              )
            current_raster_layers(c(layer_id))
            style_change_trigger(isolate(style_change_trigger()) + 1)
          })
        }, delay = 0.5)
      } else {
        style_change_trigger(isolate(style_change_trigger()) + 1)
      }
    } else {
      # RASTER STYLES (OSM, Esri Topo)
      tile_url <- if (input$basemap %in% c("osm", "osm_gray")) {
        "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
      } else {
        "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"
      }

      attribution <- if (input$basemap %in% c("osm", "osm_gray")) {
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      } else {
        "Tiles &copy; Esri"
      }

      paint_props <- list("raster-opacity" = 1)
      if (input$basemap == "osm_gray") {
        paint_props[["raster-saturation"]] <- -0.9
        paint_props[["raster-contrast"]] <- 0.3
      }

      # Blank style
      blank_style <- list(version = 8, sources = list(), layers = list())
      json_blank <- jsonlite::toJSON(blank_style, auto_unbox = TRUE)
      blank_uri <- paste0("data:application/json,", URLencode(as.character(json_blank), reserved = TRUE))

      proxy %>% set_style(blank_uri)

      session <- shiny::getDefaultReactiveDomain()
      selected_basemap <- input$basemap

      later::later(function() {
        shiny::withReactiveDomain(session, {
          if (isolate(input$basemap) != selected_basemap) {
            return()
          }

          unique_suffix <- as.numeric(Sys.time()) * 1000
          source_id <- paste0("raster_source_", unique_suffix)
          layer_id <- paste0("raster_layer_", unique_suffix)

          maplibre_proxy("station_map") %>%
            add_raster_source(id = source_id, tiles = c(tile_url), tileSize = 256, attribution = attribution) %>%
            add_layer(
              id = layer_id,
              type = "raster",
              source = source_id,
              paint = paint_props
            )

          stations_before_id(NULL) # On top of raster
          current_raster_layers(c(layer_id))
          style_change_trigger(isolate(style_change_trigger()) + 1)
        })
      }, delay = 0.5)
    }
  })

  # Toggle Labels (Vector only)
  observeEvent(input$show_labels, {
    req(input$basemap %in% c("carto_positron", "carto_voyager", "mapbox_satellite", "esri_imagery"))
    visibility <- if (input$show_labels) "visible" else "none"
    label_layers <- c(
      "place_villages", "place_town", "place_country_2", "place_country_1",
      "place_state", "place_continent", "place_city_r6", "place_city_r5",
      "place_city_dot_r7", "place_city_dot_r4", "place_city_dot_r2", "place_city_dot_z7",
      "place_capital_dot_z7", "place_capital", "roadname_minor", "roadname_sec",
      "roadname_pri", "roadname_major", "motorway_name", "watername_ocean",
      "watername_sea", "watername_lake", "watername_lake_line", "poi_stadium",
      "poi_park", "poi_zoo", "airport_label", "country-label", "state-label",
      "settlement-major-label", "settlement-minor-label", "settlement-subdivision-label",
      "road-label", "waterway-label", "natural-point-label", "poi-label", "airport-label"
    )
    proxy <- maplibre_proxy("station_map")
    for (layer_id in label_layers) {
      tryCatch(
        {
          proxy %>% set_layout_property(layer_id, "visibility", visibility)
        },
        error = function(e) {}
      )
    }
  })

  # Initial rendering of all markers
  # Initial rendering of all markers
  # Render Stations Layer (MapLibre)
  # Flag to track if map is initialized
  map_initialized <- reactiveVal(FALSE)

  # One-time observer to detect map load
  observe({
    req(input$station_map_zoom)
    if (!map_initialized()) {
      map_initialized(TRUE)
    }
  })

  # Render Stations Layer (MapLibre)
  # Render Stations Layer (MapLibre) - DATA UPDATE ONLY
  observe({
    req(map_initialized()) # Wait for map to be ready
    req(filtered_stations())
    style_change_trigger() # Re-add layer if style changes

    data <- filtered_stations()
    param <- input$parameter
    # Note: We do NOT depend on selected_station_id() here to avoid full redraws on click

    # Define palettes and units
    if (param %in% c("Temperature", "Air Temperature")) {
      palette_domain <- data$mean_value
      bins <- c(-Inf, -40, -30, -20, -15, -12.5, -10, -7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30, 35, 40, Inf)
      blues <- colorRampPalette(c("#053061", "#4393c3", "#d1e5f0"))(10)
      reds <- colorRampPalette(c("#fff7bc", "#fdae61", "#d73027", "#67001f"))(15)
      palette_colors <- c(blues, reds)
      units <- "°C"
      prefix <- "Mean Temp:"
    } else {
      palette_domain <- data$mean_value
      palette_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(12)
      units <- "mm"
      prefix <- "Mean Precip:"
      bins <- c(0, 10, 25, 50, 75, 100, 150, 200, 300, 500, 1000, 2000, Inf)
    }

    # Color Function
    pal_fun <- colorBin(palette_colors, domain = palette_domain, bins = bins, na.color = "transparent")

    # Prepare Data for MapLibre
    map_data <- data %>%
      mutate(
        circle_color = pal_fun(mean_value),
        # Default (unselected/base) styles
        # We rely on paint property updates for selection highlighting
        # Generate Label Content (Tooltip)
        popup_content = as.character(mapply(function(n, i, e, f, l, m, yr, c, p, u) {
          generateLabel(n, i, e, f, l, m, yr, c, p, u)
        }, NAME, ID, STNELEV, first_year, last_year, mean_value, list(input$year_range), Country, prefix, units))
      ) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

    # Add Layer with base styles
    maplibre_proxy("station_map") %>%
      clear_layer("stations") %>%
      add_circle_layer(
        id = "stations",
        source = map_data,
        circle_color = get_column("circle_color"),
        circle_radius = 5,
        circle_stroke_color = get_column("circle_color"),
        circle_stroke_width = 2,
        circle_opacity = 0.7,
        circle_stroke_opacity = 1,
        tooltip = get_column("popup_content"),
        before_id = stations_before_id()
      )

    # Re-apply current selection style immediately after rendering
    # (Use isolate to avoid reactivity here, though this observer is triggered by data change)
    cur_sel <- isolate(selected_station_id())
    update_selection_style(cur_sel)
  })

  # Helper to update selection styles efficiently
  update_selection_style <- function(id) {
    if (is.null(id)) {
      # Reset to base styles
      maplibre_proxy("station_map") %>%
        set_paint_property("stations", "circle-radius", 5) %>%
        set_paint_property("stations", "circle-stroke-width", 2) %>%
        # Reset stroke color to match circle color using data-driven property
        set_paint_property("stations", "circle-stroke-color", list("get", "circle_color"))
    } else {
      # Apply highlight style using expressions
      # Radius: 8 if selected, 5 otherwise
      radius_expr <- list("case", list("==", list("get", "ID"), id), 8, 5)

      # Stroke Width: 3 if selected, 2 otherwise
      width_expr <- list("case", list("==", list("get", "ID"), id), 3, 2)

      # Stroke Color: Red if selected, else use circle_color
      color_expr <- list("case", list("==", list("get", "ID"), id), "#FF0000", list("get", "circle_color"))

      maplibre_proxy("station_map") %>%
        set_paint_property("stations", "circle-radius", radius_expr) %>%
        set_paint_property("stations", "circle-stroke-width", width_expr) %>%
        set_paint_property("stations", "circle-stroke-color", color_expr)
    }
  }

  # Selection Observer - VISUAL UPDATE ONLY
  observeEvent(selected_station_id(),
    {
      req(map_initialized())
      update_selection_style(selected_station_id())
    },
    ignoreInit = TRUE
  )


  # Reactive expression to retrieve time series data for the selected station and year/month inputs
  # Reactive expression to retrieve time series data for the selected station and year/month inputs
  time_series_data <- reactive({
    req(selected_station_id()) # Ensure a station is clicked

    station_id <- selected_station_id()
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
  # Observe to handle click events on the map markers and update plot accordingly
  observeEvent(input$station_map_feature_click, {
    clicked <- input$station_map_feature_click

    # Check layer ID (support both layer_id and layer)
    is_station_layer <- FALSE
    if (!is.null(clicked$layer_id) && clicked$layer_id == "stations") is_station_layer <- TRUE
    if (!is.null(clicked$layer) && clicked$layer == "stations") is_station_layer <- TRUE

    if (!is.null(clicked) && is_station_layer) {
      # Handle potential case sensitivity or property name differences
      props <- clicked$properties
      # Try 'ID' (original) then 'id' (potentially lowercased by JS/sf)
      click_id <- if (!is.null(props$ID)) props$ID else props$id

      print(paste("Selected ID:", click_id))
      if (!is.null(click_id)) {
        selected_station_id(click_id)

        # Center map on clicked station, preserving current zoom
        if (!is.null(clicked$lng) && !is.null(clicked$lat)) {
          maplibre_proxy("station_map") %>%
            fly_to(center = c(clicked$lng, clicked$lat), zoom = input$station_map_zoom)
        }
      }
    }
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


  # Render Map Legend
  output$map_legend <- renderUI({
    param <- input$parameter

    # Define legend properties matching the map logic
    if (param %in% c("Temperature", "Air Temperature")) {
      bins <- c(-Inf, -40, -30, -20, -15, -12.5, -10, -7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30, 35, 40, Inf)
      blues <- colorRampPalette(c("#053061", "#4393c3", "#d1e5f0"))(10)
      reds <- colorRampPalette(c("#fff7bc", "#fdae61", "#d73027", "#67001f"))(15)
      colors <- c(blues, reds)
      units <- "°C"

      # For temperature, we reverse the display so high values are at top
      display_bins <- rev(bins)
      display_colors <- rev(colors)
    } else {
      # Precip
      colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(12)
      units <- "mm"
      bins <- c(0, 10, 25, 50, 75, 100, 150, 200, 300, 500, 1000, 2000, Inf)

      display_bins <- rev(bins)
      display_colors <- rev(colors)
    }

    # Generate HTML items
    legend_items <- lapply(seq_along(display_colors), function(i) {
      # Bins are effectively boundaries.
      # With n colors, we have n+1 boundaries (if we include start/end), or we map n colors to n intervals.
      # The bins array has length N+1 for N colors.

      # display_bins has high values first.
      # display_colors has high value colors first.

      # Current interval: display_bins[i] (upper) to display_bins[i+1] (lower)
      # Wait, bins c(a, b, c) -> intervals (a,b), (b,c). 2 intervals, 3 boundaries.
      # length(colors) should be length(bins) - 1.

      # Let's verify lengths.
      # Temp: 10+15 = 25 colors. Bins: 26 values. Correct.
      # Prec: 12 colors. Bins: 13 values. Correct.

      val_high <- display_bins[i]
      val_low <- display_bins[i + 1]
      color <- display_colors[i]

      label_text <- if (is.infinite(val_high)) {
        paste0("> ", val_low)
      } else if (is.infinite(val_low)) {
        paste0("< ", val_high)
      } else {
        paste0(val_low, " – ", val_high)
      }

      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 0px;",
        tags$span(
          style = sprintf("background: %s; width: 18px; height: 18px; margin-right: 8px; display: inline-block; opacity: 0.9; border: 1px solid #ccc; border-bottom: none;", color)
        ),
        tags$span(
          style = "font-size: 11px;",
          label_text
        )
      )
    })

    absolutePanel(
      bottom = 30, left = 20,
      draggable = FALSE,
      width = 130, # Fixed width for neatness
      style = "background: white; padding: 10px; border-radius: 4px; box-shadow: 0 0 5px rgba(0,0,0,0.3); max-height: 80vh; overflow-y: auto;",
      tags$h6(style = "margin-top: 0; margin-bottom: 8px; font-weight: bold; text-align: center;", units),
      do.call(tags$div, legend_items)
    )
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
