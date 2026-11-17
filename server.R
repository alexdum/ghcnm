# Define the server logic
function(input, output, session) {
  is_temp_param <- function(param) {
    !is.null(param) && length(param) > 0 && param[1] %in% c("Temperature", "Air Temperature")
  }

  # Show loading spinner on startup until stations are drawn
  session$sendCustomMessage("freezeUI", list(text = "Loading stations..."))

  # Reactive values to store the selected, previous, and deep-link pending state
  selected_station_id <- reactiveVal(NULL)
  previous_station_id <- reactiveVal(NULL)
  pending_map_fly <- reactiveVal(NULL)
  pending_url_param <- reactiveVal(NULL)
  url_param_token <- reactiveVal(0L)
  pending_url_month <- reactiveVal(NULL)
  url_month_token <- reactiveVal(0L)
  pending_url_years <- reactiveVal(NULL)
  url_years_token <- reactiveVal(0L)

  schedule_post_flush <- function(fn, delay_sec) {
    current_session <- session
    run_later <- function() {
      if (!is.null(current_session) && is.function(current_session$isClosed) && current_session$isClosed()) {
        return(invisible(NULL))
      }
      later::later(function() {
        tryCatch({
          if (!is.null(current_session) && is.function(current_session$isClosed) && current_session$isClosed()) {
            return(invisible(NULL))
          }
          shiny::withReactiveDomain(current_session, shiny::isolate(fn()))
        }, error = function(e) NULL)
      }, delay_sec)
    }
    if (!is.null(current_session) && is.function(current_session$onFlushed)) {
      current_session$onFlushed(run_later, once = TRUE)
    } else {
      run_later()
    }
  }

  arm_token_rv <- function(val_rv, tok_rv, value, clear_value, duration = 3.0) {
    tok <- isolate(tok_rv()) + 1L
    tok_rv(tok)
    val_rv(value)
    schedule_post_flush(function() {
      if (identical(isolate(tok_rv()), tok)) {
        val_rv(clear_value)
      }
    }, duration)
  }

  clear_token_rv <- function(val_rv, tok_rv, clear_value = NULL) {
    tok_rv(isolate(tok_rv()) + 1L)
    val_rv(clear_value)
  }

  effective_param <- function() {
    p <- pending_url_param()
    if (!is.null(p) && length(p) > 0 && !is.na(p[1]) && nzchar(trimws(as.character(p[1])))) {
      return(as.character(p[1]))
    }
    if (!is.null(input$parameter) && length(input$parameter) > 0 && !is.na(input$parameter[1]) && nzchar(trimws(as.character(input$parameter[1])))) {
      return(as.character(input$parameter[1]))
    }
    "Temperature"
  }

  effective_month <- function() {
    m <- pending_url_month()
    if (!is.null(m) && length(m) > 0 && !is.na(m[1]) && nzchar(trimws(as.character(m[1])))) {
      return(as.character(m[1]))
    }
    if (!is.null(input$month) && length(input$month) > 0 && !is.na(input$month[1]) && nzchar(trimws(as.character(input$month[1])))) {
      return(as.character(input$month[1]))
    }
    month.name[1]
  }

  effective_year_range <- function() {
    y <- pending_url_years()
    if (!is.null(y) && length(y) >= 2 && !any(is.na(y[1:2]))) {
      return(as.numeric(y[1:2]))
    }
    yr_inp <- input$year_range
    if (!is.null(yr_inp) && length(yr_inp) >= 2 && !any(is.na(yr_inp[1:2]))) {
      return(as.numeric(yr_inp[1:2]))
    }
    c(1961, max(stations_data$last_year, na.rm = TRUE))
  }

  lookup_station_meta <- function(sid, param) {
    if (is.null(sid) || length(sid) == 0 || is.na(sid[1]) || !nzchar(trimws(as.character(sid[1])))) {
      return(NULL)
    }
    primary <- if (is_temp_param(param)) stations_data else prec_stations_data
    res <- primary %>% filter(ID == sid) %>% head(1)
    if (nrow(res) > 0) return(res)
    fallback <- if (is_temp_param(param)) prec_stations_data else stations_data
    fallback %>% filter(ID == sid) %>% head(1)
  }

  # Reactive expression to filter the Parquet data by year and month
  filtered_parquet_data <- reactive({
    eff_month <- effective_month()
    eff_years <- effective_year_range()
    eff_param <- effective_param()
    req(eff_month, eff_years, length(eff_years) >= 2)
    month_number <- match(eff_month, month.name)
    req(!is.na(month_number))

    yr_start <- as.integer(eff_years[1])
    yr_end <- as.integer(eff_years[2])
    span_years <- max(1L, yr_end - yr_start + 1L)
    min_years <- max(1L, as.integer(ceiling(0.80 * span_years)))

    # Determine which dataset to use based on active parameter
    dataset <- if (is_temp_param(eff_param)) tavg_dataset else prec_dataset

    # Filter the dataset using arrow's dplyr interface and enforce >=80% completeness
    dataset %>%
      filter(
        VALUE >= -90,
        as.integer(YEAR) >= yr_start,
        as.integer(YEAR) <= yr_end,
        MONTH == month_number
      ) %>%
      group_by(ID) %>%
      summarize(
        mean_value = mean(VALUE, na.rm = TRUE),
        n_years = n()
      ) %>%
      collect() %>%
      filter(n_years >= min_years)
  })

  # Reactive expression to filter the stations based on year range and Parquet data
  filtered_stations <- reactive({
    filtered_data <- filtered_parquet_data()
    eff_years <- effective_year_range()
    eff_param <- effective_param()
    # Determine which station metadata to use
    stations_info <- if (is_temp_param(eff_param)) stations_data else prec_stations_data

    # Join filtered data with station data
    stations_info %>%
      filter(
        first_year <= eff_years[1],
        last_year >= eff_years[2],
        ID %in% filtered_data$ID
      ) %>%
      left_join(filtered_data, by = "ID")
  })

  output$map_title <- renderText({
    yr <- effective_year_range()
    paste("Multiannual mean:", yr[1], "to", yr[2])
  })

  # Set the initial view for the map
  initial_lng <- 5 # mean(stations_data$LONGITUDE, na.rm = TRUE)
  initial_lat <- mean(stations_data$LATITUDE, na.rm = TRUE)
  initial_zoom <- 2

  # Reactive values for map state
  style_change_trigger <- reactiveVal(0) # Triggers redraw after style change
  stations_before_id <- reactiveVal(NULL) # Layer ID to insert stations before
  current_raster_layers <- reactiveVal(character(0)) # Track raster layers
  stations_loaded <- reactiveVal(FALSE) # Track if stations have been drawn
  basemap_debounced <- shiny::debounce(reactive(input$basemap), 200)

  # Render the MapLibre map
  # Render the MapLibre map
  output$station_map <- renderMaplibre({
    print("Initializing MapLibre...")
    maplibre(
      style = ofm_positron_style,
      center = c(initial_lng, initial_lat),
      zoom = initial_zoom
    ) %>%
      add_navigation_control(show_compass = FALSE, visualize_pitch = FALSE, position = "top-left")
  })

  # Initialize map bounds / Home Zoom
  observeEvent(input$home_zoom, {
    maplibre_proxy("station_map") %>%
      fly_to(center = c(initial_lng, initial_lat), zoom = initial_zoom)
  })

  # --- Basemap Switching Logic ---
  label_layer_ids <- c(
    # OpenFreeMap Positron & Bright common labels
    "waterway_line_label", "water_name_point_label", "water_name_line_label",
    "highway-name-path", "highway-name-minor", "highway-name-major",
    "highway-shield-non-us", "highway-shield-us-interstate", "road_shield_us",
    "airport", "label_other", "label_village", "label_town", "label_state",
    "label_city", "label_city_capital", "label_country_3", "label_country_2", "label_country_1",
    # Bright specific labels (POIs & Directions)
    "road_oneway", "road_oneway_opposite", "poi_r20", "poi_r7", "poi_r1", "poi_transit",
    # Dash variants
    "waterway-line-label", "water-name-point-label", "water-name-line-label",
    "highway-shield-non-us", "highway-shield-us-interstate", "road-shield-us",
    "label-other", "label-village", "label-town", "label-state",
    "label-city", "label-city-capital", "label-country-3", "label-country-2", "label-country-1",
    # Legacy/Carto/OSM
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

  non_label_layer_ids <- c(
    "background", "park", "water", "landcover_ice_shelf", "landcover_glacier",
    "landuse_residential", "landcover_wood", "waterway", "building",
    "tunnel_motorway_casing", "tunnel_motorway_inner", "aeroway-taxiway",
    "aeroway-runway-casing", "aeroway-area", "aeroway-runway",
    "road_area_pier", "road_pier", "highway_path", "highway_minor",
    "highway_major_casing", "highway_major_inner", "highway_major_subtle",
    "highway_motorway_casing", "highway_motorway_inner", "highway_motorway_subtle",
    "railway_transit", "railway_transit_dashline", "railway_service",
    "railway_service_dashline", "railway", "railway_dashline",
    "highway_motorway_bridge_casing", "highway_motorway_bridge_inner",
    "boundary_3", "boundary_2", "boundary_disputed"
  )

  apply_label_visibility <- function(proxy, show_labels) {
    visibility <- if (isTRUE(show_labels)) "visible" else "none"
    for (layer_id in label_layer_ids) {
      tryCatch(
        {
          proxy %>% set_layout_property(layer_id, "visibility", visibility)
        },
        error = function(e) {}
      )
    }
  }

  observeEvent(basemap_debounced(), {
    basemap <- basemap_debounced()
    proxy <- maplibre_proxy("station_map")

    if (basemap %in% c("ofm_positron", "ofm_bright")) {
      style_url <- if (basemap == "ofm_positron") ofm_positron_style else ofm_bright_style
      proxy %>% set_style(style_url, preserve_layers = FALSE)
      stations_before_id("waterway_line_label")

      current_session <- session
      selected_basemap <- basemap

      later::later(function() {
        tryCatch({
          if (!is.null(current_session) && is.function(current_session$isClosed) && current_session$isClosed()) {
            return(invisible(NULL))
          }
          shiny::withReactiveDomain(current_session, shiny::isolate({
            current_basemap <- input$basemap
            if (is.null(current_basemap) || current_basemap != selected_basemap) {
              return()
            }
            apply_label_visibility(maplibre_proxy("station_map"), input$show_labels)
            style_change_trigger(style_change_trigger() + 1)
          }))
        }, error = function(e) NULL)
      }, delay = 0.35)
    } else if (basemap == "sentinel") {
      proxy %>% set_style(ofm_positron_style, preserve_layers = FALSE)

      current_session <- session
      selected_basemap <- basemap

      later::later(function() {
        tryCatch({
          if (!is.null(current_session) && is.function(current_session$isClosed) && current_session$isClosed()) {
            return(invisible(NULL))
          }
          shiny::withReactiveDomain(current_session, shiny::isolate({
            current_basemap <- input$basemap
            if (is.null(current_basemap) || current_basemap != selected_basemap) {
              return()
            }

            unique_suffix <- as.numeric(Sys.time()) * 1000
            source_id <- paste0("sentinel_source_", unique_suffix)
            layer_id <- paste0("sentinel_layer_", unique_suffix)

            maplibre_proxy("station_map") %>%
              add_raster_source(id = source_id, tiles = c(sentinel_url), tileSize = 256, attribution = sentinel_attribution) %>%
              add_layer(id = layer_id, type = "raster", source = source_id, paint = list("raster-opacity" = 1), before_id = "background")

            for (layer_id_kill in non_label_layer_ids) {
              tryCatch(
                {
                  maplibre_proxy("station_map") %>% set_layout_property(layer_id_kill, "visibility", "none")
                },
                error = function(e) {}
              )
            }

            apply_label_visibility(maplibre_proxy("station_map"), input$show_labels)
            stations_before_id("waterway_line_label")
            style_change_trigger(style_change_trigger() + 1)
          }))
        }, error = function(e) NULL)
      }, delay = 0.5)
    }
  }, ignoreInit = TRUE)

  # Toggle Labels
  observeEvent(input$show_labels, {
    req(input$basemap %in% c("ofm_positron", "ofm_bright", "sentinel"))
    apply_label_visibility(maplibre_proxy("station_map"), input$show_labels)
  }, ignoreInit = TRUE)

  # Initial rendering of all markers
  # Render Stations Layer (MapLibre)
  # Flag to track if map is initialized
  map_initialized <- reactiveVal(FALSE)

  # One-time observer to detect map load
  # Delay slightly to ensure MapLibre style is fully loaded before adding layers
  observe({
    req(input$station_map_zoom)
    if (!map_initialized()) {
      current_session <- session
      later::later(function() {
        tryCatch({
          if (!is.null(current_session) && is.function(current_session$isClosed) && current_session$isClosed()) {
            return(invisible(NULL))
          }
          shiny::withReactiveDomain(current_session, shiny::isolate({
            map_initialized(TRUE)
          }))
        }, error = function(e) NULL)
      }, delay = 0.5)
    }
  })

  # Consume deferred map fly-to once MapLibre is ready
  observe({
    req(map_initialized())
    fly <- pending_map_fly()
    req(!is.null(fly))
    pending_map_fly(NULL)
    maplibre_proxy("station_map") %>%
      fly_to(center = c(fly$lon, fly$lat), zoom = 6)
  })

  # Release startup URL echo guards when browser confirms or user overrides
  observeEvent(input$parameter, {
    pend_p <- isolate(pending_url_param())
    if (!is.null(pend_p) && nzchar(pend_p)) {
      if (identical(input$parameter, pend_p)) {
        clear_token_rv(pending_url_param, url_param_token, NULL)
      } else if (identical(input$parameter, "Temperature")) {
        updateSelectInput(session, "parameter", selected = pend_p)
      } else {
        clear_token_rv(pending_url_param, url_param_token, NULL)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$month, {
    pend_m <- isolate(pending_url_month())
    if (!is.null(pend_m) && nzchar(pend_m)) {
      if (identical(input$month, pend_m)) {
        clear_token_rv(pending_url_month, url_month_token, NULL)
      } else if (identical(input$month, "January")) {
        updateSelectInput(session, "month", selected = pend_m)
      } else {
        clear_token_rv(pending_url_month, url_month_token, NULL)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$year_range, {
    pend_y <- isolate(pending_url_years())
    if (!is.null(pend_y) && length(pend_y) >= 2 && !any(is.na(pend_y))) {
      cur_y <- suppressWarnings(as.integer(input$year_range))
      tgt_y <- suppressWarnings(as.integer(pend_y[1:2]))
      def_y <- c(1961L, as.integer(max(stations_data$last_year, na.rm = TRUE)))
      if (length(cur_y) == 2 && identical(cur_y, tgt_y)) {
        clear_token_rv(pending_url_years, url_years_token, NULL)
      } else if (length(cur_y) == 2 && identical(cur_y, def_y)) {
        updateSliderInput(session, "year_range", value = tgt_y)
      } else {
        clear_token_rv(pending_url_years, url_years_token, NULL)
      }
    }
  }, ignoreInit = TRUE)

  # Render Stations Layer (MapLibre)
  # Render Stations Layer (MapLibre) - DATA UPDATE ONLY
  observe({
    req(map_initialized()) # Wait for map to be ready
    req(filtered_stations())
    style_change_trigger() # Re-add layer if style changes

    # Show loading spinner while redrawing stations
    session$sendCustomMessage("freezeUI", list(text = "Loading stations..."))

    data <- filtered_stations()
    param <- effective_param()
    # Note: We do NOT depend on selected_station_id() here to avoid full redraws on click

    # Define palettes and units
    if (is_temp_param(param)) {
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
    # Vectorized popup HTML (replaces per-row mapply+generateLabel)
    yr <- effective_year_range()
    span_years <- max(1L, as.integer(yr[2]) - as.integer(yr[1]) + 1L)
    popup_html <- paste0(
      "Station: ", data$NAME, "<br>",
      "Country: ", data$Country, "<br>",
      "ID: ", data$ID, "<br>",
      "Elevation: ", data$STNELEV, " m<br>",
      "Available years: ", data$first_year, " - ", data$last_year, "<br>",
      "Selected years: ", yr[1], " - ", yr[2], " (", data$n_years, "/", span_years, " yrs)<br>",
      prefix, " ", round(data$mean_value, 1), " ", units,
      "<br><span style='color:red;'>Click to get graph and data</span>"
    )

    map_data <- data %>%
      mutate(
        circle_color = pal_fun(mean_value),
        popup_content = popup_html
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
    cur_sel <- isolate(selected_station_id())
    update_selection_style(cur_sel)

    # Dismiss the loading spinner after stations are drawn
    session$sendCustomMessage("unfreezeUI", list())
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
    eff_month <- effective_month()
    eff_years <- effective_year_range()
    eff_param <- effective_param()
    req(selected_station_id()) # Ensure a station is clicked
    req(eff_month, eff_years, length(eff_years) >= 2)

    station_id <- selected_station_id()
    month_num <- match(eff_month, month.name)
    req(!is.na(month_num))
    yr_start <- as.integer(eff_years[1])
    yr_end <- as.integer(eff_years[2])
    dataset <- if (is_temp_param(eff_param)) tavg_dataset else prec_dataset

    # Filter the dataset based on selected station, month, and year range
    dataset %>%
      filter(
        VALUE >= -90,
        as.integer(YEAR) >= yr_start,
        as.integer(YEAR) <= yr_end,
        MONTH == month_num,
        ID == station_id
      ) %>%
      collect() %>%
      mutate(YEAR = as.integer(YEAR))
  })


  # Render the monthly time series plot reactively when inputs or station selection change
  output$time_series_plot <- renderPlotly({
    is_mag <- plot_magnified() # Force redraw when magnification changes
    data <- time_series_data() # Get the filtered time series data
    req(nrow(data) > 0) # Ensure there is data to plot

    station_id <- selected_station_id()
    month <- effective_month()
    is_temp <- is_temp_param(effective_param())
    y_label <- if (is_temp) "Temperature (°C)" else "Precipitation (mm)"
    title <- if (is_temp) "Monthly Mean Temperature" else "Total Precipitation"

    render_time_series_plot(data, station_id, month, y_label, title)
  })

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
        pending_map_fly(NULL)
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
      slugify_ghcnm <- function(x) {
        s <- tolower(iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
        s <- gsub("[^a-z0-9]+", "-", s)
        gsub("^-+|-+$", "", s)
      }

      find_station_in_df <- function(df, station_ref, country_ref = NULL) {
        if (is.null(df) || nrow(df) == 0 || is.null(station_ref) || !nzchar(trimws(station_ref))) {
          return(df[0, , drop = FALSE])
        }
        ref <- trimws(as.character(station_ref[1]))
        match_pool <- function(pool) {
          if (is.null(pool) || nrow(pool) == 0) return(pool[0, , drop = FALSE])
          m <- pool[toupper(trimws(as.character(pool$ID))) == toupper(ref), , drop = FALSE]
          if (nrow(m) > 0) return(m)
          m <- pool[tolower(trimws(as.character(pool$NAME))) == tolower(ref), , drop = FALSE]
          if (nrow(m) > 0) return(m)
          ref_slug <- slugify_ghcnm(ref)
          if (nzchar(ref_slug)) {
            pool_slugs <- vapply(pool$NAME, slugify_ghcnm, character(1))
            m <- pool[pool_slugs == ref_slug, , drop = FALSE]
            if (nrow(m) > 0) return(m)
          }
          pool[0, , drop = FALSE]
        }

        if (!is.null(country_ref) && nzchar(trimws(as.character(country_ref[1]))) && "Country" %in% names(df)) {
          c_ref <- trimws(as.character(country_ref[1]))
          c_slug <- slugify_ghcnm(c_ref)
          c_mask <- tolower(trimws(as.character(df$Country))) == tolower(c_ref) |
            vapply(df$Country, slugify_ghcnm, character(1)) == c_slug
          res_c <- match_pool(df[which(c_mask), , drop = FALSE])
          if (nrow(res_c) > 0) return(res_c)
        }
        match_pool(df)
      }

      # 1. Parameter
      req_param <- if (!is.null(params$parameter) && nzchar(trimws(as.character(params$parameter[1])))) {
        if (is_temp_param(params$parameter[1])) "Temperature" else if (identical(trimws(as.character(params$parameter[1])), "Precipitation")) "Precipitation" else "Temperature"
      } else {
        "Temperature"
      }

      # 2. Month
      if (!is.null(params$month) && params$month[1] %in% month.name) {
        req_month <- as.character(params$month[1])
        if (!identical(req_month, "January")) {
          arm_token_rv(pending_url_month, url_month_token, req_month, NULL, 3.0)
        }
        updateSelectInput(session, "month", selected = req_month)
      }

      # 3. Resolve station first (switching to Precipitation automatically for precip-only stations)
      matched_row <- NULL
      if (!is.null(params$station) && nzchar(trimws(as.character(params$station[1])))) {
        primary_df <- if (is_temp_param(req_param)) stations_data else prec_stations_data
        other_df <- if (is_temp_param(req_param)) prec_stations_data else stations_data
        m_primary <- find_station_in_df(primary_df, params$station, params$country)
        if (nrow(m_primary) > 0) {
          matched_row <- m_primary[1, , drop = FALSE]
        } else {
          m_other <- find_station_in_df(other_df, params$station, params$country)
          if (nrow(m_other) > 0) {
            matched_row <- m_other[1, , drop = FALSE]
            req_param <- if (is_temp_param(req_param)) "Precipitation" else "Temperature"
          }
        }
      }

      if (!identical(req_param, "Temperature")) {
        arm_token_rv(pending_url_param, url_param_token, req_param, NULL, 3.0)
      }
      updateSelectInput(session, "parameter", selected = req_param)

      # 4. Year Range
      max_yr_ui <- as.integer(max(stations_data$last_year, na.rm = TRUE))
      def_years <- c(1961L, max_yr_ui)
      req_years <- NULL
      if (!is.null(params$start) && !is.null(params$end)) {
        y1 <- suppressWarnings(as.integer(params$start))
        y2 <- suppressWarnings(as.integer(params$end))
        if (!is.na(y1) && !is.na(y2)) {
          if (y1 > y2) {
            tmp <- y1; y1 <- y2; y2 <- tmp
          }
          req_years <- c(y1, y2)
        }
      } else if (!is.null(matched_row) && nrow(matched_row) > 0) {
        s_y1 <- suppressWarnings(as.integer(matched_row$first_year[1]))
        s_y2 <- suppressWarnings(as.integer(matched_row$last_year[1]))
        if (!is.na(s_y1) && !is.na(s_y2) && s_y1 <= s_y2) {
          req_years <- c(s_y1, s_y2)
        }
      }

      if (!is.null(req_years)) {
        if (!identical(as.integer(req_years), def_years)) {
          arm_token_rv(pending_url_years, url_years_token, req_years, NULL, 3.0)
        }
        updateSliderInput(session, "year_range", value = req_years)
      }

      # 5. Station Selection & deferred map centering
      if (!is.null(matched_row) && nrow(matched_row) > 0) {
        selected_station_id(matched_row$ID[1])
        lon_val <- suppressWarnings(as.numeric(matched_row$LONGITUDE[1]))
        lat_val <- suppressWarnings(as.numeric(matched_row$LATITUDE[1]))
        if (is.finite(lon_val) && is.finite(lat_val)) {
          pending_map_fly(list(lon = lon_val, lat = lat_val))
        }
      }
    }
    url_initialized(TRUE)
  })

  # Helper: Broadcast current state to parent page
  broadcast_state <- function() {
    sid <- selected_station_id()
    eff_param <- effective_param()
    eff_month <- effective_month()
    eff_years <- effective_year_range()
    st_meta <- lookup_station_meta(sid, eff_param)

    station_id <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$ID) else NULL
    station_name <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$NAME) else NULL
    country <- if (!is.null(st_meta) && nrow(st_meta) > 0) as.character(st_meta$Country) else NULL

    session$sendCustomMessage("updateParentURL", list(
      station = station_id,
      stationName = station_name,
      country = country,
      parameter = eff_param,
      month = eff_month,
      yearStart = eff_years[1],
      yearEnd = eff_years[2]
    ))
  }

  # Observer to broadcast state on any relevant change
  observe({
    # Depend on all state variables that should trigger a broadcast
    sid <- selected_station_id()
    param <- effective_param()
    month <- effective_month()
    year_range <- effective_year_range()

    req(url_initialized())

    # Broadcast the current state
    st_meta <- lookup_station_meta(sid, param)

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
  plot_magnified <- reactiveVal(FALSE)

  observeEvent(input$toggle_magnify, {
    plot_magnified(!plot_magnified())
  })

  observeEvent(input$escape_key_pressed, {
    if (plot_magnified()) {
      plot_magnified(FALSE)
    }
  })

  output$plot_panel <- renderUI({
    if (plot_available()) {
      is_magnified <- plot_magnified()

      absolutePanel(
        draggable = F,
        top = if(is_magnified) "50%" else "auto",
        bottom = if(is_magnified) "auto" else 30, # Position from the bottom
        left = "50%", # Start from the middle of the screen
        right = "auto",
        width = if(is_magnified) "90%" else "95%",
        height = "auto",
        style = if(is_magnified) "transform: translate(-50%, -50%); max-width: 1000px; background-color: rgba(255, 255, 255, 0.95); border-radius: 10px; padding: 20px; box-shadow: 0px 0px 20px rgba(0,0,0,0.5); z-index: 1050;" else "transform: translateX(-50%); max-width: 450px; background-color: rgba(255, 255, 255, 0.8); border-radius: 10px; padding: 10px;", # Transparent background with some styling

        # Time series plot
        plotlyOutput("time_series_plot", height = if(is_magnified) "60vh" else "200px"),

        # Download button positioned right below the plot, aligned to the left
        downloadButton(
          outputId = "download_data",
          label = NULL, # No text label
          icon = icon("download"), # Font Awesome download icon
          class = "custom-download-button", # Custom CSS class for styling
          style = "float: left; margin-top: -10px;" # Aligns the button to the left and positions it below the plot
        ) |>
          tooltip("Download data"), # bslib data
          
        # Magnify button positioned right below the plot, aligned to the right
        actionButton(
          inputId = "toggle_magnify",
          label = NULL,
          icon = if(is_magnified) icon("compress") else icon("expand"),
          style = "float: right; margin-top: -10px; padding: 4px 10px;"
        ) |>
          tooltip(if(is_magnified) "Restore plot" else "Magnify plot")
      )
    }
  })


  # Render Map Legend
  output$map_legend <- renderUI({
    param <- effective_param()

    # Define legend properties matching the map logic
    if (is_temp_param(param)) {
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
      month <- effective_month()
      info <- if (is_temp_param(effective_param())) tavg_meta else prec_meta
      st_name <- info$NAME[info$ID == station_id][1]
      if (is.na(st_name) || !nzchar(st_name)) {
        alt <- if (is_temp_param(effective_param())) prec_meta else tavg_meta
        st_name <- alt$NAME[alt$ID == station_id][1]
      }
      paste0(st_name, "_", station_id, "_", month, ".csv")
    },
    content = function(file) {
      # Get the time series data for the clicked station
      data <- time_series_data()

      # Write the data to a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
}
