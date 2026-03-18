# Define the UI using bslib's page_sidebar layout
ui <- page_navbar(
  theme = bs_theme(version = 5),
  title = "GHCNm Explorer",
  navbar_options = navbar_options(collapsible = TRUE),
  header = tagList(
    tags$head(
      shinyjs::useShinyjs(),
      tags$link(rel = "canonical", href = "https://climate-insights.netlify.app/ghcnm"),
      tags$meta(name = "robots", content = "noindex,indexifembedded"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css?v=1.1"),
      tags$script(src = "fullscreen.js"),
      tags$script(src = "app.js?v=1.1")
    ),
    # Frozen overlay div (startup loading spinner)
    div(
      class = "frozen-overlay",
      div(
        class = "frozen-overlay-content",
        div(class = "spinner-border text-primary", role = "status"),
        p(class = "frozen-overlay-message", "Loading stations...")
      )
    )
  ),
  fillable_mobile = T,
  sidebar = sidebar(
    title = "Filters",
    open = list(desktop = "open", mobile = "closed"),
    # Slider for selecting a range of years (first_year and last_year)
    sliderInput("year_range", "Select Year Range:",
      min = 1750,
      max = max(stations_data$last_year, na.rm = TRUE),
      value = c(1961, max(stations_data$last_year, na.rm = TRUE)),
      step = 1,
      sep = ""
    ),

    # Select input for parameter
    selectInput("parameter", "Select Parameter:",
      choices = c("Temperature", "Precipitation"),
      selected = "Temperature"
    ),

    # Select input for choosing a month
    selectInput("month", "Select Month:",
      choices = month.name, selected = month.name[1]
    ),
    hr(),
    p("Data source: NCEI GHCN monthly"),
    markdown(paste("**Data available until:**", latest_data_label)),
    hr(),
    tags$small("Tip: Click map points to view detailed weather plots.")
  ),
  nav_panel(
    "Data Explorer",
    div(
      style = "position: relative; height: 100%; width: 100%;",
      maplibreOutput("station_map", width = "100%", height = "100%"),
      # Layer Control Panel (Collapsible)
      absolutePanel(
        top = 130, left = 10, right = "auto",
        class = "map-layer-control",
        style = "z-index: 1000;",
        div(class = "control-icon", icon("layer-group")),
        div(
          class = "control-content",
          radioButtons(
            inputId = "basemap",
            label = "Basemap",
            choices = c(
              "OpenFreeMap Positron" = "ofm_positron",
              "OpenFreeMap Bright" = "ofm_bright",
              "Satellite (Sentinel-2)" = "sentinel"
            ),
            selected = "ofm_positron"
          ),
          hr(style = "margin: 8px 0;"),
          checkboxInput(
            inputId = "show_labels",
            label = "Show Labels",
            value = TRUE
          )
        )
      ),

      # Home Button Overlay
      absolutePanel(
        id = "zoom_home_panel",
        top = 80, left = 10, right = "auto",
        actionButton("home_zoom", bsicons::bs_icon("house-fill"), class = "btn-home", title = "Zoom to all stations")
      ),
      # Map Legend
      uiOutput("map_legend"),

      # Conditionally render the panel with the plot when the plotly output is available
      uiOutput("plot_panel")
    )
  ),
  nav_spacer(),
  nav_item(
    tooltip(
      tags$a(
        id = "fullscreen_toggle",
        href = "#",
        onclick = "toggleFullScreen(); return false;",
        bsicons::bs_icon("arrows-fullscreen", size = "1.2rem")
      ),
      "Toggle Fullscreen",
      placement = "bottom"
    )
  )
)
