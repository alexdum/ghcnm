# Define the UI using bslib's page_sidebar layout
ui <- page_navbar(
  theme = bs_theme(version = 5),
  title = "GHCNm Explorer",
  navbar_options = navbar_options(collapsible = TRUE),
  header = tags$head(
    shinyjs::useShinyjs(),
    tags$link(rel = "canonical", href = "https://climate-insights.netlify.app/ghcnm"),
    tags$meta(name = "robots", content = "noindex,indexifembedded"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css?v=1.0"), # Link to custom CSS
    tags$script(src = "fullscreen.js"),
    tags$script(src = "app.js?v=1.0")
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


    div(
      style = "position: relative; height: 100%; width: 100%;",
      maplibreOutput("station_map", width = "100%", height = "100%"),
      # Layer Control (Below Navigation Control)
      absolutePanel(
        top = 160, right = 10, left = "auto",
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
      # Home Zoom Button (Below Navigation and above Layer Control)
      absolutePanel(
        top = 110, right = 10, left = "auto",
        style = "z-index: 1000;",
        actionButton(
          inputId = "home_zoom",
          label = NULL,
          icon = icon("house"),
          style = "background-color: white; border: none; border-radius: 4px; box-shadow: 0 0 5px rgba(0,0,0,0.3); width: 36px; height: 36px; padding: 0; color: #333;"
        )
      ),
      # Map Legend
      uiOutput("map_legend")
    ),
    absolutePanel(
      top = 80, left = 30, right = "auto", bottom = "auto",
      width = 150, height = "auto",
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
    ),

    # Conditionally render the panel with the plot when the plotly output is available
    # Use uiOutput to conditionally render the plot panel
    uiOutput("plot_panel")
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
