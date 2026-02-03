render_time_series_plot <- function(data, station_id, month, y_label = "Temperature (°C)", title_prefix = "Daily Mean Temperature") {
  # determine station name from one of the metadata files
  # Since this is a utility function, it might not see tavg_meta easily if not in global scope or passed.
  # server.R sources this with local=T, so it sees tavg_meta.
  # But we should look in both or pass the name.
  # For simplicity, let's try to find it in tavg_meta first, then prec_meta.

  station_name <- NA
  if (station_id %in% tavg_meta$ID) {
    station_name <- tavg_meta$NAME[tavg_meta$ID == station_id]
  } else if (exists("prec_meta") && station_id %in% prec_meta$ID) {
    station_name <- prec_meta$NAME[prec_meta$ID == station_id]
  }

  if (is.na(station_name)) {
    # Fallback if both fail or only one loaded yet
    # stop("Station ID not found in metadata.")
    station_name <- "Unknown Station"
  }

  # Fit a linear model to calculate the slope
  linear_model <- lm(VALUE ~ YEAR, data = data)
  slope <- coef(linear_model)["YEAR"]

  # Create the title with slope
  title_text <- paste(
    month, min(data$YEAR), max(data$YEAR), "-", station_name, station_id
  )

  # Determine color based on parameter type (inferred from label)
  # Simple heuristic: if y_label contains "Precipitation", use blue, else red.
  line_color <- if (grepl("Precipitation", y_label)) "blue" else "red"

  # Main plot
  plot <- plot_ly(data,
    x = ~YEAR, y = ~VALUE, type = "scatter", mode = "lines",
    name = "Value", # Set neutral name
    line = list(color = line_color)
  ) %>%
    layout(
      title = list(
        text = title_text,
        x = 0,
        y = 0.99,
        xanchor = "left",
        font = list(size = 12)
      ),
      xaxis = list(
        zeroline = FALSE,
        gridcolor = "lightgray",
        title = "",
        fixedrange = TRUE
      ),
      yaxis = list(
        title = list(text = paste(month, y_label), font = list(size = 10)),
        zeroline = FALSE,
        gridcolor = "lightgray",
        fixedrange = TRUE
      ),
      showlegend = FALSE,
      plot_bgcolor = "rgba(255, 255, 255, 0)",
      paper_bgcolor = "rgba(255, 255, 255, 0)",
      margin = list(l = 20, r = 5, t = 20, b = 3), # Increase bottom margin to accommodate text
      shapes = list(
        list(
          type = "rect",
          x0 = 0, x1 = 1, y0 = 0, y1 = 1,
          xref = "paper", yref = "paper",
          fillcolor = "rgba(255, 255, 255, 0)",
          line = list(width = 0)
        )
      ),
      hovermode = "x unified",
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "rgba(255, 255, 255, 0)"
      ),
      annotations = list(
        list(
          x = 0.5, # Center the text horizontally
          y = -0.12, # Position it further below the plot
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          text = paste("Slope:", round(slope, 3), units = strsplit(y_label, " ")[[1]][2], "/year | Mean: ", round(mean(data$VALUE, na.rm = TRUE), 1), strsplit(y_label, " ")[[1]][2]),
          xanchor = "center",
          yanchor = "top",
          font = list(size = 11, color = "black")
        )
      )
    ) %>%
    add_trace(
      x = ~YEAR,
      y = fitted(linear_model),
      mode = "lines",
      name = "Linear Trend",
      line = list(color = "gray"),
      hoverinfo = "x+y" # Display only x and y without custom text
    )


  # Configure the mode bar to show only the download button
  plot <- plot %>%
    config(
      modeBarButtonsToRemove = list(
        "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"
      ),
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = "png", # Choose your desired format (png, jpeg, svg, webp)
        filename = paste0(station_id, "_", month, "_time_series")
      )
    )

  return(plot)
}
