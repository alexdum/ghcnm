evaluate_ghcnm_trend <- function(data, min_years = 24L, min_span = 30L, min_completeness = 0.80, max_gap_years = 10L) {
  valid_data <- data[!is.na(data$YEAR) & !is.na(data$VALUE) & is.finite(data$VALUE), , drop = FALSE]
  valid_data$YEAR <- as.integer(valid_data$YEAR)
  valid_data <- valid_data[order(valid_data$YEAR), , drop = FALSE]

  valid_years <- sort(unique(valid_data$YEAR))
  n_valid <- length(valid_years)
  if (n_valid == 0L) {
    return(list(
      valid_data = valid_data,
      n_valid = 0L,
      year_min = NA_integer_,
      year_max = NA_integer_,
      obs_span = 0L,
      completeness = 0,
      max_gap = NA_integer_,
      eligible = FALSE,
      linear_model = NULL,
      slope_yr = NA_real_,
      slope_decade = NA_real_
    ))
  }

  year_min <- min(valid_years)
  year_max <- max(valid_years)
  obs_span <- year_max - year_min + 1L
  completeness <- n_valid / obs_span
  max_gap <- if (n_valid >= 2L) max(diff(valid_years) - 1L) else 0L

  eligible <- isTRUE(
    n_valid >= min_years &&
      obs_span >= min_span &&
      completeness >= min_completeness &&
      max_gap <= max_gap_years
  )

  linear_model <- NULL
  slope_yr <- NA_real_
  slope_decade <- NA_real_
  if (eligible) {
    linear_model <- lm(VALUE ~ YEAR, data = valid_data)
    slope_yr <- unname(coef(linear_model)["YEAR"])
    slope_decade <- slope_yr * 10
  }

  list(
    valid_data = valid_data,
    n_valid = n_valid,
    year_min = year_min,
    year_max = year_max,
    obs_span = obs_span,
    completeness = completeness,
    max_gap = max_gap,
    eligible = eligible,
    linear_model = linear_model,
    slope_yr = slope_yr,
    slope_decade = slope_decade
  )
}

extract_ghcnm_unit <- function(y_label) {
  m <- regmatches(y_label, regexpr("\\(([^)]+)\\)", y_label))
  if (length(m) == 1L && nzchar(m)) {
    return(trimws(gsub("^\\(|\\)$", "", m)))
  }
  ""
}

render_time_series_plot <- function(data, station_id, month, y_label = "Temperature (°C)", title_prefix = "Monthly Mean Temperature") {
  station_name <- NA_character_
  if (exists("tavg_meta") && station_id %in% tavg_meta$ID) {
    station_name <- tavg_meta$NAME[tavg_meta$ID == station_id][1]
  } else if (exists("prec_meta") && station_id %in% prec_meta$ID) {
    station_name <- prec_meta$NAME[prec_meta$ID == station_id][1]
  }

  if (is.na(station_name) || !nzchar(station_name)) {
    station_name <- "Unknown Station"
  }

  trend_info <- evaluate_ghcnm_trend(data)
  valid_data <- trend_info$valid_data

  # Complete the series with missing years so Plotly breaks across data gaps
  if (nrow(valid_data) > 0L) {
    all_years <- data.frame(YEAR = seq.int(trend_info$year_min, trend_info$year_max))
    plot_data <- merge(all_years, valid_data, by = "YEAR", all.x = TRUE)
  } else {
    plot_data <- valid_data
  }

  if (trend_info$eligible) {
    plot_data$Trend <- predict(trend_info$linear_model, newdata = plot_data)
  }

  title_text <- if (nrow(valid_data) > 0L) {
    paste0(title_prefix, " - ", month, " ", trend_info$year_min, "–", trend_info$year_max, " - ", station_name, " ", station_id)
  } else {
    paste0(title_prefix, " - ", month, " - ", station_name, " ", station_id)
  }

  # Simple heuristic: if y_label contains "Precipitation", use blue, else red.
  line_color <- if (grepl("Precipitation", y_label)) "blue" else "red"
  unit_str <- extract_ghcnm_unit(y_label)
  unit_suffix <- if (nzchar(unit_str)) paste0(" ", unit_str) else ""
  mean_val <- if (nrow(valid_data) > 0L) round(mean(valid_data$VALUE, na.rm = TRUE), 1) else NA_real_

  annotation_text <- if (trend_info$eligible) {
    paste0(
      "Slope: ", sprintf("%+.2f", trend_info$slope_decade), unit_suffix, "/decade (",
      sprintf("%+.3f", trend_info$slope_yr), unit_suffix, "/yr) | Mean: ",
      mean_val, unit_suffix
    )
  } else {
    paste0(
      "Trend suppressed (requires >=24 yrs, >=30-yr span, >=80% coverage, <=10-yr gap; ",
      trend_info$n_valid, " yrs, ", round(trend_info$completeness * 100), "% coverage) | Mean: ",
      mean_val, unit_suffix
    )
  }

  # Main plot
  plot <- plot_ly(plot_data,
    x = ~YEAR, y = ~VALUE, type = "scatter", mode = "lines",
    name = "Value", # Set neutral name
    line = list(color = line_color),
    connectgaps = FALSE
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
      margin = list(l = 20, r = 5, t = 20, b = 30), # Increase bottom margin to accommodate text
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
          y = 0, # Position it at the bottom of the plot area
          yshift = -20, # Shift it down by 20 pixels
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          text = annotation_text,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 11, color = "black")
        )
      )
    )

  if (trend_info$eligible) {
    plot <- plot %>%
      add_trace(
        x = ~YEAR,
        y = ~Trend,
        mode = "lines",
        name = "Linear Trend",
        line = list(color = "gray"),
        connectgaps = FALSE,
        hoverinfo = "x+y" # Display only x and y without custom text
      )
  }


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
