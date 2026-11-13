# Function to generate labels
generateLabel <- function(name, id, elevation, first_year, last_year, mean_value, selected_years, country, value_label_prefix = "Mean Temp:", units = "°C", n_years = NULL) {
  span_years <- max(1L, as.integer(selected_years[2]) - as.integer(selected_years[1]) + 1L)
  selected_years_text <- if (!is.null(n_years) && !is.na(n_years)) {
    paste0(selected_years[1], " - ", selected_years[2], " (", n_years, "/", span_years, " yrs)")
  } else {
    paste0(selected_years[1], " - ", selected_years[2])
  }
  paste(
    "Station:", name, "<br>",
    "Country:", country, "<br>",
    "ID:", id, "<br>",
    "Elevation:", elevation, "m<br>",
    "Available years:", first_year, "-", last_year, "<br>",
    "Selected years:", selected_years_text, "<br>",
    value_label_prefix, round(mean_value, 1), units,
    "<br><span style='color:red;'>Click to get graph and data</span>"
  ) %>%
    lapply(htmltools::HTML)
}
