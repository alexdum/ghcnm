# Function to generate labels
generateLabel <- function(name, id, elevation, first_year, last_year, mean_value, selected_years, country, value_label_prefix = "Mean Temp:", units = "°C") {
  paste(
    "Station:", name, "<br>",
    "Country:", country, "<br>",
    "ID:", id, "<br>",
    "Elevation:", elevation, "m<br>",
    "Available years:", first_year, "-", last_year, "<br>",
    "Selected years:", selected_years[1], "-", selected_years[2], "<br>",
    value_label_prefix, round(mean_value, 1), units,
    "<br><span style='color:red;'>Click to get graph and data</span>"
  ) %>%
    lapply(htmltools::HTML)
}
