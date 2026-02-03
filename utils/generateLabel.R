# Function to generate labels
generateLabel <- function(name, id, elevation, first_year, last_year, mean_temp, selected_years, country) {
  paste(
    "Station:", name, "<br>",
    "Country:", country, "<br>",
    "ID:", id, "<br>",
    "Elevation:", elevation, "m<br>",
    "Available years:", first_year, "-", last_year, "<br>",
    "Selected years:", selected_years[1], "-", selected_years[2], "<br>",
    "Mean Temp:", round(mean_temp, 1), "°C",
    "<br><span style='color:red;'>Click to get graph and data</span>"
  ) %>%
    lapply(htmltools::HTML)
}
