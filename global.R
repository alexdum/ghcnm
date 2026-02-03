# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(leaflet.extras)
library(arrow) # For reading Parquet files
library(scales) # For color scaling
library(plotly)
library(bsicons)


source("utils/render_time_series_plot.R", local = T)
source("utils/generateLabel.R", local = T)

# Read the metadata and availability data
tavg_meta <- read.csv("www/data/tabs/tavg_meta.csv")
tavg_avail <- read.csv("www/data/tabs/tavg_vaialability.csv")

# Merge the metadata and availability data on 'ID'
stations_data <- merge(tavg_meta, tavg_avail, by = "ID")

# Open the Parquet dataset using arrow
tavg_dataset <- open_dataset("www/data/tabs/tavg_long.parquet")

# Read country codes
country_codes <- read.table("www/data/tabs/ghcnd-countries.txt", fill = TRUE, quote = "", comment.char = "")
# The file has code in first column and name in the rest.
# We need to parse it properly. simpler to just use fixed widths or just take the first 2 chars as code.
# The previous read.table might not be perfect because names have spaces.
# Let's use readLines and strict parsing for safety or read.fwf if we knew widths, but text suggests space separation.
# Actually, the file format from NOAA is: Code (2 chars) + Space + Name.
stations_data$CountryCode <- substr(stations_data$ID, 1, 2)

# Parse country codes file more robustly
country_lines <- readLines("www/data/tabs/ghcnd-countries.txt")
country_codes_df <- data.frame(
    CountryCode = substr(country_lines, 1, 2),
    Country = trimws(substr(country_lines, 4, nchar(country_lines))),
    stringsAsFactors = FALSE
)

stations_data <- stations_data %>%
    left_join(country_codes_df, by = "CountryCode")
