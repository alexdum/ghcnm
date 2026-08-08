# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(arrow) # For reading Parquet files
library(plotly)
library(bsicons)
library(shinyjs)


source("utils/render_time_series_plot.R", local = T)
source("utils/generateLabel.R", local = T)
library(mapgl)
library(sf)

# OpenFreeMap Style URLs
ofm_positron_style <- "https://tiles.openfreemap.org/styles/positron"
ofm_bright_style <- "https://tiles.openfreemap.org/styles/bright"

# EOX Sentinel-2 Cloudless (Free for commercial use with attribution)
sentinel_url <- "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless-2023_3857/default/GoogleMapsCompatible/{z}/{y}/{x}.jpg"
sentinel_attribution <- '<a href="https://s2maps.eu" target="_blank">Sentinel-2 cloudless - by EOX IT Services GmbH</a> (Contains modified Copernicus Sentinel data 2023)'

# Define custom raster styles for OSM and Esri
osm_style <- list(
    version = 8,
    sources = list(
        osm = list(
            type = "raster",
            tiles = list("https://a.tile.openstreetmap.org/{z}/{x}/{y}.png"),
            tileSize = 256,
            attribution = "&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors"
        )
    ),
    layers = list(
        list(
            id = "osm",
            type = "raster",
            source = "osm"
        )
    )
)

esri_style <- list(
    version = 8,
    sources = list(
        esri = list(
            type = "raster",
            tiles = list("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"),
            tileSize = 256,
            attribution = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
        )
    ),
    layers = list(
        list(
            id = "esri",
            type = "raster",
            source = "esri"
        )
    )
)

# Read the metadata and availability data
tavg_meta <- read.csv("www/data/tabs/tavg_meta.csv")
tavg_avail <- read.csv("www/data/tabs/tavg_availability.csv")

# Merge the metadata and availability data on 'ID'
stations_data <- merge(tavg_meta, tavg_avail, by = "ID")

# Open the Parquet dataset using arrow
# Open the Parquet dataset using arrow
tavg_dataset <- open_dataset("www/data/tabs/tavg_long.parquet")
prec_dataset <- open_dataset("www/data/tabs/prec_long.parquet")

# Determine the latest available date for Temperature
latest_year_tavg <- tavg_dataset %>%
    filter(VALUE != -99.99) %>%
    summarize(max_y = max(YEAR, na.rm = TRUE)) %>%
    collect() %>%
    pull(max_y)

latest_month_tavg <- tavg_dataset %>%
    filter(YEAR == latest_year_tavg, VALUE != -99.99) %>%
    summarize(max_m = max(MONTH, na.rm = TRUE)) %>%
    collect() %>%
    pull(max_m)

# Determine the latest available date for Precipitation
latest_year_prec <- prec_dataset %>%
    filter(VALUE != -99.99) %>%
    summarize(max_y = max(YEAR, na.rm = TRUE)) %>%
    collect() %>%
    pull(max_y)

latest_month_prec <- prec_dataset %>%
    filter(YEAR == latest_year_prec, VALUE != -99.99) %>%
    summarize(max_m = max(MONTH, na.rm = TRUE)) %>%
    collect() %>%
    pull(max_m)

# Format for UI display showing both
latest_data_label <- paste0(
    month.name[latest_month_tavg], " ", latest_year_tavg, " (Temp) | ",
    month.name[latest_month_prec], " ", latest_year_prec, " (Precip)"
)

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


# Read the precipitation metadata and availability data
# Read the precipitation metadata and availability data
prec_meta <- read.csv("www/data/tabs/prec_meta.csv")
# Rename columns to match tavg_meta convention for consistent server logic
prec_meta <- prec_meta %>%
    rename(
        ID = GHCN_ID,
        LATITUDE = Latitude,
        LONGITUDE = Longitude,
        STNELEV = Elevation,
        NAME = Station_Name
    )

prec_avail <- read.csv("www/data/tabs/prec_availability.csv")

# Merge the metadata and availability data on 'ID'
prec_stations_data <- merge(prec_meta, prec_avail, by = "ID")

prec_stations_data$CountryCode <- substr(prec_stations_data$ID, 1, 2)
prec_stations_data <- prec_stations_data %>%
    left_join(country_codes_df, by = "CountryCode")
