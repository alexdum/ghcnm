# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(leaflet.extras)
library(arrow) # For reading Parquet files
library(scales) # For color scaling

# Read the metadata and availability data
tavg_meta <- read.csv("www/data/tabs/tavg_meta.csv")
tavg_avail <- read.csv("www/data/tabs/tavg_vaialability.csv")

# Merge the metadata and availability data on 'ID'
meta <- merge(tavg_meta, tavg_avail, by = "ID")

# Open the Parquet dataset using arrow
tavg_dataset <- open_dataset("www/data/tabs/tavg_long.parquet")