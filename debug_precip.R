library(arrow)
library(dplyr)

# Load data
tavg_dataset <- open_dataset("www/data/tabs/tavg_long.parquet")
prec_dataset <- open_dataset("www/data/tabs/prec_long.parquet")

prec_meta <- read.csv("www/data/tabs/prec_meta.csv") %>%
    rename(
        ID = GHCN_ID,
        LATITUDE = Latitude,
        LONGITUDE = Longitude,
        STNELEV = Elevation,
        NAME = Station_Name
    )
prec_avail <- read.csv("www/data/tabs/prec_availability.csv")
prec_stations_data <- merge(prec_meta, prec_avail, by = "ID")

print(paste("Prec Meta Rows:", nrow(prec_meta)))
print(paste("Prec Avail Rows:", nrow(prec_avail)))
print(paste("Prec Stations Data Rows:", nrow(prec_stations_data)))

# Simulate Inputs
year_range <- c(2000, 2010)
month_number <- 1 # January

# Test Filter Parquet
print("Filtering Parquet...")
filtered_data <- prec_dataset %>%
    filter(
        VALUE >= -90,
        YEAR >= year_range[1],
        YEAR <= year_range[2],
        MONTH == month_number
    ) %>%
    group_by(ID) %>%
    summarize(mean_value = mean(VALUE, na.rm = TRUE)) %>%
    collect()

print(paste("Filtered Data Rows:", nrow(filtered_data)))
if (nrow(filtered_data) > 0) {
    print(head(filtered_data))
}

# Test Filter Stations
print("Filtering Stations...")
stations_result <- prec_stations_data %>%
    filter(
        first_year <= year_range[1],
        last_year >= year_range[2],
        ID %in% filtered_data$ID
    ) %>%
    left_join(filtered_data, by = "ID")

print(paste("Stations Result Rows:", nrow(stations_result)))
if (nrow(stations_result) > 0) {
    print(head(stations_result))
} else {
    print("No stations found after filtering.")
    print("Check if filtered_data IDs match prec_stations_data IDs.")

    # Check intersection
    common_ids <- intersect(filtered_data$ID, prec_stations_data$ID)
    print(paste("Common IDs:", length(common_ids)))

    if (length(common_ids) > 0) {
        print("There are common IDs, so the issue might be first_year/last_year filter.")
        print(head(prec_stations_data[prec_stations_data$ID %in% common_ids, ]))
    }
}
