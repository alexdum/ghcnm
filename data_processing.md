# GHCNM Data Processing Scripts

This document provides an overview of the Python scripts used to download, process, and format the Global Historical Climatology Network Monthly (GHCNM) datasets for the application.

## 1. `ghcnm_read_prec.py` (Precipitation Data)

**Purpose:**  
Downloads the latest GHCNM v4 precipitation archive, parses the station files concurrently, applies quality control filters, and outputs the combined dataset into a compressed Parquet format suitable for the web application.

**Workflow Details:**
1. **Download & Extract:** 
   - Scrapes the NOAA NCEI precipitation archive directory to find the latest `.tar.gz` file.
   - Downloads and extracts the fixed-width format `.csv` files into a temporary `misc/data/pp` directory.
2. **Concurrent Processing:** 
   - Uses `concurrent.futures.ProcessPoolExecutor` utilizing 75% of available CPU cores to process thousands of station files in parallel, preventing system freezing.
3. **Data Cleaning & Transformation:**
   - Parses the fixed-width formatted text files into a pandas DataFrame.
   - Splits the `year_month` string into separate `year` and `month` columns.
   - Converts precipitation values from tenths of millimeters to millimeters.
   - Handles "Trace Precipitation" records (flagged as `-1`) by converting them to `0` mm.
4. **Quality Control & Filtering:**
   - Drops rows containing bad quality flags (`O`, `R`, `T`, `S`, `K`).
   - Skips stations entirely if they have fewer than 120 valid records (10 years of data).
   - Skips stations entirely if they contain extreme, likely erroneous outliers (precipitation > 2000 mm in a single month).
5. **Output:**
   - Combines everything into a long-format dataset (`ID`, `YEAR`, `MONTH`, `VALUE`).
   - Saves the main dataset as `www/data/tabs/prec_long.parquet`.
   - Generates an availability summary (`first_year` and `last_year` per station) and saves it to `www/data/tabs/prec_availability.csv`.

---

## 2. `ghcnm_read_tavg.py` (Average Temperature Data)

**Purpose:**  
Downloads the latest GHCNM v4 average temperature dataset, parses both the data (`.dat`) and inventory/metadata (`.inv`) files, converts the data from a wide matrix to a long format, and cleans up the temporary files.

**Workflow Details:**
1. **Download & Extract:**
   - Directly downloads the `ghcnm.tavg.latest.qcf.tar.gz` archive from NOAA.
   - Extracts the contents to `misc/data`.
2. **Data Processing (`.dat` file):**
   - Finds the `.dat` file and parses the fixed-width format.
   - The raw data is in a "wide" format containing 12 columns for months (`VALUE1` to `VALUE12`).
   - Converts temperature values to Celsius (dividing by 100) and handles missing values (`-9999`).
   - Uses `pd.wide_to_long` to transform the data into a long, tidy format (`ID`, `YEAR`, `MONTH`, `VALUE`).
3. **Metadata Processing (`.inv` file):**
   - Finds the `.inv` inventory file to extract station metadata.
   - Parses fixed-width columns for `ID`, `LATITUDE`, `LONGITUDE`, `STNELEV` (elevation), and `NAME`.
   - Replaces missing elevation values (`-999.0`) with `NaN`.
4. **Output:**
   - Saves the main temperature dataset to `www/data/tabs/tavg_long.parquet`.
   - Generates an availability summary (`first_year` and `last_year` per station) and saves it to `www/data/tabs/tavg_availability.csv`.
   - Saves the parsed station metadata to `www/data/tabs/tavg_meta.csv`.
5. **Cleanup:**
   - Automatically removes all downloaded folders and extracted files in `misc/data/` using `glob` and `shutil` to free up disk space.