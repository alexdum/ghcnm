import requests
from bs4 import BeautifulSoup
import os
from urllib.request import urlretrieve
import tarfile
import glob
import pandas as pd
from pandas.errors import EmptyDataError, ParserError



# URL of the archive directory
BASE_URL = 'https://www.ncei.noaa.gov/data/ghcnm/v4/precipitation/archive/'

# Create a session
session = requests.Session()

# Get the HTML content of the archive page
response = session.get(BASE_URL)
response.raise_for_status()  # Raise an error for bad status

# Parse HTML with BeautifulSoup
soup = BeautifulSoup(response.text, 'html.parser')

# Find all .tar.gz files
tar_links = [a['href'] for a in soup.find_all('a', href=True) if a['href'].endswith('.tar.gz')]

file_url =  BASE_URL + tar_links[0]

filename = os.path.join('misc/data', tar_links[0])
urlretrieve(file_url, filename)


with tarfile.open(filename, 'r:gz') as tar:
    tar.extractall(path='misc/data/pp')



### read csv file
# https://www.ncei.noaa.gov/data/ghcnm/v4/precipitation/doc/ghcn-m_v4_prcp_readme.txt
colspecs = [
    (0, 11),    # 1. GHCN identifier (columns 1-11)
    (12, 52),   # 2. Station name (columns 13-52)
    (53, 62),   # 3. Latitude (columns 54-62)
    (63, 73),   # 4. Longitude (columns 64-73)
    (74, 82),   # 5. Elevation (meters) (columns 75-82)
    (83, 89),   # 6. Year and month (columns 84-89)
    (90, 96),   # 7. Precipitation value (columns 91-96)
    (97, 98),   # 8. Measurement flag (column 98)
    (99, 100),  # 9. Quality control flag (column 100)
    (101, 102), # 10. Source flag (column 102)
    (103, 109), # 11. Source index (columns 104-109)
]

column_names = [
    "ghcn_id",
    "station_name",
    "latitude",
    "longitude",
    "elevation_m",
    "year_month",
    "precip_tenths_mm",
    "measurement_flag",
    "quality_flag",
    "source_flag",
    "source_index"
]

dtypes = {
    "ghcn_id": "string",
    "station_name": "string",
    "latitude": "float32", # float32 often sufficient for lat/lon
    "longitude": "float32",
    "elevation_m": "float32", # Elevation might have missing values, float handles NaN
    "year_month": "string", # Read as string first
    "precip_tenths_mm": "Int32", # Use nullable integer type
    "measurement_flag": "string", # Or 'category' after loading if desired
    "quality_flag": "string",   # Or 'category'
    "source_flag": "string",    # Or 'category'
    "source_index": "string"    # Assuming it might not always be purely numeric
}
# Path to CSV files
csv_path = 'misc/data/pp/*.csv'
# Get list of all CSV files
csv_files = glob.glob(csv_path)


# List to hold DataFrames
dataframes = []

for i, file in enumerate(csv_files):
    print(f"Processing file {i+1}/{len(csv_files)}: {file}")
    df = pd.read_fwf(
                file,
                colspecs=colspecs,
                names=column_names,
                dtype=dtypes, # Apply defined data types
                header=None # Important: Ensure pandas doesn't look for a header row
            )
    # 1. Parse 'year_month' into 'year' and 'month' columns
    df['year'] = pd.to_numeric(df['year_month'].str[:4], errors='coerce').astype('Int16')
    df['month'] = pd.to_numeric(df['year_month'].str[4:], errors='coerce').astype('Int8')
    df.drop('year_month', axis=1, inplace=True) # Drop the original column
    # 2. Handle Trace Precipitation (-1 becomes 0)
    df['precip_tenths_mm'] = df['precip_tenths_mm'].replace(-1, 0)
    # 3. Create precipitation column in millimeters (float)
    df['precip_mm'] = (df['precip_tenths_mm'].astype('Float32') / 10.0).round(1)

    dataframes.append(df)

combined_df = pd.concat(dataframes, ignore_index=True)

# Save the DataFrame as a Parquet file
combined_df.to_parquet('www/data/tabs/prec_long.parquet', engine='pyarrow', index=False)
    


!rm -rf misc/data/*

combined_df.describe()

