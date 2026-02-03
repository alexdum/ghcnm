import requests
from bs4 import BeautifulSoup
import os
from urllib.request import urlretrieve
import tarfile
import glob
import pandas as pd
from pandas.errors import EmptyDataError, ParserError
import sys
import concurrent.futures

# URL of the archive directory
BASE_URL = 'https://www.ncei.noaa.gov/data/ghcnm/v4/precipitation/archive/'

# Global definitions for workers
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
    "latitude": "float32",
    "longitude": "float32",
    "elevation_m": "float32",
    "year_month": "string",
    "precip_tenths_mm": "string",
    "measurement_flag": "string",
    "quality_flag": "string",
    "source_flag": "string",
    "source_index": "string"
}

def process_file(file):
    try:
        # print(f"Processing {file}") # Too noisy for multiprocessing
        df = pd.read_fwf(
            file,
            colspecs=colspecs,
            names=column_names,
            dtype=dtypes,  # Apply defined data types
            header=None    # Important: Ensure pandas doesn't look for a header row
        )

        # 1. Parse 'year_month' into 'year' and 'month' columns
        df['year'] = pd.to_numeric(df['year_month'].str[:4], errors='coerce').astype('Int16')
        df['month'] = pd.to_numeric(df['year_month'].str[4:], errors='coerce').astype('Int8')
        df.drop('year_month', axis=1, inplace=True)  # Drop the original column

        # Convert precip_tenths_mm to numeric, handling potential errors
        df['precip_tenths_mm'] = pd.to_numeric(df['precip_tenths_mm'], errors='coerce')
        df = df.dropna(subset=['precip_tenths_mm']) # Drop rows that failed conversion
        df['precip_tenths_mm'] = df['precip_tenths_mm'].astype('Int32')

        # 2. Handle Trace Precipitation (-1 becomes 0)
        df['precip_tenths_mm'] = df['precip_tenths_mm'].replace(-1, 0)

        # 3. Create precipitation column in millimeters (float)
        df['precip_mm'] = (df['precip_tenths_mm'].astype('Float32') / 10.0).round(1)
        
        # FIXED FILTERING LOGIC
        # Using isin includes NAs as False (not in list), so ~False is True (Keep)
        bad_flags = ['O', 'R', 'T', 'S', 'K']
        mask = df['quality_flag'].isin(bad_flags).fillna(False)
        df = df[~mask]
        
        # skip is number of rows is  less than 120
        if len(df) < 120:
            return None
        # 4. Skip this file if any 'precip_mm' value is greater than 5000
        if (df['precip_mm'] > 2000).any():
            # print(f"Skipping {file} due to extreme precipitation value (> 2000mm)")
            return None

        return df
    except Exception as e:
        print(f"Error processing {file}: {e}")
        return None

def main():
    # Create a session
    session = requests.Session()
    
    # Get the HTML content of the archive page
    try:
        response = session.get(BASE_URL)
        response.raise_for_status()  # Raise an error for bad status
        
        # Parse HTML with BeautifulSoup
        soup = BeautifulSoup(response.text, 'html.parser')
        
        # Find all .tar.gz files
        tar_links = [a['href'] for a in soup.find_all('a', href=True) if a['href'].endswith('.tar.gz')]
        
        if tar_links:
            file_url =  BASE_URL + tar_links[0]
            filename = os.path.join('misc/data', tar_links[0])
            
            if os.path.exists(filename):
                print(f"File {filename} already exists. Skipping download.")
            else:
                print(f"Downloading {filename}...")
                urlretrieve(file_url, filename)
            
            if not os.path.exists('misc/data/pp') or not os.listdir('misc/data/pp'):
                print("Extracting files...")
                with tarfile.open(filename, 'r:gz') as tar:
                    tar.extractall(path='misc/data/pp')
            else:
                print("Files already extracted. Skipping extraction.")
    except Exception as e:
        print(f"Warning: Could not check/download updates: {e}")

    # Path to CSV files
    csv_path = 'misc/data/pp/*.csv'
    # Get list of all CSV files
    csv_files = glob.glob(csv_path)
    
    print(f"Found {len(csv_files)} files. Starting processing with multiprocessing...")
    
    # Process files in parallel
    dataframes = []
    # Use 75% of CPUs to avoid freezing system completely
    max_workers = max(1, int(os.cpu_count() * 0.75))
    
    with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers) as executor:
        # Map returns results in order
        for i, df in enumerate(executor.map(process_file, csv_files)):
            if i % 1000 == 0:
                print(f"Processed {i}/{len(csv_files)} files...")
            if df is not None:
                dataframes.append(df)

    if not dataframes:
        print("No valid dataframes found.")
        return

    print("Concatenating dataframes...")
    combined_df = pd.concat(dataframes, ignore_index=True)

    # Rename columns to match TAVG schema
    combined_df = combined_df.rename(columns={
        'ghcn_id': 'ID',
        'year': 'YEAR',
        'month': 'MONTH',
        'precip_mm': 'VALUE'
    })

    # Filter out unneeded columns if desired, or keep them. 
    # Server.R uses: ID, YEAR, MONTH, VALUE.
    # Ensure VALUE is float
    combined_df['VALUE'] = combined_df['VALUE'].astype('float32')

    print("Saving to parquet...")
    # Save the DataFrame as a Parquet file
    combined_df.to_parquet('www/data/tabs/prec_long.parquet', engine='pyarrow', index=False)
        
    print(combined_df.describe())
    
    # Calculate availability
    print("Calculating availability...")
    station_summary = combined_df.groupby('ID')['YEAR'].agg(first_year='min', last_year='max').reset_index()
    # ID is already named ID
    station_summary.to_csv("www/data/tabs/prec_availability.csv", index=False)
    print("Done!")

if __name__ == '__main__':
    main()



