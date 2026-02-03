import glob
import pandas as pd
import os

# Define colspecs and names (copied from original script)
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

# Path to CSV files
csv_path = 'misc/data/pp/*.csv'
csv_files = glob.glob(csv_path)

print(f"Found {len(csv_files)} files. Testing the first 50...")

for i, file in enumerate(csv_files[:50]):
    print(f"\nProcessing {file}...")
    
    try:
        df = pd.read_fwf(
            file,
            colspecs=colspecs,
            names=column_names,
            dtype=dtypes,
            header=None
        )
    except Exception as e:
        print(f"FAILED to read file: {e}")
        continue

    # 1. Parse 'year_month'
    df['year'] = pd.to_numeric(df['year_month'].str[:4], errors='coerce').astype('Int16')
    df['month'] = pd.to_numeric(df['year_month'].str[4:], errors='coerce').astype('Int8')
    df.drop('year_month', axis=1, inplace=True)

    # Convert precip
    df['precip_tenths_mm'] = pd.to_numeric(df['precip_tenths_mm'], errors='coerce')
    original_len = len(df)
    df = df.dropna(subset=['precip_tenths_mm'])
    if len(df) < original_len:
         print(f"  Dropped {original_len - len(df)} rows due to NaN precip")

    df['precip_tenths_mm'] = df['precip_tenths_mm'].astype('Int32')

    # 2. Handle Trace
    df['precip_tenths_mm'] = df['precip_tenths_mm'].replace(-1, 0)

    # 3. Create mm
    df['precip_mm'] = (df['precip_tenths_mm'].astype('Float32') / 10.0).round(1)
    
    # INSPECT Quality Flags
    print(f"  Unique Quality Flags before filtering: {df['quality_flag'].unique()}")
    
    rows_before_quality = len(df)
    # FIXED FILTERING LOGIC
    # Using isin includes NAs as False (not in list), so ~False is True (Keep)
    bad_flags = ['O', 'R', 'T', 'S', 'K']
    # Ensure we handle NAs correctly. If isin returns NA for NA, we need to be careful.
    # But usually isin returns boolean.
    # Let's check specifically for the string dtype behavior.
    mask = df['quality_flag'].isin(bad_flags)
    # If mask contains NA, fill it with False (don't drop)
    mask = mask.fillna(False) 
    
    df = df[~mask]
    
    if len(df) < rows_before_quality:
        print(f"  Dropped {rows_before_quality - len(df)} rows due to quality flags")

    # Check Length
    if len(df) < 120:
        print(f"  [SKIP] Too few rows: {len(df)} < 120")
        continue

    # Check Extreme Values
    max_precip = df['precip_mm'].max()
    if (df['precip_mm'] > 2000).any():
        print(f"  [SKIP] Extreme value found: {max_precip} > 2000mm")
        continue

    print(f"  [KEEP] File would be kept. Rows: {len(df)}, Max Precip: {max_precip}")
