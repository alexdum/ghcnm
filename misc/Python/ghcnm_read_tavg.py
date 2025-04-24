import pandas as pd
import pyarrow

def read_ghcnm_data(file_path):
  """
  Reads a GHCNM v4 .dat file and returns a pandas DataFrame.

  Args:
    file_path: Path to the .dat file.

  Returns:
    A pandas DataFrame containing the data.
  """

  colspecs = [
      (0, 11), (11, 15), (15, 19),
      (19, 24), (24, 25), (25, 26), (26, 27),
      (27, 32), (32, 33), (33, 34), (34, 35),
      (35, 40), (40, 41), (41, 42), (42, 43),
      (43, 48), (48, 49), (49, 50), (50, 51),
      (51, 56), (56, 57), (57, 58), (58, 59),
      (59, 64), (64, 65), (65, 66), (66, 67),
      (67, 72), (72, 73), (73, 74), (74, 75),
      (75, 80), (80, 81), (81, 82), (82, 83),
      (83, 88), (88, 89), (89, 90), (90, 91),
      (91, 96), (96, 97), (97, 98), (98, 99),
      (99, 104), (104, 105), (105, 106), (106, 107),
      (107, 112), (112, 113), (113, 114), (114, 115)
  ]

  names = [
      "ID", "YEAR", "ELEMENT",
      "VALUE1", "DMFLAG1", "QCFLAG1", "DSFLAG1",
      "VALUE2", "DMFLAG2", "QCFLAG2", "DSFLAG2",
      "VALUE3", "DMFLAG3", "QCFLAG3", "DSFLAG3",
      "VALUE4", "DMFLAG4", "QCFLAG4", "DSFLAG4",
      "VALUE5", "DMFLAG5", "QCFLAG5", "DSFLAG5",
      "VALUE6", "DMFLAG6", "QCFLAG6", "DSFLAG6",
      "VALUE7", "DMFLAG7", "QCFLAG7", "DSFLAG7",
      "VALUE8", "DMFLAG8", "QCFLAG8", "DSFLAG8",
      "VALUE9", "DMFLAG9", "QCFLAG9", "DSFLAG9",
      "VALUE10", "DMFLAG10", "QCFLAG10", "DSFLAG10",
      "VALUE11", "DMFLAG11", "QCFLAG11", "DSFLAG11",
      "VALUE12", "DMFLAG12", "QCFLAG12", "DSFLAG12"
  ]

  df = pd.read_fwf(file_path, colspecs=colspecs, names=names, dtype=str) 

  # Convert temperature values to numeric (Celsius)
  for i in range(1, 13):
    df[f"VALUE{i}"] = pd.to_numeric(df[f"VALUE{i}"], errors='coerce').fillna(-9999) / 100.0

  return df


# download file

import tarfile
import os
from urllib.request import urlretrieve


url = "https://www.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcf.tar.gz"
filename = os.path.join('misc/data', "ghcnm.tavg.latest.qcf.tar.gz")

urlretrieve(url, filename)

with tarfile.open(filename, 'r:gz') as tar:
    tar.extractall(path='misc/data')
    
    
def find_inv_file(directory, extension):
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith(extension):
                return os.path.join(root, file)
    return None

dat_file = find_inv_file('misc/data', '.dat')


# Example usage:
data = read_ghcnm_data(dat_file)
print(data.head()) 

# Assuming your data is already loaded into a dataframe called 'filtered_data'
# Transforming from wide to long format
long_format_data = pd.wide_to_long(data, 
                                   stubnames=['VALUE', 'DMFLAG', 'QCFLAG', 'DSFLAG'], 
                                   i=['ID', 'YEAR', 'ELEMENT'], 
                                   j='MONTH', 
                                   suffix='\d+')

# Resetting the index for better readability
long_format_data = long_format_data.reset_index()

print(long_format_data)



# Save the DataFrame as a Parquet file
long_format_data.to_parquet('www/data/tabs/tavg_long.parquet', engine='pyarrow', index=False)


# Assuming long_format_data is the name of your DataFrame
# Group by 'ID' and find the first and last year for each unique ID
station_summary = long_format_data.groupby('ID')['YEAR'].agg(first_year='min', last_year='max').reset_index()

# Display the result
print(station_summary)


station_summary.to_csv("www/data/tabs/tavg_vaialability.csv", index=False)


### read text  file 
# misc/data/ghcnm.v4.0.1.20250423/ghcnm.tavg.v4.0.1.20250423.qcf.inv

meta_file = find_inv_file('misc/data', '.inv')

# Define column specifications (start index is 0-based in pandas)
colspecs = [
    (0, 11),   # ID (1-11)
    (12, 20),  # LATITUDE (13-20)
    (21, 30),  # LONGITUDE (22-30)
    (31, 37),  # STNELEV (32-37)
    (38, 68)   # NAME (39-68)
]

# Define column names
column_names = ['ID', 'LATITUDE', 'LONGITUDE', 'STNELEV', 'NAME']

# Read the fixed-width formatted file
meta = pd.read_fwf(meta_file, colspecs=colspecs, names=column_names)

# Replace missing elevation values -999.0 with NaN
meta['STNELEV'] = meta['STNELEV'].replace(-999.0, pd.NA)

# Show the first few rows
print(meta.head())

# Write the DataFrame to a CSV file
meta.to_csv('www/data/tabs/tavg_meta.csv', index=False)




# remove all folders and files 
!rm -rf misc/data/*.*






# from huggingface_hub import HfApi
# api = HfApi()
# 
# # Upload all the content from the local folder to your remote Space.
# # By default, files are uploaded at the root of the repo
# api.upload_folder(
#     folder_path="~/Documents/clima/2024/climate/www/data",
#     path_in_repo="www",
#     repo_id="alexdum/climate",
#     repo_type="space"
# )






