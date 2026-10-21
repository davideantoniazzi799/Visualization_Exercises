"""
SCRIPT CONTENT
The script is divided in sections, which can be expaned by clicking the down arrow at the left of the #region comment
The script presentes the following sections:
- Libraries
- Data File Path
- Uploading Data
- Quick NAs check for each year
- Detail of NAs per column for each year
- Removing one NA from year 1980 (where 'magType' is NA)
- Joining all datasets from 1950 to 2025 into a unique complete Data Frame
- Converting column 'time' in two separeted columns (one 'Date' and one 'Hour')
- Exporting final dataset in 'Data' folder as 'Eartquakes_1950_2025.csv'

This script focuses on importing all the yearly data regarding all earthquakes recorded in that year with a magnitude equal or higher to 4.5 in the period 1950-2025,
cleaning the several datasets, and merging all of them into a unique final complete earthquake dataset (magnitude >= 4.5, period 1950-2025)

The data is provided by the U.S. Geological Survey, specifically the Earthquake Catalog (https://earthquake.usgs.gov/earthquakes/search/)
Additional information regarding the variables and how to download the data is provided in the 'Data_Source.txt' file in the 'Data' folder.
The datasets were not provided to save memory.

Insights regarding which variables are missing for which year are provided in the 'Missing_Values.txt' file in the 'Data' folder.
"""

# region Libraries
from pathlib import Path
import pandas as pd
# endregion

# region Data File Path
BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / 'Data'
# endregion

# region Uploading Data
#Function definition
def import_data(years, columns_to_use):
    """
    Imports .csv files for earthquakes corresponding to the specified year and selects which columns to import.
    Returns a dictionary with year as key and the DataFrame as value.
    """
    datasets_eartquakes = {}

    for year in years:
        file_path = DATA_DIR / f"{year}.csv"

        try:
            df = pd.read_csv(file_path, sep=",")
            #Selects the columns to import
            datasets_eartquakes[year] = df[columns_to_use]
            print(f"Year {year} successfully imported.")

        except FileNotFoundError:
            print(f"Error: File for Year {year} not found in {file_path}")

    return datasets_eartquakes

#Columns to import
columns_import = ["time",
                  "latitude",
                  "longitude",
                  "depth",
                  "mag",
                  "magType",
                  "id",
                  "place"]

#List of years to import
years_to_import = range(1950, 2026)

#Executing the function 'import_data'
earthquakes = import_data(years_to_import, columns_import)
# endregion

# region Quick NAs check for each year
print("\n--- TOTAL NAs PER YEAR ---")
for year, df in earthquakes.items():
    total_na = df.isnull().sum().sum()
    print(f"Year {year}: {total_na} total missing values")
# endregion

# region Detail of NAs per column for each year
print("\n--- DETAILED MISSING VALUES PER COLUMN ---")
for year, df in earthquakes.items():
    na_per_column = df.isnull().sum()
    #Filter
    columns_with_na = na_per_column[na_per_column > 0]
    if not columns_with_na.empty:
        print(f"Year {year}:")
        for column, count in columns_with_na.items():
            print(f"Column '{column}': {count} missing value")
# endregion

# region Removing one NA from year 1980 (where 'magType' is NA)
if 1980 in earthquakes:
    earthquakes[1980] = earthquakes[1980].dropna(subset=["magType"])
# endregion

# region Joining all datasets from 1950 to 2025 into a unique complete Data Frame
earthquakes_complete = pd.concat(earthquakes, names=["year_origin"]).reset_index(level="year_origin")
earthquakes_complete = earthquakes_complete.reset_index(drop=True)

#Check results
print(f"Complete dataset has {earthquakes_complete.shape[0]} rows.")
# endregion

# region Converting column 'time' in two separeted columns (one 'Date' and one 'Hour')
#Converting column 'time' in native pandas datatime object
earthquakes_complete["time"] = pd.to_datetime(earthquakes_complete["time"], errors="coerce")
#Extracting the date (format Y-M-D)
earthquakes_complete["date"] = earthquakes_complete["time"].dt.date
#Extracting the hour (format H-M-S)
earthquakes_complete["hour"] = earthquakes_complete["time"].dt.time
#Remowing original 'time' column
earthquakes_complete = earthquakes_complete.drop(columns=["time"])
# endregion

# region Exporting final dataset in 'Data' folder as 'Eartquakes_1950_2025.csv'
output_file_path = DATA_DIR / "Earthquakes_1950_2025.csv"
earthquakes_complete.to_csv(output_file_path, sep=",", index=False)
print(f"Dataset successfully exported in: {output_file_path}")
# endregion