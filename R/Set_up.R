# Set up script:
# Loads required packages for Trait data cleaning & Harmonization
# Loads scripts with used functions
# Loads paths with location for data 

# Data location
data_in <- "./Data"

# Path for intermediate output (e.g. cleaned data)
data_cleaned <- "./Cleaned_data"

# Path for output 
data_out <- "./Output"

# Location for R scripts
data_scr <- "./R"

# libraries
library(dplyr)    # basic data transformation
library(data.table) # data transformation
library(readxl) # read in xlsx /xls files
library(taxize) # retrieve taxonomical information

# source script with used functions
source(file = file.path(data_scr, "functions_used_TDB.R"))