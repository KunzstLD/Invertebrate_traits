# Set up script:
# Loads required packages for Trait data cleaning & Harmonization
# Loads paths with location for data
# Loads scripts with used functions

# libraries
# data transformation and data handling
library(dplyr)     
library(data.table) 
library(tidyr)  
library(readxl) 
library(zeallot) 

# taxonomical information 
library(taxize) 

# data overview
library(Hmisc) 

# unit testing
library(testthat) 

# Data location
data_in <- "./Data"

# Path for intermediate output (e.g. cleaned data)
data_cleaned <- "./Cleaned_data"

# Path for data to fill missing values
data_missing <- "./Missing_data"

# Path for output
data_out <- "./Output"

# Location for R scripts
data_scr <- "./R"

# source script with used functions
source(file = file.path(data_scr, "functions_used_TDB.R"))