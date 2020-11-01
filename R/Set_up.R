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
library(taxize)

# taxonomical information
# library(taxize)

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

# Data location for trait aggregation project
data_aggr <- "/home/kunzst/Dokumente/Projects/Trait_DB/Trait-aggregation"

# Path for output
data_out <- "./Output"

# Location for R scripts
data_scr <- "./R"

# Link to raw data
data_raw <- "./Data/EU/Raw_freshecol/2020"

# source script with used functions
source(file = file.path(data_scr, "functions_used_TDB.R"))

# script with helper functions for preprocessing of raw data
source(file = "./R/Preprocessing_raw/functions_used.R")
