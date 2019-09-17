# Set up script:
# Loads required packages for Trait data cleaning & Harmonization
# Loads scripts with used functions
# Loads paths with location for data 
# TODO could set relativ paths to the wd for collabarative work

# Data location
data_in <- "~/Dokumente/Projects/Trait_DB/Invertebrate_traits/Data"

# Path for intermediate output (e.g. cleaned data)
data_cleaned <- "~/Dokumente/Projects/Trait_DB/Invertebrate_traits/Cleaned_data"

# Path for output 
data_out <- "~/Dokumente/Projects/Trait_DB/Invertebrate_traits/Output"

# Location for R scripts
data_scr <- "~/Dokumente/Projects/Trait_DB/Invertebrate_traits/R"

# libraries
library(dplyr)    # basic data transformation
library(data.table) # data transformation
library(readxl) # read in xlsx /xls files
library(taxize) # retrieve taxonomical information

# source script with used functions
source(file = file.path(data_scr, "functions_used_TDB.R"))