# ________________________________________________________________________________
#### Run script ####
# ________________________________________________________________________________

# Packages, paths and functions
source("./R/Set_up.R")

# ________________________________________________________________________________
#### preprocessing raw data ####
# 03_preprocess_new_freshecol_taxa.R creates an .rds file which is
# then used in the harmonization scripts
# ________________________________________________________________________________

# combine raw freshecol exports 
# and change trait names
source("./R/Preprocessing_raw/01_preprocess_new_freshecol_2020.R")

# taxa cleaning and formation
source("./R/Preprocessing_raw/02_preprocess_new_freshecol_taxa.R")

# retrieving taxonomic information where missing and manual correction of some 
# taxonomic entries
source("./R/Preprocessing_raw/03_preprocess_new_freshecol_taxa.R")

# ________________________________________________________________________________
#### Harmonization scripts ####
# an intermediate .rds file is created at the end of each script
# ________________________________________________________________________________