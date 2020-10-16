#### Run script ####

# Packages, paths and functions
source("./R/Set_up.R")

#### OLD ####

# EU:
# all scripts can be run separately 
# preprocessing freshwaterecol (taxa)
source("./R/01_EU_preprocessing_freshecol.R")

# preprocessing & harmonizing tachet
source("./R/02_EU_preprocessing_tachet.R")
source("./R/02_01_EU_harmonize_tachet.R")

# harmonisation freshecol & integration tachet
source("./R/03_EU_harmonize_traits.R")