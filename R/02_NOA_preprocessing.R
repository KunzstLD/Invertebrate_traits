# ------------------------------------------------------------------------- 
#### Preprocessing Noa Traits Pt. 2 ####

# subset to relevant traits
# Handle duplicate entries

# -------------------------------------------------------------------------
# TODO add a more detailed description 


# Load Noa DB - taxonomical information already cleaned 
Trait_Noa <- readRDS(file = file.path(data_cleaned, 
                                      "North_America",
                                      "Traits_US_taxa_pp.rds"))

# subset to relevant traits:
cols <- "Resp_late|Ovipos_behav_prim|stages|Habit_prim|pH|Thermal_pref|Larval_disp|Feed_mode_prim|Voltinism|size|Body\\_shape|Order|Family|Genus|Species"
Trait_Noa <- Trait_Noa[, .SD, .SDcols = names(Trait_Noa) %like% cols]

# Add ID col as unique identifier
Trait_Noa[, unique_id := 1:nrow(Trait_Noa)]

#### Handle duplicate entries ####
Dupl_US <- fetch_dupl(data = Trait_Noa, col = "Species")

# data that does not contain duplicates
Trait_Noa <- Trait_Noa[!unique_id %in% Dupl_US$unique_id]

# binary/categories
# transform to long_code for those traits that have categories
# therefore, firstly convert repesctive columns into long format
Dupl_US <- melt(
  data = Dupl_US,
  id.vars = c(
    "unique_id",
    "Species",
    "Genus",
    "Family",
    "Order",
    "pH_acidic",
    "pH_normal",
    "pH_alkaline"
  )
)
# combine trait name & category names
Dupl_US[!is.na(value), value := paste0(variable,"_",value)]

# convert to long format
Dupl_US <- dcast(
  data = Dupl_US[!is.na(value)],
  formula = unique_id + Species + Genus + Family + Order + pH_acidic + pH_normal + pH_alkaline ~
    value,
  fun.aggregate = length
)

# turn NAs in pH trait into zeros
cols <- grep("pH.*", names(Dupl_US), value = TRUE)
for (j in cols) {
  data.table::set(Dupl_US, which(is.na(Dupl_US[[j]])), j, 0)
}

# condense rowwise information for duplicates 
condense <-
  Dupl_US[, lapply(.SD, max), .SDcols = -c("unique_id", "Species", "Genus", "Family"),
          by = Species]
# merge taxonomical information back
condense[Dupl_US,
         `:=`(Genus = i.Genus,
              Family = i.Family,
              Order = i.Order,
              unique_id = i.unique_id),
         on = c(Species = "Species")]

# Non duplicated data: categories -> colnames 
Trait_Noa <- melt(
  data = Trait_Noa,
  id.vars = c(
    "unique_id",
    "Species",
    "Genus",
    "Family",
    "Order",
    "pH_acidic",
    "pH_normal",
    "pH_alkaline"
  )
)

# combine trait name & category name
Trait_Noa[!is.na(value), value := paste0(variable, "_", value)]

# convert back to long format
Trait_Noa <- dcast(
  data = Trait_Noa[!is.na(value)],
  formula = unique_id + Species + Genus + Family + Order + pH_acidic + pH_normal + pH_alkaline ~
    value,
  fun.aggregate = length
)

# rbind former duplicated and not duplicated dataset
Trait_Noa <- base::rbind(Trait_Noa, condense, fill = TRUE)

# transform remaining NAs into 0
# grep relevant columns
cols <- grep("pH|Feed_mode|Habit_prim|No.Aquatic_stages", names(Trait_Noa), value = TRUE)
for(j in cols){
  data.table::set(Trait_Noa, which(is.na(Trait_Noa[[j]])),j, 0)
}

# save
saveRDS(object = Trait_Noa, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp.rds")
)