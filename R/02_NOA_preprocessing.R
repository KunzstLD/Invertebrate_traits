# ___________________________________________________________________________ 
#### Preprocessing Noa Traits Pt. 2 ####
# subset to relevant traits
# Handle duplicate entries
# ___________________________________________________________________________

# Load Noa DB - taxonomical information already cleaned 
Trait_Noa <- readRDS(file = file.path(data_cleaned, 
                                      "North_America",
                                      "Traits_US_taxa_pp.rds"))

# subset to relevant traits:
cols <- "Resp_late|Ovipos_behav_prim|stages|Habit_prim|Thermal_pref|Larval_disp|Feed_mode_prim|Voltinism|size|Body\\_shape|Order|Family|Genus|Species"
Trait_Noa <- Trait_Noa[, .SD, .SDcols = names(Trait_Noa) %like% cols]

# Add ID col as unique identifier
Trait_Noa[, unique_id := 1:nrow(Trait_Noa)]

# ___________________________________________________________________________
#### Handle duplicate entries ####
# ___________________________________________________________________________

# transform to lf
Trait_Noa <- melt(
  data = Trait_Noa,
  id.vars = c("unique_id",
              "Species",
              "Genus",
              "Family",
              "Order")
)

# combine trait name & category names  
Trait_Noa <- Trait_Noa[!is.na(value), value := paste0(variable,"_",value)] 

# convert back to long format  
Trait_Noa <- data.table::dcast(
  data = Trait_Noa[!is.na(value), ],
  formula = unique_id + Species + Genus + Family + Order ~
    value,
  fun.aggregate = length
)

# condense duplicate entries
# species-level
# Just zero & ones -> take maximum per duplicate for each trait state
trait_col <- grep("unique_id|Species|Genus|Family|Order",
                  names(Trait_Noa),
                  value = TRUE, 
                  invert = TRUE)

Trait_Noa[!is.na(Species) &
                (duplicated(Species) | duplicated(Species, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Species)]

# rm duplicate species entries
dupl_unique_id <- Trait_Noa[duplicated(Species) & !is.na(Species), unique_id]
Trait_Noa <- Trait_Noa[!unique_id %in% dupl_unique_id, ]

# genus-level
Trait_Noa[is.na(Species) & !is.na(Genus) &
                (duplicated(Genus) |
                   duplicated(Genus, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Genus)]

# family-level
Trait_Noa[is.na(Species) & is.na(Genus) & !is.na(Family) &
                (duplicated(Family) |
                   duplicated(Family, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Family)]

# save
saveRDS(object = Trait_Noa, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp.rds")
)
