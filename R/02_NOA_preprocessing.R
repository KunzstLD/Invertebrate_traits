# ___________________________________________________________________________ 
#### Preprocessing Noa Traits Pt. 2 ####
# subset to relevant traits
# Handle duplicate entries
# ___________________________________________________________________________

# Load Noa DB - taxonomical information already cleaned 
Trait_Noa <- readRDS(file = file.path(data_cleaned, 
                                      "North_America",
                                      "Traits_US_taxa_pp.rds"))

# Subset to relevant traits:
cols <- "Resp_late|Ovipos_behav_prim|stages|Habit_prim|Thermal_pref|Larval_disp|Feed_mode_prim|Voltinism|size|Body\\_shape|Order|Family|Genus|Species"
Trait_Noa <- Trait_Noa[, .SD, .SDcols = names(Trait_Noa) %like% cols]

# Add ID col as unique identifier
Trait_Noa[, unique_id := 1:nrow(Trait_Noa)]

# Taxonomical corrections
# Parapoynx & Petrophilia
Trait_Noa[Genus %in% c("Parapoynx",
                       "Petrophila"), Family := "Crambidae"]

# Rossiana
Trait_Noa[Genus == "Rossiana", Family := "Rossianidae"]

# Sperchonopsis
Trait_Noa[Genus == "Sperchonopsis", Family := "Sperchonidae"]

# Corbiculidae
Trait_Noa[Family == "Corbiculidae", Order := "Venerida"]

#__________________________________________________________________________
#### Create two DB versions with different codings ####
# NOTE: "Fuzzy coded" version not used
#__________________________________________________________________________

# transform categories to columns!
# transform to lf
Trait_Noa_lf <- melt(
  data = Trait_Noa,
  id.vars = c("unique_id",
              "Species",
              "Genus",
              "Family",
              "Order"),
  value.name = "Trait", 
  variable.name = "Trait_group"
)

# combine trait name & category names
Trait_Noa_lf[!is.na(Trait), Trait := paste0(Trait_group, "_", Trait)]

# convert back to long format
Trait_Noa <- data.table::dcast(
  data = Trait_Noa_lf[!is.na(Trait), ],
  formula = unique_id + Species + Genus + Family + Order ~
    Trait,
  fun.aggregate = length
)

# fuzzy coding based on frequency
# code from Laura Twardochleb with slight modifications
Trait_Noa_fc <- Trait_Noa_lf %>%
  filter(!is.na(Trait) & !is.na(Genus)) %>%
  group_by(Genus, Trait_group, Trait) %>%
  tally() %>%
  mutate(Percent = n / sum(n)) %>%
  dcast(., Genus ~ Trait) 
setDT(Trait_Noa_fc)

# merge information on family and order level back
Trait_Noa_fc[Trait_Noa_lf,
             `:=`(Order = i.Order,
                  Family = i.Family,
                  unique_id = i.unique_id),
             on = "Genus"]


# get trait columns
trait_col <- grep("unique_id|Species|Genus|Family|Order",
                  names(Trait_Noa_fc),
                  value = TRUE, 
                  invert = TRUE)

# convert NAs in trait cols to 0
for(j in trait_col){
  data.table::set(Trait_Noa_fc, which(is.na(Trait_Noa_fc[[j]])), j, 0)
}

# save as rds
# saveRDS(Trait_Noa_fc,
#         file = file.path(data_cleaned,
#                          "North_America",
#                          "Traits_US_pp_fc.rds"))

# ___________________________________________________________________________
#### Handle duplicate entries ####
# Just zero & ones -> take maximum per duplicate for each trait state
# ___________________________________________________________________________

# trait cols
trait_col <- grep("unique_id|Species|Genus|Family|Order",
                  names(Trait_Noa),
                  value = TRUE, 
                  invert = TRUE)

# condense duplicate entries
# species-level
Trait_Noa[!is.na(Species),
          (trait_col) := lapply(.SD, max),
          .SDcols = trait_col,
          by = "Species"]

# rm duplicate species entries
dupl_unique_id <- Trait_Noa[duplicated(Species) & !is.na(Species), unique_id]
Trait_Noa <- Trait_Noa[!unique_id %in% dupl_unique_id, ]

# genus-level:
Trait_Noa[is.na(Species) & !is.na(Genus),
          (trait_col) := lapply(.SD, max),
          .SDcols = trait_col,
          by = "Genus"]

# rm duplicate genus entries
dupl_unique_id <- Trait_Noa[is.na(Species) & !is.na(Genus), ] %>%
  .[(duplicated(Genus)), unique_id]
Trait_Noa <- Trait_Noa[!unique_id %in% dupl_unique_id, ]

# family-level:
Trait_Noa[is.na(Species) & is.na(Genus) & !is.na(Family),
          (trait_col) := lapply(.SD, max),
          .SDcols = trait_col,
          by = "Family"]

# rm duplicate family entries
dupl_unique_id <- Trait_Noa[is.na(Species) & is.na(Genus) & !is.na(Family), ] %>% 
  .[(duplicated(Family)), unique_id]
Trait_Noa <- Trait_Noa[!unique_id %in% dupl_unique_id, ]

# save
saveRDS(object = Trait_Noa, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp.rds")
)