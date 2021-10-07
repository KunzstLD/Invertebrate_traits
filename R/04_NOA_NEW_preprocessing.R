# _________________________________________________________________________
#### Preprocessing Trait database from Laura Twardochleb ####
# _________________________________________________________________________

# read in
Trait_Noa_new <- fread(file.path(data_in, "North_America", "Trait_table_LauraT.csv"))

Trait_Noa_new[!is.na(AdultFlyingStrength_abbrev), ]

# select rel columns
cols <-
  c(
    "Accepted_Name",
    "Genus",
    "Family",
    "Order",
    "Feed_prim_abbrev",
    "Habit_prim",
    "Max_body_size_abbrev",
    "Resp_abbrev",
    "Voltinism_abbrev"
  )
Trait_Noa_new <- Trait_Noa_new[, .SD, .SDcols = cols]
Trait_Noa_new[which(Trait_Noa_new$Habit_prim == "Other (specify in comments)"),]

# create species column
Trait_Noa_new[grepl("[A-z]\\s[A-z]", Accepted_Name), `:=`(Species = Accepted_Name,
                                                          Accepted_Name = NA)]
# del Accepted Name column
Trait_Noa_new[, Accepted_Name := NULL]

# Change column names
setnames(
  Trait_Noa_new,
  old = c(
    "Max_body_size_abbrev",
    "Resp_abbrev",
    "Voltinism_abbrev",
    "Feed_prim_abbrev"
  ),
  new = c("Max_body_size", "Resp", "Volt", "Feed_prim")
)

# set "Other" as category to NA
for(j in names(Trait_Noa_new)) {
  data.table::set(Trait_Noa_new,
                  which(Trait_Noa_new[[j]] %in% "Other (specify in comments)"),
                  j,
                  NA)
}

# There are duplicates in the species column hence we assign a unique_id column
Trait_Noa_new[, unique_id := 1:nrow(Trait_Noa_new)]

Trait_Noa_new[Species == "Tribolium castaneum", ]

# Taxonomical corrections
# Crambiidae to Crambidae
Trait_Noa_new[Family == "Crambiidae", Family := "Crambidae"]

# Hydrochus Hydrochidae? 
Trait_Noa_new[Genus == "Hydrochus", Family := "Hydrochidae"]

#__________________________________________________________________________
#### Create two DB versions with different codings ####
# fuzzy coded was just an experiment which I have not pursued 
# The code for this is commented out
#__________________________________________________________________________

# binary coding
# make columns out of categories
# lf
Trait_Noa_new_lf <- melt(
  data = Trait_Noa_new,
  id.vars = c("unique_id",
              "Species",
              "Genus",
              "Family",
              "Order"),
  value.name = "Trait", 
  variable.name = "Trait_group"
)

# combine trait name & category names  
Trait_Noa_new_lf <- Trait_Noa_new_lf[!is.na(Trait), Trait := paste0(Trait_group,"_",Trait)] 

# convert back to long format  
Trait_Noa_new <- data.table::dcast(
  data = Trait_Noa_new_lf[!is.na(Trait), ],
  formula = unique_id + Species + Genus + Family + Order ~
    Trait,
  fun.aggregate = length
)

# fuzzy coding based on frequency
# code Laura Twardochleb with slight modifications
# Trait_Noa_new_fc <- Trait_Noa_new_lf %>%
#   filter(!is.na(Trait) & !is.na(Genus)) %>%
#   group_by(Genus, Trait_group, Trait) %>%
#   tally() %>%
#   mutate(Percent = n / sum(n)) %>%
#   dcast(., Genus ~ Trait) 
# setDT(Trait_Noa_new_fc)
# 
# # merge information on family and order level back
# Trait_Noa_new_fc[Trait_Noa_new_lf,
#                  `:=`(Order = i.Order,
#                       Family = i.Family),
#                  on = "Genus"]
# DT Version:
# Trait_Noa_new_lf %>% 
#   .[!is.na(Trait), .(.N),
#      by = c("Genus", "Trait_group", "Trait")] %>% 
#   .[,  .(N, sum(N), Trait_group, Trait), 
#     by = .("Genus", "Trait_group")] %>%
#   .[grepl("Abedus", Genus), ]
  # dcast(., Genus ~ Trait+ Trait_group) %>% 
  # Hmisc::describe()
# trait_col <- grep("unique_id|Species|Genus|Family|Order",
#                   names(Trait_Noa_new_fc),
#                   value = TRUE, 
#                   invert = TRUE)
# 
# # convert NAs in trait cols to 0
# for(j in trait_col){
#   data.table::set(Trait_Noa_new_fc, which(is.na(Trait_Noa_new_fc[[j]])), j, 0)
# }
# 
# # change a few colnames
# setnames(x = Trait_Noa_new_fc, 
#          old = c("Genus", "Family", "Order"), 
#          new = c("genus", "family", "order"))
# 
# # save as rds
# saveRDS(Trait_Noa_new_fc,
#         file = file.path(data_cleaned,
#                          "North_America",
#                          "Traits_US_pp_LauraT_fc.rds"))

#_________________________________________________________________________
#### Handle duplicates ####
# _________________________________________________________________________

# Condense duplicate entries
# - species-level:
# Just zero & ones -> take maximum per duplicate for each trait state
trait_col <- grep("unique_id|Species|Genus|Family|Order",
                  names(Trait_Noa_new),
                  value = TRUE, 
                  invert = TRUE)
Trait_Noa_new[!is.na(Species) &
                (duplicated(Species) | duplicated(Species, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Species)]
# test
# Trait_Noa_new[grepl("Ephemerella subvaria", Species), ]

# rm duplicate species entries
dupl_unique_id <- Trait_Noa_new[duplicated(Species) & !is.na(Species), unique_id]
Trait_Noa_new <- Trait_Noa_new[!unique_id %in% dupl_unique_id, ]


Trait_Noa_new[Species == "Ephemerella subvaria", ]

# - genus-level:
Trait_Noa_new[is.na(Species) & !is.na(Genus) &
                (duplicated(Genus) |
                   duplicated(Genus, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Genus)]

# rm duplicate species entries
dupl_unique_id <- Trait_Noa_new[is.na(Species) & !is.na(Genus) & duplicated(Genus), unique_id]
Trait_Noa_new <- Trait_Noa_new[!unique_id %in% dupl_unique_id, ]

# - family-level:
Trait_Noa_new[is.na(Species) & is.na(Genus) & !is.na(Family) &
                (duplicated(Family) |
                   duplicated(Family, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Family)]
# rm duplicate species entries
dupl_unique_id <- Trait_Noa_new[is.na(Species) & is.na(Genus) &
                                  !is.na(Family) & duplicated(Family), unique_id]
Trait_Noa_new <- Trait_Noa_new[!unique_id %in% dupl_unique_id, ]

# make some colnames lower case
setnames(Trait_Noa_new, 
         old = c("Species", "Genus", "Family", "Order"), 
         new = c("species", "genus", "family", "order"))

# _________________________________________________________________________
#### Normalization ####
# Normalizing of the trait values to a range of [0 - 1] by
# dividing for a given trait each value for each trait state by the sum of all 
# trait states 
# _________________________________________________________________________
normalize_by_rowSum(Trait_Noa_new,
                    non_trait_cols = c("unique_id",
                                       "species",
                                       "genus",
                                       "family",
                                       "order"))

# save
saveRDS(object = Trait_Noa_new, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp_LauraT.rds")
)
