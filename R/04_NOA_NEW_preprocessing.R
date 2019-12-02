# _________________________________________________________________________
#### Preprocessing Traitdatabase from Laura Twardochleb ####
# _________________________________________________________________________

# read in
Trait_Noa_new <- fread(file.path(data_in, "North_America", "Trait_table_LauraT.csv"))

# select rel columns
cols <-
  c(
    "Accepted_Name",
    "Genus",
    "Family",
    "Order",
    "Feed_mode_sec",
    "Feed_prim_abbrev",
    "Habit_prim",
    "Habit_sec",
    "Max_body_size_abbrev",
    "Resp_abbrev",
    "Voltinism_abbrev"
  )
Trait_Noa_new <- Trait_Noa_new[, .SD, .SDcols = cols]

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

#_________________________________________________________________________
#### Handle duplicates ####
# _________________________________________________________________________

# lf
Trait_Noa_new <- melt(
  data = Trait_Noa_new,
  id.vars = c("unique_id",
              "Species",
              "Genus",
              "Family",
              "Order")
)

# combine trait name & category names  
Trait_Noa_new <- Trait_Noa_new[!is.na(value), value := paste0(variable,"_",value)] 

# convert back to long format  
Trait_Noa_new <- data.table::dcast(
  data = Trait_Noa_new[!is.na(value), ],
  formula = unique_id + Species + Genus + Family + Order ~
    value,
  fun.aggregate = length
)

# condense duplicate entries
# species-level
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

# genus-level
Trait_Noa_new[is.na(Species) & !is.na(Genus) &
                (duplicated(Genus) |
                   duplicated(Genus, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Genus)]

# family-level
Trait_Noa_new[is.na(Species) & is.na(Genus) & !is.na(Family) &
                (duplicated(Family) |
                   duplicated(Family, fromLast = TRUE)),
              (trait_col) := lapply(.SD, max),
              .SDcols = trait_col,
              by = .(Family)]

# make some colnames lower case
setnames(Trait_Noa_new, 
         old = c("Species", "Genus", "Family", "Order"), 
         new = c("species", "genus", "family", "order"))

# save
saveRDS(object = Trait_Noa_new, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp_LauraT.rds")
)
