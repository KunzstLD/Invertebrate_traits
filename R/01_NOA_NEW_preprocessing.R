# -------------------------------------------------------------------------
#### Preprocessing Traitdatabase from Laura Twardochleb ####

# TODO: 
 # Harmonization
 # Check completeness
 # complement with traits from Vieira DB
# -------------------------------------------------------------------------

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

#### Handle duplicates ####

# Just duplicate species 
Dupl_Noa <- fetch_dupl(data = Trait_Noa_new, col = "Species")

# data that does not contain duplicates
Trait_Noa_no_dupl <- Trait_Noa_new[!unique_id %in% Dupl_Noa$unique_id]

# binary/categories
# transform to long_code for those traits that have categories
# therefore, firstly convert repesctive columns into long format
Dupl_Noa <- melt(
  data = Dupl_Noa,
  id.vars = c(
    "unique_id",
    "Species",
    "Genus",
    "Family",
    "Order"
  )
)

# combine trait name & category names  
Dupl_Noa <- Dupl_Noa[!is.na(value), value := paste0(variable,"_",value)] 

# convert back to long format  
Dupl_Noa <- data.table::dcast(
  data = Dupl_Noa[!is.na(value), ],
  formula = unique_id + Species + Genus + Family + Order ~
    value,
  fun.aggregate = length
)

# condense rowwise information for duplicates
trait_col <- names(Dupl_Noa)[!names(Dupl_Noa) %in% c("unique_id", "Species", "Genus", "Family", 
                                                    "Order")]
Dupl_Noa[, (trait_col) := lapply(.SD, max), .SDcols = trait_col,
          by = Species]
Dupl_Noa <- Dupl_Noa[!duplicated(Species), ] 

# Non duplicated data: categories turn to colnames 
Trait_Noa_no_dupl <- melt(
  data = Trait_Noa_no_dupl,
  id.vars = c(
    "unique_id",
    "Species",
    "Genus",
    "Family",
    "Order"
  )
)
# combine trait name & category name
Trait_Noa_no_dupl[!is.na(value), value := paste0(variable, "_", value)]

# convert back to long format
Trait_Noa_no_dupl <- data.table::dcast(
  data = Trait_Noa_no_dupl[!is.na(value)],
  formula = unique_id + Species + Genus + Family + Order ~
    value,
  fun.aggregate = length
)

# rbind former duplicated and not duplicated dataset
Trait_Noa_new <- base::rbind(Trait_Noa_no_dupl, Dupl_Noa, fill = TRUE)

# NAs introduced (e.g. Habit_sec_Miner was not present in Dupl_Noa)
# transform remaining NAs into 0
trait_col <- names(Trait_Noa_new)[!names(Trait_Noa_new) %in% c("unique_id", "Species", "Genus", "Family", "Order")]
for(j in trait_col){
  data.table::set(Trait_Noa_new, which(is.na(Trait_Noa_new[[j]])),j, 0)
}

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