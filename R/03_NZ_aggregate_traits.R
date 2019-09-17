
# -------------------------------------------------------------------------
####  Aggregation of traits NZ ####
# -------------------------------------------------------------------------

# read in Trait NZ
Trait_NZ <- readRDS(file.path(data_cleaned, "NZ", "Trait_NZ_pp_harmonized.rds"))

# create unique id for bijective merge later
Trait_NZ[,unique_id := 1:nrow(Trait_NZ)]

# subset so that only traits with complete information are retained
trait_col <-
  grep(
    "(?i)Species|Genus|Family|Order|unique_id",
    names(Trait_NZ),
    invert = TRUE,
    value = TRUE
  )
name_vec <- sub("\\_.*", "", trait_col) %>% unique()
output <- matrix(ncol = 2, nrow = length(name_vec))

for (i in seq_along(name_vec)) {
  vec <- Trait_NZ[, base::sum(.SD) == 0,
                  .SDcols = names(Trait_NZ) %like% name_vec[i],
                  by = 1:nrow(Trait_NZ)]$V1
  
  # percentage of how many entries per trait lack information
  output[i,] <-
    c(round((sum(vec) / nrow(Trait_NZ)) * 100), name_vec[i])
}
output

# just return rows where for each trait there is an observation 
data <- get_complete_trait_data(trait_data = Trait_NZ)

# lapply(data, nrow)
Trait_NZ <- Reduce(merge, data[c("locom",
                                 "feed",
                                 "resp",
                                 "volt",
                                 "ovip",
                                 "size")])
Trait_NZ[, unique_id := NULL]

# -------------------------------------------------------------
#### Aggregation ####
# -------------------------------------------------------------

# 1) step -> use Median
# create name pattern to choose traits
pat_traitname <- paste(trait_col, collapse = "|")

# subset so that no NA values occur in Species data (otherwise all NA entries are viewed as a group &
# aggregated as well)
Trait_NZ_genus <- Trait_NZ[!is.na(species), lapply(.SD, median), 
                           .SDcols = names(Trait_NZ) %like% pat_traitname, 
                           by = genus]

# merge family information back
Trait_NZ_genus[Trait_NZ[!is.na(species), ], 
               `:=`(family = i.family,
                    order = order),
               on = "genus"]

# bind with data resolved on Genus level 
Trait_NZ_genus <-
  rbind(Trait_NZ_genus, Trait_NZ[is.na(species) &
                                   !is.na(genus),], fill = TRUE)

# 2 ) aggregate on family level
# take mode if duplicates, otherwise maximum
# test <- Trait_NZ_genus[, lapply(.SD, Mode, na.rm = TRUE), 
#                .SDcols = names(Trait_NZ_genus) %like% "^temp", 
#                by = "family"]
# Trait_fam <- Trait_NZ_genus[, c(lapply(.SD, function(y) {
#   if (length(unique(y)) == length(y) & length(y) > 1) {
#     max(y)
#   } else {
#     Mode(x = y)
#   }
# }), .N),
# .SDcols = names(Trait_NZ_genus) %like% pat_traitname,
# by = "Family"]
Trait_fam <- Trait_NZ_genus[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y)
    # e.g. in case (0,0,3)
  } else if (Mode(y) == 0 & !all((y) == 0))  {
    Mode(y[y != 0])
  }
  else{
    Mode(y)
  }
}), .N),
.SDcols = names(Trait_NZ_genus) %like% pat_traitname,
by = "family"]

# merge back information on order 
Trait_fam[Trait_NZ_genus, 
          `:=`(order = order), 
          on = "family"]

# del N -> maybe use in a later step 
Trait_fam[, N := NULL]

# filter for taxa resolved on family level that are not present in aggregated dataset
# will be added as well
Trait_NZ_resol_fam <- Trait_NZ[is.na(species) &
                                 is.na(genus) &
                                 !(family %in% Trait_fam$family) &
                                 !is.na(family), ]

# condense duplicates 
Trait_NZ_resol_fam <-
  condense_dupl_numeric(trait_data = Trait_NZ_resol_fam,
                        col_with_dupl_entries = "family") 

# rbind with trait data resolved on family level
Trait_NZ_agg <- rbind(Trait_NZ_resol_fam[, -c("species", "genus")], 
                      Trait_fam) 

#### Preparation for analysis ####

# feeding mode parasite -> 1 for Hirundinea
Trait_NZ_agg[, feed_parasite := ifelse(grepl("Rhynchobdellida", order), 1, 0)]

#### Add information on pattern of development ####
# Holometabolous or hemimetabolous?
hemimetabola <- c(
  "Ephemeroptera",
  "Odonata",
  "Plecoptera",
  "Grylloblattodea",
  "Orthoptera",
  "Phasmatodea",
  "Zoraptera",
  "Embioptera",
  "Dermaptera",
  "Mantodea",
  "Blattodea",
  "Isoptera",
  "Thyssanoptera",
  "Hemiptera",
  "Phthriptera",
  "Psocoptera"
)

holometabola <- c(
  "Coleoptera",
  "Streptsiptera",
  "Raphidioptera",
  "Megaloptera",
  "Neuroptera",
  "Diptera",
  "Mecoptera",
  "Siphonoptera",
  "Lepidoptera",
  "Trichoptera",
  "Hymenoptera"
)

Trait_NZ_agg[, pattern_of_development := ifelse(
  order %in% hemimetabola,
  "hemimetabolous",
  ifelse(order %in% holometabola,
         "holometabolous",
         "no_insect")
)] 

# save
saveRDS(object = Trait_NZ_agg, file = file.path(data_out, "Trait_NZ_agg.rds"))

# Depending on aggregation method:
# How many species used for Aggregation
# from Species to Genus 
# from Genus to Family (already implemented)