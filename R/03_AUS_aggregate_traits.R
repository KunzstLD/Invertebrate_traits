
# _________________________________________________________________________ 
####  Aggregation of traits AUS ####
# _________________________________________________________________________ 

# read in harmonized & preprocessed EU Trait DB
Trait_AUS <- readRDS(
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_harmonized.rds"
  )
)

# get trait columns
trait_col <-
  grep(
    "unique_id|species|genus|family|order",
    names(Trait_AUS),
    invert = TRUE,
    value = TRUE
  )

# subset so that only traits with complete information are retained
# test how complete trait sets are
name_vec <- sub("\\_.*", "", trait_col) %>% unique()
output <- matrix(ncol = 2, nrow = length(name_vec))

for (i in seq_along(name_vec)) {
  vec <- Trait_AUS[, base::sum(.SD) == 0,
                   .SDcols = names(Trait_AUS) %like% name_vec[i],
                   by = 1:nrow(Trait_AUS)]$V1
  
  # percentage of how many entries per trait lack information
  output[i, ] <-
    c(round((sum(vec) / nrow(Trait_AUS)) * 100), name_vec[i])
}
# output

# just return rows where for each trait there is an observation 
data <- get_complete_trait_data(
  trait_data = Trait_AUS,
  non_trait_col = c("unique_id",
                    "species",
                    "genus",
                    "family",
                    "order")
)
# lapply(data, nrow)
Trait_AUS <- Reduce(merge, data[c("locom",
                                  "feed",
                                  "resp",
                                  "volt",
                                  "ovip",
                                  "size",
                                  "dev")])

# Subset to interesting orders: Ephemeroptera, Hemiptera, Odonata, 
# Trichoptera, Venerida, Coleoptera, Plecoptera, Diptera, Amphipoda
Trait_AUS <- Trait_AUS[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Venerida",
  "Coleoptera",
  "Plecoptera",
  "Diptera",
  "Amphipoda"
), ]

# rm unique id col
Trait_AUS[, unique_id := NULL]

# _________________________________________________________________________
#### Aggregation of traits ####
# - Aggregation: 
 # Median to Genus/ 
 # Mode for Family 
# -> differing amount of values to calculate value for each modality
# sum(table(Trait_AUS[grepl("Chirono.*", family)]$temp_eurytherm))

# Poff et al. 2006: most common genus-level modality assigned (genus level
# trait data with family-level monitoring data)
# which(table(Trait_AUS$genus) == max(table(Trait_AUS$genus), na.rm = TRUE))
# _________________________________________________________________________

# create name pattern to choose traits
pat_traitname <- paste(trait_col, collapse = "|")

# subset so that no NA values occur in Species data (otherwise all NA entries are viewed as a group &
# aggregated as well)
Trait_AUS_genus <- Trait_AUS[!is.na(species), lapply(.SD, median), 
                             .SDcols = names(Trait_AUS) %like% pat_traitname, 
                             by = genus]
# merge family information 
Trait_AUS_genus[Trait_AUS[!is.na(species), ], 
                `:=`(family = i.family,
                     order = i.order),
                on = "genus"]
# bind with data resolved on Genus level 
Trait_AUS_genus <-
  rbind(Trait_AUS_genus, Trait_AUS[is.na(species) &
                                     !is.na(genus),], fill = TRUE)
# _________________________________________________________________________
#### Aggregate to family level ####
# take mode if duplicates, otherwise maximum
# test <- Trait_AUS_genus[, lapply(.SD, Mode, na.rm = TRUE), 
#                .SDcols = names(Trait_AUS_genus) %like% "^temp", 
#                by = "family"]
# Trait_fam <- Trait_AUS_genus[, c(lapply(.SD, function(y) {
#   if (length(unique(y)) == length(y) & length(y) > 1) {
#     max(y)
#   } else {
#     Mode(x = y)
#   }
# }), .N),
# .SDcols = names(Trait_AUS_genus) %like% pat_traitname,
# by = family]
# _________________________________________________________________________
Trait_fam <- Trait_AUS_genus[, c(lapply(.SD, function(y) {
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
.SDcols = names(Trait_AUS_genus) %like% pat_traitname,
by = "family"]

# merge back information on order 
Trait_fam[Trait_AUS_genus, 
          `:=`(order = i.order), 
          on = "family"]

# del unnecessary information 
Trait_fam[, N := NULL]

# filter for taxa resolved on family level that are not yet respresented in the 
# aggregated dataset (Trait_fam)
Trait_AUS_resol_fam <- Trait_AUS[is.na(species) & 
                                   is.na(genus) & 
                                   !(family %in% Trait_fam$family) &
                                   !is.na(family), ]
# condense duplicates in Trait_AUS_resol_fam
Trait_AUS_resol_fam <-
  condense_dupl_numeric(
    trait_data = Trait_AUS_resol_fam,
    col_with_dupl_entries = "family",
    non_trait_cols = c("species", "genus", "family", "order")
  )

# rbind with trait data resolved on family level
Trait_AUS_agg <- rbind(Trait_AUS_resol_fam[, -c("species", "genus", "unique_id")], 
                       Trait_fam)
# save
saveRDS(object = Trait_AUS_agg,
        file = file.path(data_out, "Trait_AUS_agg.rds"))