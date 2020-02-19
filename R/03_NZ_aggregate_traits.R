
# _________________________________________________________________________
####  Aggregation of traits NZ ####
# _________________________________________________________________________

# read in Trait NZ
Trait_NZ <- readRDS(file.path(data_cleaned, "NZ", "Trait_NZ_pp_harmonized.rds"))

# _________________________________________________________________________
#### Normalization #### 
# _________________________________________________________________________
Trait_NZ <- normalize_by_rowSum(
  x = Trait_NZ,
  non_trait_cols = c("order",
                     "family",
                     "genus",
                     "species")
)

# test how complete trait sets are 
# 90 % dev -> the other 10 % are not aquatic insects
completeness_trait_data(x = Trait_NZ,
                        non_trait_cols = c("order",
                                           "family",
                                           "genus",
                                           "species"))

# just return rows where for each trait there is an observation 
Trait_NZ <- na.omit(Trait_NZ,
                    cols = names(Trait_NZ[, -c("species", 
                                               "genus", 
                                               "family",
                                               "order")]))

# Subset to interesting orders:
#   "Amphipoda" & "Venerida" missing
Trait_NZ <- Trait_NZ[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera"
), ]

# _________________________________________________________________________
#### Aggregate to genus level ####

# Interesting? 
  # Depending on aggregation method:
  # How many species used for Aggregation
  # from Species to Genus 
  # from Genus to Family (already implemented)
# _________________________________________________________________________

# create vector with trait names
trait_col <- names(Trait_NZ[, -c("family",
                                 "genus",
                                 "species",
                                 "order")])

# subset so that no NA values occur in Species data 
# (otherwise all NA entries are viewed as a group & aggregated as well)
Trait_NZ_genus <- Trait_NZ[!is.na(species), lapply(.SD, median), 
                           .SDcols = trait_col, 
                           by = genus]

# merge family information back
Trait_NZ_genus[Trait_NZ[!is.na(species), ], 
               `:=`(family = i.family,
                    order = order),
               on = "genus"]

# bind with data resolved on Genus level 
Trait_NZ_genus <-
  rbind(Trait_NZ_genus, Trait_NZ[is.na(species) &
                                   !is.na(genus), -c("species")])

# _________________________________________________________________________
#### Aggregate on family level ####
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
# _________________________________________________________________________
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
})),
.SDcols = trait_col,
by = "family"]

# merge back information on order 
Trait_fam[Trait_NZ_genus, 
          `:=`(order = order), 
          on = "family"]

# filter for taxa resolved on family level that are not present in aggregated dataset
# Those will be added as well
Taxa_famlvl <-
  Trait_NZ[is.na(species) & is.na(genus) & !is.na(family), ] %>%
  .[!family %in% Trait_fam$family, -c("species", "genus")]
# no duplicates
# Taxa_famlvl$family %>% duplicated()

# rbind with trait data resolved on family level
Trait_NZ_agg <- rbind(Taxa_famlvl, Trait_fam) 

# feeding mode parasite not present in NZ -> add "artificial" column
Trait_NZ_agg[, feed_parasite := rep(0, nrow(Trait_NZ_agg))]

# save
saveRDS(object = Trait_NZ_agg, file = file.path(data_out, "Trait_NZ_agg.rds"))
