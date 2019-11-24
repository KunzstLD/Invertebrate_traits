
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
trait_names_pattern <-
  names(Trait_NZ[, -c("family",
                      "genus",
                      "species",
                      "order"
  )]) %>%
  sub("\\_.*|\\..*", "", .) %>%
  unique() %>%
  paste0("^", .)

output <- matrix(ncol = 2, nrow = length(trait_names_pattern))

for (i in seq_along(trait_names_pattern)) {
  # vector containing either 0 (no NAs) or a number (> 0) meaning that all
  # entries for this trait contained NA
  vec <-
    Trait_NZ[, apply(.SD, 1, function(y)
      base::sum(is.na(y))),
      .SDcols = names(Trait_NZ) %like% trait_names_pattern[[i]]]
  
  # How complete is the dataset for each individual trait?
  output[i,] <-
    c((length(vec[vec == 0]) / nrow(Trait_NZ))  %>% `*` (100) %>% round(),
      trait_names_pattern[[i]])
}
# output

# just return rows where for each trait there is an observation 
Trait_NZ <- na.omit(Trait_NZ,
                    cols = names(Trait_NZ[, -c("species", "genus", "family", "order")]))

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

# subset so that no NA values occur in Species data (otherwise all NA entries are viewed as a group &
# aggregated as well)
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
Trait_NZ_resol_fam <-
  Trait_NZ[is.na(species) & is.na(genus) & !is.na(family), ] %>%
  .[!family %in% Trait_fam$family, -c("species", "genus")]

# no duplicates
# Trait_NZ_resol_fam$family %>% duplicated()

# rbind with trait data resolved on family level
Trait_NZ_agg <- rbind(Trait_NZ_resol_fam, Trait_fam) 

# rm dev_no_insect category (just zeros)
Trait_NZ_agg[, dev_no_insect := NULL]

# feeding mode parasite not present in NZ -> add artificial column
Trait_NZ_agg[, feed_parasite := rep(0, nrow(Trait_NZ_agg))]

# save
saveRDS(object = Trait_NZ_agg, file = file.path(data_out, "Trait_NZ_agg.rds"))
