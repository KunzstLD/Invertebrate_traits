# =========================================================================
#### Aggregation of traits EU ####
# =========================================================================

# read in harmonized, preprocessed & already normalized EU Trait DB
# data are already normalized!
Trait_EU <-
  readRDS(file = file.path(data_cleaned, "EU", "Trait_EU_pp_harmonized.rds"))

# check completeness of trait data
completeness_trait_data(x = Trait_EU,
                        non_trait_cols =
                          c("order",
                            "family",
                            "genus",
                            "species"))

# Choose traits &
# restrict to certain orders
Trait_EU <- Trait_EU[, .SD,
                     .SDcols = names(Trait_EU) %like% "locom|feed|resp|volt|size|bf|ovip|dev|species|genus|family|order"] %>%
  .[order %in% c(
    "Ephemeroptera",
    "Hemiptera",
    "Odonata",
    "Trichoptera",
    "Coleoptera",
    "Plecoptera",
    "Diptera",
    "Megaloptera",
    "Neuroptera"
  ),]


# trait aggregation to family-lvl
Trait_EU_agg <- direct_agg(
  trait_data = Trait_EU,
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order"),
  method = median
)

# just return rows where for each trait there is an observation
Trait_EU_agg <- normalize_by_rowSum(x = Trait_EU_agg,
                                    non_trait_cols = c("order",
                                                       "family")) %>%
  na.omit(.,
          cols = names(.[, -c("family",
                              "order")]))

  
# TODO: BF information, check that again!
# family information in tachet not completely covered in agg. freshwaterecol
# BF information missing for few families
# 6 of thoses 11 taxa are covered in NOA DB
# Taxa_famlvl <-
# tachet[is.na(species) & is.na(genus) & !is.na(family),] %>%
  # .[!family %in% Trait_fam$family,] %>%
# .[, .SD, .SDcols = names(tachet) %like% "locom|feed|resp|volt|size|dev|bf|species|genus|family|order"] %>%
# .[order %in% c(
#   "Ephemeroptera",
#   "Hemiptera",
#   "Odonata",
#   "Trichoptera",
#   "Coleoptera",
#   "Plecoptera",
#   "Diptera"
# ), ]

# cache as RDS object
saveRDS(object = Trait_EU_agg, file = file.path(data_out, "Trait_EU_agg.rds"))