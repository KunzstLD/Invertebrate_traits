# =========================================================================
#### Aggregation SA traits ####
# =========================================================================

Trait_SA <- readRDS(file.path(data_cleaned, "SA", "Trait_SA_pp_harmonised.rds"))
Trait_SA[, Subfamily := NULL]

# Normalisation
test <- normalize_by_rowSum(Trait_SA[,.SD,
                                     .SDcols = patterns("bf.|feed|locom|ovip|resp|size|volt\\_|unique_ID|species|genus|family|order")],
                            non_trait_cols = c("unique_ID",
                                               "species",
                                               "genus",
                                               "family",
                                               "order"))
completeness_trait_data(test,
                        non_trait_cols = c("unique_ID",
                                           "species",
                                           "genus",
                                           "family",
                                           "order"))

# Aggregation
# 111 families left
test_agg <- direct_agg(
  trait_data = test,
  non_trait_cols = c(
    "species",
    "genus",
    "family",
    "order",
    "unique_ID"
  ),
  method = median
)
test_agg[grepl("Chironomidae", family), ]

# Traits for bf, ovip and size mostly missing
completeness_trait_data(test_agg,
                        non_trait_cols = c("unique_ID",
                                           "species",
                                           "genus",
                                           "family",
                                           "order"))

# remaining 8 families after removing incomplete trait profiles
na.omit(test_agg[, .SD, .SDcols = patterns("bf|feed|locom|resp|family")])

# Difficult to find out where to close gaps in the full dataset?
# use the final number of families for the body form data 
test_agg[, family]
