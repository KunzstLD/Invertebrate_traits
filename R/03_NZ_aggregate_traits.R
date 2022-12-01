# _________________________________________________________________________
#  Aggregation of traits NZ ----
# _________________________________________________________________________

# read in Trait NZ
Trait_NZ <- readRDS(file.path(data_cleaned, "NZ", "Trait_NZ_pp_harmonized.rds"))

# Subset to interesting orders:
# "Amphipoda" & "Venerida" missing
Trait_NZ <- Trait_NZ[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera",
  "Megaloptera",
  "Neuroptera"
), ]

# test how complete trait sets are 
# 90 % dev -> the other 10 % are not aquatic insects
completeness_trait_data(x = Trait_NZ,
                        non_trait_cols = c("order",
                                           "family",
                                           "genus",
                                           "species",
                                           "unique_id"))
Trait_NZ <- Trait_NZ[, .SD,
                     .SDcols = names(Trait_NZ) %like%
                       "locom|feed|resp|volt|size|bf|ovip|dev|species|genus|family|order"]

# Direct aggregation using the median
Trait_NZ_agg <- direct_agg(
  trait_data = Trait_NZ,
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order",
                     "unique_id"),
  method = median
)

# just return rows where for each trait there is an observation 
# add feeding mode parasite not present in NZ -> add "artificial" column
# and normalize again!
Trait_NZ_agg <-
  Trait_NZ_agg[, feed_parasite := rep(0, nrow(Trait_NZ_agg))] %>%
  normalize_by_rowSum(
    x = .,
    non_trait_cols = c("order",
                       "family")
  ) %>%
  na.omit(.,
          cols = names(.[, -c("family",
                              "order")]))

# save
saveRDS(object = Trait_NZ_agg, file = file.path(data_out, "Trait_NZ_agg.rds"))
saveRDS(object = Trait_NZ_agg, 
        file = "/home/kunzst/Dokumente/Projects/Trait_DB/Convergence-trait-profiles/Data/Trait_NZ_agg_ovip.rds")
