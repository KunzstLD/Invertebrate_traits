# _________________________________________________________________________
# Aggregation of traits NOA data from Laura T ----
# NoA trait data already normalised
# for(i in c("locom", "feed", "resp", "volt", "bf", "size", "dev", "ovip")) {
# print(all(!rowSums(Trait_Noa_new[, .SD, .SDcols = patterns(i)], na.rm = TRUE) > 1))
# }
# _________________________________________________________________________

# read in pp_harmonized
Trait_Noa_new <- readRDS(file = file.path(data_cleaned,
                                      "North_America",
                                      "Traits_US_LauraT_pp_harmonized.rds"))

# Check completeness of trait data
# just return rows where for each trait there is an observation 
# oviposition is clearly the bottleneck!
completeness_trait_data(
  x = Trait_Noa_new,
  non_trait_cols = c("unique_id",
                     "species",
                     "genus",
                     "family",
                     "order")
)

# subset of traits (remove certain columns?)
# currently, oviposition not used
Trait_Noa_new <- Trait_Noa_new[, .SD,
                               .SDcols = names(Trait_Noa_new) %like%
                                 "locom|feed|resp|volt|bf|size|dev|ovip|unique_id|species|genus|family|order"]

# subset to aq. insect orders
Trait_Noa_new <- Trait_Noa_new[order %in% c(
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

# aggregate to family-lvl using direct-agg via median
# Four families "get lost" during median aggregation
# "Leptoceridae", "Ephydridae", "Odontoceridae"
Trait_Noa_new_agg <- direct_agg(
  trait_data = Trait_Noa_new,
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order",
                     "unique_id"),
  method = median
)

# Use mean for feeding mode of 
# "Ephydridae", "Odontoceridae"
ephy_odonto_agg <- direct_agg(
  trait_data = Trait_Noa_new[family %in% c("Ephydridae",
                                           "Odontoceridae"), .SD,
                             .SDcols = patterns("feed|species|genus|family|order")],
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order"),
  method = mean
)
Trait_Noa_new_agg[ephy_odonto_agg,
                  `:=`(
                    feed_shredder = i.feed_shredder,
                    feed_gatherer = i.feed_gatherer,
                    feed_predator = i.feed_predator,
                    feed_parasite = i.feed_parasite,
                    feed_herbivore = i.feed_herbivore,
                    feed_filter = i.feed_filter
                  ),
                  on = "family"]

# Median aggregation leads to zero entries for size traits of "Leptoceridae"
leptoceridae_agg <- direct_agg(
  trait_data = Trait_Noa_new[family == "Leptoceridae", .SD,
                             .SDcols = patterns("size|species|genus|family|order")],
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order"),
  method = mean
)
Trait_Noa_new_agg[leptoceridae_agg,
                  `:=`(size_small = i.size_small,
                       size_medium = i.size_medium,
                       size_large = i.size_large),
                  on = "family"]

# Remove taxa with incomplete trait profiles and normalize again
Trait_Noa_new_agg <- normalize_by_rowSum(x = Trait_Noa_new_agg,
                                         non_trait_cols = c("order",
                                                            "family")) %>%
  na.omit(.,
          cols = names(.[, -c("family",
                              "order")]))

# Change col order
setcolorder(
  x = Trait_Noa_new_agg,
  neworder = c(
    grep("feed.*", names(Trait_Noa_new_agg), value = TRUE),
    grep("resp.*", names(Trait_Noa_new_agg), value = TRUE),
    grep("locom.*", names(Trait_Noa_new_agg), value = TRUE),
    grep("size.*", names(Trait_Noa_new_agg), value = TRUE),
    grep("ovip.*", names(Trait_Noa_new_agg), value = TRUE),
    grep("bf.*", names(Trait_Noa_new_agg), value = TRUE),
    grep("volt.*", names(Trait_Noa_new_agg), value = TRUE),
    grep("dev.*", names(Trait_Noa_new_agg), value = TRUE)
  )
)

# save
saveRDS(object = Trait_Noa_new_agg,
        file = file.path(data_out, "Trait_NOA_agg.rds"))
saveRDS(object = Trait_Noa_new_agg,
        file = "/home/kunzst/Dokumente/Projects/Trait_DB/Convergence-trait-profiles/Data/Trait_NOA_agg.rds")
