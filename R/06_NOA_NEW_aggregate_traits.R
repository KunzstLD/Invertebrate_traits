# _________________________________________________________________________
#### Aggregation of traits NOA data from Laura T #### 
# TODO: Check and simplify Aggregation functions
# _________________________________________________________________________

# read in pp_harmonized
Trait_Noa_new <- readRDS(file = file.path(data_cleaned,
                                      "North_America",
                                      "Traits_US_LauraT_pp_harmonized.rds"))

# Check completeness of trait data
# just return rows where for each trait there is an observation 
# oviposition is clearly the bottleneck!
completeness_trait_data(x = Trait_Noa_new,
                        non_trait_cols = c("unique_id",
                                           "species",
                                           "genus",
                                           "family",
                                           "order"))

# subset of traits (remove certain columns?) 
# currently, oviposition not used
Trait_Noa_new <- Trait_Noa_new[,.SD,
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
Trait_Noa_new_agg <- direct_agg(
  trait_data = Trait_Noa_new,
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order",
                     "unique_id"),
  method = median
)

# remove taxa with incomplete trait profiles and normalize again
Trait_Noa_new_agg <- normalize_by_rowSum(x = Trait_Noa_new_agg,
                                         non_trait_cols = c("order",
                                                            "family")) %>%
  na.omit(.,
          cols = names(.[, -c("family",
                              "order")]))
  
  
# save
saveRDS(object = Trait_Noa_new_agg,
        file = file.path(data_out, "Trait_Noa_agg.rds"))
