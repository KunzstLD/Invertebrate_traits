# =========================================================================
#### Aggregation SA traits ####
# =========================================================================

Trait_SA <- readRDS(file.path(data_cleaned, "SA", "Trait_SA_pp_harmonised.rds"))
Trait_SA[, subfamily := NULL]

# check completeness of trait data
completeness_trait_data(Trait_SA,
                        non_trait_cols = c("unique_ID",
                                           "species",
                                           "genus",
                                           "family",
                                           "order", 
                                           "taxon", 
                                           "Body shape_Others - specify"))

# Trait aggregation
# Overall 105 families left
# two taxa get all zeros for certain grouping features when aggregated via the 
# median: Baetidae and Aeshnidae
Trait_SA_agg <- direct_agg(
  trait_data = Trait_SA,
  non_trait_cols = c(
    "unique_ID",
    "species",
    "genus",
    "family",
    "order",
    "taxon",
    "Body shape_Others - specify"
  ),
  method = median
)

# Baetidae: aggregated body shape via mean
Baetidae_agg <-
  direct_agg(
    trait_data = Trait_SA[family == "Baetidae", .SD,
                          .SDcols = patterns("species|genus|family|order|bf")],
    non_trait_cols = c("unique_ID",
                       "species",
                       "genus",
                       "family",
                       "order"),
    method = mean
  )

# Aeshnidae: Voltinism via mean
Aeshnidae_agg <- direct_agg(
  trait_data = Trait_SA[family == "Aeshnidae", .SD,
                        .SDcols = patterns("species|genus|family|order|volt")],
  non_trait_cols = c("unique_ID",
                     "species",
                     "genus",
                     "family",
                     "order"),
  method = mean
)

# merge back
Trait_SA_agg[Baetidae_agg,
             `:=`(
               bf_flattened = i.bf_flattened,
               bf_spherical = i.bf_spherical,
               bf_streamlined = i.bf_streamlined,
               bf_cylindrical = i.bf_cylindrical
             ),
             on = "family"]

Trait_SA_agg[Aeshnidae_agg,
             `:=`(volt_semi = i.volt_semi,
                  volt_uni = i.volt_uni,
                  volt_bi_multi = i.volt_bi_multi),
             on = "family"]

# normalization
normalize_by_rowSum(
  x = Trait_SA_agg,
  non_trait_cols = c(
    "order",
    "family"
  )
)

# Traits for bf, ovip and size mostly missing
completeness_trait_data(Trait_SA_agg,
                        non_trait_cols = c("unique_ID",
                                           "species",
                                           "genus",
                                           "family",
                                           "order",
                                           "Taxon"))

# remaining 9 families after removing incomplete trait profiles
na.omit(Trait_SA_agg)


# Sum of NA's function
sum_na <- function(x){
  sum(is.na(x)) 
}
ord_1 <- which(Trait_SA_agg[, apply(.SD, 1, sum_na)] == 0)
ord_2 <- which(Trait_SA_agg[, apply(.SD, 1, sum_na)] == 1)
ord_3 <- which(Trait_SA_agg[, apply(.SD, 1, sum_na)] %between% c(3, 6))
remain <- which(Trait_SA_agg[, apply(.SD, 1, sum_na)] >= 7)
Trait_SA_agg <- rbind(Trait_SA_agg[ord_1, ],
                      Trait_SA_agg[ord_2, ],
                      Trait_SA_agg[ord_3, ],
                      Trait_SA_agg[remain, ]) 

# Postprocessing ----
# Add two columns with ovip terrestrial and ovip ovoviviparity
Trait_SA_agg[, `:=`(ovip_ter = "?", ovip_ovo = "?")]
setcolorder(Trait_SA_agg, c("family", "order"))

# Check for families in the final aggregated dataset if taxa within these families
# have body shape other == 1
# taxa_bf_others <- readRDS(file.path(data_cleaned,
#                    "SA",
#                    "taxa_bf_others.rds"))
# Trait_SA_agg[family %in% taxa_bf_others, ]
# View(Trait_SA[family %in% taxa_bf_others, .SD, .SDcols = patterns("species|genus|family|order|bf|Body shape")])

# save
saveRDS(Trait_SA_agg,
        file.path(data_cleaned,
                  "SA",
                  "Trait_SA_aggregated.rds"))

fwrite(Trait_SA_agg,
       file.path(data_cleaned,
                 "SA",
                 "Trait_SA_aggregated.csv"))



