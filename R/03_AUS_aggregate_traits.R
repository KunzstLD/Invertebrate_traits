
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

# _________________________________________________________________________ 
#### Normalization ####
# All trait states of one trait are divided by the row sum
# Hence, trait affinities are represented as "%" or ratios 
# _________________________________________________________________________ 
Trait_AUS <- normalize_by_rowSum(
  x = Trait_AUS,
  non_trait_cols = c("unique_id",
                     "species",
                     "genus",
                     "family",
                     "order")
)

# test how complete trait sets are 
completeness_trait_data(
  x = Trait_AUS,
  non_trait_cols = c("unique_id",
                     "species",
                     "genus",
                     "family",
                     "order")
)

# Subset to relevant traits & orders
Trait_AUS <- Trait_AUS[, .SD,
                       .SDcols = names(Trait_AUS) %like%
                         "locom|feed|resp|volt|size|bf|dev|ovip|unique_id|species|genus|family|order"] %>%
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


# create taxa column
Trait_AUS[, taxa := coalesce(species, genus, family, order)]

# Body form information from Ben Kefford, Leon Metz., Verena Schreiner and me
# (on family-lvl)
BF_AUS <- fread(file.path(
  data_missing,
  "Missing_traits_AUS",
  "BF",
  "AUS_BF_missing_final.csv"
)) %>%
  na.omit(.)

BF_AUS[, bf_streamlined := as.double(bf_streamlined)]

# merge bf information back 
Trait_AUS[BF_AUS,
          `:=`(
            bf_spherical = i.bf_spherical,
            bf_flattened = i.bf_flattened,
            bf_cylindrical = i.bf_cylindrical,
            bf_streamlined = i.bf_streamlined
          ),
          on = c(taxa = "family")]

# trait aggregation to family-lvl
Trait_AUS_agg <- direct_agg(
  trait_data = Trait_AUS,
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order",
                     "taxa",
                     "unique_id"),
  method = median
)

# just return rows where for each trait there is an observation 
Trait_AUS_agg <- normalize_by_rowSum(Trait_AUS_agg,
                                     non_trait_cols = c("order",
                                                        "family")) %>%
  na.omit(.,
          cols = names(.[, -c("family",
                              "order")]))
  

# save
saveRDS(object = Trait_AUS_agg,
        file = file.path(data_out, "Trait_AUS_agg.rds"))
