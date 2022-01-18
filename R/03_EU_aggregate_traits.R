# _________________________________________________________________________
#### Aggregation of traits EU ####
# _________________________________________________________________________

# read in harmonized, preprocessed & already normalized EU Trait DB
# data are already normalized!
Trait_EU <-
  readRDS(file = file.path(
    data_cleaned,
    "EU",
    "Trait_freshecol_2020_pp_harmonized.rds"
  ))

# check completeness of trait data
completeness_trait_data(
  x = Trait_EU,
  non_trait_cols =
    c(
      "order",
      "family",
      "genus",
      "species"
    )
)

# Choose traits &
# restrict to certain orders
Trait_EU <- Trait_EU[, .SD,
  .SDcols = names(Trait_EU) %like% "locom|feed|resp|volt|size|bf|ovip|dev|species|genus|family|order"
] %>%
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
  ), ]


# Trait aggregation to family-lvl
# Family that "get's lost" in median aggregation: Culicidae
# Median aggregation produces just zeros for their feeding modes
Trait_EU_agg <- direct_agg(
  trait_data = Trait_EU,
  non_trait_cols = c(
    "species",
    "genus",
    "family",
    "order"
  ),
  method = median
)
# Trait_EU_agg[family == "Culicidae", ]

# Add Culicidae (mean aggregation for feeding mode)
culicidae_agg <- direct_agg(
  trait_data = Trait_EU[family == "Culicidae", .SD,
                        .SDcols = patterns("feed|species|genus|family|order")],
  non_trait_cols = c("species",
                     "genus",
                     "family",
                     "order"),
  method = mean
)
Trait_EU_agg[culicidae_agg,
             `:=`(
               feed_shredder = i.feed_shredder,
               feed_gatherer = i.feed_gatherer,
               feed_predator = i.feed_predator,
               feed_parasite = i.feed_parasite,
               feed_herbivore = i.feed_herbivore,
               feed_filter = i.feed_filter
             ),
             on = "family"]

# Just return rows where for each trait there is an observation
Trait_EU_agg <- normalize_by_rowSum(
  x = Trait_EU_agg,
  non_trait_cols = c(
    "order",
    "family"
  )
) %>%
  na.omit(.,
    cols = names(.[, -c(
      "family",
      "order"
    )])
  )

# Save as RDS object
saveRDS(object = Trait_EU_agg, file = file.path(data_out, "Trait_EU_agg.rds"))
# For TPG analysis
saveRDS(
  object = Trait_EU_agg,
  file = "/home/kunzst/Dokumente/Projects/Trait_DB/Convergence-trait-profiles/Data/Trait_EU_agg.rds"
)
