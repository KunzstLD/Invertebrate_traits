# _________________________________________________________________________
# Harmonize EU Traits ----
# For traits from different databases, the maximum value is taken
# For traits that are combined from one db, the sum is taken
# _________________________________________________________________________

# read in RDS
Trait_EU <-
  readRDS(file.path(data_cleaned, "EU", "Trait_freshecol_2020_pp_for_harm.rds"))

# normalize freshecol & tachet separately
tachet_cols <- grep("tachet.*", names(Trait_EU), value = TRUE)
fc_cols <- grep("tachet.*", names(Trait_EU), value = TRUE, invert = TRUE)

Trait_EU <- cbind(
  normalize_by_rowSum(
    Trait_EU[, ..fc_cols],
    non_trait_cols = c(
      "species",
      "genus",
      "family",
      "order",
      "taxon_cp",
      "ID_AQEM"
    )
  ),
  normalize_by_rowSum(Trait_EU[, ..tachet_cols])
)

saveRDS(Trait_EU, 
        "/home/kunzst/Schreibtisch/Trait_DB_EU_corrected.rds")

# _________________________________________________________________________
# Voltinism ----
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________
# Trait_EU[, volt_semi := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "voltinism_semi",
#     "voltinism_semi_tachet"
#   )
# ]
# Trait_EU[, volt_uni := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "voltinism_uni",
#     "voltinism_uni_tachet"
#   )
# ]
# 
# # Trait_EU[, volt_bi_multi := apply(.SD, 1, max, na.rm = TRUE),
# #   .SDcols = c(
# #     "voltinism_bi",
# #     "voltinism_tri",
# #     "voltinism_multi",
# #     "voltinism_multi_tachet",
# #     "voltinism_flex"
# #   )
# # ]
# Trait_EU[, volt_bi_multi := apply(.SD, 1, sum),
#          .SDcols = c(
#            "voltinism_bi",
#            "voltinism_tri",
#            "voltinism_multi",
#            "voltinism_flex"
#          )
# ]
# Trait_EU[, volt_bi_multi := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("volt_bi_multi",
#                      "voltinism_multi_tachet")] 

## Semivoltine ----
Trait_EU[, volt_semi := fcase(
  !is.na(voltinism_semi), voltinism_semi,
  is.na(voltinism_semi), voltinism_semi_tachet,
  is.na(voltnism_semi) & is.na(voltinism_semi_tachet), NA_real_
)]

## Univoltine ----
Trait_EU[, volt_uni := fcase(
  !is.na(voltinism_uni), voltinism_uni,
  is.na(voltinism_uni), voltinism_uni_tachet,
  is.na(voltnism_uni) & is.na(voltinism_uni_tachet), NA_real_
)]

## Bi/multivoltine ----
Trait_EU[, volt_bi_multi := apply(.SD, 1, sum),
         .SDcols = c(
           "voltinism_bi",
           "voltinism_tri",
           "voltinism_multi",
           "voltinism_flex"
         )
]
Trait_EU[, volt_bi_multi := fcase(
  !is.na(volt_bi_multi), volt_bi_multi,
  is.na(volt_bi_multi), voltinism_multi_tachet,
  is.na(volt_bi_multi) & is.na(voltinism_multi_tachet), NA_real_
)]

Trait_EU[, c(
  "voltinism_semi",
  "voltinism_semi_tachet",
  "voltinism_uni",
  "voltinism_uni_tachet",
  "voltinism_bi",
  "voltinism_tri",
  "voltinism_multi",
  "voltinism_multi_tachet",
  "voltinism_flex"
) := NULL]

# _________________________________________________________________________
# TODO
# Aquatic stages ----
# gives information about which stage lives in the aquatic phase
# stage_egg
# stage_larva: larva and/or nymph
# stage_pupa
# stage_adult
# _________________________________________________________________________
# Trait_EU[, stage_larva := apply(.SD, 1, max),
#          .SDcols = c("stage_larva", "stage_nymph")
# ]
# Trait_EU[, stage_nymph := NULL]


# _________________________________________________________________________
# TODO
# Ph ----
# ph_acidic, ph_neutral
# pH_ind (indifferent) is dismissed from database (128 entries with 1)
# _________________________________________________________________________
# setnames(Trait_EU, "ph_neutral_alk", "ph_neutral")
# Trait_EU[, ph_ind := NULL]


# _________________________________________________________________________
# Feed mode ----
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# xylophagus can fit to the category of shredders, but see Lancaster:
# "The mouthparts of facultative species tend to fit the generic model for
# most shredders.
# Obligate xylophages, however, often have further adaptations for mining wood,
# and virtually all
# have strong, heavily sclerotized mandibles. Larvae of the caddisfly Lype phaeope
# (Psychomyiidae) scrape off layers of wood with their mandibles,
# and collect the fragments on the forelegs ( Spänhoff et al.2003 )."
# feed_gatherer: collector gatherer (gatherers, detritivores, deposit feeder)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivroe: scraper (grazer) & herbivore piercer
# feed_predator: predator
# feed_parasite: parasite

# Predators are defined as: Engulfers (ingest pref whole or in parts) or
# as Piercers (piere prey tissues and suck fluids)
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________

# coding from PUP for piercer_t
piercer_pup <- fread(file.path(data_in, "EU", "taxa_feed_piercer_PUP.csv"))
piercer_pup[, taxa := coalesce(genus, family, order)]
setnames(
  piercer_pup,
  'comment (only for the trait category ""piercer"")',
  "comment_piercer"
)
# normalize
normalize_by_rowSum(
  x = piercer_pup,
  non_trait_cols = c(
    "genus",
    "family",
    "order",
    "comment_piercer",
    "taxa"
  )
)

# new taxa col for trait EU
Trait_EU[, taxa := coalesce(species, genus, family, order)]
Trait_EU[, taxa_adjusted := sub(" sp\\.", "", taxa)]

# merge PUP comments
Trait_EU[piercer_pup,
  `:=`(
    feed_absorber_tachet = i.feed_active_filter_abs,
    feed_filter_tachet = i.feed_active_filter,
    feed_deposit_feeder_tachet = i.feed_gatherer,
    feed_shredder_tachet = i.feed_shredder,
    feed_scraper_tachet = i.feed_scraper,
    feed_predator_tachet = i.feed_predator,
    feed_parasite_tachet = i.feed_parasite,
    feed_piercer_tachet = i.feed_piercer_t,
    comment_piercer = i.comment_piercer
  ),
  on = c(taxa_adjusted = "taxa")
]

# assign affinities from feed_piercer_tachet to
# either predator, parasite or herbivore
# Trait_EU$comment_piercer %>% unique
Trait_EU[
  grep("predator\\/parasite", comment_piercer),
  `:=`(
    feed_predator_tachet = feed_piercer_tachet,
    feed_piercer_tachet = 0
  )
]
Trait_EU[
  grep("(?=.*predator)(?!.*parasite)(?!.*microphyte)",
    comment_piercer,
    perl = TRUE
  ),
  `:=`(
    feed_predator_tachet = feed_piercer_tachet,
    feed_piercer_tachet = 0
  )
]
Trait_EU[
  grep("^microinvertebrate piercer$", comment_piercer),
  `:=`(
    feed_predator_tachet = feed_piercer_tachet,
    feed_piercer_tachet = 0
  )
]
Trait_EU[
  grep("parasite", comment_piercer),
  `:=`(
    feed_parasite_tachet = feed_piercer_tachet,
    feed_piercer_tachet = 0
  )
]
Trait_EU[
  grep("microphyte.*predators.*", comment_piercer),
  `:=`(
    feed_predator_tachet = feed_piercer_tachet,
    feed_piercer_tachet = 0
  )
]
Trait_EU[
  grep("(?=.*macrophyte|microphyte)(?!.*predators)",
    comment_piercer,
    perl = TRUE
  ),
  `:=`(
    feed_scraper_tachet = feed_piercer_tachet,
    feed_piercer_tachet = 0
  )
]

# few taxa classified based on genus and family lvl
# assessment
# Trait_EU[feed_piercer_tachet > 0, .(species, genus, family,feed_piercer_tachet,
#                                     feed_predator_tachet, feed_parasite_tachet,
#                                     feed_scraper_tachet, comment_piercer)]

# genus lvl
Trait_EU[
  feed_piercer_tachet > 0 & genus == "Callicorixa",
  `:=`(
    feed_predator_tachet = feed_piercer_tachet,
    feed_piercer_tachet = 0
  )
]

# fam lvl
families_piercer <- Trait_EU[feed_piercer_tachet > 0, family]
lookup_piercer <-
  Trait_EU[
    family %in% families_piercer & !is.na(comment_piercer),
    .(
      species,
      genus,
      family,
      feed_piercer_tachet,
      feed_predator_tachet,
      feed_parasite_tachet,
      feed_scraper_tachet,
      comment_piercer
    )
  ]

# aggr. by mean
cols <- names(lookup_piercer)[names(lookup_piercer) %like% "feed.*"]
lookup_piercer[, (cols) := lapply(.SD, mean),
  .SDcols = cols,
  by = "family"
]
lookup_piercer <- lookup_piercer[!duplicated(family), ]

# merge
Trait_EU_piercer <-
  copy(Trait_EU[feed_piercer_tachet > 0, .(
    species,
    genus,
    family,
    feed_piercer_tachet,
    feed_predator_tachet,
    feed_parasite_tachet,
    feed_scraper_tachet,
    ID_AQEM
  )])

Trait_EU_piercer[lookup_piercer,
  `:=`(
    feed_piercer_tachet = i.feed_piercer_tachet,
    feed_predator_tachet = i.feed_predator_tachet,
    feed_parasite_tachet = i.feed_parasite_tachet,
    feed_scraper_tachet = i.feed_scraper_tachet
  ),
  on = "family"
]
Trait_EU[Trait_EU_piercer,
  `:=`(
    feed_piercer_tachet = i.feed_piercer_tachet,
    feed_predator_tachet = i.feed_predator_tachet,
    feed_parasite_tachet = i.feed_parasite_tachet,
    feed_scraper_tachet = i.feed_scraper_tachet
  ),
  on = "ID_AQEM"
]


# manual assignment for few species
# Margaritifera margaritifera filter feeder
# and parasite as larvae
Trait_EU[
  species == "Margaritifera margaritifera",
  `:=`(
    feed_filter_tachet = 0.5,
    feed_piercer_tachet = 0
  )
]

# Hirudo medicinalis to parasite
Trait_EU[
  species == "Hirudo medicinalis",
  `:=`(
    feed_parasite_tachet = 1,
    feed_piercer_tachet = 0
  )
]

# Haemopis sanguisuga to predators
Trait_EU[
  species == "Haemopis sanguisuga",
  `:=`(
    feed_predator_tachet = 1,
    feed_piercer_tachet = 0
  )
]

# Piscicola geometra to parasites
Trait_EU[
  species == "Piscicola geometra",
  `:=`(
    feed_parasite_tachet = 1,
    feed_piercer_tachet = 0
  )
]

# Aphelocheirus aestivalis to predators
Trait_EU[
  species == "Aphelocheirus aestivalis",
  `:=`(
    feed_predator_tachet = 1,
    feed_piercer_tachet = 0
  )
]

# for feed other use information in tachet if available
fwe_feedmodes <- c(
  "feed_grazer",
  "feed_miner",
  "feed_xylo",
  "feed_shredder",
  "feed_gatherer",
  "feed_active_filter",
  "feed_passive_filter",
  "feed_predator",
  "feed_parasite",
  "feed_other"
)

Trait_EU[
  feed_other > 0 & !is.na(feed_absorber_tachet),
  c(fwe_feedmodes) := NA_real_
]

# - Haliplidae -> herbivores as larvae!
# https://www.sciencedirect.com/topics/agricultural-and-biological-sciences/haliplidae
Trait_EU[
  feed_other > 0 & family == "Haliplidae",
  `:=`(
    feed_herbivore = 1,
    feed_other = 0
  )
]

# rm for those were feed_other > 0.5
# set to zero for other, since this
# feeding mode does not represent one of the used categories
Trait_EU <- Trait_EU[is.na(feed_other) | feed_other < 0.5, ]
Trait_EU[feed_other > 0, feed_other := 0]

## Shredders ----
# Trait_EU[, feed_shredder := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "feed_shredder",
#     "feed_shredder_tachet",
#     "feed_miner",
#     "feed_xylo"
#   )
# ]
Trait_EU[, feed_shredder := apply(.SD, 1, sum),
         .SDcols = c("feed_shredder",
                     "feed_miner",
                     "feed_xylo")]
# Trait_EU[, feed_shredder := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("feed_shredder",
#                      "feed_shredder_tachet")]
Trait_EU[, feed_shredder := fcase(
  !is.na(feed_shredder), feed_shredder,
  is.na(feed_shredder), feed_shredder_tachet,
  is.na(feed_shredder) & is.na(feed_shredder_tachet), NA_real_
)]

## Filterers ----
# Trait_EU[, feed_filter := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "feed_active_filter",
#     "feed_passive_filter",
#     "feed_filter_tachet",
#     "feed_absorber_tachet"
#   )
# ]
Trait_EU[, feed_filter := apply(.SD, 1, sum),
         .SDcols = c("feed_active_filter",
                     "feed_passive_filter")]
Trait_EU[, feed_filter_tachet := apply(.SD, 1, sum),
         .SDcols = c("feed_filter_tachet",
                     "feed_absorber_tachet")]
# Trait_EU[, feed_filter := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("feed_filter",
#                      "feed_filter_tachet")]
Trait_EU[, feed_filter := fcase(
  !is.na(feed_filter), feed_filter,
  is.na(feed_filter), feed_filter_tachet,
  is.na(feed_filter) & is.na(feed_filter_tachet), NA_real_
)]


## Herbivore ----
# Trait_EU[, feed_herbivore := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "feed_scraper_tachet",
#     "feed_grazer"
#   )
# ]
Trait_EU[, feed_herbivore := fcase(
  !is.na(feed_grazer), feed_grazer,
  is.na(feed_grazer), feed_scraper_tachet,
  is.na(feed_grazer) & is.na(feed_scraper_tachet), NA_real_
)]

## Gatherer ----
# Trait_EU[, feed_gatherer := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "feed_gatherer",
#     "feed_deposit_feeder_tachet"
#   )
# ]
Trait_EU[, feed_gatherer := fcase(
  !is.na(feed_gatherer), feed_gatherer,
  is.na(feed_gatherer), feed_deposit_feeder_tachet,
  is.na(feed_gatherer) & is.na(feed_deposit_feeder_tachet), NA_real_
)]


## Predator ----
# Trait_EU[, feed_predator := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "feed_predator",
#     "feed_predator_tachet"
#   )
# ]
Trait_EU[, feed_predator := fcase(
  !is.na(feed_predator), feed_predator,
  is.na(feed_predator), feed_predator_tachet,
  is.na(feed_predator) & is.na(feed_predator_tachet), NA_real_
)]


## Parasite ----
# Trait_EU[, feed_parasite := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "feed_parasite",
#     "feed_parasite_tachet"
#   )
# ]
Trait_EU[, feed_parasite := fcase(
  !is.na(feed_parasite), feed_parasite,
  is.na(feed_parasite), feed_parasite_tachet,
  is.na(feed_parasite) & is.na(feed_parasite_tachet), NA_real_
)]

## Other & postprocessing ----
Trait_EU[, c(
  "feed_shredder_tachet",
  "feed_miner",
  "feed_xylo",
  "feed_active_filter",
  "feed_passive_filter",
  "feed_filter_tachet",
  "feed_absorber_tachet",
  "feed_scraper_tachet",
  "feed_grazer",
  "feed_deposit_feeder_tachet",
  "feed_predator_tachet",
  "feed_parasite_tachet",
  "feed_piercer_tachet",
  "feed_other",
  "comment_piercer"
) := NULL]


# _________________________________________________________________________
# Locomotion ----
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler
# locm_burrow: burrower
# locm_sessil: sessil (attached)
# _________________________________________________________________________

## Locomotion  other ----
Trait_EU[
  locom_other > 0 & !is.na(locom_crawler_tachet),
  locom_other := 0
]

## Swimming ----
# Trait_EU[, locom_swim := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "locom_swim_skate",
#     "locom_swim_dive",
#     "locom_surface_swimmer_tachet",
#     "locom_full_water_swimmer_tachet"
#   )
# ]
Trait_EU[, locom_swim := apply(.SD, 1, sum),
         .SDcols = c("locom_swim_skate",
                     "locom_swim_dive")]
Trait_EU[, locom_swim_tachet := apply(.SD, 1, sum),
         .SDcols = c("locom_surface_swimmer_tachet",
                     "locom_full_water_swimmer_tachet")]
# Trait_EU[, locom_swim := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("locom_swim",
#                      "locom_swim_tachet")]
Trait_EU[, locom_swim := fcase(
  !is.na(locom_swim), locom_swim,
  is.na(locom_swim), locom_swim_tachet,
  is.na(locom_swim) & is.na(locom_swim_tachet), NA_real_
)]

## Burrowing ----
# Trait_EU[, locom_burrow := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "locom_burrow",
#     "locom_burrower_tachet",
#     "locom_interstitial_tachet"
#   )
# ]
Trait_EU[, locom_burrower_tachet := apply(.SD, 1, sum),
         .SDcols = c("locom_burrower_tachet",
                     "locom_interstitial_tachet")]
# Trait_EU[, locom_burrow := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("locom_burrow",
#                      "locom_burrower_tachet")]
Trait_EU[, locom_burrow := fcase(
  !is.na(locom_burrow), locom_burrow,
  is.na(locom_burrow), locom_burrower_tachet,
  is.na(locom_burrow) & is.na(locom_burrower_tachet), NA_real_
)]

## Crawling ----
# Trait_EU[, locom_crawl := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "locom_sprawl",
#     "locom_crawler_tachet"
#   )
# ]
Trait_EU[, locom_crawl := fcase(
  !is.na(locom_sprawl), locom_sprawl,
  is.na(locom_sprawl), locom_crawler_tachet,
  is.na(locom_sprawl) & is.na(locom_crawler_tachet), NA_real_
)]

## Sessil ----
# Trait_EU[, locom_sessil_tachet := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c(
#            "locom_sessil",
#            "locom_temp_attached_tachet",
#            "locom_perm_attached_tachet"
#          )
# ]
Trait_EU[, locom_sessil_tachet := apply(.SD, 1, sum),
  .SDcols = c(
    "locom_temp_attached_tachet",
    "locom_perm_attached_tachet"
  )
]
# Trait_EU[, locom_sessil := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c(
#            "locom_sessil",
#            "locom_sessil_tachet"
#          )
# ]
Trait_EU[, locom_sessil := fcase(
  !is.na(locom_sessil), locom_sessil,
  is.na(locom_sessil), locom_sessil_tachet,
  is.na(locom_sessil) & is.na(locom_sessil_tachet), NA_real_
)]

## Posprocessing ----
# locom other, del if 1 otherwise set to 0
# Definition: other locomotion type like flying or jumping (mainly outside the water)
Trait_EU <- Trait_EU[is.na(locom_other) | locom_other < 1, ]
Trait_EU[locom_other > 0, locom_other := 0]
Trait_EU[locom_flier_tachet > 0, locom_flier_tachet := 0]

# del
Trait_EU[,
  c(
    "locom_swim_skate",
    "locom_swim_dive",
    "locom_swim_tachet",
    "locom_surface_swimmer_tachet",
    "locom_full_water_swimmer_tachet",
    "locom_burrower_tachet",
    "locom_interstitial_tachet",
    "locom_sprawl",
    "locom_crawler_tachet",
    "locom_sessil_tachet",
    "locom_temp_attached_tachet",
    "locom_perm_attached_tachet",
    "locom_other",
    "locom_flier_tachet"
  ) := NULL
]

# _________________________________________________________________________
# Respiration ----
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: plastron & spiracle
# plastron & spiracle often work together in respiratory systems of aq. insects
# Present in insects with open tracheal systems -> breathe oxygen from the air
# -> Different tolerances to low oxygen compared to insects with tegument resp and gills

# resp_atm: atmospheric breathers -> no values
# no values for resp_tap, sur and ves
# table(Trait_EU$resp_ves)
# table(Trait_EU$resp_tap)
# table(Trait_EU$resp_sur)
# _________________________________________________________________________

## Tegument ----
# Trait_EU[, resp_teg := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "resp_tegument",
#     "resp_tegument_tachet"
#   )
# ]
Trait_EU[, resp_teg := fcase(
  !is.na(resp_tegument), resp_tegument,
  is.na(resp_tegument), resp_tegument_tachet,
  is.na(locom_sessil) & is.na(locom_sessil_tachet), NA_real_
)]

## Gills ----
# Trait_EU[, resp_gil := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "resp_gill",
#     "resp_gill_tachet"
#   )
# ]
Trait_EU[, resp_gil := fcase(
  !is.na(resp_gill), resp_gill,
  is.na(resp_gill), resp_gill_tachet,
  is.na(resp_gill) & is.na(resp_gill_tachet), NA_real_
)]

## Plastron & spiracle ----
# Trait_EU[, resp_pls_spi := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "resp_spiracle",
#     "resp_plastron",
#     "resp_plastron_tachet",
#     "resp_spiracle_tachet"
#   )
# ]
Trait_EU[, resp_pls_spi := apply(.SD, 1, sum),
         .SDcols = c("resp_spiracle",
                     "resp_plastron")]
Trait_EU[, resp_pls_spi_tachet := apply(.SD, 1, sum),
         .SDcols = c("resp_plastron_tachet",
                     "resp_spiracle_tachet")]
# Trait_EU[, resp_pls_spi := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("resp_pls_spi",
#                      "resp_pls_spi_tachet")]
Trait_EU[, resp_pls_spi := fcase(
  !is.na(resp_pls_spi), resp_pls_spi,
  is.na(resp_pls_spi), resp_pls_spi_tachet,
  is.na(resp_pls_spi) & is.na(resp_pls_spi_tachet), NA_real_
)]

## Postprocessing ----
Trait_EU[, c(
  "resp_tapping",
  "resp_vesicle",
  "resp_vesicle_tachet",
  "resp_surface",
  "resp_gill",
  "resp_spiracle",
  "resp_spiracle_tachet",
  "resp_plastron",
  "resp_plastron_tachet",
  "resp_pls_spi_tachet",
  "resp_gill_tachet",
  "resp_tegument",
  "resp_tegument_tachet"
) := NULL]

# ___________________________________________________________________________
# Dispersal ----
# -> This needs to be fixed
# Drift/dispersal
# use disp low, medium, high for comparability
# del dispersal_unknown
# check for merge if information is reliable
# two species will get changed their dispersal trait from low to medium
# Sericostoma personatum & Silo pallipes
# Trait_EU[(dispersal_high == 1| dispersal_low == 1) &
#              species %in% disp_EU$Species, .(dispersal_high, dispersal_low, species)]
# disp_EU[grepl("Amphinemura|Hydropsyche|Leuctra|Nemurella|Sericostoma|Silo", Species)]
# merge with disp_EU
# Trait_EU[disp_EU,
#            `:=`(dispersal_high = i.Strong,
#                 dispersal_low = i.Weak,
#                 dispersal_medium = i.Medium),
#            on = c(species = "Species")]
# # change NA's in dispersal medium to zeor
# Trait_EU[is.na(dispersal_medium), dispersal_medium := 0]
# ___________________________________________________________________________

# _________________________________________________________________________
# Oviposition ----
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# rep_parasitic no entries
# rep_asexual is deleted
# Harmonising using the sum in comb with na.rm = TRUE
# has the undesirable property to produce zeros (i.e. sum(c(NA, NA), na.rm = TRUE) == 0)
# this can lead to the result that when aggregating certain traits that trait values
# will be underestimated
# _________________________________________________________________________
# Trait_EU[, ovip_aqu := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "rep_egg_cem_iso",
#     "rep_egg_free_iso",
#     "rep_clutch_free",
#     "rep_clutch_fixed",
#     "rep_clutch_veg",
#     "rep_egg_cem_iso_tachet",
#     "rep_egg_free_iso_tachet",
#     "rep_clutch_free_tachet",
#     "rep_clutch_fixed_tachet",
#     "rep_clutch_veg_tachet"
#   )
# ]

## Aquatic ----
Trait_EU[, ovip_aqu := apply(.SD, 1, sum),
         .SDcols = c(
           "rep_egg_cem_iso",
           "rep_egg_free_iso",
           "rep_clutch_free",
           "rep_clutch_fixed",
           "rep_clutch_veg"
         )]
Trait_EU[, ovip_aqu_tachet := apply(.SD, 1, sum),
         .SDcols = c(
           "rep_egg_cem_iso_tachet",
           "rep_egg_free_iso_tachet",
           "rep_clutch_free_tachet",
           "rep_clutch_fixed_tachet",
           "rep_clutch_veg_tachet"
         )]
# Trait_EU[, ovip_aqu := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("ovip_aqu",
#                      "ovip_aqu_tachet")]
Trait_EU[, ovip_aqu := fcase(
  !is.na(ovip_aqu), ovip_aqu,
  is.na(ovip_aqu), ovip_aqu_tachet,
  is.na(ovip_aqu) & is.na(ovip_aqu_tachet), NA_real_
)]

## Ovoviviparity ----
# Trait_EU[, ovip_ovo := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "rep_ovovivipar",
#     "rep_ovovivipar_tachet"
#   )
# ]
Trait_EU[, ovip_ovo := fcase(
  !is.na(rep_ovovivipar), rep_ovovivipar,
  is.na(rep_ovovivipar), rep_ovovivipar_tachet,
  is.na(rep_ovovivipar) & is.na(rep_ovovivipar_tachet), NA_real_
)]

## Terrestrial ----
# Trait_EU[, ovip_ter := apply(.SD, 1, max, na.rm = TRUE),
#   .SDcols = c(
#     "rep_clutch_ter",
#     "rep_clutch_ter_tachet"
#   )
# ]
Trait_EU[, ovip_ter := fcase(
  !is.na(rep_clutch_ter), rep_clutch_ter,
  is.na(rep_clutch_ter), rep_clutch_ter_tachet,
  is.na(rep_clutch_ter) & is.na(rep_clutch_ter_tachet), NA_real_
)]

## Postprocessing ----
# rm
# rep asexual just deleted, few taxa with mostly
# low preferences -> not in accordance to our categorization
Trait_EU[, c(
  "rep_egg_cem_iso",
  "rep_egg_free_iso",
  "rep_clutch_free",
  "rep_clutch_fixed",
  "rep_parasitic",
  "rep_ovovivipar",
  "rep_clutch_veg",
  "rep_asexual",
  "rep_clutch_ter",
  "rep_egg_cem_iso_tachet",
  "rep_egg_free_iso_tachet",
  "rep_clutch_free_tachet",
  "rep_clutch_fixed_tachet",
  "rep_clutch_veg_tachet",
  "rep_ovovivipar_tachet",
  "rep_clutch_ter_tachet",
  "rep_asexual_tachet",
  "ovip_aqu_tachet"
) := NULL]

# _________________________________________________________________________
# Temperature ----
# temp_coldsteno: cold stenotherm (cos); preference for a small cold temperature range (below 10°C)
# temp_warmsteno: warm stenotherm	(was);	preference for a small warm temperature range (above 18°C)
# temp_euryt: eurytherm	(eut)	no specific preference, wide temperature range
# temp eurytherm (no specific preference)
# psychrophilic (< 15 °C)
# thermophilic (> 15 °C)
# -> temp_eury, temp_cold, temp_warm (themophilic & stenotherm)
# TODO: Needs recoding
# _________________________________________________________________________
# Trait_EU[, temp_eury := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("temp_euryt",
#                      "temp_euryt_tachet")]
# Trait_EU[, temp_cold := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("temp_coldsteno",
#                      "temp_psychrophil_tachet")]
# Trait_EU[, temp_warm := apply(.SD, 1, max, na.rm = TRUE),
#          .SDcols = c("temp_warmsteno", 
#                      "temp_thermophil_tachet")
# ]
# Trait_EU[, c("temp_euryt", 
#              "temp_euryt_tachet",
#              "temp_coldsteno",
#              "temp_psychrophil_tachet",
#              "temp_warmsteno", 
#              "temp_thermophil_tachet"
#              ) := NULL]
# _________________________________________________________________________
# Body form ----
# bf_streamlined: streamlined/fusiform
# bf_flattened: flattened (dorso-ventrally)
# bf_cylindrical: cylindrical/tubular
# bf_spherical: spherical
# Add BF data from PUP
# _________________________________________________________________________

# Philippe Polateras classification
body_form_pup <- fread(file.path(
  data_missing,
  "Body_form_EU_NOA",
  "body_form_polatera_EU_NOA.csv"
))

# change "," to "." and convert bf columns to numeric
cols <- c("streamlined", "flattened", "cylindrical", "spherical")
body_form_pup[, (cols) := lapply(.SD, function(y) {
  sub("\\,", ".", y) %>%
    as.numeric(.)
}),
.SDcols = cols
]

# subset to EU data
bf_EU <- body_form_pup[grepl("EU.*", array), ]

# merge on species-level
Trait_EU[bf_EU[!is.na(species), ],
  `:=`(
    bf_flattened = i.flattened,
    bf_spherical = i.spherical,
    bf_cylindrical = i.cylindrical,
    bf_streamlined = i.streamlined
  ),
  on = "species"
]

# merge on genus-level
Trait_EU[bf_EU,
  `:=`(
    bf_flattened = i.flattened,
    bf_spherical = i.spherical,
    bf_cylindrical = i.cylindrical,
    bf_streamlined = i.streamlined
  ),
  on = c(taxa_adjusted = "genus")
]

Trait_EU[bf_cylindrical == 1, .(order)] %>% unique()

# _________________________________________________________________________
# Size
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________

## Small ----
Trait_EU[, size_small := apply(.SD, 1, sum),
  .SDcols = c(
    "size_&le; 0.25 cm_tachet",
    "size_> 0.25-0.5 cm_tachet",
    "size_> 0.5-1 cm_tachet"
  )
]

## Medium ----
setnames(Trait_EU, "size_> 1-2 cm_tachet", "size_medium")

## Large ----
Trait_EU[, size_large := apply(.SD, 1, sum),
  .SDcols = c(
    "size_> 2-4 cm_tachet",
    "size_> 4-8 cm_tachet",
    "size_> 8 cm_tachet"
  )
]

## Postprocessing ----
# rm
Trait_EU[, c(
  "size_&le; 0.25 cm_tachet",
  "size_> 0.25-0.5 cm_tachet",
  "size_> 0.5-1 cm_tachet",
  "size_> 2-4 cm_tachet",
  "size_> 4-8 cm_tachet",
  "size_> 8 cm_tachet"
) := NULL]


# _________________________________________________________________________
# Pattern of development ----
# Holometabolous
# hemimetabolous?
# no insect
# _________________________________________________________________________
hemimetabola <- c(
  "Ephemeroptera",
  "Odonata",
  "Plecoptera",
  "Grylloblattodea",
  "Orthoptera",
  "Phasmatodea",
  "Zoraptera",
  "Embioptera",
  "Dermaptera",
  "Mantodea",
  "Blattodea",
  "Isoptera",
  "Thyssanoptera",
  "Hemiptera",
  "Phthriptera",
  "Psocoptera"
)
holometabola <- c(
  "Coleoptera",
  "Streptsiptera",
  "Raphidioptera",
  "Megaloptera",
  "Neuroptera",
  "Diptera",
  "Mecoptera",
  "Siphonoptera",
  "Lepidoptera",
  "Trichoptera",
  "Hymenoptera"
)
Trait_EU[, `:=`(
  dev_hemimetabol = ifelse(order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(order %in% holometabola, 1, 0)
)]

# _________________________________________________________________________

# Postprocessing ----
# normalize again (actually only needed for bf data)
normalize_by_rowSum(
  x = Trait_EU,
  non_trait_cols = c(
    "order",
    "family",
    "genus",
    "species",
    "ID_AQEM",
    "taxon_cp",
    "taxa"
  )
)

# new column order
newcolorder <- c(
  "species",
  "genus",
  "family",
  "order",
  "taxon_cp",
  "taxa",
  "ID_AQEM",
  grep("feed", names(Trait_EU), value = TRUE),
  grep("resp", names(Trait_EU), value = TRUE),
  grep("volt", names(Trait_EU), value = TRUE),
  grep("locom", names(Trait_EU), value = TRUE),
  grep("ovip", names(Trait_EU), value = TRUE),
  grep("size", names(Trait_EU), value = TRUE),
  grep("bf", names(Trait_EU), value = TRUE),
  grep("dev", names(Trait_EU), value = TRUE)
)
setcolorder(Trait_EU, newcolorder)

# save for further analysis
saveRDS(
  object = Trait_EU,
  file = file.path(
    data_cleaned, 
    "EU",
    "Trait_freshecol_2020_pp_harmonized.rds")
)
saveRDS(
  object = Trait_EU,
  file = file.path(
    data_aggr,
    "Data",
    "Trait_freshecol_2020_pp_harmonized.rds")
)

#_______________________________________________________________________________________________
# Add Ecoregions ----
#_______________________________________________________________________________________________
ecoregions_path <- file.path(data_raw, "ecoregions.csv")
ind <- grep("Taxon", readLines(ecoregions_path))

# read in ecoregions & preprocess
ecoregions <- fread(
  ecoregions_path,
  skip = ind,
  sep = ";",
  header = TRUE,
  na.strings = (""),
  fill = TRUE
)
ecoregions <- ecoregions[!(V1 %like% "Statistics|number.*"), ]
setnames(ecoregions,
         c("V1",
           "ID-AQEM (ID-fwe)"),
         c("taxon",
           "ID_AQEM"))
ecoregions[, c("EU", "ERX", "ERY") := NULL]

# subset where ID is present
ecoregions <- ecoregions[!is.na(ID_AQEM), ]

# set values to 1
cols <- grep("taxon|ID.*",
             names(ecoregions), 
             value = TRUE, 
             invert = TRUE)
for(j in cols){
  data.table::set(ecoregions, which(!is.na(ecoregions[[j]])), j, 1)
}

# merge
Trait_EU_ecoreg <- merge(x = Trait_EU,
                         y = ecoregions[, -c("taxon")],
                         by = "ID_AQEM",
                         all.x = TRUE)

# as .csv and .rds
write.csv(
  x = Trait_EU_ecoreg,
  file = file.path(data_cleaned, "EU", "Trait_freshecol_2020_pp_harmonized_ecoregions.csv"),
  row.names = FALSE
)
saveRDS(
  object = Trait_EU_ecoreg,
  file = file.path(
    data_cleaned, 
    "EU",
    "Trait_freshecol_2020_pp_harmonized_ecoregions.rds")
)

# Save also for convergence of trait profiles analysis
saveRDS(object = Trait_EU_ecoreg,
        file = "/home/kunzst/Dokumente/Projects/Trait_DB/Convergence-trait-profiles/Data/Trait_freshecol_2020_pp_harmonized_ecoregions.rds")
