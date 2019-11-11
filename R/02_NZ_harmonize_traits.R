# _________________________________________________________________________
#### Harmonize AUS Traits ####

# Remarks:
# Ecological traits not present in NZ DB
# aquatic stages? -> Leave also out?

# TODO: Create function for Normalization
# _________________________________________________________________________

# read in
Trait_NZ <- readRDS(file.path(data_cleaned, "NZ", "Trait_NZ_taxa_pp.rds"))

# _________________________________________________________________________
#### Voltinism ####
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________
setnames(Trait_NZ, 
         old = c("SEMI_semivoltine", "UNIV_univoltine","PLURIV_plurivoltine"), 
         new = c("volt_semi", "volt_uni", "volt_bi_multi"))

# _________________________________________________________________________
#### Aquatic stages ####
# stage_egg
# stage_larva: larva and/or nymph
# stage_pupa
# stage_adult
# currently in NZ: 
# - Adult, Larva
# - Adult or Larva 
# - Larva, Pupa
# View(Trait_NZ[, .(
#   Species,
#   Genus,
#   `ADUANDLAR_Adult, larva`,
#   `ADUORLAR_Adult or larva`,
#   `LARANDPUP_Larva, Pupa`
# )])
# _________________________________________________________________________

# _________________________________________________________________________
#### Feeding mode ####
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# feed_gatherer: collector gatherer (gatherers, detritivores, deposit feeders)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers (living aquatic plants) & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# deposit feeders into collector gatherer (according to Resh)
# parasite category does not exist in Trait NZ

# Predators are defined as: Engulfers (ingest pref whole or in parts) or
  # as Piercers (piere prey tissues and suck fluids) 
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________
setnames(Trait_NZ,
         old = c("FILTERFEED_filter-feeders", "PREDATOR_predator",
                 "DEPOSIT_deposit-feeders", "SHREDDER_shredders"),
         new = c("feed_filter", "feed_predator",
                 "feed_gatherer", "feed_shredder"))
# Algal piercers and scrapers together
Trait_NZ[, feed_herbivore := apply(.SD, 1, max),
        .SDcols = c("SCRAPER_scrapers", "ALGALP_algal piercer")]
# del 
Trait_NZ[, c("SCRAPER_scrapers", "ALGALP_algal piercer") := NULL]

# _________________________________________________________________________
#### Locomotion ####
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler
# locm_burrow: burrower
# locm_sessil: sessil (attached)
# _________________________________________________________________________
setnames(
  Trait_NZ,
  old = c(
    "SWIMMER_swimmers (water column)",
    "CRAWLER_crawlers (epibenthic)",
    "BURROWER_burrowers (infauna)",
    "ATTACHED_attached"
  ),
  new = c("locom_swim", "locom_crawl", "locom_burrow", "locom_sessil")
)

# _________________________________________________________________________
#### Respiration ####
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: spiracle& plastron 
# resp_pls: plastron
# NZ: tegument, gills, plastron, aerial
# aerial is spriacle
# resp_pls_spi: spiracle & plastron
  # plastron & spiracle often work together in respiratory systems of aq. insects
  # Present in insects with open tracheal systems -> breathe oxygen from the air
  # -> Different tolerances to low oxygen compared to insects with tegument resp and gills
# _________________________________________________________________________
setnames(Trait_NZ,
         old = c("TEGUMENT_tegument", "GILL_gills"),
         new = c("resp_teg", "resp_gil"))
Trait_NZ[, resp_pls_spi := apply(.SD, 1, max),
        .SDcols = c("PLASTRON_plastron", "AERIAL_aerial")]

# _________________________________________________________________________
#### Drift/dispersal ####
# dissem_air_active
# dissem_air_passive
# dissem_aq_active
# dissem_aq_passive
# Dissem traits availabile, but still too few in EU DB
# _________________________________________________________________________

# _________________________________________________________________________
#### Size ####
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# size_small: <= 5 mm & 5 - 10 mm
# _________________________________________________________________________
setnames(Trait_NZ,
         old = c("SIZE3_>10-20 mm"), 
         new = c("size_medium"))
Trait_NZ[, size_small := apply(.SD, 1, max),
         .SDcols = c("SIZE1_<=5 mm", "SIZE2_>5-10 mm")]
Trait_NZ[, size_large := apply(.SD, 1, max),
         .SDcols = c("SIZE4_>20-40 mm", "SIZE5_>40 mm")]
# del
Trait_NZ[, c("SIZE1_<=5 mm",
             "SIZE2_>5-10 mm",
             "SIZE4_>20-40 mm",
             "SIZE5_>40 mm") := NULL]
# _________________________________________________________________________
#### Reproduction/Oviposition + Egg/egg mass traits combined ####
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# _________________________________________________________________________
setnames(Trait_NZ, 
         old = c("TERRESTRIAL_terrestrial", 
                 "EGGPROTECTED_female bears eggs in/on body"), 
         new = c("ovip_ter", "ovip_ovo"))
Trait_NZ[, ovip_aqu := apply(.SD, 1, max),
         .SDcols = c(
           "SURFACE_water surface",
           "SUBMERGED_submerged",
           "EGGENDO_eggs endophytic",
           "EGGFREE_free",
           "EGGCEMENT_cemented"
         )]
# del 
Trait_NZ[, c(
  "SURFACE_water surface",
  "SUBMERGED_submerged",
  "EGGENDO_eggs endophytic",
  "EGGFREE_free",
  "EGGCEMENT_cemented"
) := NULL]

# subset to relevant traits
# missing currently: aquatic stages, dispersal/dissemination, T, pH
Trait_NZ <-
  Trait_NZ[, .SD, 
           .SDcols = names(Trait_NZ) %like% c("size|ovip|resp|feed|locom|volt|Species|Genus|Family|Order|unique_id")]

# _________________________________________________________________________
#### Pattern of development ####
# Holometabolous 
# Hemimetabolous
# No insect
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
Trait_NZ[, `:=`(
  dev_hemimetabol = ifelse(Order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(Order %in% holometabola, 1, 0),
  dev_no_insect = ifelse(!(
    Order %in% hemimetabola |
      Order %in% holometabola
  ), 1, 0)
)]

# lower colnames
setnames(Trait_NZ, 
         old = names(Trait_NZ), 
         new = tolower(names(Trait_NZ)))
# save
saveRDS(object = Trait_NZ,
        file = file.path(data_cleaned, "NZ", "Trait_NZ_pp_harmonized.rds"))
