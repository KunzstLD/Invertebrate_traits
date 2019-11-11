# _________________________________________________________________________ 
#### Harmonize NOA Traits from Laura T ####
# _________________________________________________________________________

# read in
Trait_Noa_new <- readRDS(file = file.path(data_cleaned, "North_America", "Traits_US_pp_LauraT.rds"))

# _________________________________________________________________________
#### Feed mode ####
# feed_shredder: shredder (chewers, miners, xylophagus)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite

# Predators are defined as: Engulfers (ingest pref whole or in parts) or
  # as Piercers (piere prey tissues and suck fluids) 
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Feed_prim_CF",
    "Feed_prim_CG",
    "Feed_prim_PA",
    "Feed_prim_PR",
    "Feed_prim_SH",
    "Feed_prim_HB"
  ),
  new = c(
    "feed_filter",
    "feed_gatherer",
    "feed_parasite",
    "feed_predator",
    "feed_shredder",
    "feed_herbivore"
  )
)

# _________________________________________________________________________
#### Locomotion ####
# locom_swim:  swimmer, scater (active & passive)
# locom_crawl: crawlers, walkers & sprawler, climber, clinger
# locom_burrow: burrower
# locom_sessil: sessil (attached)
# What to do with clingers? -> According to LeRoy and Chuck they should be put to crawlers
# _________________________________________________________________________
setnames(Trait_Noa_new, 
         old = c("Habit_prim_Attached/fixed", 
                 "Habit_prim_Burrower"),
         new = c("locom_sessil",
                 "locom_burrow"))
# Swimmer
Trait_Noa_new[, locom_swim := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Swimmer",
                      "Habit_prim_Planktonic",
                      "Habit_prim_Skater")]
# Crawler 
Trait_Noa_new[, locom_crawl := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Sprawler",
                      "Habit_prim_Climber",
                      "Habit_prim_Clinger")]

# _________________________________________________________________________
#### Size ####
# specifically: Maximum body size
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Max_body_size_Large",
    "Max_body_size_Small",
    "Max_body_size_Medium"
  ),
  new = c("size_large",
          "size_small",
          "size_medium")
)

# _________________________________________________________________________
#### Respiration ####
# resp_teg: tegument
# resp_gil: gills
# resp_pls_spi: spiracle & plastron
  # plastron & spiracle often work together in respiratory systems of aq. insects
  # Present in insects with open tracheal systems -> breathe oxygen from the air
  # -> Different tolerances to low oxygen compared to insects with tegument resp and gills
# _________________________________________________________________________
setnames(Trait_Noa_new, 
         old = c("Resp_Gills", 
                 "Resp_Plastron_spiracle",
                 "Resp_Tegument"),
         new = c("resp_gil", 
                 "resp_pls_spi",
                 "resp_teg"))

# _________________________________________________________________________
#### Voltinism ####
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Volt_Univoltine",
    "Volt_Semivoltine",
    "Volt_Bi_multivoltine"
  ),
  new = c("volt_uni", 
          "volt_semi",
          "volt_bi_multi")
)

# _________________________________________________________________________
#### Pattern fo development ####
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
Trait_Noa_new[, `:=`(
  dev_hemimetabol = ifelse(order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(order %in% holometabola, 1, 0)
)]


# select important traits & columns
pattern <- "feed|locom|resp|volt|size|dev|unique_id|species|genus|family|order"
Trait_Noa_new <- Trait_Noa_new[, .SD, .SDcols = names(Trait_Noa_new) %like% pattern]
setcolorder(
  Trait_Noa_new,
  neworder = c(
    "unique_id",
    "species",
    "genus",
    "family",
    "order",
    "locom_sessil",
    "locom_burrow",
    "locom_swim",
    "locom_crawl",
    "feed_filter",
    "feed_gatherer",
    "feed_parasite",
    "feed_predator",
    "feed_shredder"
  )
)

# save
saveRDS(object = Trait_Noa_new, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_LauraT_pp_harmonized.rds")
)
