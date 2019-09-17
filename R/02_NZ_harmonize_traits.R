# ------------------------------------------------------------------------- 
#### Harmonize AUS Traits ####

# includes subset to relevant traits
# -------------------------------------------------------------------------

# read in
Trait_NZ <- readRDS(file.path(data_cleaned, "NZ", "Trait_NZ_taxa_pp.rds"))

# Remarks 
# Ecological traits not present in NZ DB
# aquatic stages? -> Leave also out?

# Voltinism
# volt_semi
# volt_uni
# volt_bi_multi
setnames(Trait_NZ, 
         old = c("SEMI_semivoltine", "UNIV_univoltine","PLURIV_plurivoltine"), 
         new = c("volt_semi", "volt_uni", "volt_bi_multi"))

# aquatic stages
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


# Feed mode
# feed_shredder: shredder (chewers, miners, xylophagus, herbivore piercers)
# feed_gatherer: collector gatherer (gatherers, detritivores, deposit feeders)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_scraper: scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# deposit feeders into collector gatherer (according to Resh)
setnames(Trait_NZ, 
         old = c("SCRAPER_scrapers","FILTERFEED_filter-feeders",
                 "PREDATOR_predator", "DEPOSIT_deposit-feeders"), 
         new = c("feed_scraper", "feed_filter", "feed_predator", 
                 "feed_gatherer"))
# Algal piercers and Shredders together
Trait_NZ[, feed_shredder := apply(.SD, 1, max),
         .SDcols = c("SHREDDER_shredders", "ALGALP_algal piercer")]
# del 
Trait_NZ[, c("SHREDDER_shredders", "ALGALP_algal piercer") := NULL]

# Locomotion
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler
# locm_burrow: burrower
# locm_sessil: sessil (attached)
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

# Respiration
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_spi: spiracle
# resp_pls: plastron
# NZ: tegument, gills, plastron, aerial
# aerial is spriacle
setnames(Trait_NZ, 
         old = c("TEGUMENT_tegument", "GILL_gills", 
                 "PLASTRON_plastron", "AERIAL_aerial"),
         new = c("resp_teg", "resp_gil", "resp_pls", "resp_spi"))

# Drift/dispersal
# dissem_air_active
# dissem_air_passive
# dissem_aq_active
# dissem_aq_passive
# Dissem traits availabile, but still too few in EU DB

# Size
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# size_small: <= 5 mm & 5 - 10 mm
# size_medium: 
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

# Reproduction/Oviposition + Egg/egg mass traits combined
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# rep_asexual
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
  Trait_NZ[, .SD, .SDcols = names(Trait_NZ) %like% c("size|ovip|resp|feed|locom|volt|Species|Genus|Family|Order|unique_id")]


# Normalization ----------------------------------------------------------------
trait_names_pattern <-
  names(Trait_NZ[, -c("Family",
                      "Genus",
                      "Species",
                      "Order"
  )]) %>%
  sub("\\_.*|\\..*", "", .) %>%
  unique() %>%
  paste0("^", .)

# loop for normalization (trait categories for each trait sum up to 1) 
for(cols in trait_names_pattern) {
  
  # get row sum for a specific trait
  Trait_NZ[, rowSum := apply(.SD, 1, sum),
           .SDcols = names(Trait_NZ) %like% cols]
  
  # get column names for assignment
  col_name <- names(Trait_NZ)[names(Trait_NZ) %like% cols]
  
  Trait_NZ[, (col_name) := lapply(.SD, function(y) {
    round(y / rowSum, digits = 2)
  }),
  .SDcols = names(Trait_NZ) %like% cols]
}
Trait_NZ[, rowSum := NULL]

# get trait columns
trait_col <-
  grep(
    "Species|Genus|Family|Order|unique_id",
    names(Trait_NZ),
    invert = TRUE,
    value = TRUE
  )

# transform NA values to zero
for (j in trait_col){
  data.table::set(Trait_NZ, which(is.na(Trait_NZ[[j]])),j,0)
}

# lower colnames
setnames(Trait_NZ, 
         old = names(Trait_NZ), 
         new = tolower(names(Trait_NZ)))

# save
saveRDS(object = Trait_NZ,
        file = file.path(data_cleaned, "NZ", "Trait_NZ_pp_harmonized.rds"))
