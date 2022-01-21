# _________________________________________________________________________
# Harmonization ----
# Traits are not normalized per grouping feature 
# check individually for each grouping feature (taxa_not_normalized function)
# _________________________________________________________________________
Trait_SA <- readRDS(file.path(data_cleaned,
                                 "SA",
                                 "Trait_SA_pp.rds"))

# _________________________________________________________________________
# Size ----
# size_small: size < 10 mm 
# size_medium: 10 mm < size > 20 mm 
# size_large: size > 20 
# _________________________________________________________________________
# which(rowSums(Trait_SA[, .SD,
#                        .SDcols = patterns("Small|Very small|Medium|Large|Very large")]) > 1)

## Small ----
Trait_SA[, size_small := apply(.SD, 1, sum),
         .SDcols = patterns("Small|Very small")]

## Medium  ----
setnames(Trait_SA, 
         old = "Max. body size (mm)_Medium (>10 -20)", 
         new = "size_medium")

## Large ----
Trait_SA[, size_large := apply(.SD, 1, sum),
         .SDcols = patterns("Large|Very large")]

# Variable
# Classified by Frank & Nelson
# Chironomidae, Empididae & Tanyderidae small
# Trait_SA[`Max. body size (mm)_Variable` > 0, .SD, .SDcols = patterns("Max|species|genus|family|order")]
Trait_SA[`Max. body size (mm)_Variable` > 0 &
              family %in% c("Chironomidae (Subfamily: Chironominae)",
                            "Empididae",
                            "Tanyderidae"), size_small := 1]

# Baetids, Demoreptus & Rheoptilum size medium
Trait_SA[`Max. body size (mm)_Variable` > 0 & family == "Baetidae", size_medium := 1]

# Stenopsychidae? Some sources say: 18 to 35 mm -> large?
# https://link.springer.com/chapter/10.1007/978-94-009-4814-3_5
Trait_SA[`Max. body size (mm)_Variable` > 0 & family == "Stenopsychidae", size_large := 1]

# Delete old body size columns
Trait_SA[, c(
  "Max. body size (mm)_Large (>20 - 40)",
  "Max. body size (mm)_Small (>5-10)",
  "Max. body size (mm)_Variable",
  "Max. body size (mm)_Very large (>40)",
  "Max. body size (mm)_Very small (≤5)"
) := NULL]


# _________________________________________________________________________
# Body form/shape ----
# bf_streamlined: streamlined/fusiform
# bf_flattened: flattened (dorso-ventrally)
# bf_cylindrical: cylindrical/tubular
# bf_spherical: spherical
# Ovate?
# Others?
# _________________________________________________________________________
# taxa_not_normalized(Trait_SA, "Body shape")

## Streamlined, Flattened, Spherical ----
setnames(
  Trait_SA,
  old = c("Body shape_Streamlined", 
          "Body shape_Flattened", 
          "Body shape_Spherical"),
  new = c("bf_streamlined",
          "bf_flattened",
          "bf_spherical")
)

## Cylindrical ----
# ovate classified as cylindrical
Trait_SA[, bf_cylindrical := apply(.SD, 1, sum),
         .SDcols = patterns("Cylindrical|Ovate")]

# Newly classify Hydrophilidae which has two body shapes assigned 
# -> cylindrical and spherical
# taxa_not_normalized(x = Trait_SA, pattern = "Body shape")
Trait_SA[is.na(species) & is.na(genus) & family == "Hydrophilidae",
         `:=`(bf_streamlined = 1, bf_spherical = 1)]

## Others ----
# 187 taxa which are interesting for potential reclassification 
taxa_bf_others <-
  Trait_SA[`Body shape_Others - specify` == 1 & order %in% c(
    "Ephemeroptera",
    "Hemiptera",
    "Odonata",
    "Trichoptera",
    "Coleoptera",
    "Plecoptera",
    "Diptera",
    "Megaloptera",
    "Neuroptera"
  ), family]
saveRDS(unique(taxa_bf_others),
        file.path(data_cleaned,
                  "SA",
                  "taxa_bf_others.rds"))

# Delete old body shape columns
# leave body shape others for now
Trait_SA[, c("Body shape_Cylindrical/Tubular",
             "Body shape_Ovate") := NULL] # "Body shape_Others - specify"

# _________________________________________________________________________
# Locomotion ----
# For locomotion sessil: choose taxa with temporary and permanent attachment
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler
# locm_burrow: burrower
# locm_sessil: sessil (attached)
# Locomotion "other" used by taxa we don't consider for this analysis
# _________________________________________________________________________
# no taxon with two or more locmotion traits assigned -> taxa are normalized (1 or 0)
# taxa_not_normalized(Trait_SA, "Mobility")

## Swimming & skating ----
Trait_SA[, locom_swim := apply(.SD, 1, sum),
         .SDcols = patterns("Swimmer|Scater")]

## Crawling ----
Trait_SA[, locom_crawl := apply(.SD, 1, sum),
         .SDcols = patterns("Crawler|Sprawler|Walker|Climber")]

## Burrowing ----
setnames(Trait_SA,
         "Mobility_Burrower",
         "locom_burrow")

## Sessil ----
# Use attachment traits (in total 4 traits)
Trait_SA[, locom_sessil := apply(.SD, 1, sum),
         .SDcols = patterns("attachment")]

# Delete old locomotion trait columns
Trait_SA[, c(
  "Mobility_Climber",
  "Mobility_Crawler",
  "Mobility_Other",
  "Mobility_Skater",
  "Mobility_Sprawler",
  "Mobility_Swimmer",
  "Mobility_Walker",
  "Attachment mechanism_Permanent attachment",                                   
  "Attachment mechanism_Presence of structure for attachment to substrate",          
  "Attachment mechanism_Presence of structure for attachment to substrate (specify)",
  "Attachment mechanism_Temporary attachment",
  "Attachment mechanism_Free living"
) := NULL]


# _________________________________________________________________________
# Respiration ----
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: plastron & spiracle
# plastron & spiracle often work together in respiratory systems of aq. insects
# Present in insects with open tracheal systems -> breathe oxygen from the air
# -> Different tolerances to low oxygen compared to insects with tegument resp and gills
#
# Will not use "Aerial: lungs", only exhibited by 
# two Gastropoda
# _________________________________________________________________________
# taxa_not_normalized(Trait_SA, "Respiration")

## Tegument & gills ----
setnames(
  Trait_SA,
  c("Respiration_Tegument/Cutaneous", "Respiration_Gills"),
  c("resp_teg", "resp_gil")
)

## Plastron & Spiracle ----
Trait_SA[, "resp_pls_spi" := apply(.SD, 1, sum),
         .SDcols = patterns("Plastron|Aerial/vegetation|Aerial: spiracle")]

# Delete old respiration columns
Trait_SA[, c(
  "Respiration_Aerial/vegetation: breathing tube, straps/other apparatus",
  "Respiration_Aerial: lungs",
  "Respiration_Aerial: spiracle",
  "Respiration_Plastron"
) := NULL]


# _________________________________________________________________________
# Feeding mode ----
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# xylophagus can fit to the category of shredders, but see Lancaster:
# feed_gatherer: collector gatherer (gatherers, detritivores, deposit feeder)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: scraper (grazer) & herbivore piercer
# feed_predator: predator
# feed_parasite: parasite
# Predators are defined as: Engulfers (ingest pref whole or in parts) or
# as Piercers (piere prey tissues and suck fluids)
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________
# taxa_not_normalized(Trait_SA, "FUNCTIONAL FEEDING GROUP")
# some taxa not normalized, we only need to worry about those 
# that have varied or omnivore -> turns out there are none

## Shredder ----
Trait_SA[, feed_shredder := apply(.SD, 1, sum),
         .SDcols = patterns("Shredder")]

## Filterer ----
Trait_SA[, feed_filter := apply(.SD, 1, sum),
         .SDcols = patterns("Filter")]

## Herbivore ----
Trait_SA[, feed_herbivore := apply(.SD, 1, sum),
         .SDcols = grepl("^(?i)((?=.*Grazer)|(?=.*Scraper))(?!.*Shredder)",
                         names(Trait_SA),
                         perl = TRUE)]

## Predator ----
Trait_SA[, feed_predator := apply(.SD, 1, sum),
         .SDcols = patterns("(?i)Predator")]

## Gatherer ----
Trait_SA[, feed_gatherer := apply(.SD, 1, sum),
         .SDcols = patterns("(?i)Deposit feeder")]

## Varied and Omnivore ----
# Both Chironomidae genera and Leptoceridae classified as predator and shredder
Trait_SA[`FUNCTIONAL FEEDING GROUP (FFG)_Omnivore (Feed on anything and everything available, mainly reserved for scavengers)` == 1,
         `:=`(feed_shredder = 1, feed_predator = 1)]
Trait_SA[`FUNCTIONAL FEEDING GROUP (FFG)_Varied` == 1, `:=`(feed_shredder = 1, feed_predator = 1)]

# Delete old FFG columns
Trait_SA[,
         c(
           "FUNCTIONAL FEEDING GROUP (FFG)_Deposit feeder 1",
           "FUNCTIONAL FEEDING GROUP (FFG)_Deposit feeder 1 (Feeding mode: collect and gather off deposited organic material off and within the substrata)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Deposit feeder 2",
           "FUNCTIONAL FEEDING GROUP (FFG)_Deposit feeder 3",
           "FUNCTIONAL FEEDING GROUP (FFG)_Filter feeder",
           "FUNCTIONAL FEEDING GROUP (FFG)_Filter feeder (Feeding mode: collecting small particles of food suspended in the water column)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Grazer 1",
           "FUNCTIONAL FEEDING GROUP (FFG)_Grazer 1 (Feeding mode: feeds on whole living plants, leaves and stems.  Algal mats can be included where scraping is not employed as mechanism for collection).",
           "FUNCTIONAL FEEDING GROUP (FFG)_Grazer 2",
           "FUNCTIONAL FEEDING GROUP (FFG)_Predator 1",
           "FUNCTIONAL FEEDING GROUP (FFG)_Predator 2",
           "FUNCTIONAL FEEDING GROUP (FFG)_Predator 2 (Feeding mode: feeds on animals via active predation)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Predator 2 (Feeding mode: feeds on animals via active predation) (Feeding mode: feeds on animals via active predation)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Predator 2 (Feeding mode: feeds on animals via active predation) (Predator – feeds on animals via active predation)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Scraper 1",
           "FUNCTIONAL FEEDING GROUP (FFG)_Scraper 1 (Feeding Mode: scrapes thin film of micro-organisms off substrata)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Scraper 1 (Feeding Mode: scrapes thin film of micro-organisms off substrata) (Feeding Mode:  – scrapes thin film of micro-organisms off substrata)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Scraper 1 (Feeding Mode: scrapes thin film of micro-organisms off substrata) (Feeding Mode: scrapes thin film of micro-organisms off substrata)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Scraper 1 (Feeding Mode: scrapes thin film of micro-organisms off substrata) (Scraper – scrapes thin film of micro-organisms off substrata)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Scraper 2",
           "FUNCTIONAL FEEDING GROUP (FFG)_Scraper 2 (Feeding mode: scrape thin film of both micro-organisms and detritus off substrata)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Shredder",
           "FUNCTIONAL FEEDING GROUP (FFG)_Shredder (Feeding mode:  feeds by fragmenting leaves and large pieces of plant material",
           "FUNCTIONAL FEEDING GROUP (FFG)_Shredder (Feeding mode:  feeds by fragmenting leaves and large pieces of plant material (Feeding mode:  feeds by fragmenting leaves and large pieces of plant material",
           "FUNCTIONAL FEEDING GROUP (FFG)_Shredder (Feeding mode:  feeds by fragmenting leaves and large pieces of plant material (Feeding mode:  feeds by fragmenting leaves and large pieces of plant material and Grazer 1",
           "FUNCTIONAL FEEDING GROUP (FFG)_grazer",
           "FUNCTIONAL FEEDING GROUP (FFG)_Omnivore (Feed on anything and everything available, mainly reserved for scavengers)",
           "FUNCTIONAL FEEDING GROUP (FFG)_Varied"
         ) := NULL]

# _________________________________________________________________________
# Voltinism ----
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________

# taxa_not_normalized(Trait_SA, "Voltinism")
# Two taxa have multiple voltinism traits assigned
# Since we use all voltinism traits and normalize at the end, we do not need to worry about these

## Semivoltine ----
Trait_SA[, volt_semi := apply(.SD, 1, sum),
         .SDcols = patterns("(?i)Semivoltine")]

## Univoltine ----
Trait_SA[, volt_uni := apply(.SD, 1, sum),
         .SDcols = patterns("(?i)Univoltine")]

## Bi/multivoltine ----
Trait_SA[, volt_bi_multi := apply(.SD, 1, sum),
         .SDcols = patterns("(?i)Bivoltine|Multivoltine")]

# Unknown to NA
Trait_SA[, "Potential no. of generation per year (Voltinism)_Unknown" := NULL]

# Partial-voltinism?
# Psephenidae seems to be mostly univoltine
Trait_SA[`Potential no. of generation per year (Voltinism)_Partial-voltinism` == 1, 
         volt_uni :=1]

# Delete old voltinism columns
Trait_SA[, c(
  "Potential no. of generation per year (Voltinism)_1 (univoltine)",
  "Potential no. of generation per year (Voltinism)_2 (Bivoltine)",
  "Potential no. of generation per year (Voltinism)_3 (Bivoltine)",
  "Potential no. of generation per year (Voltinism)_4 (Bivoltine)",
  "Potential no. of generation per year (Voltinism)_6 (Bivoltine)",
  "Potential no. of generation per year (Voltinism)_> 2 (Multivoltine)",
  "Potential no. of generation per year (Voltinism)_Multivoltine - >2",
  "Potential no. of generation per year (Voltinism)_Partial-voltinism",
  "Potential no. of generation per year (Voltinism)_Semivoltine - > one year",
  "Potential no. of generation per year (Voltinism)_Univoltine - 1",
  "Potential no. of generation per year (Voltinism)_longer than one year (Semivoltine)"
) := NULL]

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

# taxa_not_normalized(Trait_SA, "Oviposition")
# One taxon we do not need to worry about

## aquatic oviposition ----
# Debris also aquatic oviposition
# "Wide preference" -> Simuliidae -> aquatic oviposition
Trait_SA[, ovip_aqu := apply(.SD, 1, sum),
         .SDcols = patterns(
           "Bottom sediments|Vegetation|floating|Submerged|water|Bank soil|Debris|Wide preferance"
         )]

# Delete old ovipositon columns
Trait_SA[, c(
  "Oviposition behaviour_Bank soil",
  "Oviposition behaviour_Bottom sediments",
  "Oviposition behaviour_Debris",
  "Oviposition behaviour_Free-floating",
  "Oviposition behaviour_Submerged stones",
  "Oviposition behaviour_The eggs of Chaoborus are laid on the water surface and are enclosed in a gelatinous mass and in other genera they are deposited in the debris at the edge of pools ( Hutson, 1980e)",
  "Oviposition behaviour_Vegetation (macrophyte)",
  "Oviposition behaviour_Wide preferance"
) := NULL]


# Harmonisation postprocessing ----

## Filter only for aquatic insects ----
Trait_SA <- Trait_SA[order %in% c(
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

## Handle Chironomidae ----
# Move Subfamily to new column
Trait_SA[grepl("Chironomidae", family), subfamily := sub("(Chironomidae)(.+)", "\\2", family)]
Trait_SA[grepl("Chironomidae", family), family := sub("(Chironomidae)(.+)", "\\1", family)]

## Normalize ----
normalize_by_rowSum(
  Trait_SA,
  non_trait_cols = c(
    "species",
    "genus",
    "subfamily",
    "family",
    "order",
    "taxon",
    "unique_ID",
    "Body shape_Others - specify"
  )
)

## Postprocessing ----
# column order
new_order <-
  c(
    "taxon",
    "order",
    "family",
    "genus",
    "species",
    "unique_ID",
    grep("bf",
         names(Trait_SA),
         value = TRUE),
    "Body shape_Others - specify",
    grep("size",
         names(Trait_SA),
         value = TRUE),
    grep("locom",
         names(Trait_SA),
         value = TRUE),
    grep("resp",
         names(Trait_SA),
         value = TRUE),
    grep("feed",
         names(Trait_SA),
         value = TRUE),
    grep("volt",
         names(Trait_SA),
         value = TRUE),
    grep("ovip",
         names(Trait_SA),
         value = TRUE)
  )
setcolorder(Trait_SA, new_order)

# save
saveRDS(Trait_SA,
        file.path(data_cleaned,
                  "SA",
                  "Trait_SA_pp_harmonised.rds"))
