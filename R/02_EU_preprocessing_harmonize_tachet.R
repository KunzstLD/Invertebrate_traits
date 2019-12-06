# ==============================================================================
#### Preprocess Tachet data ####
# ==============================================================================
tachet <-
  fread(file.path(data_in, "EU", "Tachet_mod_S.csv"))

# Prepare key columns: 
# Species Col -> first name from Genus when no sp. or Gen. in Species
# prepare genus col to merge with sp.// 
tachet[!grepl("^sp\\.$|^Gen\\.$|\\/", Species), species := paste(Genus, Species)]

# genus column (used for merge later)
setnames(tachet,
         old = c("Genus", "Family", "Group"), 
         new = c("genus", "family", "group"),
         skip_absent = TRUE
)
# tachet[grepl("^sp\\.$|^Gen\\.$|\\/", Species), Genus_merge := Genus]

# everything with other in new column unknown taxa
tachet[grepl("other", species), `:=`(unknown_taxa = species,
                                     species = NA,
                                     genus = NA)]

# everything with () in Species_merge deleted
tachet[grepl("\\(.*\\)|\\[", species), species := NA]
tachet[grepl("\\]", species), `:=`(species = NA, genus = NA)] 

# one word in Species_merge column
tachet[!grepl("(?i)[a-z]{1,}[[:space:]][a-z]{1,}", species)
       & !is.na(species), species := NA]

# del "Species" column (capital S)
tachet[, Species := NULL]

# ---------------------------------------------------
#### Change trait names ####
# ---------------------------------------------------

# voltinism
# grep("volt", names(dat_EU), value = TRUE)
# grep("volt", names(tachet), value = TRUE)
setnames(tachet, old = c("volt1_t", "volt2_t", "volt3_t"), 
         new = c("volt_semi", "volt_uni", "volt_bi_multi"))

# life stage
# grep("stage", names(dat_EU), value = TRUE)
# grep("stage", names(tachet), value = TRUE)
setnames(tachet, old = c("stage_egg_t", "stage_larva_t", "stage_nymph_t", "stage_adult_t"), 
         new = c("stage_egg","stage_larva","stage_nymph","stage_adult"))

# Reproduction
# grep("rep", names(dat_EU), value = TRUE)
# grep("rep", names(tachet), value = TRUE)
setnames(tachet,
         old = c(
           "rep_ovo_t", "rep_eggs_freeiso_t", "rep_egg_isocem_t",
           "rep_clutch_fix_t", "rep_clutch_free_t", "rep_clutch_veg_t",
           "rep_clutch_terr_t", "rep_asexual_t"
         ),
         new = c(
           "rep_ovovipar", "rep_egg_free_iso", "rep_egg_cem_iso",
           "rep_clutch_fixed", "rep_clutch_free",
           "rep_clutch_veg", "rep_clutch_terr", "rep_asexual"
         )
)

# Dispersal
# grep("diss", names(dat_EU), value = TRUE)
# grep("disp", names(tachet), value = TRUE)
# setnames(tachet, old = grep("disp", names(tachet), value = TRUE), 
#                  new = grep("diss", names(dat_EU), value = TRUE))

# Respiration
# grep("resp", names(dat_EU), value = TRUE)
# grep("resp", names(tachet), value = TRUE)
setnames(tachet,
         old = c("resp_tegument_t", "resp_gill_t", 
                 "resp_plastron_t", "resp_spiracle_t", "resp_vesicle_t"),
         new = c("resp_teg", "resp_gil", "resp_pls", 
                 "resp_spi", "resp_ves")
)
# Note: resp_pls - respiration plastron 
# resp_spi - respiration spiracle
# resp_ves - respiration hydrostatic vesicle

# Locomotion
# grep("loc", names(dat_EU), value = TRUE)
# grep("loc", names(tachet), value = TRUE)
setnames(tachet, 
         old = c("loc_swimmer_surf_t", "loc_swimmer_full_t",     
                 "loc_burrower_t" ,"loc_att_temp_t",     "loc_att_perm_t",
                 "loc_crawler_t"),
         new = c("locom_swim_dive", "locom_swim_full", "locom_burrow", 
                 "locom_sessil_temp", "locom_sessil", 
                 "locom_crawl"))

# Note: 
# - loco_swim_full needs to be added to loco_swimming_diving
# - loco_sessil_temp needs to be added to loco_sessil

# Feeding mode
# grep("feed", names(dat_EU), value = TRUE)
# grep("feed", names(tachet), value = TRUE)
setnames(tachet, 
         old = c("feed_scraper_t", "feed_shredder_t", "feed_deposit_t", 
                 "feed_filter_t", "feed_abs_t", "feed_predator_t",
                 "feed_parasite_t"),
         new = c("feed_scraper", "feed_shredder", "feed_gatherer", 
                 "feed_active_filter", "feed_active_filter_abs", "feed_predator", 
                 "feed_parasite"))
# Note:
# - feed_active_filter_abs to feed_active_filter

# Temp pref
# grep("temp", names(dat_EU), value = TRUE)
# grep("temp", names(tachet), value = TRUE)
setnames(tachet, 
         old = c("temp_cold_t", "temp_warm_t", "temp_eury_t"), 
         new = c("temp_cold", "temp_warm", "temp_eurytherm"))

# throw out not relevant columns
tachet <- tachet[, -c("Subfamily", "Taxa", "unknown_taxa"), with = TRUE]

# format family information
# two species acutally on order level -> removed
tachet <- tachet[!grepl("\\[", family), ]
tachet[, family := simpleCap(family)]

# add information on order (use freshecol data from 01_EU_preprocessing_freshecol.R script)
dat_EU <- readRDS(file = file.path(data_cleaned, "EU", "Trait_Freshecol_pp.rds"))
tachet[dat_EU[!duplicated(family), .(family, order)], 
       `:=`(order = i.order), 
       on = "family"]

# add missing order manually
tachet[grepl("Clavidae", family), order := "Hydrozoa"]
tachet[grepl("Barentsiidae", family), order := "Urnatellida"]
tachet[grepl("Dugesiidae", family), order := "Tricladida"]
tachet[grepl("Tetrastemmatidae", family), order := "Monostilifera"]
tachet[grepl("Mermithidae", family), order := "Mermithida"]
tachet[grepl("Tubificidae", family), order := "Tubificida"]
tachet[grepl("Dorydrilidae", family), order := "Tubificida"]
tachet[grepl("Sparganophilidae", family), order := "Crassiclitellata"]
tachet[grepl("Corbiculidae", family), order := "Venerida"]
tachet[grepl("Crangonycitidae", family), order := "Amphipoda"]
tachet[grepl("Grapsidae", family), order := "Decapoda"]
tachet[grepl("Potamidae", family), order := "Decapoda"]
tachet[grepl("Neurorthidae", family), order := "Neuroptera"]
tachet[grepl("Pyralidae", family), order := "Lepidoptera"]
tachet[grepl("Agriotypidae", family), order := "Hymenoptera"]
tachet[grepl("Hydroscaphidae", family), order := "Coleoptera"]
tachet[grepl("Chrysomelidae", family), order := "Coleoptera"]
tachet[grepl("Dipseudopsidae", family), order := "Trichoptera"]
tachet[grepl("Tipulidae", family), order := "Diptera"]
tachet[grepl("Stratiomyiidae", family), order := "Diptera"]
tachet[grepl("Dolichopodidae", family), order := "Diptera"]

# few family entries in genus col
tachet[grepl(".*dae", genus), genus := NA]

# ---------------------------------------------------
#### Harmonize Tachet data #### 
# ---------------------------------------------------

# Modify ph preference
# rule for combining ph fuzzy codes:
# rounded rowMean
tachet[, `:=`(ph_acidic = apply(.SD, 1, max)),
       .SDcols = c("Ph_4_t", "Ph_445_t", "Ph_455_t", "Ph_555_t", "Ph_556_t", "Ph_6_t")
       ] %>% 
  .[, ph_acidic := as.integer(ph_acidic)]

# del other ph columns
tachet[, c("Ph_4_t", "Ph_445_t", "Ph_455_t", "Ph_555_t", "Ph_556_t", "Ph_6_t") := NULL]

# _________________________________________________________________________
# Voltinism
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________

# _________________________________________________________________________
# Size
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________
setnames(tachet, "Size_4", "size_medium")
tachet[, size_small := apply(.SD, 1, max),
       .SDcols = c("Size_1", "Size_2", "Size_3")]
tachet[, size_large := apply(.SD, 1, max),
       .SDcols = c("Size_5", "Size_6", "Size_7")]
# del other size columns
tachet[, c(
  "Size_1", "Size_2", "Size_3", "Size_5",
  "Size_6", "Size_7"
) := NULL]

# _________________________________________________________________________
# Locomotion
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler
# locm_burrow: burrower
# locm_sessil: sessil (attached)
# _________________________________________________________________________
tachet[, locom_swim := apply(.SD, 1, max), 
       .SDcols = c("locom_swim_full", "locom_swim_dive")]
tachet[, locom_sessil := apply(.SD, 1, max), 
       .SDcols = c("locom_sessil", "locom_sessil_temp")]
tachet[, c("locom_swim_full", "locom_swim_dive",
           "locom_sessil_temp", 
           "loc_flier_t", "loc_interstitial_t") := NULL]

# _________________________________________________________________________
# Respiration
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: plastron & spiracle
  # plastron & spiracle often work together in respiratory systems of aq. insects
  # Present in insects with open tracheal systems -> breathe oxygen from the air
  # -> Different tolerances to low oxygen compared to insects with tegument resp and gills
# TODO: currently respiration vesicle is deleted -> only two entries
# _________________________________________________________________________
tachet[, resp_pls_spi := apply(.SD, 1, max),
        .SDcols = c("resp_pls", "resp_spi")]
tachet[, c("resp_ves",
           "resp_pls",
           "resp_spi") := NULL]

# _________________________________________________________________________
# Feeding mode
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# _________________________________________________________________________
tachet[, feed_filter := apply(.SD, 1, max),
       .SDcols = c("feed_active_filter", "feed_active_filter_abs")]
tachet[, feed_herbivore := apply(.SD, 1, max),
       .SDcols = c("feed_scraper", "feed_piercer_t")]
# del columns
tachet[, c("feed_active_filter",
           "feed_active_filter_abs",
           "feed_piercer_t",
           "feed_scraper") := NULL]
# _________________________________________________________________________
# Oviposition
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# _________________________________________________________________________
setnames(tachet,
         c("rep_clutch_terr", "rep_ovovipar"),
         c("ovip_ter", "ovip_ovo"))
tachet[, ovip_aqu := apply(.SD, 1, max),
       .SDcols = c(
         "rep_egg_free_iso",
         "rep_egg_cem_iso",
         "rep_clutch_fixed",
         "rep_clutch_free",
         "rep_clutch_veg"
       )]
# del
tachet[, c("rep_egg_free_iso",
           "rep_egg_cem_iso",
           "rep_clutch_fixed",
           "rep_clutch_free",
           "rep_clutch_veg", 
           "rep_asexual") := NULL]
# _________________________________________________________________________
#### Pattern of development ####
# Holometabolous or hemimetabolous
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

tachet[, `:=`(
  dev_hemimetabol = ifelse(order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(order %in% holometabola, 1, 0)
)]


# ---------------------------------------------------
#### Normalization Tachet ####
# ---------------------------------------------------
tachet <- normalize_by_rowSum(
  x = tachet,
  non_trait_cols = c("group",
                     "family",
                     "genus",
                     "species",
                     "order")
)

# del group column
tachet[, "group" := NULL]

# ---------------------------------------------------
#### Handle duplicate Genus entries in Tachet database ####
# duplicate genus entries are condensed
# ---------------------------------------------------
cols <- grep("unique_id|species|genus|family|order",
                          names(tachet),
                          value = TRUE,
                          invert = TRUE)

tachet[is.na(species) & !is.na(genus),
       (cols) := lapply(.SD, function(y)
         as.numeric(condense_dupl_numeric_agg(y))),
       .SDcols = cols,
       by = .(genus)]

# save
saveRDS(
  object = tachet,
  file = file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds")
)
