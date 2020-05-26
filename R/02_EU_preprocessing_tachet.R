# ==============================================================================
#### Preprocess Tachet data ####
# ==============================================================================
tachet <-
  fread(file.path(data_in, "EU", "Tachet_mod_S.csv"))

# Species Col -> first name from Genus when no "sp." or "Gen." in Species
tachet[!grepl("^sp\\.$|^Gen\\.$|\\/", Species) &
         Genus != Species, species := paste(Genus, Species)]

# del "Species" column (capital S)
tachet[, Species := NULL]

# genus column (used for merge later)
setnames(tachet,
         old = c("Genus", "Family", "Group"),
         new = c("genus", "family", "group"),
         skip_absent = TRUE
)

# "ini" are actually tribes -> remain in genus col for now
tachet[grepl("other", species) & grepl(".*ini", species),
       `:=`(
         species = NA,
         genus = sub("other ", "", species),
         unknown_taxa = species
       )]

# everything else with other in new column unknown taxa
tachet[grepl("other", species), `:=`(unknown_taxa = species,
                                     species = NA,
                                     genus = NA)]

# everything with () in species column deleted
tachet[grepl("\\(.*\\)|\\[", species), species := NA]
tachet[grepl("\\]", species), `:=`(species = NA, genus = NA)]

# one word entries in species column
tachet[!grepl("(?i)[a-z]{1,}[[:space:]][a-z]{1,}", species)
       & !is.na(species), species := NA]

# family col to lower case
tachet[, family := simpleCap(family)]

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
setnames(
  tachet,
  old = c(
    "loc_swimmer_surf_t",
    "loc_swimmer_full_t",
    "loc_burrower_t" ,
    "loc_att_temp_t",
    "loc_att_perm_t",
    "loc_crawler_t"
  ),
  new = c(
    "locom_swim_dive",
    "locom_swim_full",
    "locom_burrow",
    "locom_sessil_temp",
    "locom_sessil",
    "locom_crawl"
  )
)

# Note: 
# - loco_swim_full needs to be added to loco_swimming_diving
# - loco_sessil_temp needs to be added to loco_sessil

# Feeding mode
# grep("feed", names(dat_EU), value = TRUE)
# grep("feed", names(tachet), value = TRUE)
setnames(
  tachet,
  old = c(
    "feed_scraper_t",
    "feed_shredder_t",
    "feed_deposit_t",
    "feed_filter_t",
    "feed_abs_t",
    "feed_predator_t",
    "feed_parasite_t"
  ),
  new = c(
    "feed_scraper",
    "feed_shredder",
    "feed_gatherer",
    "feed_active_filter",
    "feed_active_filter_abs",
    "feed_predator",
    "feed_parasite"
  )
)
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

# ____________________________________________________
#### Handle duplicate entries in Tachet database ####
# duplicate genus & family entries are condensed
# just use mean()
# ____________________________________________________

# convert int cols to numeric
cols <- names(Filter(is.integer, tachet))
tachet[, (cols) := lapply(.SD, as.numeric) , .SDcols = cols]

# trait cols
trait_cols <- grep("unique_id|species|genus|family|order|group",
                   names(tachet),
                   value = TRUE,
                   invert = TRUE)

# species-level
# tachet[!is.na(species) & duplicated(species),]

# genus-level
# tachet[is.na(species) & !is.na(genus),] %>% 
#   .[duplicated(genus), ]
tachet[is.na(species) & !is.na(genus),
       (trait_cols) := lapply(.SD, mean),
       .SDcols = trait_cols,
       by = "genus"]

# family-level
# tachet[is.na(species) & is.na(genus) & !is.na(family),] %>%
#   .[duplicated(family) | duplicated(family, fromLast = TRUE), ]
tachet[is.na(species) &
         is.na(genus) & !is.na(family),
       (trait_cols) := lapply(.SD, mean),
       .SDcols = trait_cols,
       by = "family"]