
# _________________________________________________________________________  
#### Harmonize AUS Traits ####
# _________________________________________________________________________ 
# TODO add a more detailed description 

# read in preprocessed trait data
Trait_AUS <- readRDS(
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_preproc.rds"
  )
)

# _________________________________________________________________________ 
#### Voltinism #### 
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________ 
Trait_AUS[, volt_semi := apply(.SD, 1, max),
          .SDcols = c("Voltinism_less_than_1_VicEPA",
                      "Volt1_botwe",
                      "volt1_Maxwell",
                      "volt_semi")]
Trait_AUS[, volt_uni := apply(.SD, 1, max),
          .SDcols = c("Voltinism_1_VicEPA",
                      "Volt2_botwe",
                      "volt2_Maxwell",
                      "volt_uni", 
                      "volt_uni_Shafer")]
Trait_AUS[, volt_bi_multi := apply(.SD, 1, max),
          .SDcols = c("Voltinism_2_VicEPA",
                      "Volt3_botwe",
                      "volt3_Maxwell",
                      "volt4_Maxwell",
                      "volt_bi_multi", 
                      "volt_bi_multi_Shafer")]
Trait_AUS[, c(
  "Voltinism_less_than_1_VicEPA",
  "Volt1_botwe",
  "volt1_Maxwell",
  "Voltinism_1_VicEPA",
  "Volt2_botwe",
  "volt2_Maxwell",
  "volt_uni_Shafer",
  "Voltinism_2_VicEPA",
  "Volt3_botwe",
  "volt3_Maxwell",
  "volt4_Maxwell",
  "volt_bi_multi_Shafer",
  "Voltinism_more_than_2_VicEPA"
) := NULL]

# _________________________________________________________________________ 
#### Aquatic stages #### 
# stage_egg
# stage_larva: larva and/or nymph
# stage_pupa
# stage_adult
# _________________________________________________________________________ 
setnames(
  Trait_AUS,
  old = c(
    "Aquatic_eggs_VicEPA",
    "Aquatic_nymph_VicEPA",
    "Aquatic_larva_VicEPA",
    "Aquatic_pupa_VicEPA"
  ),
  new = c("stage_egg",
          "stage_larva",
          "stage_pupa",
          "stage_adult")
)

# _________________________________________________________________________ 
#### PH ####
# only one modality
# Trait_AUS$ph_acidic
# _________________________________________________________________________ 

# _________________________________________________________________________ 
#### Feed mode ####
# feed_shredder: shredder (chewers, miners, xylophagus, herbivore piercers)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_scraper: scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# Feeding piercers have not been used -> ask Verena?
# names(Trait_AUS[, .SD,.SDcols = names(Trait_AUS) %like% "(?i)Trop|feed"])
# _________________________________________________________________________ 
Trait_AUS[, feed_shredder := apply(.SD, 1, max), 
          .SDcols = c("feeding_shredder_Marchant", 
                      "Feeding_shredders_VicEPA", 
                      "feeding_shredder_maxwell",
                      "feed_shredder",
                      "feed_shredder_Shafer",
                      "Trop_shredder_detritivore_botwe",
                      "Shredder_proportion_of_feeding_fam_Chessman2017",
                      "Shredder_proportion_of_feeding_genus_Chessman2017"
          )]
Trait_AUS[, feed_gatherer := apply(.SD, 1, max),
          .SDcols = c("feeding_detritivore_Marchant",
                      "feeding_collector_maxwell",
                      "Trop_collector_gatherer_botwe",
                      "Gatherer_proportion_of_feeding_fam_Chessman2017",
                      "Gatherer_proportion_of_feeding_genus_Chessman2017")]
Trait_AUS[, feed_filter := apply(.SD, 1, max),
          .SDcols = c("feeding_filterer_Marchant",
                      "Feeding_filterers_VicEPA",
                      "feeding_filterer_maxwell", 
                      "Trop_collector_filterer_botwe", 
                      "Filterer_proportion_of_feeding_fam_Chessman2017",
                      "Filterer_proportion_of_feeding_genus_Chessman2017")]
Trait_AUS[, feed_scraper := apply(.SD, 1, max),
          .SDcols = c("feeding_grazer_Marchant",
                      "Feeding_scrapers_VicEPA",
                      "Feeding_deposit_grazer_VicEPA", 
                      "feeding_scraper_maxwell", 
                      "Trop_scraper_botwe", 
                      "Scraper_proportion_of_feeding_fam_Chessman2017", 
                      "Scraper_proportion_of_feeding_genus_Chessman2017")]
Trait_AUS[, feed_predator := apply(.SD, 1, max),
          .SDcols = c("feeding_predator_Marchant",
                      "Feeding_predators_VicEPA",
                      "feeding_predator_maxwell", 
                      "feed_predator_Shafer", 
                      "feed_predator", 
                      "Trop_predator_botwe", 
                      "Predator_proportion_of_feeding_fam_Chessman2017",
                      "Predator_proportion_of_feeding_genus_Chessman2017")]
Trait_AUS[, feed_parasite := apply(.SD, 1, max),
          .SDcols = c("feed_parasite", 
                      "feed_parasite_Shafer", 
                      "feeding_parasite_maxwell")]
Trait_AUS[, c(
  "feeding_shredder_Marchant", 
  "Feeding_shredders_VicEPA", 
  "feeding_shredder_maxwell",
  "feed_shredder_Shafer",
  "feeding_detritivore_Marchant",
  "feeding_collector_maxwell",
  "feeding_filterer_Marchant",
  "Feeding_filterers_VicEPA",
  "feeding_filterer_maxwell",
  "feeding_grazer_Marchant",
  "Feeding_scrapers_VicEPA",
  "Feeding_deposit_grazer_VicEPA", 
  "feeding_scraper_maxwell",
  "feeding_predator_Marchant",
  "Feeding_predators_VicEPA",
  "feeding_predator_maxwell", 
  "feed_predator_Shafer",
  "feed_parasite_Shafer", 
  "Trop_collector_gatherer_botwe",
  "Trop_collector_filterer_botwe",
  "Trop_scraper_botwe",
  "Trop_predator_botwe", 
  "Trop_shredder_detritivore_botwe", 
  "feeding_collector_scraper_maxwell",
  "feeding_collec_shredder_maxwell",
  "feeding_parasite_maxwell",
  "Feeding_piercers_VicEPA",
  "Shredder_proportion_of_feeding_genus_Chessman2017",
  "Scraper_proportion_of_feeding_genus_Chessman2017", 
  "Predator_proportion_of_feeding_genus_Chessman2017",
  "Gatherer_proportion_of_feeding_genus_Chessman2017",
  "Filterer_proportion_of_feeding_genus_Chessman2017",
  "Shredder_proportion_of_feeding_fam_Chessman2017",
  "Scraper_proportion_of_feeding_fam_Chessman2017",   
  "Predator_proportion_of_feeding_fam_Chessman2017",  
  "Gatherer_proportion_of_feeding_fam_Chessman2017",  
  "Filterer_proportion_of_feeding_fam_Chessman2017" 
) := NULL]

# _________________________________________________________________________ 
#### Locomotion ####  
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler, climber, clinger (Habi4_botwe)
# locm_burrow: burrower
# locm_sessil: sessil (attached)
# Attach_perm_VicEPA just contained zeros
# _________________________________________________________________________ 
Trait_AUS[, locom_swim := apply(.SD, 1, max), 
          .SDcols = c("Habi5_botwe",
                      "Habi6_botwe",
                      "Attach_swim_VicEPA")]
Trait_AUS[, locom_crawl := apply(.SD, 1, max), 
          .SDcols = c("Habi3_botwe",
                      "Habi2_botwe",
                      "Habi4_botwe",
                      "Attach_crawl_VicEPA")]
Trait_AUS[, locom_burrow := apply(.SD, 1, max), 
          .SDcols = c("Habi1_botwe", 
                      "Attach_burrow_VicEPA")]
setnames(Trait_AUS,
         old = c("Attach_temp_VicEPA"),
         new = c("locom_sessil"))
Trait_AUS[, c(
  "Habi5_botwe",
  "Habi6_botwe",
  "Habi3_botwe",
  "Habi2_botwe",
  "Attach_crawl_VicEPA",
  "Habi1_botwe",
  "Attach_burrow_VicEPA",
  "Habi4_botwe",
  "Attach_swim_VicEPA"
) := NULL]

# _________________________________________________________________________ 
#### Respiration ####  
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_spi: spiracle (atmospheric & plant breathers (Maxwell_1 & 2))
# resp_pls: plastron
# Maxwell trait on Plastron not used, since ambiguous (Plastron and gills)
# _________________________________________________________________________ 

Trait_AUS[, resp_teg := apply(.SD, 1, max), 
          .SDcols = c("Respiration_tegument_VicEPA", 
                      "Resp1_botwe",
                      "resp3_Maxwell",
                      "resp_teg", 
                      "resp_teg_Shafer")]
Trait_AUS[, resp_gil := apply(.SD, 1, max), 
          .SDcols = c("Respiration_gills_VicEPA", 
                      "Resp2_botwe",
                      "resp4_Maxwell",
                      "resp_gil", 
                      "resp_gil_Shafer",
                      "Gills_aquatic_stages_fam_Chessman2017",
                      "Gills_aquatic_stages_genus_Chessman2017")]
Trait_AUS[, resp_spi := apply(.SD, 1, max), 
          .SDcols = c("Respiration_spiracle_VicEPA", 
                      "resp7_Maxwell",
                      "resp1_Maxwell", 
                      "resp2_Maxwell",
                      "resp_atm", 
                      "resp_atm_Shafer",
                      "Air_respiration_aquatic_stages_fam_Chessman2017",
                      "Air_respiration_aquatic_stages_genus_Chessman2017",
                      "Functional_spiracles_aquatic_stages_genus_Chessman2017",
                      "Functional_spiracles_aquatic_stages_fam_Chessman2017" 
          )]
Trait_AUS[, resp_pls := apply(.SD, 1, max), 
          .SDcols = c("Resp3_botwe",
                      "resp_pls", 
                      "resp_pls_Shafer")]
Trait_AUS[, c("Respiration_tegument_VicEPA", 
              "Resp1_botwe",
              "resp3_Maxwell",
              "resp_teg_Shafer", 
              "Respiration_gills_VicEPA", 
              "Resp2_botwe",
              "resp4_Maxwell",
              "resp_gil_Shafer",
              "Respiration_spiracle_VicEPA",
              "resp1_Maxwell", 
              "resp2_Maxwell",
              "resp_atm",
              "resp_atm_Shafer",
              "Resp3_botwe", 
              "resp7_Maxwell",
              "resp_pls_Shafer", 
              "resp5_Maxwell", 
              "resp6_Maxwell",
              "Air_respiration_aquatic_stages_fam_Chessman2017",
              "Air_respiration_aquatic_stages_genus_Chessman2017",
              "Gills_aquatic_stages_fam_Chessman2017",
              "Gills_aquatic_stages_genus_Chessman2017",
              "Functional_spiracles_aquatic_stages_genus_Chessman2017",
              "Functional_spiracles_aquatic_stages_fam_Chessman2017") := NULL]
# _________________________________________________________________________ 
#### Size #### 
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________ 
Trait_AUS[, size_small := apply(.SD, 1, max), 
          .SDcols = c("Max_size_less_than_5_VicEPA", 
                      "Size1_botwe",
                      "size_small", 
                      "size_small_Shafer",
                      "size_small_fam_Chessman",
                      "size_small_genus_Chessman")]
Trait_AUS[, size_medium := apply(.SD, 1, max), 
          .SDcols = c("Max_size_5_to_10_VicEPA", 
                      "Max_size_10_to_20_VicEPA",
                      "Size2_botwe",
                      "size_medium", 
                      "size_medium_Shafer",
                      "size_medium_fam_Chessman",
                      "size_medium_genus_Chessman")]
Trait_AUS[, size_large := apply(.SD, 1, max), 
          .SDcols = c("Max_size_20_to_40_VicEPA", 
                      "Max_size_more_than_40_VicEPA",
                      "Size3_botwe",
                      "size_large", 
                      "size_large_Shafer",
                      "size_large_genus_Chessman",
                      "size_large_fam_Chessman")]
Trait_AUS[, c(
  "Max_size_less_than_5_VicEPA",
  "Size1_botwe",
  "size_small_Shafer",
  "Max_size_5_to_10_VicEPA",
  "Max_size_10_to_20_VicEPA",
  "Size2_botwe",
  "size_medium_Shafer",
  "Max_size_20_to_40_VicEPA",
  "Size3_botwe",
  "size_large_Shafer",
  "Max_size_more_than_40_VicEPA",
  "size_large_genus_Chessman",
  "size_large_fam_Chessman",
  "size_medium_fam_Chessman",
  "size_medium_genus_Chessman",
  "size_small_fam_Chessman",
  "size_small_genus_Chessman"
) := NULL]

# _________________________________________________________________________ 
#### Oviposition ####  
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# _________________________________________________________________________ 
Trait_AUS[,  ovip_ter := apply(.SD, 1, max),
          .SDcols = c("Rep2_botwe",
                      "repro2_Maxwell")]
Trait_AUS[, ovip_aqu := apply(.SD, 1, max),
          .SDcols = c("Rep1_botwe",
                      "repro1_Maxwell", 
                      "ovip_aqu", 
                      "ovip_aqu_Shafer")]
Trait_AUS[, ovip_ovo := apply(.SD, 1, max),
          .SDcols = c("Rep3_botwe",
                      "repro3_Maxwell",
                      "ovip_ovo", 
                      "ovip_ovo_Shafer")]
Trait_AUS[, c(
  "Reproduction_single_ind_VicEPA",
  "Reproduction_m_and_f_VicEPA",
  "Rep1_botwe",
  "Rep2_botwe",
  "Rep3_botwe",
  "repro1_Maxwell",
  "repro2_Maxwell",
  "repro3_Maxwell",
  "ovip_aqu_Shafer",
  "ovip_ovo_Shafer"
) := NULL]

# _________________________________________________________________________ 
#### Temperature ####  
# temp very cold
# temp cold + temp moderate
# temp warm
# temp eurytherm
# Ther1_botwe not used (cool/warm eurythermal) -> ambiguou
# _________________________________________________________________________ 
Trait_AUS[, temp_eurytherm := apply(.SD, 1, max),
          .SDcols = c("Ther2_botwe", "Ther3_botwe")]
Trait_AUS[, c("Ther1_botwe",
              "Ther2_botwe",
              "Ther3_botwe") := NULL]

# _________________________________________________________________________ 
#### Pattern of development ####
# hemimetabolous
# holometabolous
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
Trait_AUS[, `:=`(
  dev_hemimetabol = ifelse(Order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(Order %in% holometabola, 1, 0),
  dev_no_insect = ifelse(!(
    Order %in% hemimetabola |
      Order %in% holometabola
  ), 1, 0)
)]

# _________________________________________________________________________ 
#### Normalization ####
# _________________________________________________________________________ 

# get trait names & create pattern for subset
trait_names_pattern <-
  names(Trait_AUS[, -c("unique_id",
                       "Family",
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
  Trait_AUS[, rowSum := apply(.SD, 1, sum),
            .SDcols = names(Trait_AUS) %like% cols]
  
  # get column names for assignment
  col_name <- names(Trait_AUS)[names(Trait_AUS) %like% cols]
  
  Trait_AUS[, (col_name) := lapply(.SD, function(y) {
    round(y / rowSum, digits = 2)
  }),
  .SDcols = names(Trait_AUS) %like% cols]
}
Trait_AUS[, rowSum := NULL]

# get trait columns
trait_col <-
  grep(
    "(?i)unique_id|species|genus|family|order",
    names(Trait_AUS),
    invert = TRUE,
    value = TRUE
  )

# transform NA values to zero
for (j in trait_col){
  data.table::set(Trait_AUS, which(is.na(Trait_AUS[[j]])),j,0)
}

# Change column order
setcolorder(x = Trait_AUS,
            neworder = names(Trait_AUS)[-c(1:5)][order(names(Trait_AUS)[-c(1:5)])])

# make colnames all lower case
setnames(Trait_AUS,
         old = names(Trait_AUS),
         new = tolower(names(Trait_AUS)))

# save
saveRDS(
  object = Trait_AUS,
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_harmonized.rds"
  )
)