
# _________________________________________________________________________  
#### Harmonize AUS Traits ####
# TODO add a more detailed description 
# _________________________________________________________________________ 

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
Trait_AUS[, volt_semi := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Voltinism_less_than_1_VicEPA",
                      "Volt1_botwe",
                      "volt1_Maxwell",
                      "Less_than_one_generation_per_year_Schaefer",
                      "Less_than_one_generation_per_year_bugs_gbr")]
Trait_AUS[, volt_uni := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Voltinism_1_VicEPA",
                      "Volt2_botwe",
                      "One_generation_per_year_bugs_gbr", 
                      "One_generation_per_year_Schaefer")]
# Trait_AUS[, volt_bi_multi := apply(.SD, 1, max),
#           .SDcols = c("Voltinism_2_VicEPA",
#                       "Volt3_botwe",
#                       "volt2_Maxwell",
#                       "volt3_Maxwell",
#                       "volt4_Maxwell",
#                       "More_than_two_generations_per_year_bugs_gbr", 
#                       "More_than_two_generations_per_year_Schaefer")]
Trait_AUS[, volt_bi_multi_maxwell := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("volt2_Maxwell",
                      "volt3_Maxwell",
                      "volt4_Maxwell")]
Trait_AUS[, volt_bi_multi_schaefer := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Two_generations_per_year_Schaefer",
                      "More_than_two_generations_per_year_Schaefer")]
Trait_AUS[, volt_bi_multi_gbr := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Two_generations_per_year_bugs_gbr",
                      "More_than_two_generations_per_year_bugs_gbr")]
Trait_AUS[, volt_bi_multi := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Voltinism_2_VicEPA",
                      "Volt3_botwe",
                      "volt_bi_multi_maxwell",
                      "volt_bi_multi_schaefer",
                      "volt_bi_multi_gbr")]

Trait_AUS[, c(
  "Voltinism_less_than_1_VicEPA",
  "Volt1_botwe",
  "volt1_Maxwell",
  "Voltinism_1_VicEPA",
  "Less_than_one_generation_per_year_Schaefer",
  "Less_than_one_generation_per_year_bugs_gbr",
  "Volt2_botwe",
  "volt2_Maxwell",
  "One_generation_per_year_bugs_gbr", 
  "One_generation_per_year_Schaefer",
  "Voltinism_2_VicEPA",
  "Volt3_botwe",
  "volt3_Maxwell",
  "volt4_Maxwell",
  "Voltinism_more_than_2_VicEPA",
  "More_than_two_generations_per_year_bugs_gbr", 
  "More_than_two_generations_per_year_Schaefer",
  "volt_bi_multi_schaefer",
  "volt_bi_multi_gbr",
  "volt_bi_multi_maxwell"
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
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# TODO: Feeding piercers have not been used -> ask Verena?
# names(Trait_AUS[, .SD,.SDcols = names(Trait_AUS) %like% "(?i)Trop|feed"])

# Predators are defined as: Engulfers (ingest pref whole or in parts) or
# as Piercers (piere prey tissues and suck fluids) 
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________ 
Trait_AUS[, feed_shredder := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("feeding_shredder_Marchant",
                      "Feeding_shredders_VicEPA",
                      "feeding_shredder_maxwell",
                      "feed_shredder",
                      "feed_shredder_Schaefer",
                      "Trop_shredder_detritivore_botwe",
                      "Shredder_proportion_of_feeding_fam_Chessman2017",
                      "Shredder_proportion_of_feeding_gen_Chessman2017"
          )]
Trait_AUS[, feed_gatherer := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("feeding_detritivore_Marchant",
                      "feeding_collector_maxwell",
                      "Trop_collector_gatherer_botwe",
                      "Gatherer_proportion_of_feeding_fam_Chessman2017",
                      "Gatherer_proportion_of_feeding_gen_Chessman2017")]
Trait_AUS[, feed_filter := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("feeding_filterer_Marchant",
                      "Feeding_filterers_VicEPA",
                      "feeding_filterer_maxwell",
                      "Trop_collector_filterer_botwe",
                      "Filterer_proportion_of_feeding_fam_Chessman2017",
                      "Filterer_proportion_of_feeding_gen_Chessman2017")]

# Trait_AUS[, feed_herbivore := apply(.SD, 1, max),
#           .SDcols = c("feeding_grazer_Marchant",
#                       "Feeding_scrapers_VicEPA",
#                       "Feeding_deposit_grazer_VicEPA",
#                       "feeding_scraper_maxwell",
#                       "Trop_scraper_botwe",
#                       "Scraper_proportion_of_feeding_fam_Chessman2017",
#                       "Scraper_proportion_of_feeding_gen_Chessman2017")]
Trait_AUS[, feed_herbivore_VicEPA := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Feeding_scrapers_VicEPA",
                      "Feeding_deposit_grazer_VicEPA")]
Trait_AUS[, feed_herbivore := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("feeding_grazer_Marchant",
                      "feeding_scraper_maxwell",
                      "Trop_scraper_botwe",
                      "Scraper_proportion_of_feeding_fam_Chessman2017",
                      "Scraper_proportion_of_feeding_gen_Chessman2017", 
                      "feed_herbivore_VicEPA")]

Trait_AUS[, feed_predator := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("feeding_predator_Marchant",
                      "Feeding_predators_VicEPA",
                      "feeding_predator_maxwell",
                      "feed_predator_Schaefer",
                      "feed_predator",
                      "Trop_predator_botwe",
                      "Predator_proportion_of_feeding_fam_Chessman2017",
                      "Predator_proportion_of_feeding_gen_Chessman2017")]
Trait_AUS[, feed_parasite := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("feed_parasite",
                      "feed_parasite_Schaefer",
                      "feeding_parasite_maxwell")]

Trait_AUS[, c(
  "feeding_shredder_Marchant",
  "Feeding_shredders_VicEPA",
  "feeding_shredder_maxwell",
  "feed_shredder_Schaefer",
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
  "feed_predator_Schaefer",
  "feed_parasite_Schaefer",
  "Trop_collector_gatherer_botwe",
  "Trop_collector_filterer_botwe",
  "Trop_scraper_botwe",
  "Trop_predator_botwe",
  "Trop_shredder_detritivore_botwe",
  "feeding_collector_scraper_maxwell",
  "feeding_collec_shredder_maxwell",
  "feeding_parasite_maxwell",
  "Feeding_piercers_VicEPA",
  "Shredder_proportion_of_feeding_gen_Chessman2017",
  "Scraper_proportion_of_feeding_gen_Chessman2017",
  "Predator_proportion_of_feeding_gen_Chessman2017",
  "Gatherer_proportion_of_feeding_gen_Chessman2017",
  "Filterer_proportion_of_feeding_gen_Chessman2017",
  "Shredder_proportion_of_feeding_fam_Chessman2017",
  "Scraper_proportion_of_feeding_fam_Chessman2017",
  "Predator_proportion_of_feeding_fam_Chessman2017",
  "Gatherer_proportion_of_feeding_fam_Chessman2017",
  "Filterer_proportion_of_feeding_fam_Chessman2017",
  "feed_herbivore_VicEPA"
) := NULL]

# _________________________________________________________________________ 
#### Locomotion ####  
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler, climber, clinger (Habi4_botwe)
# locm_burrow: burrower
# locm_sessil: sessil (attached)
# Attach_perm_VicEPA just contained zeros
# _________________________________________________________________________ 
# Trait_AUS[, locom_swim := apply(.SD, 1, max), 
#           .SDcols = c("Habi5_botwe",
#                       "Habi6_botwe",
#                       "Attach_swim_VicEPA")]
Trait_AUS[, locom_swim_botwe := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Habi5_botwe",
                      "Habi6_botwe")]
Trait_AUS[, locom_swim := apply(.SD, 1, max, na.rm = TRUE), 
          .SDcols = c("locom_swim_botwe",
                      "Attach_swim_VicEPA")]

# Trait_AUS[, locom_crawl := apply(.SD, 1, max), 
#           .SDcols = c("Habi3_botwe",
#                       "Habi2_botwe",
#                       "Habi4_botwe",
#                       "Attach_crawl_VicEPA")]
Trait_AUS[, locom_crawl_botwe := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Habi3_botwe",
                      "Habi2_botwe",
                      "Habi4_botwe")]
Trait_AUS[, locom_crawl := apply(.SD, 1, max, na.rm = TRUE), 
          .SDcols = c("locom_crawl_botwe",
                      "Attach_crawl_VicEPA")]

Trait_AUS[, locom_burrow := apply(.SD, 1, max, na.rm = TRUE), 
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
  "Attach_swim_VicEPA",
  "locom_swim_botwe",
  "locom_crawl_botwe"
) := NULL]

# _________________________________________________________________________ 
#### Respiration ####  
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: spiracle & plastron
#  spiracle (atmospheric & plant breathers (Maxwell_1 & 2))
# plastron & spiracle often work together in respiratory systems of aq. insects
# Present in insects with open tracheal systems -> breathe oxygen from the air
# -> Different tolerances to low oxygen compared to insects with tegument resp and gills
# Maxwell trait on Plastron not used, since ambiguous (Plastron and gills)
# _________________________________________________________________________ 
Trait_AUS[, resp_teg := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Respiration_tegument_VicEPA",
                      "Resp1_botwe",
                      "resp3_Maxwell",
                      "resp_teg",
                      "resp_teg_Schaefer")]
Trait_AUS[, resp_gil := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Respiration_gills_VicEPA",
                      "Resp2_botwe",
                      "resp4_Maxwell",
                      "resp_gil",
                      "resp_gil_Schaefer",
                      "Gills_aquatic_stages_fam_Chessman2017",
                      "Gills_aquatic_stages_gen_Chessman2017")]

# Trait_AUS[, resp_pls_spi := apply(.SD, 1, max),
#           .SDcols = c(
#             "Respiration_spiracle_VicEPA",
#             "resp7_Maxwell",
#             "resp1_Maxwell",
#             "resp2_Maxwell",
#             "resp_atm",
#             "resp_atm_Schaefer",
#             "Air_respiration_aquatic_stages_fam_Chessman2017",
#             "Air_respiration_aquatic_stages_gen_Chessman2017",
#             "Functional_spiracles_aquatic_stages_gen_Chessman2017",
#             "Functional_spiracles_aquatic_stages_fam_Chessman2017",
#             "Resp3_botwe",
#             "resp_pls",
#             "resp_pls_Schaefer"
#           )]
Trait_AUS[, resp_pls_spi_maxwell := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("resp7_Maxwell",
                      "resp1_Maxwell",
                      "resp2_Maxwell")]
Trait_AUS[, resp_pls_spi_ot := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("resp_atm",
                      "resp_pls")]
Trait_AUS[, resp_pls_spi_schaefer := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("resp_atm_Schaefer",
                      "resp_pls_Schaefer")]
Trait_AUS[, resp_pls_spi := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c(
            "Respiration_spiracle_VicEPA",
            "resp_pls_spi_maxwell",
            "resp_pls_spi_ot",
            "Air_respiration_aquatic_stages_fam_Chessman2017",
            "Air_respiration_aquatic_stages_gen_Chessman2017",
            "Functional_spiracles_aquatic_stages_gen_Chessman2017",
            "Functional_spiracles_aquatic_stages_fam_Chessman2017",
            "Resp3_botwe",
            "resp_pls_spi_schaefer"
          )]

Trait_AUS[, c(
  "Respiration_tegument_VicEPA",
  "Resp1_botwe",
  "resp3_Maxwell",
  "resp_teg_Schaefer",
  "Respiration_gills_VicEPA",
  "Resp2_botwe",
  "resp4_Maxwell",
  "resp_gil_Schaefer",
  "Respiration_spiracle_VicEPA",
  "resp1_Maxwell",
  "resp2_Maxwell",
  "resp_atm",
  "resp_atm_Schaefer",
  "Resp3_botwe",
  "resp7_Maxwell",
  "resp_pls_Schaefer",
  "resp5_Maxwell",
  "resp6_Maxwell",
  "Air_respiration_aquatic_stages_fam_Chessman2017",
  "Air_respiration_aquatic_stages_gen_Chessman2017",
  "Gills_aquatic_stages_fam_Chessman2017",
  "Gills_aquatic_stages_gen_Chessman2017",
  "Functional_spiracles_aquatic_stages_gen_Chessman2017",
  "Functional_spiracles_aquatic_stages_fam_Chessman2017",
  "resp_pls",
  "resp_pls_spi_maxwell",
  "resp_pls_spi_ot",
  "resp_pls_spi_schaefer"
) := NULL]
# _________________________________________________________________________ 
#### Size #### 
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________ 
# Trait_AUS[, size_small := apply(.SD, 1, max), 
#           .SDcols = c("Max_size_less_than_5_VicEPA",
#                       "Max_size_5_to_10_VicEPA",
#                       "Size1_botwe",
#                       "Max_size_less_than_5__text_ref",
#                       "Max_size_5_to_10__text_ref",
#                       "Max_size_less_than_5__text_bugs_gbr",    
#                       "Max_size_5_to_10__text_bugs_gbr",
#                       "size_small", 
#                       "size_small_Schaefer",
#                       "size_small_fam_Chessman",
#                       "size_small_gen_Chessman")]
Trait_AUS[, size_small_VicEPA := apply(.SD, 1, sum, na.rm = TRUE), 
          .SDcols = c("Max_size_less_than_5_VicEPA",
                      "Max_size_5_to_10_VicEPA")]
Trait_AUS[, size_small_text_ref := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Max_size_less_than_5__text_ref",
                      "Max_size_5_to_10__text_ref")]
Trait_AUS[, size_small_bugs_gbr := apply(.SD, 1, sum, na.rm = TRUE), 
          .SDcols = c("Max_size_less_than_5__text_bugs_gbr",    
                      "Max_size_5_to_10__text_bugs_gbr")]
Trait_AUS[, size_small := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c(
            "size_small_VicEPA",
            "Size1_botwe",
            "size_small_text_ref",
            "size_small_bugs_gbr",
            "size_small",
            "size_small_Schaefer",
            "size_small_fam_Chessman",
            "size_small_gen_Chessman"
          )]

Trait_AUS[, size_medium := apply(.SD, 1, max, na.rm = TRUE), 
          .SDcols = c("Max_size_10_to_20_VicEPA",
                      "Size2_botwe",
                      "Max_size_10_to_20__text_ref",
                      "Max_size_10_to_20__text_bugs_gbr",
                      "size_medium", 
                      "size_medium_Schaefer",
                      "size_medium_fam_Chessman",
                      "size_medium_gen_Chessman")]

# Trait_AUS[, size_large := apply(.SD, 1, max), 
#           .SDcols = c("Max_size_20_to_40_VicEPA", 
#                       "Max_size_more_than_40_VicEPA",
#                       "Size3_botwe",
#                       "Max_size_20_to_40__text_ref", 
#                       "Max_size_greater_than_40__text_ref",
#                       "Max_size_20_to_40__text_bugs_gbr",       
#                       "Max_size_greater_than_40__text_bugs_gbr",
#                       "size_large", 
#                       "size_large_Schaefer",
#                       "size_large_gen_Chessman",
#                       "size_large_fam_Chessman")]
Trait_AUS[, size_large_VicEPA := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Max_size_20_to_40_VicEPA",
                      "Max_size_more_than_40_VicEPA")]
Trait_AUS[, size_large_text_ref := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Max_size_20_to_40__text_ref",
                      "Max_size_greater_than_40__text_ref")]
Trait_AUS[, size_large_bugs_gbr := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Max_size_20_to_40__text_bugs_gbr",
                      "Max_size_greater_than_40__text_bugs_gbr")]
Trait_AUS[, size_large := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c(
            "size_large_VicEPA",
            "Size3_botwe",
            "size_large_text_ref",
            "size_large_bugs_gbr",
            "size_large",
            "size_large_Schaefer",
            "size_large_gen_Chessman",
            "size_large_fam_Chessman"
          )]

Trait_AUS[, c(
  "Max_size_less_than_5_VicEPA",
  "Size1_botwe",
  "size_small_Schaefer",
  "Max_size_5_to_10_VicEPA",
  "Max_size_10_to_20_VicEPA",
  "Size2_botwe",
  "size_medium_Schaefer",
  "Max_size_20_to_40_VicEPA",
  "Size3_botwe",
  "size_large_Schaefer",
  "Max_size_more_than_40_VicEPA",
  "size_large_gen_Chessman",
  "size_large_fam_Chessman",
  "size_medium_fam_Chessman",
  "size_medium_gen_Chessman",
  "size_small_fam_Chessman",
  "size_small_gen_Chessman", 
  "Max_size_less_than_5__text_ref",
  "Max_size_5_to_10__text_ref",
  "Max_size_less_than_5__text_bugs_gbr",    
  "Max_size_5_to_10__text_bugs_gbr",
  "Max_size_10_to_20__text_ref",
  "Max_size_10_to_20__text_bugs_gbr",
  "Max_size_20_to_40__text_ref", 
  "Max_size_greater_than_40__text_ref",
  "Max_size_20_to_40__text_bugs_gbr",       
  "Max_size_greater_than_40__text_bugs_gbr",
  "size_large_VicEPA",
  "size_large_text_ref",
  "size_large_bugs_gbr",
  "size_small_VicEPA",
  "size_small_text_ref",
  "size_small_bugs_gbr"
) := NULL]

# _________________________________________________________________________ 
#### Oviposition ####  
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# _________________________________________________________________________ 
Trait_AUS[,  ovip_ter := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Rep2_botwe",
                      "repro2_Maxwell")]
Trait_AUS[, ovip_aqu := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Rep1_botwe",
                      "repro1_Maxwell", 
                      "ovip_aqu", 
                      "ovip_aqu_Schaefer")]
Trait_AUS[, ovip_ovo := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = c("Rep3_botwe",
                      "repro3_Maxwell",
                      "ovip_ovo", 
                      "ovip_ovo_Schaefer")]
Trait_AUS[, c(
  "Reproduction_single_ind_VicEPA",
  "Reproduction_m_and_f_VicEPA",
  "Rep1_botwe",
  "Rep2_botwe",
  "Rep3_botwe",
  "repro1_Maxwell",
  "repro2_Maxwell",
  "repro3_Maxwell",
  "ovip_aqu_Schaefer",
  "ovip_ovo_Schaefer"
) := NULL]

# _________________________________________________________________________ 
#### Temperature ####  
# temp very cold
# temp cold + temp moderate
# temp warm
# temp eurytherm
# Ther1_botwe not used (cool/warm eurythermal) -> ambiguou
# _________________________________________________________________________ 
Trait_AUS[, temp_eurytherm := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = c("Ther2_botwe", "Ther3_botwe")]
Trait_AUS[, c("Ther1_botwe",
              "Ther2_botwe",
              "Ther3_botwe") := NULL]

# _________________________________________________________________________ 
#### Body form ####
# streamlined
# cylindrical
# spherical
# flattened
# _________________________________________________________________________ 
setnames(x = Trait_AUS, 
         old = c("Body_form_clindrical_VicEPA",
                 "Body_form_flattended_VicEPA",
                 "Body_form_spherical_VicEPA",
                 "Body_form_streamlined_VicEPA"),
         new = c("bf_cylindrical",
                 "bf_flattened",
                 "bf_spherical",
                 "bf_streamlined"))

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
  dev_holometabol = ifelse(Order %in% holometabola, 1, 0)
)]

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
saveRDS(
  object = Trait_AUS,
  file = file.path(
    data_aggr,
    "Data",
    "Trait_AUS_harmonized.rds")
)
