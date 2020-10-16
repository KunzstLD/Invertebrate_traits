####################################################
#### Species with traits that could not be used ####
## load Set_up.R prior to this script
####################################################

#### freshwaterecology ####
dat_EU <- fread(
  file.path(
    data_in,
    "EU",
    "macroinvertebrate_EUR_complete.csv"
  )
)

# Subset to relevant traits
dat_EU <- dat_EU[, .SD, .SDcols = names(dat_EU) %like%
                   "order|family|genus|species|^ph|feed|loco|resp|disp|stage|volt|rep|temp"]

cols <- grep("species|genus|family|order|feed.*|locom.*|resp.*|rep.*",
             names(dat_EU),
             value = TRUE)

overview_EU <- dat_EU[!is.na(feed_other) |
                        !is.na(locom_other) |
                        !is.na(resp_sur) |
                        !is.na(resp_tap) |
                        !is.na(resp_ves) |
                        !is.na(rep_asexual), .SD, .SDcols = cols]

# too many species?
feed_EU <- overview_EU[!is.na(feed_other), .SD,
                       .SDcols = names(overview_EU) %like% "species|genus|family|order|feed.*"]
write.csv(feed_EU[order(-feed_other), ], 
          file = file.path(data_out, "freshecolg_not_compatible_feeding.csv"), 
          row.names = FALSE)

# locom_other relates mainly to flying or jumping outside the water
locom_EU <- overview_EU[!is.na(locom_other), .SD,
                        .SDcols = names(overview_EU) %like% "species|genus|family|order|locom.*"]
write.csv(locom_EU[order(-locom_other), ], 
          file = file.path(data_out, "freshecolg_not_compatible_locom.csv"), 
          row.names = FALSE)

# no species
overview_EU[!is.na(resp_sur), ]
overview_EU[!is.na(resp_tap), ]
overview_EU[!is.na(resp_ves), ]
overview_EU[!is.na(rep_asexual), ]


#### Tachet ####
# run 02_EU_preprocessing_tachet.R beforehand
write.csv(
  tachet[feed_piercer_t > 0, .SD,
         .SDcols = names(tachet) %like% "species|genus|family|order|feed.*"] %>%
    .[order(-feed_piercer_t),],
  file = file.path(data_out, "tachet_not_compatible_feeding.csv"),
  row.names = FALSE
)

# critical species
sel_col <- c("feed_active_filter_abs", 
             "feed_gatherer", 
             "feed_shredder",
             "feed_scraper", 
             "feed_active_filter",
             "feed_predator",
             "feed_parasite")
tachet[tachet[, Reduce(`&`, lapply(.SD, `==`, 0)),.SDcols = sel_col], 
       .SD, 
       .SDcols = names(tachet) %like% "species|genus|family|order|feed.*"]


# tachet locomotion 
write.csv(
  tachet[loc_flier_t > 0 | loc_interstitial_t > 0, .SD,
         .SDcols = names(tachet) %like% "species|genus|family|order|loc.*"] %>%
    .[order(-loc_flier_t, -loc_interstitial_t),],
  file = file.path(data_out, "tachet_not_compatible_locomotion.csv"),
  row.names = FALSE
)

#### North America (Vieira et al.) ####
Trait_Noa <- readRDS(file = file.path(data_cleaned, 
                                      "North_America",
                                      "Traits_US_pp.rds"))

## feed mode other
# detritivores in comment section have been assigned to collector gatherer:
# "Ceraclea maculata", "Lumbriculus variegatus", "Stylodrilus heringianus"
Trait_Noa_feed <-
  copy(Trait_Noa[`Feed_mode_prim_Other (specify in comments)` == 1, .SD,
                 .SDcols = names(Trait_Noa) %like% "Species|Genus|Family|Order|Feed.*"])

# critical taxa
sel_col <- c(
  "Feed_mode_prim_Collector-filterer",
  "Feed_mode_prim_Collector-gatherer",
  "Feed_mode_prim_Parasite",
  "Feed_mode_prim_Piercer herbivore",
  "Feed_mode_prim_Predator",
  "Feed_mode_prim_Scraper/grazer",
  "Feed_mode_prim_Shredder"
)
write.csv(Trait_Noa_feed[Trait_Noa_feed[, Reduce(`&`, lapply(.SD, `==`, 0)), .SDcols = sel_col],
               .SD,
               .SDcols = names(Trait_Noa_feed) %like% "Species|Genus|Family|Order|Feed.*"],
          file = file.path(data_out, "NOA_not_compatible_feeding.csv"), 
          row.names = FALSE)

# Load raw data to access comments
Trait_Noa_raw <- readRDS(file = file.path(data_in, "North_America", "Inverttraitstable_raw.rds"))
Trait_Noa_raw[, unique_id := 1:nrow(Trait_Noa_raw)]

# most of it is rather ambiguous
Trait_Noa_raw$Feed_mode_comments %>% table()


#### locomotion other
Trait_Noa_locom <- copy(Trait_Noa[`Habit_prim_Other (specify in comments)` == 1, .SD,
          .SDcols = names(Trait_Noa) %like% "Species|Genus|Family|Order|Habit.*"])

# critical taxa
# all taxa with locom_other == 1 also have another locomotion mode assigned
sel_col <- c("Habit_prim_Attached/fixed",
             "Habit_prim_Burrower",
             "Habit_prim_Climber",
             "Habit_prim_Clinger",
             "Habit_prim_Other (specify in comments)",
             "Habit_prim_Planktonic",
             "Habit_prim_Skater",
             "Habit_prim_Sprawler",
             "Habit_prim_Swimmer")
Trait_Noa_locom[Trait_Noa_locom[, Reduce(`&`, lapply(.SD, `==`, 0)), .SDcols = sel_col],
               .SD,
               .SDcols = names(Trait_Noa_locom) %like% "Species|Genus|Family|Order|Habit.*"]


#### respiration hemoglobin
write.csv(Trait_Noa[Resp_late_Hemoglobin == 1, .SD,
          .SDcols = names(Trait_Noa) %like% "Species|Genus|Family|Order|Resp.*"], 
          file = file.path(data_out, "NOA_not_compatible_resp.csv"), 
          row.names = FALSE)


#### Australia ####
Trait_AUS <- readRDS(
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_preproc.rds"
  )
)

# Collector Scraper, Collector Shredder
Trait_AUS_feeding_CS <-
  copy(Trait_AUS[feeding_collec_shredder_maxwell == 1, .SD,
                 .SDcols = names(Trait_AUS) %like% "Species|Genus|Family|Order|feed.*"])

Trait_AUS_feeding_CSr <-
  copy(Trait_AUS[feeding_collector_scraper_maxwell == 1, .SD,
                 .SDcols = names(Trait_AUS) %like% "Species|Genus|Family|Order|feed.*"])

# critical taxa
# no critical taxa
sel_col <- c("feeding_predator_Marchant",
             "feeding_detritivore_Marchant",
             "feeding_grazer_Marchant",
             "feeding_shredder_Marchant",
             "feeding_filterer_Marchant",
             "Shredder_proportion_of_feeding_fam_Chessman2017",
             "Scraper_proportion_of_feeding_fam_Chessman2017",
             "Predator_proportion_of_feeding_fam_Chessman2017",
             "Gatherer_proportion_of_feeding_fam_Chessman2017",
             "Filterer_proportion_of_feeding_fam_Chessman2017",
             "feeding_collector_maxwell",
             "feeding_predator_maxwell",
             "feeding_shredder_maxwell",
             "feeding_collec_shredder_maxwell",
             "feeding_scraper_maxwell",
             "feeding_parasite_maxwell",
             "feeding_collector_scraper_maxwell",
             "feeding_filterer_maxwell",
             "Shredder_proportion_of_feeding_gen_Chessman2017",
             "Scraper_proportion_of_feeding_gen_Chessman2017",
             "Predator_proportion_of_feeding_gen_Chessman2017",
             "Gatherer_proportion_of_feeding_gen_Chessman2017",
             "Filterer_proportion_of_feeding_gen_Chessman2017",
             "feed_shredder",
             "feed_predator",
             "feed_parasite",
             "feed_shredder_Schaefer",
             "feed_predator_Schaefer",
             "feed_parasite_Schaefer")

Trait_AUS_feeding_CS[Trait_AUS_feeding_CS[, Reduce(`&`, lapply(.SD, `==`, 0)), .SDcols = sel_col],
                .SD,
                .SDcols = names(Trait_AUS_feeding_CS) %like% "Species|Genus|Family|Order|feeding.*"]

Trait_AUS_feeding_CSr[Trait_AUS_feeding_CSr[, Reduce(`&`, lapply(.SD, `==`, 0)), .SDcols = sel_col],
                     .SD,
                     .SDcols = names(Trait_AUS_feeding_CSr) %like% "Species|Genus|Family|Order|feeding.*"]


# respiration
# resp5_Maxwell: plastron and gills
# resp7_Maxwell: Pneumostome -> only with snails
Trait_AUS_resp <- copy(Trait_AUS[resp5_Maxwell == 1 | resp7_Maxwell == 1, .SD, 
          .SDcols = names(Trait_AUS) %like% "Species|Genus|Family|Order|resp.*"])

# critical taxa
# no critical taxa
sel_col <- c("Air_respiration_aquatic_stages_fam_Chessman2017",
             "resp1_Maxwell",
             "resp2_Maxwell",
             "resp3_Maxwell",
             "resp4_Maxwell",
             "resp5_Maxwell",
             "resp6_Maxwell",
             "resp7_Maxwell",
             "Air_respiration_aquatic_stages_gen_Chessman2017",
             "resp_teg",
              "resp_gil",
             "resp_pls",
             "resp_atm",
             "resp_teg_Schaefer",
             "resp_gil_Schaefer",
             "resp_pls_Schaefer",
             "resp_atm_Schaefer")

Trait_AUS_resp[Trait_AUS_resp[, Reduce(`&`, lapply(.SD, `==`, 0)), .SDcols = sel_col],
               .SD,
               .SDcols = names(Trait_AUS_resp) %like% "Species|Genus|Family|Order|resp.*"]



# parasite taxa
# EU
Trait_EU <-
  readRDS(file = file.path(data_cleaned, "EU", "Trait_EU_pp_harmonized.rds"))

cols <- grep("order|family|genus|species|feed.*", names(Trait_EU),
             value = TRUE)

write.csv(
  Trait_EU[feed_parasite > 0, .SD, .SDcols = cols] %>%
    .[order(-feed_parasite), ],
  file = file.path(data_out, "EU_parasite.csv"),
  row.names = FALSE
)

# NOA
Trait_Noa_new <- readRDS(file = file.path(data_cleaned,
                                          "North_America",
                                          "Traits_US_LauraT_pp_harmonized.rds"))

cols <- grep("(?i)order|family|genus|species|feed.*", names(Trait_Noa_new),
             value = TRUE)

write.csv(
  Trait_Noa_new[feed_parasite > 0, .SD, .SDcols = cols] %>%
    .[order(-feed_parasite),],
  file = file.path(data_out, "NOA_parasite.csv"),
  row.names = FALSE
)

# AUS
Trait_AUS <- readRDS(
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_harmonized.rds"
  )
)

cols <- grep("order|family|genus|species|feed.*", names(Trait_AUS),
             value = TRUE)
write.csv(Trait_AUS[feed_parasite > 0, .SD, .SDcols = cols],
          file = file.path(data_out, "AUS_parasite.csv"),
          row.names = FALSE)




