# _________________________________________________________________________ 
#### Harmonize NOA Traits ####
# Includes also normalization
# _________________________________________________________________________

# read in
Trait_Noa_fc <- readRDS(file = file.path(data_cleaned, 
                                         "North_America", 
                                         "Traits_US_pp_fc.rds"))

# _________________________________________________________________________
#### Voltinism ####
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________
setnames(
  Trait_Noa_fc,
  old = c(
    "Voltinism_1 Generation per year",
    "Voltinism_< 1 Generation per year",
    "Voltinism_> 1 Generation per year"
  ),
  new = c("volt_uni", "volt_semi", "volt_bi_multi")
)

# _________________________________________________________________________
#### aquatic stages ####
# gives information about which stage lives in the aquatic phase 
# stage1: egg
# stage2: larva and/or nymph
# stage3: egg, larva, pupa
# stage4: egg, larva, pupa, adult
# _________________________________________________________________________
setnames(
  Trait_Noa_fc,
  old = c(
    "No.Aquatic_stages_1 (larvae/nymph only)" ,
    "No.Aquatic_stages_2 (egg, larvae/nymph)",
    "No.Aquatic_stages_3 (egg, larvae, pupae)",
    "No.Aquatic_stages_4 (egg, larvae, pupae, adult"
  ),
  new = c("stage_1", "stage_2", "stage_3", "stage_4")
)

# _________________________________________________________________________
# PH 
# ph_acidic, ph_neutral
# neutral and alcaline combined to neutral
# _________________________________________________________________________
# setnames(Trait_Noa, "pH_acidic", "ph_acidic")
# Trait_Noa[, ph_neutral := apply(.SD, 1, max),
#           .SDcols = c("pH_normal", "pH_alkaline")]
# Trait_Noa[, c("pH_alkaline", "pH_normal") := NULL]

# _________________________________________________________________________
#### Feed mode ####
# feed_shredder: shredder (chewers, miners, xylophagus, herbivore piercers)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_scraper: scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite

# feed_shredder: shredder (chewers, miners, xylophagus)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# _________________________________________________________________________
setnames(
  Trait_Noa_fc,
  old = c(
    "Feed_mode_prim_Collector-filterer",
    "Feed_mode_prim_Collector-gatherer",
    "Feed_mode_prim_Parasite",
    "Feed_mode_prim_Predator",
    "Feed_mode_prim_Shredder"
  ),
  new = c(
    "feed_filter",
    "feed_gatherer",
    "feed_parasite",
    "feed_predator",
    "feed_shredder"
  )
)
Trait_Noa_fc[, feed_herbivore := apply(.SD, 1. , max),
          .SDcols = c("Feed_mode_prim_Scraper/grazer",
                      "Feed_mode_prim_Piercer herbivore")]
Trait_Noa_fc[, c("Feed_mode_prim_Scraper/grazer",
              "Feed_mode_prim_Piercer herbivore") := NULL]

# incorporating information from comments 
# load raw NoA data
Trait_Noa_raw <- readRDS(file = file.path(data_in, "North_America", "Inverttraitstable_raw.rds"))
Trait_Noa_raw[, unique_id := 1:nrow(Trait_Noa_raw)]

# taxa with feeding mode "other"
Taxa_feed_other <-
  Trait_Noa_fc[`Feed_mode_prim_Other (specify in comments)` > 0 &
              feed_herbivore == 0 & feed_filter == 0 &
              feed_gatherer == 0 & feed_parasite == 0 &
              feed_predator == 0 &
              feed_shredder == 0, .(`Feed_mode_prim_Other (specify in comments)`, 
                                    Genus)]

# detritivores -> collector gatherer
# no values on Genus-level (data is on species-level).
# Hence, no assignment
# detrivores <-
#   Trait_Noa_raw[Feed_mode_prim %in% "Other (specify in comments)" &
#                   Genus %in% Taxa_feed_other$Genus &
#                   grepl("detritivore.*|Detritivore", Feed_mode_comments) &
#                   is.na(Species) &
#                   !is.na(Genus),
#                 .(Species, Genus, Feed_mode_prim, Feed_mode_comments, unique_id)]
# Trait_Noa_fc[detrivores,
#           `:=`(feed_gatherer = 1),
#           on = "Genus"]

# omnivorous to predators?
# mouthparts of shredders often suited for scavenging

# del feed mode others
Trait_Noa_fc[, `Feed_mode_prim_Other (specify in comments)` := NULL]

# _________________________________________________________________________
#### Locomotion ####
# locom_swim:  swimmer, scater (active & passive)
# locom_crawl: crawlers, walkers & sprawler, climber
# locom_burrow: burrower
# locom_sessil: sessil (attached)
# What to do with clingers? -> According to LeRoy and Chuck they should be put to crawlers
# _________________________________________________________________________
setnames(Trait_Noa_fc, 
         old = c("Habit_prim_Attached/fixed", "Habit_prim_Burrower"), 
         new = c("locom_sessil", "locom_burrow"))
Trait_Noa_fc[, locom_swim := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Swimmer",
                      "Habit_prim_Planktonic",
                      "Habit_prim_Skater")]
Trait_Noa_fc[, locom_crawl := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Sprawler", "Habit_prim_Climber", "Habit_prim_Clinger")]
Trait_Noa_fc[, c(
  "Habit_prim_Swimmer",
  "Habit_prim_Planktonic",
  "Habit_prim_Skater",
  "Habit_prim_Sprawler",
  "Habit_prim_Climber",
  "Habit_prim_Clinger"
) := NULL]

# What to do with habit prim other?
locom_comment <- Trait_Noa_fc[`Habit_prim_Other (specify in comments)` == 1 &
                             (locom_sessil == 0 & locom_burrow == 0 &
                                locom_swim == 0 &
                                locom_crawl == 0) & 
                               !is.na(Genus),]
# crawler
crawler <- Trait_Noa_raw[Genus %in% locom_comment$Genus & 
                           is.na(Species) &
                           grepl("(?i)crawl", Habit_comments) &
                           !grepl("(?i)burrow", Habit_comments)
                         & !grepl("(?i)excellent swimmer.*", Habit_comments), 
                         .(Habit_prim, Habit_comments, Genus)]
Trait_Noa_fc[crawler,
          `:=`(locom_crawl = 1),
          on = "Genus"]

# crawler & burrower
crawl_burrow <- Trait_Noa_raw[Genus %in% locom_comment$Genus & 
                                is.na(Species) &
                                grepl("(?i)crawl", Habit_comments) &
                                grepl("(?i)burrow", Habit_comments) &
                               !grepl("(?i)excellent swimmer.*", Habit_comments), 
                              .(Habit_prim, Habit_comments, Genus)]

Trait_Noa_fc[crawl_burrow,
          `:=`(locom_crawl = 1,
               locom_burrow = 1),
          on = "Genus"]

# crawler & swimmer
# all data on species-level -> not used
# crawl_swim <- Trait_Noa_raw[Genus %in% locom_comment$Genus & 
#                               is.na(Species) &
#                               grepl("(?i)crawl", Habit_comments) &
#                               !grepl("(?i)burrow", Habit_comments) &
#                               grepl("(?i)excellent swimmer.*", Habit_comments), 
#                             .(Habit_prim, Habit_comments, Genus, Species)]
# Trait_Noa_fc[crawl_swim,
#           `:=`(locom_crawl = 1,
#                locom_swim = 1),
#           on = "Genus"]

# del Habit_prim_other & Habit_prim_Clinger
Trait_Noa_fc[, `Habit_prim_Other (specify in comments)` := NULL]

# _________________________________________________________________________
#### Respiration ####
# late the right choice?
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: plastron & spiracle

# Resp_late_Atmospheric breathers -> Spiracle
# Resp_late_Hemoglobin  -> just 9 entries, can be ignored
# Resp_late_Other (specify in comments) -> Most just describe the trait more precisely
# Resp_late_Plant breathers? -> Must have spiracles (exchange 02 with the environment)
# Resp_late_Plastron (permanent air store) -> Plastron
# Resp_late_Spiracular gills -> Plastron!
# Resp_late_Temporary air store -> Gills
# Resp_late_Tracheal gills -> Gills
# _________________________________________________________________________
setnames(Trait_Noa_fc,
         old = c("Resp_late_Cutaneous"),
         new = c("resp_teg"))
Trait_Noa_fc[, resp_pls_spi := apply(.SD, 1, max),
          .SDcols = c("Resp_late_Spiracular gills",
                      "Resp_late_Plastron (permanent air store)",
                      "Resp_late_Atmospheric breathers",
                      "Resp_late_Plant breathers")]
Trait_Noa_fc[, resp_gil := apply(.SD, 1, max),
          .SDcols = c("Resp_late_Temporary air store",
                      "Resp_late_Tracheal gills")]
Trait_Noa_fc[, c(
  "Resp_late_Atmospheric breathers",
  "Resp_late_Plant breathers",
  "Resp_late_Spiracular gills",
  "Resp_late_Plastron (permanent air store)",
  "Resp_late_Temporary air store",
  "Resp_late_Tracheal gills",
  "Resp_late_Hemoglobin"
) := NULL]

# respiration comments -> not enough information
# resp_comments <-
#   Trait_Noa_fc[(resp_teg == 0 & resp_pls_spi == 0 &
#                resp_gil == 0) &
#               `Resp_late_Other (specify in comments)` == 1,
#             .(Genus)]
# Trait_Noa_raw[Genus %in% resp_comments$Genus,
#              .(Resp_comments)]

# del Resp_later_Other
Trait_Noa_fc[, `Resp_late_Other (specify in comments)` := NULL]

# _________________________________________________________________________
#### Drift/dispersal ####
# use disp low, medium, high for comparability 
# del dispersal_unknown
# _________________________________________________________________________
setnames(
  Trait_Noa_fc,
  old = c(
    "Larval_disp_< 1 m",
    "Larval_disp_1-10 m",
    "Larval_disp_11-100 m"
  ),
  new = c("disp_low",
          "disp_medium",
          "disp_high")
)

# _________________________________________________________________________
#### Size ####
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________
setnames(
  Trait_Noa_fc,
  old = c(
    "Max_body_size_Large (length > 16 mm)",
    "Max_body_size_Small (length < 9 mm)",
    "Max_body_size_Medium (length 9-16 mm)"
  ),
  new = c("size_large",
          "size_small",
          "size_medium")
)

# _________________________________________________________________________
#### Oviposition ####
# Modalities
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# _________________________________________________________________________
Trait_Noa_fc[, ovip_ter := apply(.SD, 1, max),
          .SDcols = c("Ovipos_behav_prim_Bank soil",
                      "Ovipos_behav_prim_Overhanging substrate (dry)")]
Trait_Noa_fc[, ovip_aqu := apply(.SD, 1., max) ,
          .SDcols = c(
            "Ovipos_behav_prim_Algal mats",
            "Ovipos_behav_prim_Bottom sediments",
            "Ovipos_behav_prim_Floating debris",
            "Ovipos_behav_prim_In moss/macrophytes (submerged)",
            "Ovipos_behav_prim_In wet wood",
            "Ovipos_behav_prim_On/under stones (submerged)",
            "Ovipos_behav_prim_Free-floating"
          )]

# del original categories
Trait_Noa_fc[, c("Ovipos_behav_prim_Algal mats",
              "Ovipos_behav_prim_Bottom sediments",
              "Ovipos_behav_prim_Floating debris",
              "Ovipos_behav_prim_In moss/macrophytes (submerged)",
              "Ovipos_behav_prim_In wet wood",
              "Ovipos_behav_prim_On/under stones (submerged)", 
              "Ovipos_behav_prim_Bank soil",
              "Ovipos_behav_prim_Overhanging substrate (dry)",
              "Ovipos_behav_prim_Free-floating"
) := NULL]

# extract entries with ovovivip. according to comments
Ovovi_US <-
  Trait_Noa_raw[grepl(
    "Ovovivi.*|ovoviv.*|Female hold eggs|marsupium|Eggs carried by female",
    Ovipos_behav_comments
  ) &
    is.na(Species) &
    !is.na(Genus), .(Ovipos_behav_comments, Genus)]

# merge back to Trait_Noa 
Trait_Noa_fc[Ovovi_US,
          `:=`(ovip_ovo = 1),
          on = "Genus"]

# turn NAs into zero
Trait_Noa_fc[, ovip_ovo := ifelse(is.na(ovip_ovo), 0, ovip_ovo)]

# complete with further information from comment section -> no need
# missing_ovip <-
#   Trait_Noa[`Ovipos_behav_prim_Other (specify in comments)` == 1 &
#               (ovip_ovo == 0 & ovip_ter == 0 & ovip_aqu == 0), ]
# comments ovip ter
# Ovip_ter_US <-
#   Trait_Noa_raw[unique_id %in% missing_ovip &
#                  grepl("dry|bank|rocks|shores", Ovipos_behav_comments),
#                .(Ovipos_behav_comments, Ovipos_behav_sec, unique_id)]
# 
# # comments ovip aqu
# Ovip_aq_US <-
#   Trait_Noa_raw[unique_id %in% missing_ovip &
#                  grepl("water.* surface|plant", Ovipos_behav_comments),
#                .(Ovipos_behav_comments, Ovipos_behav_sec, unique_id)]
# 
# # merge back 
# Trait_Noa[Ovip_ter_US,
#          `:=`(ovip_ter = 1),
#          on = "unique_id"]
# Trait_Noa[Ovip_aq_US,
#          `:=`(ovip_aqu = 1),
#          on = "unique_id"]

# del Ovipos_other
Trait_Noa_fc[,`Ovipos_behav_prim_Other (specify in comments)` := NULL ]

# _________________________________________________________________________
#### Temperature ####
# temp very cold (EU < 6 °C, NoA < 5 °C)
# temp cold (EU < 10 °C) + temp moderate (EU < 18 °C) (NoA 0-15)
# temp warm (EU >= 18 °C, NoA >15)
# temp eurytherm (no pref)
# _________________________________________________________________________
setnames(Trait_Noa_fc, 
         old = c("Thermal_pref_Cold stenothermal (<5 C)", 
                 "Thermal_pref_Cold-cool eurythermal (0-15 C)", 
                 "Thermal_pref_No strong preference"),
         new = c("temp_very_cold", 
                 "temp_cold_mod", 
                 "temp_eurytherm"))
Trait_Noa_fc[, temp_warm := apply(.SD, 1, max),
          .SDcols = c("Thermal_pref_Warm eurythermal (15-30 C)",
                      "Thermal_pref_Hot euthermal (>30 C)")]
Trait_Noa_fc[, c("Thermal_pref_Warm eurythermal (15-30 C)",
              "Thermal_pref_Hot euthermal (>30 C)") := NULL]

# change some colnames 
setnames(Trait_Noa_fc, 
         old = c("Genus", "Family", "Order"), 
         new = c("genus", "family", "order"))

# _________________________________________________________________________
#### Body form ####
# bf_streamlined: streamlined/fusiform
# bf_flattened: flattened (dorso-ventrally)
# bf_cylindrical: cylindrical/tubular 
# bf_spherical: spherical
# _________________________________________________________________________

## rm body_shape_case columns
cols <- grep("case", names(Trait_Noa_fc), value = TRUE)
Trait_Noa_fc[, (cols) := NULL]

## Using Philippe Polateras classification
body_form_pup <- fread(file.path(data_missing, "Body_form_EU_NOA", "body_form_polatera_EU_NOA.csv"))

# change "," to "." and convert bf columns to numeric
cols <- c("streamlined", "flattened", "cylindrical", "spherical")
body_form_pup[, (cols) := lapply(.SD, function(y) {
  sub("\\,", ".", y) %>%
    as.numeric(.)
}),
.SDcols = cols]

# Add body form traits from PUP (have priority)
blocky_pup <- body_form_pup[array %in% "NOA_trait_project_taxa_blocky" |
                              array %in% "NOA_trait_project_taxa_n_agg", ]

# remove duplicated taxa
blocky_pup <- blocky_pup[!(!is.na(species) & duplicated(species)), ]

# 15 entries have no information on body form
blocky_pup <- blocky_pup[!(is.na(streamlined) | is.na(cylindrical) | is.na(spherical) | is.na(flattened)),]

# since we can only merge on genus level, we allocate 
# entries on species-level in the PUP dataset to genus-level
cols <- c("streamlined",
          "flattened",
          "cylindrical",
          "spherical")
blocky_pup[, (cols) := lapply(.SD, median), .SDcols = cols,
           by = "genus"]
blocky_pup <- blocky_pup[!duplicated(genus), ]

# merge bf information on genus level
Trait_Noa_fc[blocky_pup[!is.na(genus), ],
          `:=`(
            streamlined = i.streamlined,
            flattened = i.flattened,
            cylindrical = i.cylindrical,
            spherical = i.spherical
          ),
          on = "genus"]

# put bf data NOA with PUP assignments together
Trait_Noa_fc[, `:=`(
  bf_flattened = coalesce(flattened , `Body_shape_Dorsoventrally flattened`),
  bf_spherical = coalesce(spherical , `Body_shape_Round (humped)`),
  bf_streamlined = coalesce(streamlined, `Body_shape_Streamlined / fusiform`),
  bf_cylindrical = coalesce(cylindrical, Body_shape_Tubular)
)]

# del other bf columns
Trait_Noa_fc[, c("Body_shape_Bluff (blocky)",
              "Body_shape_Dorsoventrally flattened",
              "Body_shape_Round (humped)",
              "Body_shape_Streamlined / fusiform",
              "Body_shape_Tubular",
              "streamlined",
              "flattened",
              "cylindrical",
              "spherical") := NULL]

# _________________________________________________________________________
#### Normalization ####
# First step is normalizing of the trait values to a range of [0 - 1] by
# dividing for a given trait each value for a trait state by the sum of all 
# trait states
# _________________________________________________________________________
Trait_Noa_fc <- normalize_by_rowSum(
  x = Trait_Noa_fc,
  non_trait_cols = c("unique_id",
                     "order",
                     "family",
                     "genus",
                     "species")
)

# check completenes of the trait data
completeness_trait_data(x = Trait_Noa_fc,
                        non_trait_cols = c("unique_id",
                                           "order",
                                           "family",
                                           "genus",
                                           "species"))
# save
saveRDS(object = Trait_Noa_fc, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp_harmonized_fc.rds")
)
