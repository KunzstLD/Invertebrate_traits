# ------------------------------------------------------------------------- 
#### Harmonize NOA Traits ####
# -------------------------------------------------------------------------

# read in
Trait_Noa <- readRDS(file = file.path(data_cleaned, "North_America", "Traits_US_pp.rds"))

# Voltinism ---------------------------------------------------------------
# volt_semi
# volt_uni
# volt_bi_multi
setnames(
  Trait_Noa,
  old = c(
    "Voltinism_1 Generation per year",
    "Voltinism_< 1 Generation per year",
    "Voltinism_> 1 Generation per year"
  ),
  new = c("volt_uni", "volt_semi", "volt_bi_multi")
)

# aquatic stages
# gives information about which stage lives in the aquatic phase 
# stage1: egg
# stage2: larva and/or nymph
# stage3: egg, larva, pupa
# stage4: egg, larva, pupa, adult
setnames(
  Trait_Noa,
  old = c(
    "No.Aquatic_stages_1 (larvae/nymph only)" ,
    "No.Aquatic_stages_2 (egg, larvae/nymph)",
    "No.Aquatic_stages_3 (egg, larvae, pupae)",
    "No.Aquatic_stages_4 (egg, larvae, pupae, adult"
  ),
  new = c("stage_1", "stage_2", "stage_3", "stage_4")
)

# ------------------------------------------------------------------------

# ph ----------------------------------------------------------------------
# ph_acidic, ph_neutral
# neutral and alcaline combined to neutral
setnames(Trait_Noa, "pH_acidic", "ph_acidic")
Trait_Noa[, ph_neutral := apply(.SD, 1, max),
          .SDcols = c("pH_normal", "pH_alkaline")]
Trait_Noa[, c("pH_alkaline", "pH_normal") := NULL]

# ------------------------------------------------------------------------

# Feed mode ---------------------------------------------------------------
# feed_shredder: shredder (chewers, miners, xylophagus, herbivore piercers)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_scraper: scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
setnames(
  Trait_Noa,
  old = c(
    "Feed_mode_prim_Collector-filterer",
    "Feed_mode_prim_Collector-gatherer",
    "Feed_mode_prim_Parasite",
    "Feed_mode_prim_Predator",
    "Feed_mode_prim_Scraper/grazer"
  ),
  new = c(
    "feed_filter",
    "feed_gatherer",
    "feed_parasite",
    "feed_predator",
    "feed_scraper"
  )
)
Trait_Noa[, feed_shredder := apply(.SD, 1. , max),
          .SDcols = c("Feed_mode_prim_Shredder",
                      "Feed_mode_prim_Piercer herbivore")]
Trait_Noa[, c("Feed_mode_prim_Shredder",
              "Feed_mode_prim_Piercer herbivore") := NULL]

# incorporating information from comments 
# load raw NoA data
Trait_Noa_raw <- readRDS(file = file.path(data_in, "North_America", "Inverttraitstable_raw.rds"))
Trait_Noa_raw[, unique_id := 1:nrow(Trait_Noa_raw)]

# taxa with feeding mode "other"
Taxa_feed_other <-
  Trait_Noa[`Feed_mode_prim_Other (specify in comments)` == 1 &
              feed_scraper == 0 & feed_filter == 0 &
              feed_gatherer == 0 & feed_parasite == 0 &
              feed_predator == 0 &
              feed_shredder == 0, .(`Feed_mode_prim_Other (specify in comments)`, 
                                    unique_id, Species)]

# View(Trait_Noa_raw[Feed_mode_prim %in% "Other (specify in comments)" &
#                     unique_id %in% Taxa_feed_other$unique_id &
#                     !is.na(Species) & !is.na(Genus), 
#                   .(Species, Genus,
#                       Feed_mode_comments)])

# detritivores -> collector gatherer
detrivores <-
  Trait_Noa_raw[Feed_mode_prim %in% "Other (specify in comments)" &
                 unique_id %in% Taxa_feed_other$unique_id &
                 grepl("detritivore.*|Detritivore", Feed_mode_comments) &
                 !is.na(Species) & !is.na(Genus),
               .(Species, Feed_mode_prim, Feed_mode_comments, unique_id)]
Trait_Noa[detrivores,
          `:=`(feed_gatherer = 1),
          on = "unique_id"]
# omnivorous to predators?
# mouthparts of shredders often suited for scavenging
Trait_Noa[, `Feed_mode_prim_Other (specify in comments)` := NULL]

# -------------------------------------------------------------------------

# Locomotion --------------------------------------------------------------
# locom_swim:  swimmer, scater (active & passive)
# locom_crawl: crawlers, walkers & sprawler, climber
# locom_burrow: burrower
# locom_sessil: sessil (attached)
setnames(Trait_Noa, 
         old = c("Habit_prim_Attached/fixed", "Habit_prim_Burrower"), 
         new = c("locom_sessil", "locom_burrow"))
Trait_Noa[, locom_swim := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Swimmer",
                      "Habit_prim_Planktonic",
                      "Habit_prim_Skater")]
Trait_Noa[, locom_crawl := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Sprawler", "Habit_prim_Climber")]
Trait_Noa[, c(
  "Habit_prim_Swimmer",
  "Habit_prim_Planktonic",
  "Habit_prim_Skater",
  "Habit_prim_Sprawler",
  "Habit_prim_Climber",
  "Habit_prim_Clinger"
) := NULL]

# What to do with clingers?
# Some clingers also burrowers & swimmer -> Scan comment section
# Trait_Noa_raw[!is.na(Habit_comments) & 
#                     Habit_prim %in% "Clinger" & 
#                grepl("brrow|swim|crawl", Habit_comments), .(Habit_comments, Habit_prim)]

# habit prim other?
locom_comment <- Trait_Noa[`Habit_prim_Other (specify in comments)` == 1 &
                             (locom_sessil == 0 & locom_burrow == 0 &
                                locom_swim == 0 &
                                locom_crawl == 0) & !is.na(Species)
                           & !is.na(Genus),
                           .(Species, Genus, unique_id)]

# crawler
crawler <- Trait_Noa_raw[unique_id %in% locom_comment$unique_id & 
                          grepl("(?i)crawl", Habit_comments) & !grepl("(?i)burrow", Habit_comments)
                        & !grepl("(?i)excellent swimmer.*", Habit_comments), 
                        .(Habit_prim, Habit_comments, unique_id)]
Trait_Noa[crawler,
          `:=`(locom_crawl = 1),
          on = "unique_id"]

# crawler & burrower
crawl_burrow <- Trait_Noa_raw[unique_id %in% locom_comment$unique_id & 
                               grepl("(?i)crawl", Habit_comments) & grepl("(?i)burrow", Habit_comments)
                             & !grepl("(?i)excellent swimmer.*", Habit_comments), 
                             .(Habit_prim, Habit_comments, unique_id)]
Trait_Noa[crawl_burrow,
          `:=`(locom_crawl = 1,
               locom_burrow = 1),
          on = "unique_id"]
# crawler & swimmer
crawl_swim <- Trait_Noa_raw[unique_id %in% locom_comment$unique_id & 
                             grepl("(?i)crawl", Habit_comments) & !grepl("(?i)burrow", Habit_comments)
                           & grepl("(?i)excellent swimmer.*", Habit_comments), 
                           .(Habit_prim, Habit_comments, unique_id)]
Trait_Noa[crawl_swim,
          `:=`(locom_crawl = 1,
               locom_swim = 1),
          on = "unique_id"]

Trait_Noa[, `Habit_prim_Other (specify in comments)` := NULL]
# -------------------------------------------------------------------------

# Respiration -------------------------------------------------------------
# late the right choice?
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_spi: spiracle
# resp_pls: plastron

# Resp_late_Atmospheric breathers -> Spiracle
# Resp_late_Hemoglobin  -> just 9 entries, can be ignored
# Resp_late_Other (specify in comments) -> Most just describe the trait more precisely
# Resp_late_Plant breathers? -> Must have spiracles (exchange 02 with the environment)
# Resp_late_Plastron (permanent air store) -> Plastron
# Resp_late_Spiracular gills -> Plastron!
# Resp_late_Temporary air store -> Gills
# Resp_late_Tracheal gills -> Gills
setnames(Trait_Noa,
         old = c("Resp_late_Cutaneous"),
         new = c("resp_teg"))
Trait_Noa[, resp_spi := apply(.SD, 1, max),
          .SDcols = c("Resp_late_Atmospheric breathers",
                      "Resp_late_Plant breathers")]
Trait_Noa[, resp_pls := apply(.SD, 1, max),
          .SDcols = c("Resp_late_Spiracular gills",
                      "Resp_late_Plastron (permanent air store)")]
Trait_Noa[, resp_gil := apply(.SD, 1, max),
          .SDcols = c("Resp_late_Temporary air store",
                      "Resp_late_Tracheal gills")]
Trait_Noa[, c(
  "Resp_late_Atmospheric breathers",
  "Resp_late_Plant breathers",
  "Resp_late_Spiracular gills",
  "Resp_late_Plastron (permanent air store)",
  "Resp_late_Temporary air store",
  "Resp_late_Tracheal gills",
  "Resp_late_Hemoglobin"
) := NULL]

# respiration comments
resp_comments <-
  Trait_Noa[(resp_teg == 0 & resp_spi == 0 & resp_pls == 0 &
               resp_gil == 0) &
              `Resp_late_Other (specify in comments)` == 1,
            .(unique_id)]

# Trait_Noa_raw[unique_id %in% resp_comments$unique_id, 
#              .(Resp_comments)]

Trait_Noa[, `Resp_late_Other (specify in comments)` := NULL]
# -------------------------------------------------------------------------

# Drift/dispersal ---------------------------------------------------------
# use disp low, medium, high for comparability 
# del dispersal_unknown
setnames(
  Trait_Noa,
  old = c(
    "Larval_disp_< 1 m",
    "Larval_disp_1-10 m",
    "Larval_disp_11-100 m"
  ),
  new = c("disp_low",
          "disp_medium",
          "disp_high")
)
# -------------------------------------------------------------------------

# Size --------------------------------------------------------------------
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
setnames(
  Trait_Noa,
  old = c(
    "Max_body_size_Large (length > 16 mm)",
    "Max_body_size_Small (length < 9 mm)",
    "Max_body_size_Medium (length 9-16 mm)"
  ),
  new = c("size_large",
          "size_small",
          "size_medium")
)
# -------------------------------------------------------------------------

# Oviposition -------------------------------------------------------------
# Modalities
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
Trait_Noa[, ovip_ter := apply(.SD, 1, max),
          .SDcols = c("Ovipos_behav_prim_Bank soil",
                      "Ovipos_behav_prim_Overhanging substrate (dry)")]
Trait_Noa[, ovip_aqu := apply(.SD, 1., max) ,
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
Trait_Noa[, c("Ovipos_behav_prim_Algal mats",
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
  ), .(Ovipos_behav_comments, unique_id)]

# merge back to Trait_Noa 
Trait_Noa[Ovovi_US,
          `:=`(ovip_ovo = 1),
          on = "unique_id"]
# turn NAs into zero
Trait_Noa[, ovip_ovo := ifelse(is.na(ovip_ovo), 0, ovip_ovo)]

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
Trait_Noa[,`Ovipos_behav_prim_Other (specify in comments)` := NULL ]
# -------------------------------------------------------------------------

# Temperature -------------------------------------------------------------
# temp very cold (EU < 6 °C, NoA < 5 °C)
# temp cold (EU < 10 °C) + temp moderate (EU < 18 °C) (NoA 0-15)
# temp warm (EU >= 18 °C, NoA >15)
# temp eurytherm (no pref)
setnames(Trait_Noa, 
         old = c("Thermal_pref_Cold stenothermal (<5 C)", 
                 "Thermal_pref_Cold-cool eurythermal (0-15 C)", 
                 "Thermal_pref_No strong preference"),
         new = c("temp_very_cold", 
                 "temp_cold_mod", 
                 "temp_eurytherm"))
Trait_Noa[, temp_warm := apply(.SD, 1, max),
          .SDcols = c("Thermal_pref_Warm eurythermal (15-30 C)",
                      "Thermal_pref_Hot euthermal (>30 C)")]
Trait_Noa[, c("Thermal_pref_Warm eurythermal (15-30 C)",
              "Thermal_pref_Hot euthermal (>30 C)") := NULL]


# change some colnames 
setnames(Trait_Noa, 
         old = c("Species", "Genus", "Family", "Order"), 
         new = c("species", "genus", "family", "order"))

# save
saveRDS(object = Trait_Noa, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp_harmonized.rds")
)
