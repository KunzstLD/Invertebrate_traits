# _________________________________________________________________________ 
#### Harmonize NOA Traits ####
# Includes also normalization
# _________________________________________________________________________

# read in
Trait_Noa <- readRDS(file = file.path(data_cleaned, 
                                      "North_America",
                                      "Traits_US_pp.rds"))

# convert integer cols to double
int_cols <- names(Filter(is.integer, Trait_Noa))
Trait_Noa[, (int_cols) := lapply(.SD, as.double), .SDcols = int_cols]

# _________________________________________________________________________
#### Voltinism ####
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________
setnames(
  Trait_Noa,
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
  Trait_Noa,
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
  Trait_Noa,
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
Trait_Noa[, feed_herbivore := apply(.SD, 1. , max),
          .SDcols = c("Feed_mode_prim_Scraper/grazer",
                      "Feed_mode_prim_Piercer herbivore")]
Trait_Noa[, c("Feed_mode_prim_Scraper/grazer",
              "Feed_mode_prim_Piercer herbivore") := NULL]

# incorporating information from comments 
# load raw NoA data
Trait_Noa_raw <- readRDS(file = file.path(data_in, "North_America", "Inverttraitstable_raw.rds"))
Trait_Noa_raw[, unique_id := 1:nrow(Trait_Noa_raw)]

# taxa with feeding mode "other"
Taxa_feed_other <-
  Trait_Noa[`Feed_mode_prim_Other (specify in comments)` == 1 &
              feed_herbivore == 0 & feed_filter == 0 &
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

# Add from final preprocessed EU data
Trait_EU <-
  readRDS(file = file.path(data_cleaned, "EU", "Trait_freshecol_2020_pp_harmonized.rds"))
taxa_other_feed_mode <-
  Trait_Noa[`Feed_mode_prim_Other (specify in comments)` > 0,
            .(Species,
              Genus,
              Family,
              Order)]
taxa_other_feed_mode[, taxa := coalesce(Species, Genus, Family, Order)]

# 98 species with feed_mode other
# of those 10 species are covered in EU DB
# taxa_other_feed_mode[!is.na(Species), ]
# Trait_EU[species %in% taxa_other_feed_mode[!is.na(Species), Species], ]
Trait_Noa[Trait_EU[species %in% taxa_other_feed_mode$taxa,
                   .(
                     species,
                     genus,
                     family,
                     order,
                     feed_filter,
                     feed_gatherer,
                     feed_predator,
                     feed_parasite,
                     feed_shredder,
                     feed_herbivore
                   )],
          `:=`(
            feed_filter = i.feed_filter,
            feed_gatherer = i.feed_gatherer,
            feed_predator = i.feed_predator,
            feed_parasite = i.feed_parasite,
            feed_shredder = i.feed_shredder,
            feed_herbivore = i.feed_herbivore,
            `Feed_mode_prim_Other (specify in comments)` = 0
          ),
          on = c(Species = "species")]

# create taxa column for Trait_Noa
Trait_Noa[, Taxa := coalesce(Species, Genus, Family, Order)]

# 58 taxa on genus-level with feed_mode other
# Of these, 11 genera covered in EU data (data on genus level)
taxa_other_feed_mode[is.na(Species) & !is.na(Genus), ]
Trait_EU[is.na(species) & genus %in% taxa_other_feed_mode[is.na(Species) & !is.na(Genus), Genus], ] %>% 
  .[, unique(genus)] 

Trait_Noa[Trait_EU[is.na(species) &
                     genus %in% taxa_other_feed_mode$taxa,
                   .(
                     genus,
                     feed_filter,
                     feed_gatherer,
                     feed_predator,
                     feed_parasite,
                     feed_shredder,
                     feed_herbivore
                   )],
          `:=`(
            feed_filter = i.feed_filter,
            feed_gatherer = i.feed_gatherer,
            feed_predator = i.feed_predator,
            feed_parasite = i.feed_parasite,
            feed_shredder = i.feed_shredder,
            feed_herbivore = i.feed_herbivore,
            `Feed_mode_prim_Other (specify in comments)` = 0
          ),
          on = c(Taxa = "genus")]

# 31 taxa on family-level present in NOA other with feed_mode other
# of these, 4 families covered by EU data
taxa_other_feed_mode[is.na(Species) & is.na(Genus), ]
Trait_EU[is.na(species) & is.na(genus) & family %in% taxa_other_feed_mode[, coalesce(Species, Genus, Family, Order)], ] %>% 
  .[, unique(family)]

Trait_Noa[Trait_EU[is.na(species) & is.na(genus) &
                     family %in% taxa_other_feed_mode$taxa,
                   .(
                     family,
                     feed_filter,
                     feed_gatherer,
                     feed_predator,
                     feed_parasite,
                     feed_shredder,
                     feed_herbivore
                   )],
          `:=`(
            feed_filter = i.feed_filter,
            feed_gatherer = i.feed_gatherer,
            feed_predator = i.feed_predator,
            feed_parasite = i.feed_parasite,
            feed_shredder = i.feed_shredder,
            feed_herbivore = i.feed_herbivore,
            `Feed_mode_prim_Other (specify in comments)` = 0
          ),
          on = c(Taxa = "family")]

# A few of the taxa can also be found in the 
# load NOA_new
# TODO: check as well
# load Trait_Noa_new
# taxa_other_feed_mode[is.na(Species) & !is.na(Genus) , Genus] %in% Trait_Noa_new[, unique(genus)]


# del feed mode others
# still 187 taxa
# Trait_Noa[`Feed_mode_prim_Other (specify in comments)` > 0, .SD, .SDcols = names(Trait_Noa) %like% "(?i)Feed|Species|Genus|Family"] %>% View()
Trait_Noa[, `Feed_mode_prim_Other (specify in comments)` := NULL]

# _________________________________________________________________________
#### Locomotion ####
# locom_swim:  swimmer, scater (active & passive)
# locom_crawl: crawlers, walkers & sprawler, climber
# locom_burrow: burrower
# locom_sessil: sessil (attached)
# What to do with clingers? -> According to LeRoy and Chuck they should be put to crawlers
# _________________________________________________________________________
setnames(Trait_Noa, 
         old = c("Habit_prim_Attached/fixed", "Habit_prim_Burrower"), 
         new = c("locom_sessil", "locom_burrow"))
Trait_Noa[, locom_swim := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Swimmer",
                      "Habit_prim_Planktonic",
                      "Habit_prim_Skater")]
Trait_Noa[, locom_crawl := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Sprawler", "Habit_prim_Climber", "Habit_prim_Clinger")]
Trait_Noa[, c(
  "Habit_prim_Swimmer",
  "Habit_prim_Planktonic",
  "Habit_prim_Skater",
  "Habit_prim_Sprawler",
  "Habit_prim_Climber",
  "Habit_prim_Clinger"
) := NULL]

# What to do with habit prim other?
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

# check with EU data
Trait_Noa[`Habit_prim_Other (specify in comments)` > 0,]
taxa_other_locom <-
  Trait_Noa[`Habit_prim_Other (specify in comments)` > 0,
            .(Species,
              Genus,
              Family,
              Order)]
taxa_other_locom[, taxa := coalesce(Species, Genus, Family, Order)]

# 77 species with locom_mode other
# of those 10 species are covered in EU DB
taxa_other_locom[!is.na(Species), ]
Trait_EU[species %in% taxa_other_locom$taxa, ]

Trait_Noa[Trait_EU[species %in% taxa_other_locom$taxa,
                   .(species,
                     genus,
                     family,
                     order,
                     locom_burrow,
                     locom_crawl,
                     locom_swim,
                     locom_sessil)],
          `:=`(
            locom_burrow = i.locom_burrow,
            locom_crawl = i.locom_crawl,
            locom_swim = i.locom_swim,
            locom_sessil = i.locom_sessil,
            `Habit_prim_Other (specify in comments)` = 0
          ),
          on = c(Species = "species")]

# 35 genera with locom_mode other
# 6 taxa on genus-level in EU DB
taxa_other_locom[is.na(Species) & !is.na(Genus), ]
Trait_EU[is.na(species) & genus %in% taxa_other_locom$taxa, ]

Trait_Noa[Trait_EU[is.na(species) & genus %in% taxa_other_locom$taxa,
                   .(genus,
                     family,
                     order,
                     locom_burrow,
                     locom_crawl,
                     locom_swim,
                     locom_sessil)],
          `:=`(
            locom_burrow = i.locom_burrow,
            locom_crawl = i.locom_crawl,
            locom_swim = i.locom_swim,
            locom_sessil = i.locom_sessil,
            `Habit_prim_Other (specify in comments)` = 0
          ),
          on = c(Taxa = "genus")]

# 17 taxa on family-lvl with locom_mode other
# 4 of those taxa on family-lvl present in EU DB
taxa_other_locom[is.na(Species) & is.na(Genus) & !is.na(Family), ]
Trait_EU[is.na(species) & is.na(genus) & family %in% taxa_other_locom$taxa, ]

Trait_Noa[Trait_EU[is.na(species) & is.na(genus) &
                     family %in% taxa_other_locom$taxa,
                   .(family,
                     order,
                     locom_burrow,
                     locom_crawl,
                     locom_swim,
                     locom_sessil)],
          `:=`(
            locom_burrow = i.locom_burrow,
            locom_crawl = i.locom_crawl,
            locom_swim = i.locom_swim,
            locom_sessil = i.locom_sessil,
            `Habit_prim_Other (specify in comments)` = 0
          ),
          on = c(Taxa = "family")]


# del Habit_prim_other & Habit_prim_Clinger
# Still 109 taxa undecided 
# TODO: Think about removal of these
# Trait_Noa[`Habit_prim_Other (specify in comments)` > 0,]
Trait_Noa[, `Habit_prim_Other (specify in comments)` := NULL]


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
# Resp_late_Temporary air store -> Plastron!
# Resp_late_Tracheal gills -> Gills
# _________________________________________________________________________
setnames(Trait_Noa,
         old = c("Resp_late_Cutaneous", 
                 "Resp_late_Tracheal gills"),
         new = c("resp_teg",
                 "resp_gil"))
Trait_Noa[, resp_pls_spi := apply(.SD, 1, max),
          .SDcols = c("Resp_late_Spiracular gills",
                      "Resp_late_Plastron (permanent air store)",
                      "Resp_late_Atmospheric breathers",
                      "Resp_late_Plant breathers", 
                      "Resp_late_Temporary air store")]

# rm hemiglobin taxa
Trait_Noa <- Trait_Noa[!Resp_late_Hemoglobin  > 0, ]

# rm all other former resp traits
Trait_Noa[, c(
  "Resp_late_Atmospheric breathers",
  "Resp_late_Plant breathers",
  "Resp_late_Spiracular gills",
  "Resp_late_Plastron (permanent air store)",
  "Resp_late_Temporary air store",
  "Resp_late_Hemoglobin"
) := NULL]

# respiration comments
# resp_comments <-
#   Trait_Noa[(resp_teg == 0 & resp_pls_spi == 0 &
#                resp_gil == 0) &
#               `Resp_late_Other (specify in comments)` == 1,
#             .(unique_id)]
# Trait_Noa_raw[unique_id %in% resp_comments$unique_id, 
#              .(Resp_comments)]

# del Resp_later_Other
Trait_Noa[, `Resp_late_Other (specify in comments)` := NULL]

# _________________________________________________________________________
#### Drift/dispersal ####
# use disp low, medium, high for comparability 
# del dispersal_unknown
# _________________________________________________________________________
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

# _________________________________________________________________________
#### Size ####
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________
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

# _________________________________________________________________________
#### Oviposition ####
# Modalities
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# _________________________________________________________________________
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

# del Ovipos_other
Trait_Noa[,`Ovipos_behav_prim_Other (specify in comments)` := NULL ]

# _________________________________________________________________________
#### Temperature ####
# temp very cold (EU < 6 °C, NoA < 5 °C)
# temp cold (EU < 10 °C) + temp moderate (EU < 18 °C) (NoA 0-15)
# temp warm (EU >= 18 °C, NoA >15)
# temp eurytherm (no pref)
# _________________________________________________________________________
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

# _________________________________________________________________________
#### Body form ####
# bf_streamlined: streamlined/fusiform
# bf_flattened: flattened (dorso-ventrally)
# bf_cylindrical: cylindrical/tubular 
# bf_spherical: spherical
# _________________________________________________________________________

# transform all integers to numeric to avoid losses of data during merging 
cols_integer <- Filter(is.integer, Trait_Noa) %>% names()
Trait_Noa[, (cols_integer) := lapply(.SD, as.double), .SDcols = cols_integer]

## rm body_shape_case columns
cols <- grep("case", names(Trait_Noa), value = TRUE)
Trait_Noa[, (cols) := NULL]
 
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

# merge bf information on species level
Trait_Noa[blocky_pup[!is.na(species), .(species,
                                        streamlined,
                                        flattened,
                                        cylindrical,
                                        spherical)],
          `:=`(
            streamlined = i.streamlined,
            flattened = i.flattened,
            cylindrical = i.cylindrical,
            spherical = i.spherical
          ),
          on = "species"]
 
# merge bf information on genus level
# needs intermediate step
Trait_Noa_bf_genus <- coalesce_join(
  x = Trait_Noa[is.na(species),],
  y = blocky_pup[is.na(species) &
                   !is.na(genus) & !duplicated(genus), .(genus,
                                                         streamlined,
                                                         flattened,
                                                         cylindrical,
                                                         spherical)],
  by = "genus",
  join = dplyr::left_join
) %>%
  as.data.table(.)

# merge back to whole dataset
Trait_Noa[Trait_Noa_bf_genus,
          `:=`(
            flattened = i.flattened,
            spherical = i.spherical,
            streamlined = i.streamlined,
            cylindrical = i.cylindrical
          ),
          on = "unique_id"]

# merge bf information on family level (one entry)
Trait_Noa_bf_family <- coalesce_join(
  x = Trait_Noa[is.na(species) & is.na(genus),],
  y = blocky_pup[is.na(genus), .(family,
                                 streamlined,
                                 flattened,
                                 cylindrical,
                                 spherical)],
  by = "family",
  join = dplyr::left_join
) %>%
  as.data.table(.)

# merge back to whole dataset
Trait_Noa[Trait_Noa_bf_family,
          `:=`(
            flattened = i.flattened,
            spherical = i.spherical,
            streamlined = i.streamlined,
            cylindrical = i.cylindrical
          ),
          on = "unique_id"]
 
# # interesting taxa where trait assignments deviate for body size:
# # Trait_Noa[`Body_shape_Dorsoventrally flattened` == 1 & 
# #             cylindrical == 1, .(flattened , `Body_shape_Dorsoventrally flattened`,
# #                             cylindrical, Body_shape_Tubular, 
# #                             species, genus, family, order)]
# 
# # find conflict data -> use PUP assignments in these cases 
# Trait_Noa[!is.na(flattened) & `Body_shape_Dorsoventrally flattened` > 0, 
#           `Body_shape_Dorsoventrally flattened` := 0] 
# Trait_Noa[!is.na(spherical) & `Body_shape_Round (humped)` > 0, 
#           `Body_shape_Round (humped)` := 0]
# Trait_Noa[!is.na(streamlined) & `Body_shape_Streamlined / fusiform` > 0, 
#            `Body_shape_Streamlined / fusiform` := 0] 
# Trait_Noa[!is.na(cylindrical) & Body_shape_Tubular > 0, 
#            Body_shape_Tubular := 0] 
# 
# put bf data NOA with PUP assignments together
Trait_Noa[, `:=`(
  bf_flattened = coalesce(flattened , `Body_shape_Dorsoventrally flattened`),
  bf_spherical = coalesce(spherical , `Body_shape_Round (humped)`),
  bf_streamlined = coalesce(streamlined, `Body_shape_Streamlined / fusiform`),
  bf_cylindrical = coalesce(cylindrical, Body_shape_Tubular)
)]

# del other bf columns
Trait_Noa[, c("Body_shape_Bluff (blocky)",
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
Trait_Noa <- normalize_by_rowSum(
  x = Trait_Noa,
  non_trait_cols = c("unique_id",
                     "order",
                     "family",
                     "genus",
                     "species",
                     "Taxa")
)

# check completenes of the trait data
completeness_trait_data(x = Trait_Noa,
                        non_trait_cols = c("unique_id",
                                           "order",
                                           "family",
                                           "genus",
                                           "species",
                                           "Taxa"))
# save
saveRDS(object = Trait_Noa, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_pp_harmonized.rds")
)