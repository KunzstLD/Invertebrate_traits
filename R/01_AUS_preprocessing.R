# _________________________________________________________________________
#### Preprocessing AUS Traits ####
# TODO add a more detailed description 
# _________________________________________________________________________

# load raw Trait database
Trait_AUS <-
  fread(file.path(
    data_in,
    "Australia",
    "Australian_macroinv_trait_database_final.csv"
  ))

# delete entries with "Adult"
Trait_AUS <- Trait_AUS[!grepl("(?i)Adult", Species),]

# remove reference columns
Trait_AUS[, grep("(?i)ref.+", names(Trait_AUS), value = TRUE) := NULL]

# subset with comments
Comments_AUS <-
  Trait_AUS[, .SD, .SDcols = names(Trait_AUS) %like% "Comment|unique_ID"]
Trait_AUS[, grep("(?i)comment.+", names(Trait_AUS), value = TRUE) := NULL]

# feeding mode: adjust col names Marchant
setnames(
  Trait_AUS,
  old = grep("Marchant", names(Trait_AUS), value = TRUE),
  new = paste0("feeding_", grep("Marchant", names(Trait_AUS), value = TRUE))
)

# Change names for Maxwell
# Botwe for feeding mode
setnames(
  Trait_AUS,
  old = c(
    "Trop1_botwe",
    "Trop2_botwe",
    "Trop3_botwe",
    "Trop4_botwe",
    "Trop5_botwe",
    "C_Maxwell",
    "P_Maxwell",
    "SH_Maxwell",
    "C_SH_Maxwell",
    "SC_Maxwell",
    "PA_Maxwell",
    "C_SC_Maxwell",
    "F_Maxwell"
  ),
  new = c(
    "Trop_collector_gatherer_botwe",
    "Trop_collector_filterer_botwe",
    "Trop_scraper_botwe",
    "Trop_predator_botwe",
    "Trop_shredder_detritivore_botwe",
    "feeding_collector_maxwell",
    "feeding_predator_maxwell",
    "feeding_shredder_maxwell",
    "feeding_collec_shredder_maxwell",
    "feeding_scraper_maxwell",
    "feeding_parasite_maxwell",
    "feeding_collector_scraper_maxwell",
    "feeding_filterer_maxwell"
  ),
  skip_absent = TRUE
)

# subset to relevant traits:
# body form
# ph
# feeding mode
# locomotion
# respiration
# dispersal
# stage
# voltinism
# reproduction/oviposition
# temp
# size
Trait_AUS <- Trait_AUS[, c(
  "unique_ID",
  "Species",
  "Genus",
  "Family",
  "Order",
  grep("(?i)body.*form", names(Trait_AUS), value = TRUE),
  grep("^pH_minimum", names(Trait_AUS), value = TRUE),
  grep(
    "(?i)(?=.*feed)(?!.*assessment)(?!.*changes)|trop",
    names(Trait_AUS),
    value = TRUE,
    perl = TRUE
  ),
  grep("(?i)habi|attach", names(Trait_AUS), value = TRUE),
  grep(
    "(?i)(?=.*resp)(?!.*assessment)|(?=.*aquatic\\_stages)(?!.*assessment)",
    names(Trait_AUS),
    perl = TRUE,
    value = TRUE
  ),
  # maybe use dispersal at some later stage
  # grep(
  #   "(?i)(?=.*disp)(?!.*assessment)(?!.*changes)",
  #   names(Trait_AUS),
  #   value = TRUE,
  #   perl = TRUE
  # ),
  grep("(?i)^aquatic", names(Trait_AUS), value = TRUE),
  grep("(?i)volt|generation", names(Trait_AUS), value = TRUE),
  grep(
    "(?i)(?=.*rep)(?!.*assessment)(?!.*changes)(?!.*time)",
    names(Trait_AUS),
    value = TRUE,
    perl = TRUE
  ),
  grep(
    "(?i)ther[0-9]{1,}|therm",
    names(Trait_AUS),
    value = TRUE,
    perl = TRUE
  ),
  grep(
    "(?i)(?=.*size)(?!.*assessment)|(?=.*length)(?!.*assessment)",
    names(Trait_AUS),
    value = TRUE,
    perl = TRUE
  )
)
, with = FALSE]

# ________________________________________________________________________
#### Trait preprocessing categorical traits ####
# Trait_AUS[, lapply(.SD, max, na.rm =TRUE),
#           .SDcols = -c("unique_id", "Species", "Genus", "Family", "Order")]
# Each trait category is transformed into a column with an indication
# of present or not present (one or zero/ 100 % or 0 %)
# ________________________________________________________________________

# ________________________________________________________________________
##### Respiration ####
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_spi: spiracle
# resp_pls: plastron
# resp_atm: atmospheric breathers
# ________________________________________________________________________

# Respiration bugs gbr
Trait_AUS[, `:=`(
  resp_teg = ifelse(grepl("Cutaneous", Respiration_bugs_gbr), 1, 0),
  resp_gil = ifelse(grepl("Gills", Respiration_bugs_gbr), 1, 0),
  resp_pls = ifelse(grepl("Plastron", Respiration_bugs_gbr), 1, 0),
  resp_atm = ifelse(grepl("Air\\-breathing", Respiration_bugs_gbr), 1, 0)
)]
Trait_AUS[, Respiration_bugs_gbr := NULL]

# Respiration Schaefer
# unique(Trait_AUS$Respiration_Schaefer)
Trait_AUS[, `:=`(
  resp_teg_Schaefer = ifelse(grepl("Cutaneous" , Respiration_Schaefer) , 1, 0),
  resp_gil_Schaefer = ifelse(grepl("Gills", Respiration_Schaefer), 1, 0),
  resp_pls_Schaefer = ifelse(grepl("Plastron", Respiration_Schaefer), 1, 0),
  resp_atm_Schaefer = ifelse(grepl("Air\\-breathing", Respiration_Schaefer), 1, 0)
)]
Trait_AUS[, Respiration_Schaefer := NULL]

# ________________________________________________________________________
#### Reproduction/Oviposition ####
# ________________________________________________________________________

# Reproduction_type_bugs_gbr
# unique(Trait_AUS$Reproduction_type_bugs_gbr)
Trait_AUS[, `:=`(
  ovip_aqu = ifelse(
    grepl(
      "(?i)aquatic eggs$|shallow|stones$|algae|adults|plants|substrate|free",
      Reproduction_type_bugs_gbr,
      perl = TRUE
    ),
    1,
    0
  ),
  ovip_ter = ifelse(
    grepl(
      "(?i)(?=.*terrestrial)(?!.*some)(?!.*almost)",
      Reproduction_type_bugs_gbr,
      perl = TRUE
    ),
    1,
    0
  ),
  ovip_ovo = ifelse(
    grepl("(?i)(?=.*ovo)(?!.*or)",
          Reproduction_type_bugs_gbr, perl = TRUE),
    1,
    0
  )
)]
Trait_AUS[, Reproduction_type_bugs_gbr := NULL]

# Reproduction_type_Schaefer
# no ovip_ter, since only description ambiguous (some taxa terrestrial eggs)
# unique(Trait_AUS$Reproduction_type_Schaefer)
Trait_AUS[, `:=`(
  ovip_aqu_Schaefer = ifelse(
    grepl(
      "(?i)aquatic|plants|substrate|fre",
      Reproduction_type_Schaefer
    ),
    1,
    0
  ),
  ovip_ovo_Schaefer = ifelse(grepl("(?i)ovo", Reproduction_type_Schaefer), 1, 0)
)]
Trait_AUS[, Reproduction_type_Schaefer := NULL]

# rm Changes_time_until_repro_bugs_gbr column
Trait_AUS[, "Changes_time_until_repro_bugs_gbr" := NULL]

# _________________________________________________________________________
#### Feeding mode ####
# feed_shredder: shredder (chewers, miners, xylophagus, herbivore piercers)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_scraper: scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# _________________________________________________________________________
Trait_AUS[, `:=`(
  feed_shredder = ifelse(
    grepl("(?i)herbivores|detritivores$",
          Feeding_group_bugs_gbr),
    1,
    0
  ),
  feed_predator = ifelse(
    grepl("Predator|larvea predators", Feeding_group_bugs_gbr),
    1,
    0
  ),
  feed_parasite = ifelse(grepl("^Parasite$", Feeding_group_bugs_gbr), 1, 0)
)]
Trait_AUS[, Feeding_group_bugs_gbr := NULL]

# feeding groups from Schaefer
Trait_AUS[, `:=`(
  feed_shredder_Schaefer = ifelse(
    grepl(
      "(?i)herbivores|detritivores$|detritivores and herbivores",
      Feeding_group_Schaefer
    ),
    1,
    0
  ),
  feed_predator_Schaefer = ifelse(grepl("Predator", Feeding_group_Schaefer), 1, 0),
  feed_parasite_Schaefer = ifelse(grepl("^Parasite$", Feeding_group_Schaefer), 1, 0)
)]
Trait_AUS[, Feeding_group_Schaefer := NULL]

# _________________________________________________________________________
#### Classify continuous traits ####
# Max body size number & Max body text carry the same information
# _________________________________________________________________________

# _________________________________________________________________________
#### Size ####
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________

# Max_body_size_mm_gbr
Trait_AUS[, `:=`(
  size_small = ifelse(Max_body_size_mm__number_bugs_gbr < 9, 1, 0),
  size_medium = ifelse(
    Max_body_size_mm__number_bugs_gbr >= 9 &
      Max_body_size_mm__number_bugs_gbr <= 16,
    1,
    0
  ),
  size_large = ifelse(Max_body_size_mm__number_bugs_gbr > 16, 1, 0)
)]

# Max body size Schaefer
Trait_AUS[, `:=`(
  size_small_Schaefer = ifelse(Max_body_size_mm__number_Schaefer < 9, 1, 0),
  size_medium_Schaefer = ifelse(
    Max_body_size_mm__number_Schaefer >= 9 &
      Max_body_size_mm__number_Schaefer <= 16,
    1,
    0
  ),
  size_large_Schaefer = ifelse(Max_body_size_mm__number_Schaefer > 16, 1, 0)
)]

# Max length Chessman
Trait_AUS[, `:=`(
  size_small_genus_Chessman = ifelse(Maximum_length_mm_genus_Chessman2017 < 9,
                                     1, 0),
  size_medium_genus_Chessman = ifelse(
    Maximum_length_mm_genus_Chessman2017 >= 9 &
      Maximum_length_mm_genus_Chessman2017 <= 16  ,
    1,
    0
  ),
  size_large_genus_Chessman = ifelse(Maximum_length_mm_genus_Chessman2017 > 16,
                                     1, 0)
)]
Trait_AUS[, `:=`(
  size_small_fam_Chessman = ifelse(Maximum_length_mm_fam_Chessman2017 < 9,
                                   1, 0),
  size_medium_fam_Chessman = ifelse(
    Maximum_length_mm_fam_Chessman2017 >= 9 &
      Maximum_length_mm_fam_Chessman2017 <= 16  ,
    1,
    0
  ),
  size_large_fam_Chessman = ifelse(Maximum_length_mm_fam_Chessman2017 > 16,
                                   1, 0)
)]

# del
Trait_AUS[, c(
  "Max_body_size_mm__number_Schaefer",
  "Max_body_size_mm__number_bugs_gbr",
  "Maximum_length_mm_genus_Chessman2017",
  "Maximum_length_mm_fam_Chessman2017"
) := NULL]

# _________________________________________________________________________
#### PH ####
# just minimum ph -> highest value is 6.61
# _________________________________________________________________________
Trait_AUS[, pH_minimum_fam_Chessman2017 := as.numeric(pH_minimum_fam_Chessman2017)]
Trait_AUS[, ph_acidic := ifelse(pH_minimum_fam_Chessman2017 < 7, 1, 0)]
Trait_AUS[, pH_minimum_fam_Chessman2017 := NULL]

# _________________________________________________________________________
#### Temperature ####
# temp very cold (EU < 6 °C, NoA < 5 °C)
# temp cold (EU < 10 °C) + temp moderate (EU < 18 °C) (NoA 0-15)
# temp warm (EU >= 18 °C, NoA >15)
# temp eurytherm (no pref)
# _________________________________________________________________________
Trait_AUS[, `:=`(
  temp_very_cold = ifelse(Thermophily_fam_Chessman2017 < 6, 1, 0),
  temp_cold_mod = ifelse(
    Thermophily_fam_Chessman2017 >= 6 &
      Thermophily_fam_Chessman2017 < 18,
    1,
    0
  ),
  temp_warm = ifelse(Thermophily_fam_Chessman2017 >= 18, 1, 0)
)]
Trait_AUS[, Thermophily_fam_Chessman2017 := NULL]

# _________________________________________________________________________
#### Data Preparation & Taxonomical corrections ####
# _________________________________________________________________________

# rm columns with maximum value zero
rm_col <- Trait_AUS[, lapply(.SD, max, na.rm = TRUE)] %>%
  lapply(., function(y)
    y[y == 0]) %>%
  unlist() %>%
  names()
Trait_AUS[, (rm_col) := NULL]

# transform all integer col to numeric
col_names <-
  names(Trait_AUS[, unlist(lapply(Trait_AUS, is.integer)), with = FALSE])
Trait_AUS[, (col_names) := lapply(.SD, as.numeric), .SDcols = col_names]

# no duplicates
# fetch_dupl(data = Trait_AUS, col = "Species")

# set all NAs to zeros
col_names <- names(Trait_AUS)[6:length(names(Trait_AUS))]
for (j in col_names) {
  data.table::set(Trait_AUS, which(is.na(Trait_AUS[[j]])), j, 0)
}

# Polychaeta is actually a class
Trait_AUS[grepl("Aeolosomatidae", Family), Order := "Aeolosomatida"]
Trait_AUS[grepl("Capitellidae", Family), Order := "Capitellida"]

# Diplostraca is actually an infraclass of Branchiopoda
Trait_AUS[grepl("Cyzicidae", Family), Order := "Spinicaudata"]
Trait_AUS[grepl("Lynceidae", Family), Order := "Onychura"]

# Hirudinida is a subclass
Trait_AUS[grepl("Erpobdellidae|Hirudinidae", Family), Order := "Arhynchobdellida"]
Trait_AUS[grepl("Glossiphoniidae", Family), Order := "Rhynchobdellida"]

# Phyllodocida is actually a suborder
Trait_AUS[grepl("Nereididae|Syllidae", Family), Order := "Aciculata"]

# Gastropoda is actually a class
Trait_AUS[grepl("Thiaridae", Family), Order := "Sorbeoconcha"]

# "Veneroida" -> old way of writing, now Venerida
Trait_AUS[grepl("Veneroida", Order), Order := "Venerida"]
# actually there is a new classification for this family
Trait_AUS[Family %in% "Sphaeriidae", Order := "Venerida"]
Trait_AUS[Family %in% "Cyrenidae", Order := "Venerida"]

# few orders contain NA values
Trait_AUS[Family %in% "Enchytraeidae", Order := "Haplotaxida"]
Trait_AUS[Family %in% "Hydracarina", Order := "Trombidiformes"]

# Mesostigmata is actually an order
Trait_AUS[grepl("Mesostigmata", Family), `:=`(Genus = NA,
                                              Family = NA,
                                              Order = "Mesostigmata")]

# Family NULL?
Trait_AUS[grepl("NULL", Family), Family := NA]

# "Naididae", "Hydrobiidae", "Hydridae" assigned to different orders
Trait_AUS[Family == "Naididae", Order := "Haplotaxida"]
Trait_AUS[Family == "Hydrobiidae", Order := "Littorinimorpha"]
Trait_AUS[Family == "Hydridae", Order := "Anthoathecata"]

# Temnocephalidae
Trait_AUS[Family == "Temnocephalidae", Order := "Rhabdocoela"]

# one entry without genus and family information
Trait_AUS[Species == "Cheumatopsyche deani",
          `:=`(Genus = "Cheumatopsyche",
               Family = "Hydropsychidae",
               Order = "Trichoptera")]

# rm entries with taxonomical resolution higher than Family
Trait_AUS <-
  Trait_AUS[!(is.na(Species) & is.na(Genus) & is.na(Family)),]

# ____________________________________________________________________
#### Normalization ####
# In order to harmonize the trait states from various
# authors (e.g VicEPA, Schaefer, Botwe,...) they must have the same range
# Hence, values are normalized to a range of [0 - 1]
# ____________________________________________________________________
trait_author_pattern <- c(
  "feeding.*Marchant",
  "Feeding.*VicEPA",
  "feeding.*fam.*Chessman2017",
  "Trop.*botwe",
  "feeding.*maxwell",
  "feeding.*genus.*Chessman2017",
  "Attach.*VicEPA",
  "Habi.*botwe",
  "Respiration.*VicEPA",
  "aquatic.*stages.*fam.*Chessman2017",
  "Resp.*botwe",
  "resp.*Maxwell",
  "aquatic.*stages.*genus.*Chessman2017",
  "Aquatic.*VicEPA",
  "Voltinism.*VicEPA",
  "Volt.*botwe",
  "volt.*Maxwell",
  "Reproduction.*VicEPA",
  "Rep.*botwe",
  "repro.*Maxwell",
  "Ther.*botwe",
  "Max.*size.*VicEPA",
  "Max.*size.*text",
  "Size.*botwe",
  "^resp\\_",
  "resp.*Schaefer",
  "More.*Schaefer|Two.*Schaefer|One.*Schaefer|Less.*Schaefer",
  "More.*gbr|Two.*gbr|One.*gbr|Less.*gbr",
  "^ovip\\_",
  "^feed\\_",
  "feed.*Schaefer",
  "^size\\_",
  "size.*Schaefer",
  "size.*genus.*Chessman",
  "size.*fam.*Chessman",
  "^ph\\_",
  "^temp\\_",
  "Body.*form.*VicEPA"
)

for(i in trait_author_pattern) {
  Trait_AUS[, tmp_sum := apply(.SD, 1, sum), .SDcols = patterns(i)]
  col_name <- names(Trait_AUS)[names(Trait_AUS) %like% i]
  Trait_AUS[, (col_name) := lapply(.SD, function(y)
    y / tmp_sum), .SDcols = patterns(i)]
}
Trait_AUS[tmp_sum := NULL]

# change column names from Chessman that contain word "genus"
setnames(
  Trait_AUS,
  old = grep("genus.*Chessman", names(Trait_AUS), value = TRUE),
  new = grep("genus.*Chessman", names(Trait_AUS), value = TRUE) %>%
    sub("genus", "gen", .)
)

#________________________________________________________________________
#### Handle duplicates ####
#________________________________________________________________________

# trait cols
cols <- grep(
  "(?i)unique_id|species|genus|family|order",
  names(Trait_AUS),
  value = TRUE,
  invert = TRUE
)

# no duplicate entries in Species column
# Trait_AUS[!is.na(Species), ] %>%
#   .[duplicated(Species),]

# genus column:
# inspecting duplicates on genus col:
# Trait_AUS[is.na(Species) & !is.na(Genus), ] %>%
#   .[duplicated(Genus), ]
# Trait_AUS[is.na(Species) & !is.na(Genus), ] %>%
#    .[Genus %in% "Mirawara", ] %>%
# #   .[duplicated(Genus) | duplicated(Genus, fromLast = TRUE), ] %>%
#   melt(., id.vars = c("unique_id", "Species", "Genus", "Family", "Order")) %>%
#    .[, .(diff(value)), by = c("Genus", "variable")] %>%
#    .[V1 != 0, ]

# merging of duplicate genera using the median
Trait_AUS[is.na(Species) & !is.na(Genus),
          (cols) := lapply(.SD, median),
          .SDcols = cols,
          by = "Genus"]

# rm duplicate genus entries
ids <- Trait_AUS[is.na(Species) & !is.na(Genus),] %>%
  .[duplicated(Genus), unique_ID]
Trait_AUS <- Trait_AUS[!unique_ID %in% ids,]

# family column:
Trait_AUS[is.na(Species) & is.na(Genus) & !is.na(Family),
          (cols) := lapply(.SD, median),
          .SDcols = cols,
          by = "Family"]

# rm duplicate family entries
ids <-
  Trait_AUS[is.na(Species) & is.na(Genus) & !is.na(Family),] %>%
  .[duplicated(Family), unique_ID]
Trait_AUS <- Trait_AUS[!unique_ID %in% ids,]

# save
saveRDS(
  object = Trait_AUS,
  file = file.path(data_cleaned,
                   "Australia",
                   "Trait_AUS_preproc.rds")
)
