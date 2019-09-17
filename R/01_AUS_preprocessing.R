
# ------------------------------------------------------------------------- 
#### Preprocessing AUS Traits ####
# -------------------------------------------------------------------------
# TODO add a more detailed description 

# load raw Trait database
Trait_AUS <-
  fread(file.path(data_in,
                  "Australia",
                  "Australian macroinv trait database 17 05.csv"))

# delete entries with "Adult"
Trait_AUS <- Trait_AUS[!grepl("(?i)Adult", Species), ]

# Add ID col as unique identifier
Trait_AUS[, unique_id := 1:nrow(Trait_AUS)]

# remove reference columns
Trait_AUS[, grep("(?i)ref.+", names(Trait_AUS), value = TRUE) := NULL]

# subset with comments
Comments_AUS <- Trait_AUS[, .SD, .SDcols = names(Trait_AUS) %like% "Comment|unique_id"]
Trait_AUS[, grep("(?i)comment.+", names(Trait_AUS), value = TRUE) := NULL]

# subset to relevant traits
# ph pref
# feeding mode - adjust col names Marchant
setnames(
  Trait_AUS,
  old = grep("Marchant", names(Trait_AUS), value = TRUE),
  new = paste0("feeding_", grep("Marchant", names(Trait_AUS), value = TRUE))
)
# adjust col names Maxwell
setnames(
  Trait_AUS,
  old = c(
    "C_Maxwell",
    "P_Maxwell",
    "SH_Maxwell",
    "C_SH_Maxwell",
    "SC_Maxwell",
    "PA_Maxwell",
    "C_SC_Maxwell",
    "F_Maxwell"
  ),
  new = paste0(
    "feeding_",
    c(
      "C_Maxwell",
      "P_Maxwell",
      "SH_Maxwell",
      "C_SH_Maxwell",
      "SC_Maxwell",
      "PA_Maxwell",
      "C_SC_Maxwell",
      "F_Maxwell"
    )
  )
)


# TODO incorporate Body form as trait!
# grep("(?i)body|shape|form", names(Trait_AUS), value = TRUE)

# subset to relevant traits
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
  "unique_id",
  "Species",
  "Genus",
  "Family",
  "Order",
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
  grep(
    "(?i)(?=.*disp)(?!.*assessment)(?!.*changes)",
    names(Trait_AUS),
    value = TRUE,
    perl = TRUE
  ),
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

# Dispersal columns disregarded for now
Trait_AUS <- Trait_AUS[, .SD, .SDcols = !names(Trait_AUS) %like% "(?i)disp"]

# change names for Maxwell & Botwe for feeding mode
setnames(
  Trait_AUS,
  old = c(
    "Trop1_botwe",
    "Trop2_botwe",
    "Trop3_botwe",
    "Trop4_botwe",
    "Trop5_botwe", 
    "feeding_C_Maxwell",                                
    "feeding_P_Maxwell",                                
    "feeding_SH_Maxwell",                               
    "feeding_C_SH_Maxwell",                             
    "feeding_SC_Maxwell",                               
    "feeding_PA_Maxwell",                              
    "feeding_C_SC_Maxwell",                             
    "feeding_F_Maxwell" 
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

# Trait preprocessing categorical traits ---------------------------------

# Trait_AUS[, lapply(.SD, max, na.rm =TRUE),
#           .SDcols = -c("unique_id", "Species", "Genus", "Family", "Order")]
# Respiration
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_spi: spiracle
# resp_pls: plastron
# resp_atm: atmospheric breathers 

# Respiration bugs gbr
Trait_AUS[, `:=`(
  resp_teg = ifelse(grepl("Cutaneous" ,Respiration_bugs_gbr) , 1, 0),
  resp_gil = ifelse(grepl("Gills", Respiration_bugs_gbr), 1, 0),
  resp_pls = ifelse(grepl("Plastron", Respiration_bugs_gbr), 1, 0),
  resp_atm = ifelse(grepl("Air\\-breathing", Respiration_bugs_gbr), 1, 0)
)]
Trait_AUS[, Respiration_bugs_gbr := NULL]

# Respiration Shafer
# unique(Trait_AUS$Respiration_Shafer)
Trait_AUS[, `:=`(
  resp_teg_Shafer = ifelse(grepl("Cutaneous" ,Respiration_Shafer) , 1, 0),
  resp_gil_Shafer = ifelse(grepl("Gills", Respiration_Shafer), 1, 0),
  resp_pls_Shafer = ifelse(grepl("Plastron", Respiration_Shafer), 1, 0),
  resp_atm_Shafer = ifelse(grepl("Air\\-breathing", Respiration_Shafer), 1, 0)
)]
Trait_AUS[, Respiration_Shafer := NULL]

# Number_of_generations_per_year_bugs_gbr
# only categories used that are useful & not ambiguous
# volt_semi
Trait_AUS[, `:=`(
  volt_semi = ifelse(grepl("0\\.25|0\\.5$", Number_of_generations_per_year_bugs_gbr), 1, 0),
  volt_uni = ifelse(grepl("^1$|<\\=1|0.5\\-1", Number_of_generations_per_year_bugs_gbr), 1, 0),
  volt_bi_multi = ifelse(grepl("2-3|^2|>2|>1$|^3|2-10", Number_of_generations_per_year_bugs_gbr), 1, 0)
)]
Trait_AUS[, Number_of_generations_per_year_bugs_gbr := NULL]

# Number_of_generations_per_year_Shafer
Trait_AUS[, `:=`(
  volt_uni_Shafer = ifelse(grepl("^1$|0.5\\-1", Number_of_generations_per_year_Shafer), 1, 0),
  volt_bi_multi_Shafer = ifelse(grepl("2-3|^2|2-10", Number_of_generations_per_year_Shafer), 1, 0)
)]
Trait_AUS[, Number_of_generations_per_year_Shafer := NULL]

# Reproduction_type_bugs_gbr
Trait_AUS[, `:=`(
  ovip_aqu = ifelse(
    grepl(
      "(?i)aquatic eggs$|shallow|stones$|algae|adults|plants|substrate|free",
      Reproduction_type_Shafer,
      perl = TRUE
    ),
    1,
    0
  ),
  ovip_ter = ifelse(
    grepl(
      "(?i)(?=.*terrestrial)(?!.*some)(?!.*almost)",
      Reproduction_type_Shafer,
      perl = TRUE
    ),
    1,
    0
  ),
  ovip_ovo = ifelse(
    grepl("(?i)(?=.*ovo)(?!.*or)",
          Reproduction_type_Shafer, perl = TRUE),
    1,
    0
  )
)]
Trait_AUS[, Reproduction_type_bugs_gbr := NULL]

# Reproduction_type_Shafer
# no ovip_ter, since only description ambiguous (some taxa terrestrial eggs)
# unique(Trait_AUS$Reproduction_type_Shafer)
Trait_AUS[, `:=`(
  ovip_aqu_Shafer = ifelse(grepl("(?i)aquatic|plants|substrate|fre", 
                                 Reproduction_type_Shafer), 1, 0),
  ovip_ovo_Shafer = ifelse(grepl("(?i)ovo", Reproduction_type_Shafer), 1, 0))]
Trait_AUS[, Reproduction_type_Shafer := NULL]

# Feed mode bugs gbr
# feed_shredder: shredder (chewers, miners, xylophagus, herbivore piercers)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_scraper: scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
Trait_AUS[, `:=`(
  feed_shredder = ifelse(grepl("(?i)herbivores|detritivores$", 
                               Feeding_group_bugs_gbr), 1, 0),
  feed_predator = ifelse(grepl("Predator|larvea predators", Feeding_group_bugs_gbr), 1, 0), 
  feed_parasite = ifelse(grepl("^Parasite$", Feeding_group_bugs_gbr), 1, 0)
)]
Trait_AUS[, Feeding_group_bugs_gbr := NULL]

# feeding groups from Shafer
Trait_AUS[, `:=`(
  feed_shredder_Shafer = ifelse(grepl("(?i)herbivores|detritivores$|detritivores and herbivores", 
                                      Feeding_group_Shafer), 1, 0),
  feed_predator_Shafer = ifelse(grepl("Predator", Feeding_group_Shafer), 1, 0), 
  feed_parasite_Shafer = ifelse(grepl("^Parasite$", Feeding_group_Shafer), 1, 0)
)]
Trait_AUS[, Feeding_group_Shafer := NULL]

# Classify continuous traits 
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# Max body size number & Max body text carry the same information  

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

# Max body size Schäfer
Trait_AUS[, `:=`(
  size_small_Shafer = ifelse(Max_body_size_mm__number_Shafer < 9, 1, 0),
  size_medium_Shafer = ifelse(
    Max_body_size_mm__number_Shafer >= 9 &
      Max_body_size_mm__number_Shafer <= 16,
    1,
    0
  ),
  size_large_Shafer = ifelse(Max_body_size_mm__number_Shafer > 16, 1, 0)
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
  "Max_body_size_mm__number_Shafer",
  "Max_body_size_mm__text_ref",
  "Max_body_size_mm__number_bugs_gbr",
  "Max_body_size_mm__text_bugs_gbr",
  "Maximum_length_mm_genus_Chessman2017",
  "Maximum_length_mm_fam_Chessman2017"
) := NULL]

#### PH ####
# just minimum ph -> highest value is 6.61
Trait_AUS[, pH_minimum_fam_Chessman2017 := as.numeric(pH_minimum_fam_Chessman2017)]
Trait_AUS[, ph_acidic := ifelse(pH_minimum_fam_Chessman2017<7, 1, 0)]
Trait_AUS[, pH_minimum_fam_Chessman2017 := NULL]

#### Temperature #### 
# temp very cold (EU < 6 °C, NoA < 5 °C)
# temp cold (EU < 10 °C) + temp moderate (EU < 18 °C) (NoA 0-15)
# temp warm (EU >= 18 °C, NoA >15)
# temp eurytherm (no pref)
Trait_AUS[, `:=`(temp_very_cold = ifelse(Thermophily_fam_Chessman2017 < 6, 1, 0), 
                 temp_cold_mod = ifelse(Thermophily_fam_Chessman2017 >= 6 & 
                                          Thermophily_fam_Chessman2017 < 18, 1, 0), 
                 temp_warm = ifelse(Thermophily_fam_Chessman2017 >= 18, 1, 0))]
Trait_AUS[, Thermophily_fam_Chessman2017 := NULL]

# TODO check warnings and improve code!
# rm columns with maximum zero (a bit hacky atm)
rm_col <- names(Trait_AUS[, lapply(.SD, function(y) {
  y <- max(y, na.rm = TRUE)
  y[y == 0]
}),
.SDcols = -c("unique_id", "Species", "Genus", "Family", "Order")])
Trait_AUS[, (rm_col) := NULL]

# transform all integer col to numeric  
col_names <- names(Trait_AUS[, unlist(lapply(Trait_AUS, is.integer)), with = FALSE])
Trait_AUS[, (col_names) := lapply(.SD, as.numeric), .SDcols = col_names]

# ------------------------------------------------------------------------
#### Handle duplicates & data cleaning ####

# rm entries with taxonomical resolution higher than Family
Trait_AUS <- Trait_AUS[!(is.na(Species) & is.na(Genus) & is.na(Family)), ]

# handle duplicates
Dupl_AST <- fetch_dupl(data = Trait_AUS, col = "Species")

# data that does not contain duplicates
Trait_AUS <- Trait_AUS[!unique_id %in% Dupl_AST$unique_id]

# TODO check warning
# condense rowwise information for duplicates 
condense <-
  Dupl_AST[, lapply(.SD, max, na.rm = TRUE), 
           .SDcols = -c("unique_id", "Species", "Genus", "Family", "Order"),
           by = Species]
# Warning: infinte data entries created(from condense of c(NA,NA))
for(j in names(condense)){
  data.table::set(condense, which(is.infinite(condense[[j]])), j, 0)
}

# merge taxonomical information back
condense[Dupl_AST,
         `:=`(Genus = i.Genus,
              Family = i.Family,
              Order = i.Order,
              unique_id = i.unique_id),
         on = "Species"]

# rbind to dataset without duplicates
Trait_AUS <- rbind(Trait_AUS, condense)

# set all NAs to zeros
col_names <- names(Trait_AUS)[6:length(names(Trait_AUS))]
for(j in col_names){
  data.table::set(Trait_AUS, which(is.na(Trait_AUS[[j]])), j, 0)
}

#### Taxonomical corrections ####

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

# save
saveRDS(
  object = Trait_AUS,
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_preproc.rds"
  )
)
