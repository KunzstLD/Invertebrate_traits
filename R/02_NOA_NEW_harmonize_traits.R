# _________________________________________________________________________ 
#### Harmonize NOA Traits from Laura T ####
# Includes also merge with old NOA trait DB (based on Vieira et al. 2006)
# _________________________________________________________________________

# read in
Trait_Noa_new <- readRDS(file = file.path(data_cleaned, "North_America", "Traits_US_pp_LauraT.rds"))

# _________________________________________________________________________
#### Feeding mode ####
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite

# Predators are defined as: Engulfers (ingest pref whole or in parts) or
  # as Piercers (piere prey tissues and suck fluids) 
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Feed_prim_CF",
    "Feed_prim_CG",
    "Feed_prim_PA",
    "Feed_prim_PR",
    "Feed_prim_SH",
    "Feed_prim_HB"
  ),
  new = c(
    "feed_filter",
    "feed_gatherer",
    "feed_parasite",
    "feed_predator",
    "feed_shredder",
    "feed_herbivore"
  )
)

# _________________________________________________________________________
#### Locomotion ####
# locom_swim:  swimmer, scater (active & passive)
# locom_crawl: crawlers, walkers & sprawler, climber, clinger
# locom_burrow: burrower
# locom_sessil: sessil (attached)
# What to do with clingers? -> According to LeRoy and Chuck they should be put to crawlers
# _________________________________________________________________________
setnames(Trait_Noa_new, 
         old = c("Habit_prim_Attached/fixed", 
                 "Habit_prim_Burrower"),
         new = c("locom_sessil",
                 "locom_burrow"))
# Swimmer
Trait_Noa_new[, locom_swim := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Swimmer",
                      "Habit_prim_Planktonic",
                      "Habit_prim_Skater")]
# Crawler 
Trait_Noa_new[, locom_crawl := apply(.SD, 1, max),
          .SDcols = c("Habit_prim_Sprawler",
                      "Habit_prim_Climber",
                      "Habit_prim_Clinger")]

# _________________________________________________________________________
#### Size ####
# specifically: Maximum body size
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Max_body_size_Large",
    "Max_body_size_Small",
    "Max_body_size_Medium"
  ),
  new = c("size_large",
          "size_small",
          "size_medium")
)

# _________________________________________________________________________
#### Respiration ####
# resp_teg: tegument
# resp_gil: gills
# resp_pls_spi: spiracle & plastron
  # plastron & spiracle often work together in respiratory systems of aq. insects
  # Present in insects with open tracheal systems -> breathe oxygen from the air
  # -> Different tolerances to low oxygen compared to insects with tegument resp and gills
# _________________________________________________________________________
setnames(Trait_Noa_new, 
         old = c("Resp_Gills", 
                 "Resp_Plastron_spiracle",
                 "Resp_Tegument"),
         new = c("resp_gil", 
                 "resp_pls_spi",
                 "resp_teg"))

# _________________________________________________________________________
#### Voltinism ####
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Volt_Univoltine",
    "Volt_Semivoltine",
    "Volt_Bi_multivoltine"
  ),
  new = c("volt_uni", 
          "volt_semi",
          "volt_bi_multi")
)

# _________________________________________________________________________
#### Normalize Noa New trait data ####
# Normalizing of the trait values to a range of [0 - 1] by
# dividing for a given trait each value for a trait state by the sum of all 
# trait states
# _________________________________________________________________________

# get trait names & create pattern for subset
trait_names_pattern <-
  names(Trait_Noa_new[, -c("unique_id",
                           "family",
                           "genus",
                           "species",
                           "order")]) %>%
  sub("\\_.*|\\..*", "", .) %>%
  unique() %>%
  paste0("^", .)

# loop for normalization (trait categories for each trait sum up to 1) 
for(cols in trait_names_pattern) {
  
  # get row sum for a specific trait
  Trait_Noa_new[, rowSum := apply(.SD, 1, sum),
                .SDcols = names(Trait_Noa_new) %like% cols]
  
  # get column names for assignment
  col_name <- names(Trait_Noa_new)[names(Trait_Noa_new) %like% cols]
  
  Trait_Noa_new[, (col_name) := lapply(.SD, function(y) {
    round(y / rowSum, digits = 2)
  }),
  .SDcols = names(Trait_Noa_new) %like% cols]
}

# del unnecessary columns
Trait_Noa_new[, c("rowSum") := NULL]

# _________________________________________________________________________
#### Complement trait data with old NOA trait data (Vieira et al. 2006) ####
# _________________________________________________________________________

# Load harmoized & normalized Trait NOA
Trait_Noa <- readRDS(file.path(data_cleaned, "North_America", "Traits_US_pp_harmonized.rds"))

# Overlap Old & New NOA DB
Trait_Noa <- Trait_Noa[order %in% c(
  "Coleoptera",
  "Diptera",
  "Ephemeroptera",
  "Hemiptera",
  "Lepidoptera",
  "Megaloptera",
  "Neuroptera",
  "Odonata",
  "Plecoptera",
  "Trichoptera"
)]

# _________________________________________________________________________
#### Oviposition ####
# ovip_aqu
# ovip_ter
# ovip_ovo
# _________________________________________________________________________

# Merge oviposition data (only present in old NOA trait DB) on species level 
Trait_Noa_new[Trait_Noa[!is.na(species),],
              `:=`(ovip_ter = i.ovip_ter,
                   ovip_aqu = i.ovip_aqu,
                   ovip_ovo = i.ovip_ovo),
              on = "species"] 

# Select relevant columns
Trait_Noa_new <- Trait_Noa_new[, .SD,
                               .SDcols = names(Trait_Noa_new) %like% "feed|locom|size|resp|volt|ovip|unique_id|species|genus|family|order"]

# select columns from old Noa Trait DB that are also in Trait_Noa_New
cols <- names(Trait_Noa)[names(Trait_Noa) %in% names(Trait_Noa_new)]
Trait_Noa <- Trait_Noa[, .SD, .SDcols = cols]

# get trait columns
name_vec <- grep("unique_id|order|family|genus|species",
                 names(Trait_Noa),
                 value = TRUE,
                 invert = TRUE) %>%
  sub("\\_.*", "", .) %>%
  unique() %>%
  paste0("^",.)

# coalesce merge on species level to complement missing information with the old trait DB
# (likely that there isn't much that can be complemented by the old NOA trait DB)
# % NA val before 
# na_before <- sum(is.na(Trait_Noa_new))/(sum(is.na(Trait_Noa_new))+ sum(!is.na(Trait_Noa_new)))
final <- Trait_Noa_new
for(i in name_vec){
  subset_vec <- !(rowSums(is.na(final[, .SD, .SDcols = names(final) %like% i])) == 0)
  
  # subset to NA values -> complement these with information from NOA old (if possible)
  step <- coalesce_join(x = final[subset_vec, ],
                        y = Trait_Noa[!is.na(species), .SD,
                                   .SDcols = names(Trait_Noa) %like% paste0(i, "|", "species")],
                        by = "species",
                        join = dplyr::left_join)
  setDT(step)
  
  # merge back to whole dataset
  final <- coalesce_join(x = final,
                         y = step[!is.na(species), 
                                  .SD, .SDcols = names(step) %like% paste0(i, "|", "species")],
                         by = "species",
                         join = dplyr::left_join) 
  setDT(final)
}
# % of na_values after merge -> slight improve
# na_after <- sum(is.na(final))/(sum(is.na(final))+ sum(!is.na(final)))
Trait_Noa_new <- final

# merge information from taxa level that are only in old Noa DB
# species level
Trait_Noa_new <-
  rbind(Trait_Noa_new, Trait_Noa[!is.na(species),] %>%
          .[!(species %in% Trait_Noa_new[!is.na(species),]$species)])
  
# genus level & family level added
# NOTE: no taxa has complete trait information (see uncommented code)
# Trait_Noa[is.na(species) & !is.na(genus),] %>%
#   .[!genus %in% Trait_Noa_new[is.na(species) & !is.na(genus),]$genus, ] %>% 
#   na.omit(., cols = names(.[, -c("species")]))
# Trait_Noa[is.na(species) & is.na(genus) & !is.na(family), ] %>%
#   .[!family %in% Trait_Noa_new[is.na(species) & is.na(genus) & !is.na(family), ]$family,] %>% 
#   na.omit(., cols = names(.[, -c("species", "genus")]))
# genus level
Trait_Noa_new  <-
  rbind(Trait_Noa_new,
        Trait_Noa[is.na(species) & !is.na(genus),] %>%
          .[!genus %in% Trait_Noa_new[is.na(species) & !is.na(genus),]$genus, ])
# family level
Trait_Noa_new <- rbind(Trait_Noa_new,
                       Trait_Noa[is.na(species) & is.na(genus) & !is.na(family), ] %>%
                         .[!family %in% Trait_Noa_new[is.na(species) & is.na(genus) & !is.na(family), ]$family,])

# _________________________________________________________________________
#### Pattern fo development ####
# Holometabolous 
# Hemimetabolous
# No insect
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
Trait_Noa_new[, `:=`(
  dev_hemimetabol = ifelse(order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(order %in% holometabola, 1, 0)
)]

# save
saveRDS(object = Trait_Noa_new, 
        file = file.path(data_cleaned, 
                         "North_America", 
                         "Traits_US_LauraT_pp_harmonized.rds")
)
