# _________________________________________________________________________ 
#### Harmonize EU Traits ####
# _________________________________________________________________________ 

# read in RDS
Trait_EU <- readRDS(file.path(data_cleaned, "EU", "Trait_Freshecol_pp.rds"))

# _________________________________________________________________________ 
#### Voltinism ####
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________ 
Trait_EU[, volt_bi_multi := apply(.SD, 1, max),
           .SDcols = c("volt_bi", "volt_tri", "volt_multi", "volt_flex")]
Trait_EU[, c("volt_bi", "volt_tri", "volt_multi", "volt_flex") := NULL]

# _________________________________________________________________________ 
#### aquatic stages ####
# gives information about which stage lives in the aquatic phase 
# stage_egg
# stage_larva: larva and/or nymph
# stage_pupa
# stage_adult
# _________________________________________________________________________ 
Trait_EU[, stage_larva := apply(.SD, 1, max),
           .SDcols = c("stage_larva", "stage_nymph")]
Trait_EU[, stage_nymph := NULL]

# _________________________________________________________________________ 
#### ph ####
# ph_acidic, ph_neutral
# pH_ind (indifferent) is dismissed from database (128 entries with 1)
# _________________________________________________________________________ 
setnames(Trait_EU, "ph_neutral_alk", "ph_neutral")
Trait_EU[, ph_ind := NULL]

# _________________________________________________________________________ 
#### Feed mode ####
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
 # xylophagus can fit to the category of shredders, but see Lancaster:
 # "The mouthparts of facultative species tend to fit the generic model for most shredders. 
 # Obligate xylophages, however, often have further adaptations for mining wood, and virtually all
 # have strong, heavily sclerotized mandibles. Larvae of the caddisfly Lype phaeope 
 # (Psychomyiidae) scrape off layers of wood with their mandibles, 
 # and collect the fragments on the forelegs ( Spänhoff et al.2003 )."
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivroe: scraper (grazer) & herbivore piercer
# feed_predator: predator
# feed_parasite: parasite

# Predators are defined as: Engulfers (ingest pref whole or in parts) or
  # as Piercers (piere prey tissues and suck fluids) 
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________ 
setnames(Trait_EU,
         c("feed_gath", "feed_grazer"),
         c("feed_gatherer", "feed_herbivore"))
Trait_EU[, feed_shredder := apply(.SD, 1, max),
           .SDcols = c("feed_shred", "feed_miner", "feed_xylo")]
Trait_EU[, feed_filter := apply(.SD, 1, max),
           .SDcols = c("feed_active_filter", "feed_passive_filter")]
Trait_EU[, c(
  "feed_shred",
  "feed_miner",
  "feed_xylo",
  "feed_active_filter",
  "feed_passive_filter",
  "feed_other"
) := NULL]

# _________________________________________________________________________ 
#### Locomotion ####
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler
# locm_burrow: burrower
# locm_sessil: sessil (attached)
# _________________________________________________________________________ 
Trait_EU[, locom_swim := apply(.SD, 1, max), 
           .SDcols = c("locom_swim_skate", "locom_swim_dive")]
setnames(Trait_EU,
         old = c("locom_sprawl"),
         new = c("locom_crawl"))
# del
Trait_EU[, c("locom_swim_skate", "locom_swim_dive",
               "locom_other") := NULL]

# locom other -> critical taxa 
# View(Trait_EU[locom_other > locom_burrow & locom_other > locom_crawl &
#                   locom_other > locom_sessil & locom_other > locom_swim, 
#                 .(species, genus, family, locom_other, 
#                   locom_burrow, locom_crawl, 
#                   locom_sessil, locom_swim)]
# )

# _________________________________________________________________________ 
#### Respiration ####
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: plastron & spiracle
  # plastron & spiracle often work together in respiratory systems of aq. insects
  # Present in insects with open tracheal systems -> breathe oxygen from the air
  # -> Different tolerances to low oxygen compared to insects with tegument resp and gills

# resp_atm: atmospheric breathers -> no values
# respiration vesicle -> just 0
# no values for resp_tap
# table(Trait_EU$resp_ves)
# table(Trait_EU$resp_tap)
# table(Trait_EU$resp_sur)
# _________________________________________________________________________ 
Trait_EU[, resp_pls_spi := apply(.SD, 1, max), 
          .SDcols = c("resp_spi", "resp_pls")]
Trait_EU[, c("resp_tap", 
             "resp_ves", 
             "resp_sur",
             "resp_spi",
             "resp_pls") := NULL]

# =================== Dispersal -> This needs to be fixed ======================
# Drift/dispersal
# use disp low, medium, high for comparability 
# del dispersal_unknown
Trait_EU[, dispersal_unknown := NULL]
# check for merge if information is reliable
# two species will get changed their dispersal trait from low to medium
# Sericostoma personatum & Silo pallipes
# Trait_EU[(dispersal_high == 1| dispersal_low == 1) &
#              species %in% disp_EU$Species, .(dispersal_high, dispersal_low, species)] 
# disp_EU[grepl("Amphinemura|Hydropsyche|Leuctra|Nemurella|Sericostoma|Silo", Species)]
# merge with disp_EU
# Trait_EU[disp_EU,
#            `:=`(dispersal_high = i.Strong,
#                 dispersal_low = i.Weak,
#                 dispersal_medium = i.Medium),
#            on = c(species = "Species")]
# # change NA's in dispersal medium to zeor
# Trait_EU[is.na(dispersal_medium), dispersal_medium := 0]
# =================================================================

# _________________________________________________________________________ 
#### Oviposition ####
# Modalities
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# rep_asexual is deleted
# _________________________________________________________________________ 
Trait_EU[,  ovip_ter := apply(.SD, 1, max),
           .SDcols = c("rep_clutch_veg", "rep_clutch_ter")]
Trait_EU[, ovip_aqu := apply(.SD, 1, max),
           .SDcols = c("rep_egg_cem_iso",
                       "rep_egg_free_iso",
                       "rep_clutch_free",
                       "rep_clutch_fixed")]
Trait_EU[, ovip_ovo := apply(.SD, 1, max),
           .SDcols = c("rep_parasitic", "rep_ovovipar")]

# del
Trait_EU[, c(
  "rep_egg_cem_iso",
  "rep_egg_free_iso",
  "rep_clutch_free",
  "rep_clutch_fixed",
  "rep_parasitic",
  "rep_ovovipar",
  "rep_clutch_veg",
  "rep_clutch_ter",
  "rep_asexual"
) := NULL]

# _________________________________________________________________________ 
#### Temperature ####
# temp very cold (< 6 °C)
# temp cold (< 10 °C)
# temp moderate (< 18 °C)
# temp warm (>= 18 °C)
# temp eurytherm (no specific preference)
# _________________________________________________________________________ 
Trait_EU[, temp_cold := apply(.SD, 1, max),
           .SDcols = c("temp_cold", "temp_moderate", "temp_very_cold")]
Trait_EU[, c("temp_moderate", "temp_very_cold") := NULL]

# _________________________________________________________________________ 
#### Pattern fo development ####
# Holometabolous 
# hemimetabolous?
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
Trait_EU[, `:=`(
  dev_hemimetabol = ifelse(order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(order %in% holometabola, 1, 0)
)]

# _________________________________________________________________________
#### Body form ####
# bf_streamlined: streamlined/fusiform
# bf_flattened: flattened (dorso-ventrally)
# bf_cylindrical: cylindrical/tubular 
# bf_spherical: spherical

# Will be added in 04_EU_aggregate_traits.R!
# _________________________________________________________________________

# _________________________________________________________________________ 
#### Normalization Freshecol ####
# TODO make a function out of this!
# get trait names & create pattern for subset
# leave out ph (needs to be harmonized when merged together with Freshwaterecol)
# _________________________________________________________________________ 

Trait_EU <- normalize_by_rowSum(
  x = Trait_EU,
  non_trait_cols = c("order",
                     "family",
                     "genus",
                     "species")
)

# exclude life stage for now -> can not be complemented by tachet and not needed for now
Trait_EU <- Trait_EU[, .SD, .SDcols = !names(Trait_EU) %like% "stage"] 

# _________________________________________________________________________ 
#### Complement with tachet data ####
# Information from tachet is just considered for entries in freshecol with missing information
# Information on the same trait was taken from Freshecol
# _________________________________________________________________________ 

# Load normalized & harmonized tachet data
tachet <- readRDS(file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds"))

# get names of trait columns
name_vec <- grep("order|family|genus|species",
                 names(Trait_EU),
                 value = TRUE,
                 invert = TRUE) %>%
                 sub("\\_.*", "", .) %>%
                 unique() %>%
                 paste0("^", .)

final <- Trait_EU
for (i in name_vec) {
  subset_vec <-
    !(rowSums(is.na(final[, .SD, .SDcols = names(final) %like% i])) == 0)

  # subset to NA values -> complement these with Tachet traits
  step <- coalesce_join(x = final[subset_vec,],
                        y = tachet[!is.na(species), .SD,
                                   .SDcols = names(tachet) %like% paste0(i, "|", "species")],
                        by = "species",
                        join = dplyr::left_join)
  setDT(step)

  # merge back to whole dataset
  final <- coalesce_join(x = final,
                         y = step[, .SD, .SDcols = names(step) %like% paste0(i, "|", "species")],
                         by = "species",
                         join = dplyr::left_join)
  setDT(final)
}
# check temp & ph 
# apply(Trait_EU[, .SD, .SDcols = names(Trait_EU) %like% name_vec[5]], 
#       2, 
#       table)
# apply(final[, .SD, .SDcols = names(Trait_EU) %like% name_vec[5]],
#       2,
#       table)
# tachet[grepl("Acentria ephemerella", Species_merge), ]
# Trait_EU[grepl("Acentria ephemerella", species), ]
Trait_EU <- final

# merge information from taxa on species level that are only in tachet
Trait_EU <- rbind(Trait_EU, 
                  tachet[!(species %in% Trait_EU$species) & !is.na(species), .SD , 
                           .SDcols = !(names(tachet) %like% "^stage|^disp|size")], 
                  use.names = TRUE,
                  fill = TRUE)

# _________________________________________________________________________ 
#### Size ####
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________ 

# merge size information from tachet
Trait_EU[tachet, 
           `:=`(size_large = i.size_large,
                size_medium = i.size_medium,
                size_small = i.size_small), 
           on = "species"]
# _________________________________________________________________________ 
#### Taxonomical corrections ####
# _________________________________________________________________________ 

# Macromia instead of MaRcromia! 
Trait_EU[grepl("Marcromia", genus), `:=`(genus = "Macromia",
                                         species = sub("Marcromia", "Macromia", species))]

# Heteroptera is a suborder in Hemiptera
Trait_EU[grepl("Heteroptera", order), order := "Hemiptera"]

# Crustaceans: 
Trait_EU[grepl("Grapsidae", family), order := "Decapoda"]
Trait_EU[grepl("Triopsidae", family), order := "Notostraca"]
Trait_EU[grepl("Cambaridae", family), order := "Decapoda"]
Trait_EU[grepl("Asellidae", family), order := "Isopoda"]
Trait_EU[grepl("Astacidae", family), order := "Decapoda"]
Trait_EU[grepl("Atyidae", family), order := "Decapoda"]

# Oligochaeta is actually a subclass
# Tubificidae outdated
Trait_EU[grepl("Tubificidae", family), family := "Naididae"]
Trait_EU[grepl("Lumbricidae|Propappidae|Tubificidae", family), 
           order := "Haplotaxida"]

# Hirudinea is actually a subclass
Trait_EU[grepl("Glossiphoniidae", family), order := "Rhynchobdellida"]
Trait_EU[grepl("Haemopidae", family), order := "Hirudiniformes"]

# Gastropoda is actually a class
Trait_EU[grepl("Lymnaeidae|Planorbidae|Acroloxidae", family), 
           order := "Pulmonata"]
Trait_EU[grepl("Physidae", family), order := "Basommatophora"]

# Nermertia seems to be a spelling error -> should be Nemertea instead 
# (which is Phylum)
Trait_EU[grepl("Tetrastemmatidae", family), order := "Monostilifera"]

# Coelenterata is a phylum actually
Trait_EU[grepl("Clavidae", order), `:=`(family = "Hydractiniidae", 
                                          order = "Anthoathecata")]

# Turbellaria is actually a class
Trait_EU[grepl("Turbellaria", order), order := "Tricladida"]

# Bivalvia is actually a class
Trait_EU[grepl("Margaritiferidae", family), order := "Unionida"]

# Bryozoa is actually a phylum
Trait_EU[grepl("Fredericellidae|Cristatellidae|Plumatellidae", family), 
           order := "Plumatellida"]
Trait_EU[grepl("Paludicellidae", family), order := "Ctenostomata"]
# Lophopodidae currently unranked

# order gentianales is actually a plant
Trait_EU[grepl("Lepidostoma", genus), `:=`(family = "Lepidostomatidae", 
                                             order = "Trichoptera")]
Trait_EU[grepl("Normandia", genus), `:=`(family = "Elmidae", 
                                           order = "Coleoptera")]
Trait_EU[grepl("Stenostomum", genus), `:=`(family = "Stenostomidae", 
                                             order = "Catenulida")]
# save
saveRDS(object = Trait_EU, 
        file = file.path(data_cleaned, "EU", "Trait_EU_pp_harmonized.rds"))
