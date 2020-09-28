# --------------------------------------------------------------------
#### Preparation: European database ####

# The following steps have been carried out by Sebastian Scheu in 2018
# (reading in as rds object has only been modified)
# ---------------------------------------------------------------

# original
# df_EUR <- read.csv(file.path(data_in, "Freshwater_info","Freshwaterecol_Aug_2018.csv"), stringsAsFactors = FALSE)

df_EUR <- readRDS(file.path(data_in, "EU", "Freshwaterecol_Aug_2018.rds"))

# Weird entries and Ad. & Lv. are removed
# TODO check removed Lv.!
row_del <- grep("[[:punct:]]", df_EUR$Taxon, ignore.case = TRUE)
df_EUR <- df_EUR[-row_del, ]


#### Get taxa information
# Two Species can not be found in the gbif database. 
# 1. Lepidostoma doehleri is missing. Belongs to the family Lepidostoma.
# 2. Holostomis phalaenoides is also known as Phryganea phalaenoides (https://eprints.lib.hokudai.ac.jp/dspace/bitstream/2115/22222/1/2_P1-6.pdf).
spec_gbif <- df_EUR$Taxon[df_EUR$Taxon != "Lepidostoma doehleri" & df_EUR$Taxon != "Holostomis phalaenoides"]
spec_nbn <- c("Lepidostoma doehleri", "Phryganea phalaenoides")

# Get family and genus levels from "Global Biodiversity Information Facility" (gbif)
tax_gbif <- get_ids(names = spec_gbif, db = "gbif")
cl_gbif <- cbind(classification(tax_gbif$gbif, return_id = FALSE))#; beep(4)

# Get missing family and genus levels from "National Biodiversity Network" (nbn)
tax_nbn <- get_ids(names = spec_nbn, db = "nbn")
cl_nbn <- cbind(classification(tax_nbn$nbn, return_id = FALSE))

# Combine all necessary taxa info (order, family, genus) into one dataframe, then order
class_full <- rbind(cl_gbif[4:7], cl_nbn[6:9])
class_full <- class_full[order(class_full$order), ]

df_EUR_complete <- cbind(df_EUR, class_full)
df_EUR_complete <- df_EUR_complete %>% select(order:genus, Taxon:order)

#
write.table(df_EUR_complete, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR.csv", sep = ",")

#### Add trait size from Tachet database via table join
tachet <- read.csv(file.path(data_in, "Tachet_mod_csv.csv"), stringsAsFactors = FALSE)[, c(1:14)]
df_EUR <- read.csv(file.path(data_in, "macroinvertebrate_EUR.csv"), stringsAsFactors = FALSE)

# Merge df_EUR with size information from Tachet
EU_size <- df_EUR[, 3:4]

# 1. Merge on species level
EU_size <- merge(EU_size, tachet, by.x = "Taxon", by.y = "taxa", all.x =  TRUE)

# 2. Merge on genus level
EU_size <- merge(EU_size, tachet, by.x = "genus.x", by.y = "taxa", all.x =  TRUE)

# 3. Combine size columns and add to df_EU
EU_size <- EU_size %>%
  transmute(size_1 = size_1.x + size_1.y,
            size_2 = size_2.x + size_2.y,
            size_3 = size_3.x + size_3.y,
            size_4 = size_4.x + size_4.y,
            size_5 = size_5.x + size_5.y,
            size_6 = size_6.x + size_6.y,
            size_7 = size_7.x + size_7.y)

df_EUR <- cbind(df_EUR, EU_size)

write.table(df_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR.csv", sep = ",")

#### Renaming columns
df_EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR.csv"))


## Microhabitat
names(df_EUR)[grep(c("pel$|arg$|psa$|aka$|mil$|mal$|hpe$|alg$|mph$|pom$|woo$|mad$|oth$"), names(df_EUR))] <- 
  c("microhab_pel", "microhab_arg", "microhab_psa", "microhab_aka", "microhab_mil", 
    "microhab_mal", "microhab_hpe", "microhab_alg", "microhab_mph", "microhab_pom",
    "microhab_woo", "microhab_mad", "microhab_oth")

# Explanation:  microhab_pel: pelal - mud
#               microhab_arg: argyllal - silt, loam, clay
#               microhab_psa: psammal - sand
#               microhab_aka: akal - fine to medium-sized gravel
#               microhab_mil: micro/mesolithal - coarse gravel to hand-sized cobbles
#               microhab_mal: macro/megalithal - stones, boulders, bedrocks
#               microhab_hpe: hygropetric habitats - thin layers of water over bedrocks, waterfalls
#               microhab_alg: algae - micro- and macroalgae
#               microhab_mph: macrophytes - macrophytes, mosses, living parts of terrestrial plants
#               microhab_pom: particulate organic matter - coarse and fine particulate organic matter
#               microhab_woo: woody debris - woody debris, twigs, roots, logs
#               microhab_mad: madicol - edge of water bodies, moist substrates
#               microhab_oth: other substrates - e.g. host of a parasite


## Current preference
names(df_EUR)[grep(c("lib$|lip$|lrp$|rlp$|rhp$|rhb$|ind$"), names(df_EUR))] <- 
  c("current_lib", "current_lip", "current_lrp", "current_rlp", "current_rhp", "current_rhb", "current_ind")

# Explanation:  current_lib: limnobiont - only standing waters
#               current_lip: limnophil - preferably standing waters, rarely found in slowly flowing streams
#               current_lrp: limno to retrophil - preferably in standing waters, but regularly occurring in slowly flowing streams
#               current_rlp: rheo to limnophil - usually found in streams, prefers slow current and lentic zones, also found in standing waters
#               current_rhp: rheophil - occurring in streams, prefers zones with moderate to high current
#               current_rhb: rheobiont - occuring in streams with high current
#               current_ind: indifferent - no preferance for a certain current velocity


## Temperature preference
names(df_EUR)[grep(c("vco$|cod$|mod$|war$|eut$"), names(df_EUR))] <- 
  c("temp_very_cold", "temp_cold", "temp_moderate", "temp_warm", "temp_eurytherm")

# Explanation:  temp_very_cold:   < 6 °C
#               temp_cold:        < 10 °C
#               temp_moderate:    < 18 °C
#               temp_warm:        > 18 °C
#               temp_eurytherm:   no specific preference


## pH preference
names(df_EUR)[grep(c("aci$|neu$|ind$"), names(df_EUR))] <- 
  c("ph_acidic", "ph_neutral_alk", "ph_ind")

# Explanation:  ph_acidic:      acidic - pH < 7
#               ph_neutral_alk: neutral to alkaline - ph = 7
#               ph_ind:         indifferent - no specific preference


## Salinity preference
names(df_EUR)[grep(c("bws$|bwi$"), names(df_EUR))] <- 
  c("sal_brackish_sea", "sal_brackish_inland")

# Explanation:  sal_brackish_sea:     brackish water: sea - total salt content, 0.5 - 34.7
#               sal_brackish_inland:  brackish water: inland - total salt content, 0.5 - 34.7


## Saprobity
names(df_EUR)[grep(c("alpha$|beta$|^g$|oligo$|poly$|si$|xeno$"), names(df_EUR))] <- 
  c("oxy_index", "oxy_indw", "oxy_xeno", "oxy_oligo", "oxy_beta", "oxy_alpha", "oxy_poly")

# Explanation:  oxy_index:  saprobic index
#               oxy_indw:   indicator weight
#               oxy_xeno:   xeno-saprobic
#               oxy_oligo:  oligo-saprobic
#               oxy_beta:   beta-meso-saprobic
#               oxy_alpha:  alpha-meso-saprobic
#               oxy_poly:   poly-saprobic


## Feeding mode
names(df_EUR)[grep(c("gra$|min$|xyl$|shr$|gat$|aff$|pff$|pre$|par$|oth.1$"), names(df_EUR))] <- 
  c("feed_grazer", "feed_miner", "feed_xylo", "feed_shredder", "feed_gatherer",
    "feed_active_filter", "feed_passive_filter", "feed_predator", "feed_parasite", "feed_other")

# Explanation:  feed_grazer:          grazer/scraper - feed from endolithic and epilithic material
#               feed_miner:           miners - feed from aquatic plants and algae (leaves, cells)
#               feed_xylo:            xylophagous - feed from woody debris
#               feed_shredder:        shredder - feed from fallen leaves, plant tissue, CPOM
#               feed_gatherer:        gatherer/collector - feed from sedimented FPOM
#               feed_active_filter:   active filter feeder - food is actively filtered from water column
#               feed_passive_filter:  passive filter feeder - food is filtered from running water (e.g. by nets, special mouthparts)
#               feed_predator:        predator - feed from prey
#               feed_parasite:        parasite - feed from host
#               feed_other:           use other then mentioned food sources


## Locomotion
names(df_EUR)[grep(c("sws$|swd$|$|bub$|spw$|ses$|oth.2$"), names(df_EUR))] <- 
  c("locom_swim_skate", "locom_swim_dive", "locom_burrow", "locom_sprawl", "locom_sessil", "locom_other")

# Explanation:  locom_swim_skate: swimming/scating - floating in lakes or drifting in rivers passively
#               locom_swim_dive:  swimming/diving - swimming or active diving
#               locom_burrow:     burrow/boring - burrowing in soft or boring in hard substrates
#               locom_swim_walk:  sprawling/walking - sprawling or walking (actively with legs, pseudopods or muccus)
#               locom_sessil:     (semi)sessil - tightening to substrates
#               locom_other:      other locomotion type - flying, jumping, ... mostly outside the water


## Respiration
names(df_EUR)[grep(c("teg|gil|pls|spi|ves|tap|sur"), names(df_EUR))] <- 
  c("resp_tegument", "resp_gill", "resp_plastron", "resp_spiracle", "resp_vesicle", "resp_tapping", "resp_surface")

# Explanation:  resp_tegument:  tegument - respiration through body surface
#               resp_gill:      gill
#               resp_plastron:  plastron
#               resp_spiracle:  spiracle (aerial)
#               resp_vesicle:   hydrostatic vesicle (aerial)
#               resp_tapping:   tapping of air stores of aquatic plants
#               resp_surface:   extension/excursion to surface - respiration of atmospheric oxygen


## Resistance form
names(df_EUR)[grep(c("egg|coc|hou|did|qui|non"), names(df_EUR))] <- 
  c("res_egg_stato", "res_cocoon", "res_house", "res_diapause_dormancy", "res_quiesence", "res_non")

# Explanation:  res_egg_stato:          eggs, statoblasts
#               resp_cocoon:            cocoons
#               res_house:              housing against desiccation
#               res_diapause_dormancy:  diapause or dormancy
#               res_quiescence:         quiesence
#               res_none:               none


## Dissemination strategy
names(df_EUR)[grep(c("aqp|aqa|aep|aea"), names(df_EUR))] <- 
  c("dissem_aq_passive", "dissem_aq_active", "dissem_air_passive", "dissem_air_active")

# Explanation:  dissem_aq_passive:  aquatic passive
#               dissem_aq_active:   aquatic active
#               dissem_air_passive: aerial passive
#               dissem_air_active:  aerial active


## Dispercal capacity
names(df_EUR)[grep(c("hig|low|unk|"), names(df_EUR))] <- 
  c("dispersal_high", "dispersal_low", "dispersal_unknown")

# Explanation:  dispersal_high:     high dispersal capacity 
#               dispersal_low:      low dispersal capacity
#               dispersal_unknown:  unknown dispersal capacity


## Life duration
names(df_EUR)[grep(c("l1y|g1y"), names(df_EUR))] <- 
  c("lifedur_one_yr", "lifedur_more_yr")

# Explanation:  lifedur_one_yr:   life duration up to one year
#               lifedur_more_yr:  life duration longer than one year  


## Aquatic stages
names(df_EUR)[grep(c("egg.1|lar|nym|pup|adu"), names(df_EUR))] <- 
  c("stage_egg", "stage_larva", "stage_nymph", "stage_pupa", "stage_adult")

# Explanation:  stage_egg:    aquatic stage as egg
#               stage_larva:  aquatic stage as larva
#               stage_nymph:  aquatic stage as nymph
#               stage_pupa:   aquatic stage as pupa
#               stage_adult:  aquatic stage as adult


## Emergence period
names(df_EUR)[grep(c("win|spr|sum|aut"), names(df_EUR))] <- 
  c("ermerge_winter", "emerge_summer", "emerge_spring", "emerge_autumn")

# Explanation:  ermerge_winter: emerging mainly in winter
#               emerge_summer:  emerging mainly in summer
#               emerge_spring:  emerging mainly in spring
#               emerge_autumn:  emerging mainly in autumn


## Reproductive cycles per year (voltinism)
names(df_EUR)[grep(c("sev|unv|biv|trv|muv|flx"), names(df_EUR))] <- 
  c("volt_semi", "volt_uni", "volt_bi", "volt_tri", "volt_multi", "volt_flex")

# Explanation:  volt_semi:  one generation in two years
#               volt_uni:   one generation per year
#               volt_bi:    two generations per year
#               volt_tri:   three generations per year
#               volt_multi: more than three generations per year
#               volt_flex:  flexible number of life cycles per year


## Reproduction
names(df_EUR)[grep(c("ovo|fie|cie|fic|frc|vec|tec|ase|pas"), names(df_EUR))] <- 
  c("rep_ovovipar", "rep_egg_free_iso", "rep_egg_cem_iso", "rep_clutch_fixed",
    "rep_clutch_free", "rep_clutch_veg", "rep_clutch_ter", "rep_asexual", "rep_parasitic")

# Explanation:  rep_ovovipar:     ovovivipar - eggs remain within the mother's body until they hatch
#               rep_egg_free_iso: free isolated eggs - separate eggs are laid down in the water freely
#               rep_egg_cem_iso:  cemented isolated eggs - separate eggs are laid down and fixed
#               rep_clutch_fixed: fixed clutches - groups of eggs are laid down and fixed
#               rep_clutch_free:  free clutches - groups of eggs are laid down in the water freely
#               rep_clutch_veg:   clutches in vegetation - groups of eggs are laid down in the vegetation
#               rep_clutch_ter:   terrestrial clutches - groups of eggs are laid down in the riparian zone
#               rep_asexual:      asexual - reproduction without fertilisation
#               rep_parasitic:    parasitic - reproduction within a host

# 