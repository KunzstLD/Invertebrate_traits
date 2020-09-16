
# link to raw data
data_raw <- "./Data/EU/Raw_freshecol/2020"

# packages
library(data.table)
library(dplyr)
library(taxize)

# script with helper functions
source(file = "./R/Preprocessing_raw/functions_used.R")



#### read taxaexports ####
filelinks <- list.files(
  path = data_raw,
  pattern = "taxadbexport",
  full.names = TRUE
)

output_db <- replicate(length(filelinks), data.table())
for (i in seq_along(filelinks)) {

  # fetch line where actual DB starts
  ind <- grep("Taxon", readLines(filelinks[i]))

  # read in
  db <- fread(filelinks[i],
    skip = ind,
    sep = ";",
    header = TRUE,
    na.strings = (""),
    fill = TRUE
  )

  # put into list
  output_db[[i]] <- db
}

#### data processing ####
# delete summary statistics (not part of DB)
output_db <- lapply(
  output_db,
  function(y) y[!(V1 %like% "Statistics|number.*"), ]
)

# rm col V1 in all but one data table
lapply(
  output_db[2:length(output_db)],
  function(y) {
    y[, c("V1", "EU") := NULL]
  }
)

# bind
freshwaterecol <- Reduce(f = cbind, output_db)
setnames(freshwaterecol, "V1", "taxon")
freshwaterecol[, EU := NULL]

# trait preparation ----

# pH preference
setnames(
  freshwaterecol, c(
    "aci",
    "neu",
    "ind"
  ),
  c(
    "ph_acidic",
    "ph_neutral_alk",
    "ph_ind"
  )
)
setnames(
  freshwaterecol, c(
    "&le; 4",
    "> 4-4.5",
    "> 4.5-5",
    "> 5-5.5",
    "> 5.5-6",
    "> 6"
  ),
  paste0("tachet_ph_", c(
    "&le; 4",
    "> 4-4.5",
    "> 4.5-5",
    "> 5-5.5",
    "> 5.5-6",
    "> 6"
  ))
)
# Explanation:  ph_acidic:      acidic - pH < 7
#               ph_neutral_alk: neutral to alkaline - ph = 7
#               ph_ind:         indifferent - no specific preference


# Temperature preference
setnames(
  freshwaterecol, c(
    "vco",
    "cod",
    "mod",
    "war",
    "eut"
  ),
  paste0("temp_", c(
    "very_cold",
    "cold",
    "moderate",
    "warm",
    "eurytherm"
  ))
)
setnames(
  freshwaterecol,
  c(
    "psy",
    "the",
    "eut"
  ),
  paste0(
    "tachet_temp_",
    c(
      "psy",
      "the",
      "eut"
    )
  )
)

# Explanation:  temp_very_cold:   < 6 °C
#               temp_cold:        < 10 °C
#               temp_moderate:    < 18 °C
#               temp_warm:        > 18 °C
#               temp_eurytherm:   no specific preference


# Feeding mode
setnames(
  freshwaterecol,
  c(
    "gra",
    "min",
    "xyl",
    "shr",
    "gat",
    "aff",
    "pff",
    "pre",
    "par",
    "oth"
  ),
  paste0(
    "feed_",
    c(
      "grazer",
      "miner",
      "xylo",
      "shredder",
      "gatherer",
      "active_filter",
      "passive_filter",
      "feed_predator",
      "feed_parasite",
      "feed_other"
    )
  )
)
setnames(
  freshwaterecol,
  c(
    "abs",
    "dpf",
    "shr",
    "scr",
    "fif",
    "pie",
    "pre",
    "par"
  ),
  paste0(
    "tachet_feed_",
    c(
      "absorber",
      "deposit_feeder",
      "shredder",
      "scraper",
      "filter",
      "piercer",
      "predator",
      "parasite"
    )
  )
)

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
# 				feed_absorber:        gather organisms direct uptake through tegument
# 				feed_deposit: 		  feed on small debris on surface of fine sediments
# 				feed_scraper		  grazing fine organic particles, ...
# 				feed_piercer:         piercing plants or animals


## Locomotion
setnames(
  freshwaterecol,
  c(
    "sws",
    "swd",
    "bub",
    "spw",
    "ses",
    "oth"
  ),
  paste0(
    "locom_",
    c(
      "swim_skate",
      "swim_dive",
      "burrow",
      "sprawl",
      "sessil",
      "other"
    )
  )
)
setnames(
  freshwaterecol,
  c(
    "flr",
    "ssw",
    "wsw",
    "crw",
    "bur",
    "int",
    "tat",
    "pat"
  ),
  paste0(
    "tachet_locom_",
    c(
      "flier",
      "surface_swimmer",
      "full_water_swimmer",
      "crawler",
      "burrower",
      "interstitial",
      "temp_attached",
      "perm_attached"
    )
  )
)
# Explanation:  locom_swim_skate: swimming/scating - floating in lakes or drifting in rivers passively
#               locom_swim_dive:  swimming/diving - swimming or active diving
#               locom_burrow:     burrow/boring - burrowing in soft or boring in hard substrates
#               locom_swim_walk:  sprawling/walking - sprawling or walking (actively with legs, pseudopods or muccus)
#               locom_sessil:     (semi)sessil - tightening to substrates
#               locom_other:      other locomotion type - flying, jumping, ... mostly outside the water
# 				locom_crawler:
# 				locom_flier: 	  active and passive fliers
# 				locom_interstitial: endobenthic
# 				locom_temp/locom_perm_attached:

# Respiration
setnames(
  freshwaterecol,
  c(
    "teg",
    "gil",
    "pls",
    "spi",
    "ves",
    "tap",
    "sur"
  ),
  paste0(
    "resp_",
    c(
      "tegument",
      "gill",
      "plastron",
      "spiracle",
      "vesicle",
      "tapping",
      "surface"
    )
  )
)
setnames(
  freshwaterecol,
  c(
    "teg",
    "gil",
    "pls",
    "spi",
    "ves"
  ),
  paste0(
    "tachet_resp_",
    c(
      "tegument",
      "gill",
      "plastron",
      "spiracle",
      "vesicle"
    )
  )
)
# Explanation:  resp_tegument:  tegument - respiration through body surface
#               resp_gill:      gill
#               resp_plastron:  plastron
#               resp_spiracle:  spiracle (aerial)
#               resp_vesicle:   hydrostatic vesicle (aerial)
#               resp_tapping:   tapping of air stores of aquatic plants
#               resp_surface:   extension/excursion to surface - respiration of atmospheric oxygen


# aquatic stages
setnames(
  freshwaterecol,
  c(
    "egg",
    "lar",
    "nym",
    "pup",
    "adu"
  ),
  paste0(
    "stage_",
    c(
      "egg",
      "larva",
      "nymph",
      "pupa",
      "adult"
    )
  )
)
setnames(
  freshwaterecol,
  c(
    "larva",
    "nymph",
    "adult"
  ),
  paste0(
    "tachet_stage_",
    c(
      "larva",
      "nymph",
      "adult"
    )
  )
)
# Explanation:  stage_egg:    aquatic stage as egg
#               stage_larva:  aquatic stage as larva
#               stage_nymph:  aquatic stage as nymph
#               stage_pupa:   aquatic stage as pupa
#               stage_adult:  aquatic stage as adult


# Reproductive cycles per year (voltinism)
setnames(
  freshwaterecol,
  c(
    "sev",
    "unv",
    "biv",
    "trv",
    "muv",
    "flx"
  ),
  paste0(
    "voltinism_",
    c(
      "semi",
      "uni",
      "bi",
      "tri",
      "multi",
      "flex"
    )
  )
)
setnames(
  freshwaterecol,
  c(
    "< 1",
    "1",
    "> 1"
  ),
  paste0(
    "tachet_voltinism_",
    c(
      "semi",
      "uni",
      "multi"
    )
  )
)
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


# Dispercal capacity
names(df_EUR)[grep(c("hig|low|unk|"), names(df_EUR))] <- 
  c("dispersal_high", "dispersal_low", "dispersal_unknown")

# Explanation:  dispersal_high:     high dispersal capacity 
#               dispersal_low:      low dispersal capacity
#               dispersal_unknown:  unknown dispersal capacity

# size




# taxonomic preparation ----
# copy of taxon column
freshwaterecol[, taxon_cp := taxon]

# - rm Ad. from taxon column
freshwaterecol <- freshwaterecol[!grep("Ad\\.", taxon), ]

# - move just CAPTIAL family taxa to new column
# & fill empty spots
freshwaterecol[
  grep(".*NAE|.*DAE", taxon),
  `:=`(
    family = taxon,
    taxon = NA_character_
  )
]
freshwaterecol[, family := copy_till_next_occur(x = family)]

# - Class/Phylum/Order entries into order_or_higher column
# & fill empty spots
freshwaterecol[
  grep("^[A-z]{1,}$|\\[", taxon),
  `:=`(
    order_or_higher = taxon,
    taxon = NA_character_
  )
]
freshwaterecol[, order_or_higher := copy_till_next_occur(x = order_or_higher)]

# Delete gen. sp
# TODO: How to handle later in analysis?
freshwaterecol[grep("Gen\\. sp\\.", taxon), taxon := NA_character_]

# - sort out all taxa with zero information
trait_cols <- grep("taxon.*|EU|genus|family|order.*",
  names(freshwaterecol),
  value = TRUE,
  invert = TRUE
)
no_info <- freshwaterecol[, apply(.SD, 1, function(y) {
  is.na(y) %>%
    sum()
}),
.SDcols = trait_cols
]
pos <- no_info == ncol(freshwaterecol[, ..trait_cols])
freshwaterecol <- freshwaterecol[!pos, ]

# - Proceed with creating genus col
# everything with sp. to genus column
freshwaterecol[
  grep("(?!.*ssp\\.)(?=[[:space:]]sp\\.)", taxon, perl = TRUE),
  `:=`(
    genus = taxon,
    taxon = NA_character_
  )
]

# - add genera from first name of species col
freshwaterecol[
  !is.na(taxon),
  genus := sub(
    "([A-z]{1,})([[:space:]])(.+)",
    "\\1", taxon
  )
]

# - create species column from taxon column
setnames(freshwaterecol, "taxon", "species")

# - cleaning:
# species col:
# rm Lv. and spp., Gr., Agg.
freshwaterecol[
  !is.na(species),
  species := sub(
    "[[:space:]]Lv\\.|[[:space:]]spp\\.|\\-Gr\\.|[[:space:]]Agg\\.",
    "", species
  )
]
freshwaterecol[grep("Thienemannimyia Gr\\.", species), species := NA]

# genus col:
# rm sp., Gen, Gr.-, (new sp. GR)
# ?possibly duplicates introduced
freshwaterecol[, genus := sub("[[:space:]]sp\\.|\\-Gr\\.|[[:space:]]Lv\\.", "", genus)]
freshwaterecol[, genus := sub(
  "[0-9].+|Pe 1",
  "", genus
)]

# family col:
freshwaterecol[, family := simpleCap(family)]

# order_or_higher column:
# create subfamily col for NAE taxa
# move chironomidae from order col
# rm [Ord: ...]
# move suborder to new column
freshwaterecol[grep("(?i)nae", family), `:=`(
  subfamily_or_tribe = family,
  family = NA_character_
)]
freshwaterecol[grep(".*dae", order_or_higher), `:=`(
  family = order_or_higher,
  order_or_higher = "Diptera"
)]
freshwaterecol[, order_or_higher := sub(
  "(\\[Ord\\:)(.+)(\\])",
  "\\2", order_or_higher
)]
freshwaterecol[grep("\\[UOrd\\:.+\\]", order_or_higher), `:=`(
  suborder = order_or_higher,
  order_or_higher = NA_character_
)]

# create class & phylum column
freshwaterecol[
  grep("\\[Kl\\:.+\\]", order_or_higher), `:=`(
    class = sub("(\\[Kl\\:)(.+)(\\])", "\\2", order_or_higher),
    order_or_higher = NA_character_
  )
]
freshwaterecol[
  grep("\\[Ph\\:.+\\]", order_or_higher), `:=`(
    phylum = sub("(\\[Ph\\:)(.+)(\\])", "\\2", order_or_higher),
    order_or_higher = NA_character_
  )
]

# taxonomic corrections ----
# few order are actually class, phylum, or suborder
freshwaterecol[
  order_or_higher %in% c(
    "Porifera",
    "Nemertini",
    "Coelenterata",
    "Nematomorpha",
    "Crustacea",
    "Bryozoa"
  ),
  `:=`(
    phylum = order_or_higher,
    order_or_higher = NA_character_
  )
]
freshwaterecol[
  order_or_higher %in% c(
    "Turbellaria",
    "Bivalvia",
    "Oligochaeta",
    "Hirudinea",
    "Polychaeta",
    "Gastropoda"
  ),
  `:=`(
    class = order_or_higher,
    order_or_higher = NA_character_
  )
]
freshwaterecol[
  grep(
    "Heteroptera|Hydrachnidia|Planipennia|Conchostraca", order_or_higher
  ),
  order_or_higher := case_when(
    order_or_higher == "Heteroptera" ~ "Hemiptera",
    order_or_higher == "Hydrachnidia" ~ "Trombidiformes",
    order_or_higher == "Planipennia" ~ "Neuroptera",
    order_or_higher == "Conchostraca" ~ "Onychura"
  )
]

# retrieve missing tax. information with taxizie

# order an higher levels
taxa <- freshwaterecol[
  is.na(order_or_higher) & !is.na(family),
  family
] %>% unique()
ID_gbif <- get_gbifid(taxa)

tax_classf <- classification(
  id = ID_gbif,
  db = "gbif"
)

tax_classf <- lapply(tax_classf, function(y) {
  as.data.table(y) %>%
    .[, .(rank, name)] %>%
    dcast(., ... ~ rank, value.var = "name")
})
tax_classf <- rbindlist(tax_classf, fill = TRUE)
setDT(tax_classf)
tax_classf[, "." := NULL]

# subfamilies
taxa_subfamily <- freshwaterecol[!is.na(subfamily_or_tribe), genus] %>%
  unique()
ID_subf_gbif <- get_gbifid(taxa_subfamily,
  rows = 1
)

taxa_subf_classf <- classification(
  id = ID_subf_gbif,
  db = "gbif"
)
taxa_subf_classf <- taxa_subf_classf[!is.na(taxa_subf_classf)]

taxa_subf_classf <- lapply(taxa_subf_classf, function(y) {
  as.data.table(y) %>%
    .[, .(rank, name)] %>%
    dcast(., ... ~ rank, value.var = "name")
})
taxa_subf_classf <- rbindlist(taxa_subf_classf, fill = TRUE)
taxa_subf_classf[, "." := NULL]
taxa_subf_classf <- taxa_subf_classf[!duplicated(genus), ]

# merge taxonomic information back to freshecol
freshwaterecol[tax_classf,
  `:=`(
    order = i.order,
    class = i.class,
    phylum = i.phylum
  ),
  on = "family"
]

merge(freshwaterecol,
  taxa_subf_classf[, .(genus, family, order)],
  by = "genus",
  all.x = TRUE
)

names(freshwaterecol)[duplicated(names(freshwaterecol))]

freshwaterecol[taxa_subf_classf,
  `:=`(
    family = i.family,
    order = i.order,
    class = i.class,
    phylum = i.phylum
  ),
  on = "genus"
]

taxa_subf_classf[genus == "Bythinella", ]
str(taxa_subf_classf)
freshwaterecol[is.na(family), ]
