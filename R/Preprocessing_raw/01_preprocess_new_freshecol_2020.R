# --------------------------------------------------------------------------------------------------
#### Lead taxaexports from fwecol ####
# --------------------------------------------------------------------------------------------------
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

# --------------------------------------------------------------------------------------------------
#### Data processing ####
# --------------------------------------------------------------------------------------------------

# delete summary statistics (not part of DB)
output_db <- lapply(
  output_db,
  function(y) y[!(V1 %like% "Statistics|number.*"), ]
)

# rm col V1 in all but one data table
lapply(
  output_db[2:length(output_db)],
  function(y) {
    y[, c("V1", "EU", "ID-AQEM (ID-fwe)") := NULL]
  }
)

# bind
freshwaterecol <- Reduce(f = cbind, output_db)
setnames(freshwaterecol, "V1", "taxon")
freshwaterecol[, EU := NULL]

# - add AQEM codes:
aqem_path <- file.path(data_raw, "AQEM.csv")
ind <- grep("Taxon", readLines(aqem_path))

# read in aqem
aqem <- fread(aqem_path,
              skip = ind,
              sep = ";",
              header = TRUE,
              na.strings = (""),
              fill = TRUE
)
aqem <- aqem[!(V1 %like% "Statistics|number.*"), ]
setnames(aqem, "V1", "taxon")

# merge ID
freshwaterecol[aqem, ID_AQEM := `i.ID-AQEM (ID-fwe)`, on = "taxon"]

# --------------------------------------------------------------------------------------------------
#### Trait preparation ####
# Some columns have the same colname 
# that's because sometimes the same traits are named the same way, although they originate
# from different databases. From these duplicate colnames, the second duplicate always 
# originates from the tachet database.
# --------------------------------------------------------------------------------------------------

#### pH preference ####
# Explanation:  ph_acidic:      acidic - pH < 7
#               ph_neutral_alk: neutral to alkaline - ph = 7
#               ph_ind:         indifferent - no specific preference
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
  paste0("ph_", c(
    "&le; 4",
    "> 4-4.5",
    "> 4.5-5",
    "> 5-5.5",
    "> 5.5-6",
    "> 6"
  ), 
  "_tachet")
)

#### Temperature preference ####
# Explanation:  
#             temp_coldsteno: cold stenotherm (cos); preference for a small cold temperature range (below 10°C)
#             temp_warmsteno: warm stenotherm	(was);	preference for a small warm temperature range (above 18°C)
#             temp_euryt: eurytherm	(eut)	no specific preference, wide temperature range
#             temp_psychrophil_tachet: (psy)	psychrophilic (<15°C)
#             temp_thermophil_tachet: (the)	thermophilic (>15°C)
#             temp_euryt_tachet: (eut)	eurythermic
setnames(
  freshwaterecol, c(
    "cos",
    "was",
    "eut"
  ),
  paste0("temp_", c(
    "coldsteno",
    "warmsteno",
    "euryt"
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
    "temp_",
    c(
      "psychrophil",
      "thermophil",
      "euryt"
    ), 
    "_tachet"
  )
)

#### Salinity preference ####
# Explanation: ?
#             bws: brackish water sea (salt content 0.5 prom. - 34,7 prom.)
#             bwi: brackish water inland (salt content 0.5 prom. - 34,7 prom.)
#             brw: fresh water
#             frw: brackish water
setnames(freshwaterecol,
         c("bws",
           "bwi"),
         c("sal_bwsea",
           "sal_bw_inland")
)
setnames(freshwaterecol,
         c("brw",
           "frw"),
         paste0("sal_",
                c("bwater",
                  "fwater"),
                "_tachet"))

#### Feeding mode ####
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
# 				      feed_absorber:        gather organisms direct uptake through tegument
# 				      feed_deposit: 		    feed on small debris on surface of fine sediments
# 				      feed_scraper		      grazing fine organic particles, ...
# 				      feed_piercer:         piercing plants or animals
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
      "predator",
      "parasite",
      "other"
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
    "feed_",
    c(
      "absorber",
      "deposit_feeder",
      "shredder",
      "scraper",
      "filter",
      "piercer",
      "predator",
      "parasite"
    ),
    "_tachet"
  )
)

#### Locomotion ####
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
    "locom_",
    c(
      "flier",
      "surface_swimmer",
      "full_water_swimmer",
      "crawler",
      "burrower",
      "interstitial",
      "temp_attached",
      "perm_attached"
    ),
    "_tachet"
  )
)

#### Respiration ####
# Explanation:  resp_tegument:  tegument - respiration through body surface
#               resp_gill:      gill
#               resp_plastron:  plastron
#               resp_spiracle:  spiracle (aerial)
#               resp_vesicle:   hydrostatic vesicle (aerial)
#               resp_tapping:   tapping of air stores of aquatic plants
#               resp_surface:   extension/excursion to surface - respiration of atmospheric oxygen
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
    "resp_",
    c(
      "tegument",
      "gill",
      "plastron",
      "spiracle",
      "vesicle"
    ),
    "_tachet"
  )
)

#### Aquatic stages ####
# Explanation:  stage_egg:    aquatic stage as egg
#               stage_larva:  aquatic stage as larva
#               stage_nymph:  aquatic stage as nymph
#               stage_pupa:   aquatic stage as pupa
#               stage_adult:  aquatic stage as adult
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
    "egg",
    "larva",
    "nymph",
    "adult"
  ),
  paste0(
    "stage_",
    c(
      "egg",
      "larva",
      "nymph",
      "adult"
    ),
    "_tachet"
  )
)

#### Reproductive cycles per year (voltinism) ####
# Explanation:  volt_semi:  one generation in two years
#               volt_uni:   one generation per year
#               volt_bi:    two generations per year
#               volt_tri:   three generations per year
#               volt_multi: more than three generations per year
#               volt_flex:  flexible number of life cycles per year
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
    "voltinism_",
    c(
      "semi",
      "uni",
      "multi"
    ),
    "_tachet"
  )
)

#### Reproduction ####
# Explanation:  rep_ovovipar:     ovovivipar - eggs remain within the mother's body until they hatch
#               rep_egg_free_iso: free isolated eggs - separate eggs are laid down in the water freely
#               rep_egg_cem_iso:  cemented isolated eggs - separate eggs are laid down and fixed
#               rep_clutch_fixed: fixed clutches - groups of eggs are laid down and fixed
#               rep_clutch_free:  free clutches - groups of eggs are laid down in the water freely
#               rep_clutch_veg:   clutches in vegetation - groups of eggs are laid down in the vegetation
#               rep_clutch_ter:   terrestrial clutches - groups of eggs are laid down in the riparian zone
#               rep_asexual:      asexual - reproduction without fertilisation
#               rep_parasitic:    parasitic - reproduction within a host
setnames(
  freshwaterecol,
  c(
    "ovo",
    "fie",
    "cie",
    "fic",
    "frc",
    "vec",
    "tec",
    "ase",
    "pas"
  ),
  paste0(
    "rep_",
    c(
      "ovovivipar",
      "egg_free_iso",
      "egg_cem_iso",
      "clutch_fixed",
      "clutch_free",
      "clutch_veg",
      "clutch_ter",
      "asexual",
      "parasitic"
    )
  )
)
setnames(
  freshwaterecol,
  c(
    "ovo",
    "fie",
    "cie",
    "fic",
    "frc",
    "vec",
    "tec",
    "ase"
  ),
  paste0(
    "rep_",
    c(
      "ovovivipar",
      "egg_free_iso",
      "egg_cem_iso",
      "clutch_fixed",
      "clutch_free",
      "clutch_veg",
      "clutch_ter",
      "asexual"
    ),
    "_tachet"
  )
)

#### Dispercal capacity ####
# Explanation:  dispersal_high:     high dispersal capacity
#               dispersal_low:      low dispersal capacity
#               dispersal_unknown:  unknown dispersal capacity
setnames(
  freshwaterecol,
  c(
    "hig",
    "low",
    "unk"
  ),
  paste0(
    "dispersal_",
    c(
      "high",
      "low",
      "unknown"
    )
  )
)
setnames(
  freshwaterecol,
  c(
    "aquatic passive",
    "aquatic active",
    "aerial passive",
    "aerial active"
  ),
  paste0(
    "dispersal_",
    c(
      "aquatic passive",
      "aquatic active",
      "aerial passive",
      "aerial active"
    ),
    "_tachet"
  )
)

#### Size ####
setnames(
  freshwaterecol,
  c(
    "&le; 0.25 cm",
    "> 0.25-0.5 cm",
    "> 0.5-1 cm",
    "> 1-2 cm",
    "> 2-4 cm",
    "> 4-8 cm",
    "> 8 cm"
  ),
  paste0(
    "size_",
    c(
      "&le; 0.25 cm",
      "> 0.25-0.5 cm",
      "> 0.5-1 cm",
      "> 1-2 cm",
      "> 2-4 cm",
      "> 4-8 cm",
      "> 8 cm"
    ),
    "_tachet"
  )
)
