# ____________________________________________________________________________
#### Harmonize Tachet data ####
# ____________________________________________________________________________

# read in
tachet <- readRDS(file.path(data_cleaned, "EU", "Trait_Tachet_pp.rds"))

# create taxa column (for merges)
tachet[, taxa := coalesce(species, genus, family, order)]

# ____________________________________________________________________________
# pH 
# Modify ph preference
# rule for combining ph fuzzy codes:
# rounded rowMean
# ____________________________________________________________________________
tachet[, `:=`(ph_acidic = apply(.SD, 1, max)),
       .SDcols = c("Ph_4_t", "Ph_445_t", "Ph_455_t", "Ph_555_t", "Ph_556_t", "Ph_6_t")
       ] %>% 
  .[, ph_acidic := as.integer(ph_acidic)]

# del other ph columns
tachet[, c("Ph_4_t", "Ph_445_t", "Ph_455_t", "Ph_555_t", "Ph_556_t", "Ph_6_t") := NULL]

# _________________________________________________________________________
# Voltinism
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________

# _________________________________________________________________________
# Size
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________
setnames(tachet, "Size_4", "size_medium")
tachet[, size_small := apply(.SD, 1, max),
       .SDcols = c("Size_1", "Size_2", "Size_3")]
tachet[, size_large := apply(.SD, 1, max),
       .SDcols = c("Size_5", "Size_6", "Size_7")]
# del other size columns
tachet[, c(
  "Size_1", "Size_2", "Size_3", "Size_5",
  "Size_6", "Size_7"
) := NULL]

# _________________________________________________________________________
# Locomotion
# locm_swim:  swimmer, scater (active & passive)
# locm_crawl: crawlers, walkers & sprawler
# locm_burrow: burrower
# locm_sessil: sessil (attached)

# locom_flier: ignored, only relevant for adult individuals
# _________________________________________________________________________
tachet[, locom_swim := apply(.SD, 1, max),
  .SDcols = c("locom_swim_full", "locom_swim_dive")
]

tachet[, locom_sessil := apply(.SD, 1, max),
  .SDcols = c("locom_sessil", "locom_sessil_temp")
]

tachet[, locom_burrow := apply(.SD, 1, max),
       .SDcols = c("locom_burrow", "loc_interstitial_t")]

tachet[, c(
  "locom_swim_full", 
  "locom_swim_dive",
  "locom_sessil_temp",
  "loc_interstitial_t",
  "loc_flier_t"
) := NULL]

# _________________________________________________________________________
# Respiration
# resp_teg: cutaneous/tegument
# resp_gil: gills
# resp_pls_spi: plastron & spiracle
  # plastron & spiracle often work together in respiratory systems of aq. insects
  # Present in insects with open tracheal systems -> breathe oxygen from the air
  # -> Different tolerances to low oxygen compared to insects with tegument resp and gills
# TODO: currently respiration vesicle is deleted -> only two entries
# _________________________________________________________________________
tachet[, resp_pls_spi := apply(.SD, 1, max),
        .SDcols = c("resp_pls", "resp_spi")]
tachet[, c("resp_ves",
           "resp_pls",
           "resp_spi") := NULL]
# _________________________________________________________________________
# Feeding mode
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite
# _________________________________________________________________________

# feed_piercer_t -> cannot ne harmonized easily
# feeding strictly coding based on strategies(taking into account the morphology
# of mouth parts etc.) 
# 25 taxa will anyways not end up in the final DBs
# tachet_piercer_taxa <- tachet[feed_piercer_t > 0, coalesce(species, genus, family, order)]
# tachet[feed_piercer_t > 0,] %>%
#   .[!species %in% Trait_EU[species %in% tachet_piercer_taxa, species],
#     .(
#       species,
#       genus,
#       family,
#       order,
#       feed_active_filter_abs,
#       feed_active_filter,
#       feed_gatherer,
#       feed_shredder,
#       feed_scraper,
#       feed_predator,
#       feed_parasite,
#       feed_piercer_t
#     )] %>%
#   .[order(-feed_piercer_t),] %>%
#   write.csv(.,
#             file = file.path(data_out, "taxa_feed_piercer_t.csv"),
#             row.names = FALSE)

# coding from PUP for piercer_t
piercer_pup <- fread(file.path(data_in, "EU", "taxa_feed_piercer_PUP.csv"))
piercer_pup[, taxa := coalesce(genus, family, order)]

# merge comments from PUP back
tachet[piercer_pup,
       `:=`(
         feed_active_filter_abs = i.feed_active_filter_abs,
         feed_active_filter = i.feed_active_filter,
         feed_gatherer = i.feed_gatherer,
         feed_shredder = i.feed_shredder,
         feed_scraper = i.feed_scraper,
         feed_predator = i.feed_predator,
         feed_parasite = i.feed_parasite,
         feed_piercer_t = i.feed_piercer_t,
         piercer_comment = `i.comment (only for the trait category \"\"piercer\"\")`
       ),
       on = "taxa"]

# filterer
tachet[, feed_filter := apply(.SD, 1, max),
       .SDcols = c("feed_active_filter", "feed_active_filter_abs")]

# herbivores
setnames(tachet,
         "feed_scraper",
         "feed_herbivore")

# assign affinities from feed_pericer_t to either predator, parsite or herbivore
tachet$piercer_comment %>% unique
tachet[grep("predator\\/parasite", piercer_comment),  `:=`(feed_predator = feed_piercer_t,
                                                           feed_piercer_t = 0)]
tachet[grep("(?=.*predator)(?!.*parasite)(?!.*microphyte)",
            piercer_comment,
            perl = TRUE), `:=`(feed_predator = feed_piercer_t,
                               feed_piercer_t = 0)] 

tachet[grep("^microinvertebrate piercer$", piercer_comment), `:=`(feed_predator = feed_piercer_t,
                                                                  feed_piercer_t = 0)]

tachet[grep("parasite", piercer_comment), `:=`(feed_parasite = feed_piercer_t,
                                               feed_piercer_t = 0)]

tachet[grep("microphyte.*predators.*", piercer_comment), `:=`(feed_predator = feed_piercer_t,
                                                              feed_piercer_t = 0)]

tachet[grep("(?=.*macrophyte|microphyte)(?!.*predators)",
            piercer_comment,
            perl = TRUE),
       `:=`(feed_herbivore = feed_piercer_t,
            feed_piercer_t = 0)]

# del columns
tachet[, c("feed_active_filter",
           "feed_active_filter_abs",
           "feed_piercer_t",
           "piercer_comment") := NULL]

# _________________________________________________________________________
# Oviposition
# ovip_aqu: Reproduction via aquatic eggs
# ovip_ter: Reproduction via terrestric eggs
# ovip_ovo: Reproduction via ovoviparity
# _________________________________________________________________________
setnames(tachet,
         c("rep_clutch_terr", "rep_ovovipar"),
         c("ovip_ter", "ovip_ovo"))
tachet[, ovip_aqu := apply(.SD, 1, max),
       .SDcols = c(
         "rep_egg_free_iso",
         "rep_egg_cem_iso",
         "rep_clutch_fixed",
         "rep_clutch_free",
         "rep_clutch_veg"
       )]
# del
tachet[, c("rep_egg_free_iso",
           "rep_egg_cem_iso",
           "rep_clutch_fixed",
           "rep_clutch_free",
           "rep_clutch_veg", 
           "rep_asexual") := NULL]

# _________________________________________________________________________
#### Body form ####
# bf_streamlined: streamlined/fusiform
# bf_flattened: flattened (dorso-ventrally)
# bf_cylindrical: cylindrical/tubular 
# bf_spherical: spherical
# Add BF data from PUP
# _________________________________________________________________________

# Philippe Polateras classification
body_form_pup <- fread(file.path(data_missing, 
                                 "Body_form_EU_NOA", 
                                 "body_form_polatera_EU_NOA.csv"))

# change "," to "." and convert bf columns to numeric
cols <- c("streamlined", "flattened", "cylindrical", "spherical")
body_form_pup[, (cols) := lapply(.SD, function(y) {
  sub("\\,", ".", y) %>%
    as.numeric(.)
}),
.SDcols = cols]

# subset to EU data
bf_EU <- body_form_pup[grepl("EU.*", array),]

# merge to tachet data
# species-level:
tachet[bf_EU[!is.na(species),],
       `:=`(
         bf_flattened = i.flattened,
         bf_spherical = i.spherical,
         bf_cylindrical = i.cylindrical,
         bf_streamlined = i.streamlined
       ),
       on = "species"]

# genus-level:
bf_EU_genus <- bf_EU[is.na(species) ,]
tachet[bf_EU_genus,
         `:=`(
           bf_flattened = i.flattened,
           bf_spherical = i.spherical,
           bf_cylindrical = i.cylindrical,
           bf_streamlined = i.streamlined
         ),
         on = "genus"]

# _________________________________________________________________________
#### Pattern of development ####
# Holometabolous or hemimetabolous
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

tachet[, `:=`(
  dev_hemimetabol = ifelse(order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(order %in% holometabola, 1, 0)
)]

# ---------------------------------------------------
#### Normalization Tachet ####
# ---------------------------------------------------
tachet <- normalize_by_rowSum(
  x = tachet,
  non_trait_cols = c(
    "group",
    "family",
    "genus",
    "species",
    "order",
    "taxa"
  )
)

# del group column
tachet[, "group" := NULL]

# 
saveRDS(
  object = tachet,
  file = file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds")
)
