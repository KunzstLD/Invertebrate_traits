# _________________________________________________________________________
# Preprocessing of Eu Trait DB ----
# _________________________________________________________________________

# read in
trait_EU <- readRDS(
  file.path(
    data_cleaned,
    "EU",
    "Trait_freshecol_2020_pp.rds"
  )
)
names(trait_EU)
trait_EU %>% dim

# Subset to relevant traits
trait_EU <- trait_EU[, .SD,
  .SDcols = names(trait_EU) %like% "^order$|^family$|genus|species|taxon.*|feed|loco|resp|volt|rep|size|temp|ID.*"
]

# Voltinism trait contains thermal conditions, change every string to 1
col <- grep("(?=volt.*)(?!.*tachet)", 
  names(trait_EU), value = TRUE, perl = TRUE)

trait_EU[, (col) := lapply(.SD, function(y) {
  ifelse(!is.na(y), 1, NA)
}), .SDcols = col]

# get trait columns and transform all to numeric
col <- grep("order|family|genus|species|taxon.*|ID.*",
  names(trait_EU),
  value = TRUE,
  invert = TRUE
)
trait_EU[, (col) := lapply(.SD, as.numeric),
  .SDcols = col
]

# select only taxa that have at least information for one trait
no_info <- trait_EU[, apply(.SD, 1, function(y) {
  is.na(y) %>%
    sum()
}),
.SDcols = col
]
pos <- no_info == ncol(trait_EU[, ..col])
trait_EU <- trait_EU[!pos, ]

# __________________________________________________________________________________________________
# Correct orders ----
# __________________________________________________________________________________________________

# Some families have gen. sp. -> rm
trait_EU[family %like% "gen. sp.", family := sub(" gen\\. sp\\.", "", family)] 
trait_EU[family %like% "gen 1", family := "Ceratopogonidae"]

# remove lv., but take the taxon 
trait_EU[family %like% "lv.", family := sub(" lv.", "", family)]

# remove ambigious Leuctridae
trait_EU <- trait_EU[!family %like% "Leuctridae/", ]

# Some Lepidoptera are wrongly classified (should be Diptera?)
# unique(na.omit(trait_EU[order == "Lepidoptera", family])) %>% 
#   .[21:25] %>% 
#   query_google()
trait_EU[family %in% c(
  "Dixidae",
  "Dolichopodidae",
  "Chaoboridae",
  "Culicidae",
  "Ceratopogonidae",
  "Cylindrotomidae",
  "Athericidae",
  "Blephariceridae",
  "Anthomyiidae",
  "Simuliidae",
  "Ptychopteridae",
  "Sciomyzidae",
  "Rhagionidae",
  "Psychodidae",
  "Scathophagidae",
  "Empididae",
  "Limoniidae",
  "Ephydridae",
  "Muscidae",
  "Tipulidae",
  "Thaumaleidae",
  "Tabanidae",
  "Stratiomyidae",
  "Syrphidae",
  "Scatophagidae",
  "Pediciidae"
), order := "Diptera"]

# __________________________________________________________________________________________________
# Consolidate duplicate taxa ----
# only for family-level
# species and genus duplicates have been introduced by rm e.g. "new Gen. sp." from some species
# has not been done now
# __________________________________________________________________________________________________

# binary columns -> aggr via max
# fuzzy coded columns -> aggr via median
cols_bin <- grep("(?=volt.*|resp.*|temp.*)(?!.*tachet)",
                 names(trait_EU),
                 value = TRUE,
                 perl = TRUE
)  
cols_fuzzy <- grep("(?=volt.*|resp.*|species|genus|family|order|taxon.*|ID.*)(?!.*tachet)",
                   names(trait_EU),
                   value = TRUE,
                   invert = TRUE,
                   perl = TRUE
)

# combine duplicated families
dupl_families <- trait_EU[is.na(species) & is.na(genus) & !is.na(family), ] %>%
  .[duplicated(family) | duplicated(family, fromLast = TRUE), family] %>%
  .[!duplicated(.)]

# agg via median except voltinism & respiration
trait_EU[is.na(species) & is.na(genus) & family %in% dupl_families,
  (cols_fuzzy) := lapply(.SD, median, na.rm = TRUE),
  .SDcols = cols_fuzzy,
  by = "family"
]
trait_EU[is.na(species) & is.na(genus) & family %in% dupl_families,
         (cols_bin) := lapply(.SD, max, na.rm = TRUE),
         .SDcols = cols_bin,
         by = "family"
]

# Don't rm the duplicates, some of the aggregated taxa
# were on subfamily-tribe level which will be used later 
# in the re-analysis of Szöcs et al. 2014
# code to remove:
# rm_dupl <- trait_EU[is.na(species) & is.na(genus) & !is.na(family), ] %>%
# .[duplicated(family), taxon_cp]
# trait_EU <- trait_EU[!(taxon_cp %in% rm_dupl),]

# __________________________________________________________________________________________________
# Data post-processing ----
# __________________________________________________________________________________________________

# transform all NA values to 0
# needed for harmonization later
for (j in col) {
  data.table::set(trait_EU, which(is.na(trait_EU[[j]])), j, 0)
}

# save as RDS
saveRDS(
  object = trait_EU,
  file = file.path(data_cleaned, "EU", "Trait_freshecol_2020_pp_for_harm.rds")
)
