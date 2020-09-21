# ------------------------------------------------------------------------------
#### Preprocessing of Eu Trait DB ####
# ------------------------------------------------------------------------------

# read in
trait_EU <- readRDS(
  file.path(
    data_cleaned,
    "EU",
    "Trait_freshecol_2020_pp.rds"
  )
)

# Subset to relevant traits
trait_EU <- trait_EU[, .SD,
  .SDcols = names(trait_EU) %like% "^order$|^family$|genus|species|taxon.*|feed|loco|resp|volt|rep|size"
]

# Voltinism trait contains thermal conditions, change every string to 1
col <- grep("^volt.*", names(trait_EU), value = TRUE)
trait_EU[, (col) := lapply(.SD, function(y) {
  ifelse(!is.na(y), 1, NA)
}), .SDcols = names(trait_EU) %like% "^volt.*"]


# get trait columns and transform all to numeric
cols <- grep("order|family|genus|species|taxon.*",
  names(trait_EU),
  value = TRUE,
  invert = TRUE
)
trait_EU[, (cols) := lapply(.SD, as.numeric),
  .SDcols = !names(trait_EU) %like% "order|family|genus|species|taxon.*"
]

# select only taxa that have at least information for one trait
no_info <- trait_EU[, apply(.SD, 1, function(y) {
  is.na(y) %>%
    sum()
}),
.SDcols = cols
]
pos <- no_info == ncol(trait_EU[, ..cols])
trait_EU <- trait_EU[!pos, ]

# aggr. duplicated species
dupl_species <- trait_EU[!is.na(species), ] %>%
  .[duplicated(species) | duplicated(species, fromLast = TRUE), species] %>%
  .[duplicated(.)]

# agg via median
trait_EU[species %in% dupl_species,
  (cols) := lapply(.SD, median, na.rm = TRUE),
  .SDcols = cols,
  by = "species"
]

# rm duplicates
trait_EU <- trait_EU[!(!is.na(species) & duplicated(species)), ]

# aggr. duplicated genus
dupl_genus <- trait_EU[is.na(species) & !is.na(genus), ] %>%
  .[duplicated(genus) | duplicated(genus, fromLast = TRUE), genus] %>%
  .[!duplicated(.)]

# agg via median
trait_EU[is.na(species) & genus %in% dupl_genus,
  (cols) := lapply(.SD, median, na.rm = TRUE),
  .SDcols = cols,
  by = "genus"
]

# rm duplicates
rm_dupl <- trait_EU[is.na(species) & !is.na(genus), ] %>%
  .[duplicated(genus), taxon_cp]
trait_EU <- trait_EU[!(taxon_cp %in% rm_dupl), ]

# aggr. duplicated families
dupl_families <- trait_EU[is.na(species) & is.na(genus) & !is.na(family), ] %>%
  .[duplicated(family) | duplicated(family, fromLast = TRUE), family] %>%
  .[!duplicated(.)]

# agg via median
trait_EU[is.na(species) & is.na(genus) & family %in% dupl_families,
  (cols) := lapply(.SD, median, na.rm = TRUE),
  .SDcols = cols,
  by = "family"
]

# rm duplicates
rm_dupl <- trait_EU[is.na(species) & is.na(genus) & !is.na(family), ] %>% 
.[duplicated(family), taxon_cp]
trait_EU <- trait_EU[!(taxon_cp %in% rm_dupl),]

# transform all NA values to 0
# needed for harmonization later
for (j in cols) {
  data.table::set(trait_EU, which(is.na(trait_EU[[j]])), j, 0)
}

# taxonomical corrections
# Microhydra sowerbyi is actually a freshwater jellyfish
# family: Olindiidae, order: Hydrozoa
trait_EU[grepl("Microhydra sowerbyi", species), `:=`(
  family = "Olindiidae",
  order = "Hydrozoa"
)]

# Bythinella
trait_EU[genus == "Bythinella", family := "Hydrobiidae"]

# save as RDS
saveRDS(
  object = trait_EU,
  file = file.path(data_cleaned, "EU", "Trait_freshecol_2020_pp_for_harm.rds")
)
