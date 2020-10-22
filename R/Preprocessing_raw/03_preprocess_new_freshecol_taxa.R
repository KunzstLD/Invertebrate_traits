# #######################################
# retrieve missing tax. information ----
# #######################################

# - taxonomic corrections:

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

# - retrive information with taxize:
# for order and higher levels
taxa <- freshwaterecol[
  is.na(order_or_higher) & !is.na(family),
  family
] %>% unique()
ID_gbif <- get_gbifid(taxa, rows = 1)
saveRDS(ID_gbif, file = "./R/Preprocessing_raw/Cache/ID_gbif_family.rds")
# ID_gbif <- readRDS(file = "./R/Preprocessing_raw/Cache/ID_gbif_family.rds")

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


# for subfamilies
# - first, take the genus (easier to find and match)
taxa_subfamily <- freshwaterecol[!is.na(subfamily_or_tribe), genus] %>%
  unique()
ID_subf_gbif <- get_gbifid(taxa_subfamily,
  rows = 1
)
saveRDS(ID_subf_gbif,
  file = "./R/Preprocessing_raw/Cache/ID_gbif_subfamily.rds"
)
# ID_subf_gbif <- readRDS(file = "./R/Preprocessing_raw/Cache/ID_gbif_subfamily.rds")

taxa_subf_classf <- classification(
  id = ID_subf_gbif,
  db = "gbif"
)
taxa_subf_classf <- taxa_subf_classf[!is.na(taxa_subf_classf)] %>%
  .[!duplicated(.)]
taxa_subf_classf <- lapply(taxa_subf_classf, function(y) {
  as.data.table(y) %>%
    .[, .(rank, name)] %>%
    dcast(., ... ~ rank, value.var = "name")
})
taxa_subf_classf <- rbindlist(taxa_subf_classf, fill = TRUE)
taxa_subf_classf[, "." := NULL]


# subfamilies where no genus
# information was available
taxa_subf_2 <- freshwaterecol[
  !is.na(subfamily_or_tribe) & is.na(genus) & is.na(family),
  subfamily_or_tribe
] %>%
  sub(" Gen\\. sp\\.", "", .) %>%
  unique()
ID_subf2_gbif <- get_tsn(taxa_subf_2,
  rows = 1
)
saveRDS(ID_subf2_gbif,
  file = "./R/Preprocessing_raw/Cache/ID_gbif_subfamily_2.rds"
)
# ID_subf2_gbif <- readRDS(file = "./R/Preprocessing_raw/Cache/ID_gbif_subfamily_2.rds")

taxa_subf2_classf <- classification(
  id = ID_subf2_gbif,
  db = "itis"
)
taxa_subf2_classf <- taxa_subf2_classf[!is.na(taxa_subf2_classf)] %>%
  .[!duplicated(.)]
taxa_subf2_classf <- lapply(taxa_subf2_classf, function(y) {
  as.data.table(y) %>%
    .[, .(rank, name)] %>%
    dcast(., ... ~ rank, value.var = "name")
})
taxa_subf2_classf <- rbindlist(taxa_subf2_classf, fill = TRUE)
taxa_subf2_classf[, "." := NULL]


# merge taxonomic information back to freshecol
freshwaterecol[tax_classf,
  `:=`(
    order_or_higher = i.order,
    class = i.class,
    phylum = i.phylum
  ),
  on = "family"
]
freshwaterecol[taxa_subf_classf,
  `:=`(
    family = i.family,
    order_or_higher = i.order,
    class = i.class,
    phylum = i.phylum
  ),
  on = "genus"
]
freshwaterecol[taxa_subf2_classf[!is.na(subfamily)],
  `:=`(
    family = i.family,
    order_or_higher = i.order
  ),
  on = c(subfamily_or_tribe = "subfamily")
]

# rename order_or_higher col
setnames(
  freshwaterecol,
  "order_or_higher",
  "order"
)

# manual taxonomic corrections:
freshwaterecol[genus == "Orientalina", `:=`(
  family = "Hydrobiidae",
  order = "Littorinimorpha"
)]
freshwaterecol[genus == "Normandia", family := "Elmidae"]
freshwaterecol[grep("Simulium", genus), `:=`(
  family = "Simuliidae",
  order = "Diptera"
)]
freshwaterecol[grep("Onychogomphus", genus), family := "Gomphidae"]
freshwaterecol[grep("Paradelphomyia", genus), `:=`(
  family = "Limoniidae",
  order = "Diptera"
)]
freshwaterecol[genus == "Culiseta", `:=`(
  family = "Culicidae",
  order = "Diptera"
)]
freshwaterecol[subfamily_or_tribe == "Trochetinae", `:=`(
  family = "Erpobdellidae",
  order = "Arhynchobdellida"
)]
freshwaterecol[subfamily_or_tribe == "Palpomyiinae", `:=`(
  family = "Ceratopogonidae",
  order = "Diptera"
)]
freshwaterecol[
  subfamily_or_tribe == "Argyronetinae",
  family := "Cybaeidae"
]
freshwaterecol[
  subfamily_or_tribe == "Eucyclopinae",
  `:=`(family = "Cyclopidae", order = "Cyclopoida")
]
freshwaterecol[
  subfamily_or_tribe == "Acentropinae",
  `:=`(family = "Crambidae", order = "Lepidoptera")
]
freshwaterecol[
  subfamily_or_tribe == "Anophelinae",
  `:=`(family = "Culicidae", order = "Diptera")
]
freshwaterecol[
  subfamily_or_tribe == "Pediciinae",
  `:=`(family = "Pediciidae", order = "Diptera")
]
freshwaterecol[
  subfamily_or_tribe == "Platycnemidinae",
  `:=`(family = "Pediciidae", order = "Odonata")
]
freshwaterecol[
  subfamily_or_tribe == "Veliinae",
  `:=`(family = "Veliidae", order = "Hemiptera")
]

# TODO: some Ceratopogonidae have been classified as Lepidoptera
# Not sure where this "bug" got introduced
freshwaterecol[family == "Ceratopogonidae", order := "Diptera"]

# spelling error
freshwaterecol[grep("Marcromia", genus), `:=`(
  species = "Macromia splendens",
  genus = "Macromia",
  family = "Macromiidae",
  taxon_cp = "Macromia splendens"
)]

# taxonomical corrections
# Microhydra sowerbyi is actually a freshwater jellyfish
# family: Olindiidae, order: Hydrozoa
freshwaterecol[grepl("Microhydra sowerbyi", species), `:=`(
  family = "Olindiidae",
  order = "Hydrozoa"
)]

# Bythinella
freshwaterecol[genus == "Bythinella", family := "Hydrobiidae"]

# Tipulidae is order Diptera
freshwaterecol[family == "Tipulidae", order := "Diptera"]

# set column order
setcolorder(
  freshwaterecol,
  c(
    "species",
    "genus",
    "subfamily_or_tribe",
    "family",
    "order",
    "suborder",
    "class",
    "phylum",
    "taxon_cp"
  )
)

saveRDS(
  freshwaterecol,
  "./Cleaned_data/EU/Trait_freshecol_2020_pp.rds"
)
