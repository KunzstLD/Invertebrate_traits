
# link to raw data
data_raw <- "./Data/EU/Raw_freshecol/2020"

# packages
library(data.table)
library(dplyr)

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

# taxonomic preparation:
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

# rm
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
  species := sub("Lv\\.|spp\\.|Gr\\.|Agg\\.", "", species)
]

# genus col:
# rm sp., Gen, Gr.-, (new sp. GR)
# ?possibly duplicates introduced
freshwaterecol[, genus := sub("sp\\.|Gr\\.|Lv\\.", "", genus)]
freshwaterecol[, genus := sub(
  "[0-9].+|Pe 1",
  "", genus
)]

# order_or_higher column:
# create subfamily col for NAE taxa
# move chironomidae from order col
# rm [Ord: ...]
# move suborder to new column
freshwaterecol[grep("(?i)nae", family), `:=`(
  Subfamily_or_tribe = family,
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