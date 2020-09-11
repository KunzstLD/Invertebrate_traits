data_raw <- "./Data/EU/Raw_freshecol/2020"


library(data.table)
library(dplyr)

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
  function(y) y[, V1 := NULL]
)

# cbind
freshwaterecol <- Reduce(f = cbind, output_db)
setnames(freshwaterecol, "V1", "taxon")

# taxonomic preparation:
# copy of taxon column
freshwaterecol[, taxon_cp := taxon]

# - move DAE and similar into family column
freshwaterecol[
  grep(".*NAE|.*DAE|.*dae|.*nae", taxon),
  `:=`(
    family = taxon,
    taxon = NA_character_
  )
]

# - Class/Phlyum/Order entries into order_or_higher column
freshwaterecol[grep(
  "^[A-z]{1,}$|\\[",
  taxon
), `:=`(
  order_or_higher = taxon,
  taxon = NA_character_
)]

# - rm Ad. from taxon column
freshwaterecol <- freshwaterecol[!grep("Ad\\.", taxon), ]

# - everything with sp. to genus column
freshwaterecol[
  grep("(?!.*ssp\\.)(?=[[:space:]]sp\\.)", taxon, perl = TRUE),
  `:=`(
    genus = taxon,
    taxon = NA_character_
  )
]

# - create species column
setnames(freshwaterecol, "taxon", "species")

# - add genera from first name of species col
freshwaterecol[
  !is.na(species),
  genus := sub(
    "([A-z]{1,})([[:space:]])(.+)",
    "\\1", species
  )
]

# - clean:
# species col:
# rm Lv. and spp.,
# ?Gr., Agg., ""JDS2""
freshwaterecol[
  !is.na(species),
  species := sub("Lv\\.|spp\\.", "", species)
]

# genus col:
# rm sp., Gen, Gr.-
# possibly duplicates introduced
freshwaterecol[, genus := sub("([A-z]{1,})([[:space:]])(.+)", "\\1", genus)] 
freshwaterecol[, genus := sub("([A-z]{1,})(\\-)(.+)", "\\1", genus)] 

# - handle family col
# NAs need to be filled until next upper case word comes

# - handle order_or_higher column
# check for Klass, order and so on
# fill from one taxon to the next
