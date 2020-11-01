
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

# - sort out all taxa with zero information
trait_cols <- grep("taxon.*|EU|genus|family|order.*|ID_AQEM",
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

# - Add gen. sp with ending dae/nae/ini to family column
# or subfamily column
freshwaterecol[grep(".*dae.*Gen\\..+", taxon), `:=`(
  family = taxon,
  taxon = NA_character_
)]
freshwaterecol[grep("(?i).*ini Gen\\. sp\\.|.*nae Gen\\. sp\\.", taxon), `:=`(
  subfamily_or_tribe = taxon,
  taxon = NA_character_
)]
freshwaterecol[grep("Gen\\. sp\\.", taxon), taxon := NA_character_]

# - Proceed with creating genus col
# add genera from first name of species col
# leave sp. into species col
freshwaterecol[!is.na(taxon), genus := sub("([A-z]{1,})([[:space:]])(.*)", "\\1", taxon)]

# new sp. GR are newly not yet in the studied area observed taxa
# If no values assigned take values from corresponding sp.!
target <- freshwaterecol[grepl("new sp\\. GR", taxon), sub("([A-z]{1,})([[:space:]])(sp\\.)(.*)", 
                                                           "\\1", taxon)]
target <- target[!duplicated(target)]
target <- paste(target, "sp\\.")

for(sp_taxa in target){
  assign <- freshwaterecol[taxon %like% sp_taxa, ][1, ..trait_cols]
  freshwaterecol[taxon %like% sp_taxa, (trait_cols) := assign]
}

# rm Lv. from taxa in taxon col
freshwaterecol[, taxon := sub(" Lv\\.", "", taxon)]

# - create species column from taxon column
setnames(freshwaterecol, "taxon", "species")

# - cleaning:
# species col: Thienemannimyia Gr. deleted from specie scol
freshwaterecol[grepl("Thienemannimyia Gr\\.", species), species := NA]

# genus col:
freshwaterecol[grepl("Bezzia-Gr\\.", genus), genus := "Bezzia"]

# family col:
# change capital letters to normal writing
# Ceratopogonidae new Gen sp. has already same values as Ceratopogonidae Gen sp.
# freshwaterecol[grepl("Ceratopogonidae", family), ..trait_cols]
# freshwaterecol[grepl("new Gen\\. sp\\.", family), ]
freshwaterecol[, family := simpleCap(family)]

# order_or_higher column:
# create subfamily col for NAE taxa
# move chironomidae from order col
# rm [Ord: ...]
# move suborder to new column
freshwaterecol[grep(".*dae", order_or_higher), `:=`(
  family = order_or_higher,
  order_or_higher = "Diptera"
)]
freshwaterecol[grep("nae$|ini$", family), `:=`(
  subfamily_or_tribe = family,
  family = NA_character_
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
