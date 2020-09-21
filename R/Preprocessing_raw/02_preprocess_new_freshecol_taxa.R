
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

# - Add gen. sp with endign dae/nae/ini to family column
# or subfamily column
# delete the rest frp, genus col
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
freshwaterecol[, genus := sub(
  "[[:space:]]sp\\.|\\-Gr\\..+|[[:space:]]Lv\\.|[[:space:]]sp\\.[[:space:]]Lv\\.",
  "", genus
)]
freshwaterecol[, genus := sub(
  "[[:space:]][0-9].+|[[:space:]]Pe[[:space:]]1",
  "", genus
)]

# family col:
freshwaterecol[, family := sub(
  "[[:space:]]Gen\\.[[:space:]].+|\\,.*|[[:space:]]Gen.*[[:digit:]]",
  "", family
)]
freshwaterecol[, family := sub(
  "[[:space:]]\\(.+\\)[[:space:]]sp\\.",
  "", family
)]
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