# _________________________________________________________________________
# South African trait database by
# Odume et al.
# TODO: Create a second table with the comments for Body shape, ...
# _________________________________________________________________________

# some warnings are thrown by R which should not concern us for now
Trait_SA <- read_xlsx(
  file.path(
    data_in,
    "SA",
    "A_trait_database_for_SA_macroinvertebrates_submited.xlsx"
  ),
  sheet = 2,
  range = "A12:CK3616"
)
setDT(Trait_SA)

# Select grouping features we're interested in:
# Voltinism
# Body Size
# Respiration
# Feeding mode
# Reproduction
# Body form/shape
# Locomotion
pat <-
  c(
    "Order|Family|Genus|Species|Taxon.*|^Max\\. body size \\(mm\\)$|^Body shape$|^Mobility$|^Attachment mechanism$|FFG|Oviposition|Voltinism|^Respiration$"
  )
Trait_SA <- Trait_SA[, .SD, .SDcols = patterns(pat)]

# Use only information on family-level or higher taxonomic resolution
Trait_SA <- Trait_SA[`Taxon Rank` != "Order", ]

# But Genus + Species in Species col
Trait_SA[!is.na(Species), Species := paste(Genus, Species)]

# Create unique identifier for 
Trait_SA[, unique_ID := 1:nrow(Trait_SA)]


# Hydrophilidae: Body shape, Oviposition behaviour, univoltine
Trait_SA[Family == "Hydrophilidae" & is.na(Genus) & is.na(Species), ]

# Traits:
#   Spelling issues:
#   - walker and Walker 
#   - "Tegument/Cutaneaous", "Tegument/Cutaneous"
Trait_SA[Mobility == "walker", Mobility := "Walker"]
Trait_SA[Respiration == "Tegument/Cutaneaous", Respiration := "Tegument/Cutaneous"]

# Long format
Trait_SA_lf <- melt(
  Trait_SA,
  id.vars = c(
    "Order",
    "Family",
    "Genus",
    "Species",
    "Taxon",
    "Taxon Rank",
    "unique_ID"
  ),
  value.name = "Trait",
  variable.name = "Trait_group"
)
Trait_SA_lf <- Trait_SA_lf[!grepl(".+comment", Trait_group), ]
Trait_SA_lf <- Trait_SA_lf[!is.na(Trait), ]

# combine trait name & category names
Trait_SA_lf[, Trait := paste0(Trait_group, "_", Trait)]
Trait_SA <- dcast(Trait_SA_lf, 
      formula = Taxon + Order + Family + Genus + Species + unique_ID ~ Trait, 
      fun.aggregate = length)

# Handle Duplicates ----
# combine by taking the maximum value (values are 0 or 1) for a duplicate species
trait_col <- grep("unique_ID|Species|Genus|Family|Order|Taxon",
                  names(Trait_SA),
                  value = TRUE, 
                  invert = TRUE)

## Duplicated Species ----
dupl_spec <- Trait_SA[!is.na(Species), ] %>% 
  .[duplicated(Species), Species]

# "Psychomyiellodes dentatus" in Taxon but "Ecnomus dentatus" in Species column
Trait_SA[Taxon == "Psychomyiellodes dentatus", Species := "Psychomyiellodes dentatus"]

# Information seems to be the same, 
# View(Trait_SA[Species %in% dupl_spec, ] %>% .[order(Species), ])
# except for "Afronurus ethiopicus", where one entry has "Grazer 2" as FFG
Trait_SA[Species == "Afronurus ethiopicus", 
         (trait_col) := lapply(.SD, max),
         .SDcols = trait_col]

# rm duplicate species entries
dupl_unique_id <- Trait_SA[duplicated(Species) & !is.na(Species), unique_ID]
Trait_SA <- Trait_SA[!unique_ID %in% dupl_unique_id, ]

## Duplicated Genera ----
dupl_genus <- Trait_SA[is.na(Species) & !is.na(Genus),] %>%
  .[duplicated(Genus), Genus]

# Ecnomus, but Taxon Psychomyiellodes (monphyletic group within Ecnomus)
Trait_SA[Genus == "Ecnomus" & is.na(Species), Taxon := "Ecnomus"]

# Elporia, two different families
Trait_SA[Genus == "Elporia" & is.na(Species), Family := "Blephariceridae"]

# genus-level:
Trait_SA[is.na(Species) & !is.na(Genus) & Genus %in% dupl_genus,
         (trait_col) := lapply(.SD, max),
         .SDcols = trait_col,
         by = "Genus"] 

# rm duplicate genus entries
dupl_unique_id <- Trait_SA[is.na(Species) & !is.na(Genus), ] %>%
  .[(duplicated(Genus)), unique_ID]
Trait_SA <- Trait_SA[!unique_ID %in% dupl_unique_id, ]


## Duplicated Families ----
dupl_family <- Trait_SA[is.na(Species) & is.na(Genus) & !is.na(Family),] %>%
  .[duplicated(Family), Family]

Trait_SA[is.na(Species) &
           is.na(Genus) & !is.na(Family) & Family %in% dupl_family,
         (trait_col) := lapply(.SD, max),
         .SDcols = trait_col,
         by = "Family"] 

# rm duplicate family entries
dupl_unique_id <- Trait_SA[is.na(Species) & is.na(Genus) & !is.na(Family),] %>% 
  .[duplicated(Family), unique_ID]
Trait_SA <- Trait_SA[!unique_ID %in% dupl_unique_id, ]

# change a few column names
setnames(
  Trait_SA,
  old = c("Taxon", "Species", "Genus", "Family", "Order"),
  new = c("taxon", "species", "genus", "family", "order")
)

# save
saveRDS(Trait_SA,
        file.path(data_cleaned,
                  "SA",
                  "Trait_SA_pp.rds"))
