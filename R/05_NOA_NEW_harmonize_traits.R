# _________________________________________________________________________
#### Harmonize NOA Traits from Laura T ####
# Includes also merge with old NOA trait DB (based on Vieira et al. 2006)
# _________________________________________________________________________

# read in
Trait_Noa_new <- readRDS(file = file.path(
  data_cleaned,
  "North_America",
  "Traits_US_pp_LauraT.rds"
))

# _________________________________________________________________________
# Feeding mode ----
# feed_shredder: shredder (chewers, miners, xylophagus, decomposing plants)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_herbivore: herbivore piercers & scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite

# Predators are defined as: Engulfers (ingest prey whole or in parts) or
# as Piercers (piere prey tissues and suck fluids)
# Herbivore: Insects that scrape algae, shred or pierce living aquatic plants
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Feed_prim_CF",
    "Feed_prim_CG",
    "Feed_prim_PA",
    "Feed_prim_PR",
    "Feed_prim_SH",
    "Feed_prim_HB"
  ),
  new = c(
    "feed_filter",
    "feed_gatherer",
    "feed_parasite",
    "feed_predator",
    "feed_shredder",
    "feed_herbivore"
  )
)

# _________________________________________________________________________
# Locomotion ----
# locom_swim:  swimmer, scater (active & passive)
# locom_crawl: crawlers, walkers & sprawler, climber, clinger
# locom_burrow: burrower
# locom_sessil: sessil (attached)
# What to do with clingers? According to LeRoy and Chuck they should be put to crawlers
# _________________________________________________________________________
setnames(Trait_Noa_new,
  old = c(
    "Habit_prim_Attached/fixed",
    "Habit_prim_Burrower"
  ),
  new = c(
    "locom_sessil",
    "locom_burrow"
  )
)

## Swimmer ----
Trait_Noa_new[, locom_swim := apply(.SD, 1, sum),
  .SDcols = c(
    "Habit_prim_Swimmer",
    "Habit_prim_Planktonic",
    "Habit_prim_Skater"
  )
]

## Crawler ----
Trait_Noa_new[, locom_crawl := apply(.SD, 1, sum),
  .SDcols = c(
    "Habit_prim_Crawler",
    "Habit_prim_Sprawler",
    "Habit_prim_Climber",
    "Habit_prim_Clinger"
  )
]

Trait_Noa_new[, c(
  "Habit_prim_Swimmer",
  "Habit_prim_Planktonic",
  "Habit_prim_Skater",
  "Habit_prim_Crawler",
  "Habit_prim_Sprawler",
  "Habit_prim_Climber",
  "Habit_prim_Clinger"
) := NULL]

# _________________________________________________________________________
# Size ----
# specifically: Maximum body size
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Max_body_size_Large",
    "Max_body_size_Small",
    "Max_body_size_Medium"
  ),
  new = c(
    "size_large",
    "size_small",
    "size_medium"
  )
)

# _________________________________________________________________________
#### Respiration ####
# resp_teg: tegument
# resp_gil: gills
# resp_pls_spi: spiracle & plastron
# plastron & spiracle often work together in respiratory systems of aq. insects
# Present in insects with open tracheal systems -> breathe oxygen from the air
# -> Different tolerances to low oxygen compared to insects with tegument resp and gills
# _________________________________________________________________________
setnames(Trait_Noa_new,
  old = c(
    "Resp_Gills",
    "Resp_Plastron_spiracle",
    "Resp_Tegument"
  ),
  new = c(
    "resp_gil",
    "resp_pls_spi",
    "resp_teg"
  )
)

# _________________________________________________________________________
#### Voltinism ####
# volt_semi
# volt_uni
# volt_bi_multi
# _________________________________________________________________________
setnames(
  Trait_Noa_new,
  old = c(
    "Volt_Univoltine",
    "Volt_Semivoltine",
    "Volt_Bi_multivoltine"
  ),
  new = c(
    "volt_uni",
    "volt_semi",
    "volt_bi_multi"
  )
)

# _________________________________________________________________________
#### Complement trait data with old NOA trait data (Vieira et al. 2006) ####
# _________________________________________________________________________

# Load harmoized & normalized Trait NOA
Trait_Noa <-
  readRDS(file.path(data_cleaned, "North_America", "Traits_US_pp_harmonized.rds"))

# Subset to relevant orders
# Trait_Noa$order %>% table()
# Trait_Noa <- Trait_Noa[order %in% c(
#   "Coleoptera",
#   "Diptera",
#   "Ephemeroptera",
#   "Hemiptera",
#   "Lepidoptera",
#   "Megaloptera",
#   "Neuroptera",
#   "Odonata",
#   "Plecoptera",
#   "Trichoptera"
# )]

# Overlap old and new trait database
# Trait_Noa[!is.na(species) & !species %in% Trait_Noa_new$species, ] %>%
#   .[, .SD, .SDcols = names(Trait_Noa) %like% "species|^size|^resp|^feed|^locom|^volt"] %>%
#   na.omit(.)

# _________________________________________________________________________
# Oviposition & body form ----
# merged from old NOA trait dataset
# ovip_aqu
# ovip_ter
# ovip_ovo

# bf_streamlined: streamlined/fusiform
# bf_flattened: flattened (dorso-ventrally)
# bf_cylindrical: cylindrical/tubular
# bf_spherical: spherical
# TODO: Can merges be improved? Quite complicated
# _________________________________________________________________________

#### Merge oviposition data and body form data ####
# -> Oviposition & BF data only present in old NOA trait DB

# merge on species level:
# only species are merged that exist in Trait_Noa_new
Trait_Noa_new[Trait_Noa[!is.na(species), ],
  `:=`(
    ovip_ter = i.ovip_ter,
    ovip_aqu = i.ovip_aqu,
    ovip_ovo = i.ovip_ovo,
    bf_streamlined = i.bf_streamlined,
    bf_flattened = i.bf_flattened,
    bf_cylindrical = i.bf_cylindrical,
    bf_spherical = i.bf_spherical
  ),
  on = "species"
]

# merge on genus level:
Trait_Noa_genus_merge <- Trait_Noa[is.na(species) & !is.na(genus), ]

# intermediate dataset
stepGenus_Trait_Noa_new <-
  merge(
    x = Trait_Noa_new[is.na(species) & !is.na(genus), ],
    y = Trait_Noa_genus_merge[, .(
      genus,
      ovip_aqu,
      ovip_ter,
      ovip_ovo,
      bf_flattened,
      bf_streamlined,
      bf_spherical,
      bf_cylindrical
    )],
    by = "genus",
    all.x = TRUE
  )

# coalesce traite on genus level
stepGenus_Trait_Noa_new[, `:=`(
  ovip_ter = coalesce(ovip_ter.x, ovip_ter.y),
  ovip_aqu = coalesce(ovip_aqu.x, ovip_aqu.y),
  ovip_ovo = coalesce(ovip_ovo.x, ovip_ovo.y),
  bf_flattened = coalesce(bf_flattened.x, bf_flattened.y),
  bf_streamlined = coalesce(bf_streamlined.x, bf_streamlined.y),
  bf_cylindrical = coalesce(bf_cylindrical.x, bf_cylindrical.y),
  bf_spherical = coalesce(bf_spherical.x, bf_spherical.y)
)]

# merge subset back
Trait_Noa_new[stepGenus_Trait_Noa_new,
  `:=`(
    bf_flattened = i.bf_flattened,
    bf_streamlined = i.bf_streamlined,
    bf_cylindrical = i.bf_cylindrical,
    bf_spherical = i.bf_spherical,
    ovip_ter = i.ovip_ter,
    ovip_aqu = i.ovip_aqu,
    ovip_ovo = i.ovip_ovo
  ),
  on = "unique_id"
]

# merge on family level:
Trait_Noa_family_merge <- Trait_Noa[is.na(species) &
  is.na(genus) &
  !is.na(family), ]

# intermediate dataset
stepFamily_Trait_Noa_new <-
  merge(
    x = Trait_Noa_new[is.na(species) & is.na(genus) & !is.na(family), ],
    y = Trait_Noa_family_merge[, .(
      family,
      ovip_aqu,
      ovip_ter,
      ovip_ovo,
      bf_flattened,
      bf_streamlined,
      bf_spherical,
      bf_cylindrical
    )],
    by = "family",
    all.x = TRUE
  )
# coalesce
stepFamily_Trait_Noa_new[, `:=`(
  ovip_ter = coalesce(ovip_ter.x, ovip_ter.y),
  ovip_aqu = coalesce(ovip_aqu.x, ovip_aqu.y),
  ovip_ovo = coalesce(ovip_ovo.x, ovip_ovo.y),
  bf_flattened = coalesce(bf_flattened.x, bf_flattened.y),
  bf_streamlined = coalesce(bf_streamlined.x, bf_streamlined.y),
  bf_cylindrical = coalesce(bf_cylindrical.x, bf_cylindrical.y),
  bf_spherical = coalesce(bf_spherical.x, bf_spherical.y)
)]

# merge subset back
Trait_Noa_new[stepFamily_Trait_Noa_new,
  `:=`(
    bf_flattened = i.bf_flattened,
    bf_streamlined = i.bf_streamlined,
    bf_cylindrical = i.bf_cylindrical,
    bf_spherical = i.bf_spherical,
    ovip_ter = i.ovip_ter,
    ovip_aqu = i.ovip_aqu,
    ovip_ovo = i.ovip_ovo
  ),
  on = "unique_id"
]

# Coalesce merge on species level ----
# Complement missing information with the old NOA trait DB
# for the traits feeding mode, locomotion, size, respiration, voltinism

# Select relevant columns
Trait_Noa_new <- Trait_Noa_new[, .SD,
  .SDcols = names(Trait_Noa_new) %like% "feed|locom|size|resp|volt|ovip|^bf|unique_id|species|genus|family|order"
]

# select columns from old Noa Trait DB that are also in Trait_Noa_New
cols <- names(Trait_Noa)[names(Trait_Noa) %in% names(Trait_Noa_new)]
Trait_Noa <- Trait_Noa[, .SD, .SDcols = cols]

# create pattern for trait columns
name_vec <- grep("unique_id|order|family|genus|species",
  names(Trait_Noa),
  value = TRUE,
  invert = TRUE
) %>%
  sub("\\_.*", "", .) %>%
  unique() %>%
  paste0("^", .)

# TODO: Needs fix -> subset_vec is logical vector which can lead to
# an empty dataset "final"
# likely that there isn't much that can be complemented by the old NOA trait DB
# % NA val before
# na_before <- sum(is.na(Trait_Noa_new))/(sum(is.na(Trait_Noa_new))+ sum(!is.na(Trait_Noa_new)))
final <- Trait_Noa_new
for (i in name_vec) {
  # check if for a certain grouping feature all the traits contain NA values
  subset_vec <-
    !(rowSums(is.na(final[, .SD, .SDcols = names(final) %like% i])) == 0)

  # subset to NA values
  # complemented with information from NOA old (if possible)
  # data need(!) to be normalized for this operation
  step <- coalesce_join(
    x = final[subset_vec, ],
    y = Trait_Noa[!is.na(species), .SD,
      .SDcols = names(Trait_Noa) %like% paste0(i, "|", "species")
    ],
    by = "species",
    join = dplyr::left_join
  )
  setDT(step)
  # merge back to whole dataset
  final <- coalesce_join(
    x = final,
    y = step[!is.na(species),
      .SD,
      .SDcols = names(step) %like% paste0(i, "|", "species")
    ],
    by = "species",
    join = dplyr::left_join
  )
  setDT(final)
}
# % of na_values after merge -> ~ 0.7 % less NA's
# na_after <- sum(is.na(final))/(sum(is.na(final))+ sum(!is.na(final)))
Trait_Noa_new <- final


# Merge information from taxa that are only in old Noa DB ----
# species level
Trait_Noa_new <-
  rbind(Trait_Noa_new, Trait_Noa[!is.na(species), ] %>%
    .[!(species %in% Trait_Noa_new[!is.na(species), ]$species)])

# NOTE: no taxa has complete trait information (see uncommented code)
# Trait_Noa[is.na(species) & !is.na(genus),] %>%
#   .[!genus %in% Trait_Noa_new[is.na(species) & !is.na(genus),]$genus, ] %>%
#   na.omit(., cols = names(.[, -c("species")]))
# Trait_Noa[is.na(species) & is.na(genus) & !is.na(family), ] %>%
#   .[!family %in% Trait_Noa_new[is.na(species) & is.na(genus) & !is.na(family), ]$family,] %>%
#   na.omit(., cols = names(.[, -c("species", "genus")]))

# genus level
Trait_Noa_new <-
  rbind(
    Trait_Noa_new,
    Trait_Noa[is.na(species) & !is.na(genus), ] %>%
      .[!genus %in% Trait_Noa_new[is.na(species) &
        !is.na(genus), ]$genus, ]
  )

# family level
Trait_Noa_new <- rbind(
  Trait_Noa_new,
  Trait_Noa[is.na(species) &
    is.na(genus) &
    !is.na(family), ] %>%
    .[!family %in% Trait_Noa_new[is.na(species) &
      is.na(genus) &
      !is.na(family), ]$family, ]
)

# taxonomical changes
# Corbiculidae - Venerida
Trait_Noa_new[family == "Corbiculidae", order := "Venerida"]

# _________________________________________________________________________
# Pattern of development ----
# Holometabolous
# Hemimetabolous
# No insect
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
Trait_Noa_new[, `:=`(
  dev_hemimetabol = ifelse(order %in% hemimetabola, 1, 0),
  dev_holometabol = ifelse(order %in% holometabola, 1, 0)
)]
# rm entries with taxonomical resolution higher than Family
# Trait_Noa_new[!(is.na(species) & is.na(genus) & is.na(family)), ]

# save
saveRDS(
  object = Trait_Noa_new,
  file = file.path(
    data_cleaned,
    "North_America",
    "Traits_US_LauraT_pp_harmonized.rds"
  )
)
saveRDS(
  object = Trait_Noa_new,
  file = file.path(
    data_aggr,
    "Data",
    "Traits_US_LauraT_pp_harmonized.rds"
  )
)

# For convergence of Trait profiles analysis
saveRDS(
  object = Trait_Noa_new,
  file = "/home/kunzst/Dokumente/Projects/Trait_DB/Convergence-trait-profiles/Data/Traits_US_LauraT_pp_harmonized.rds"
)
