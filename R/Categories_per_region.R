# EU
Trait_EU <- readRDS(file.path(data_cleaned, "EU", "Trait_Freshecol_pp.rds"))

# Tachet
tachet <-  fread(file.path(data_in, "EU", "Tachet_mod_S.csv"))

# AUS
Trait_AUS <- readRDS(
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_preproc.rds"
  )
)

# NZ
Trait_NZ <- readRDS(file.path(data_cleaned, "NZ", "Trait_NZ_taxa_pp.rds"))

# NOA new (Laura Twardochleb)
Trait_Noa_new <- readRDS(file = file.path(data_cleaned, "North_America", "Traits_US_pp_LauraT.rds"))

# NOA old (Vieira)
Trait_Noa <- readRDS(file = file.path(data_cleaned, "North_America", "Traits_US_pp.rds"))

# get categories for each trait per region and synthesize
# function
fun <- function(x, id.vars, region) {
  x %>%
    melt(., id.vars = id.vars) %>%
    .[, .(Trait_state = unique(variable))] %>%
    .[, .(
      Trait = sub("\\_.*", "", Trait_state),
      Trait_state
    )] %>%
    .[, .(Trait_state,
      Trait,
      Region = rep(region, nrow(.))
    )]
}

# Freshwaterecology
fun(
  x = Trait_EU,
  id.vars = c(
    "order",
    "family",
    "genus",
    "species"
  ),
  region = "EU_freshwaterecology"
)

# Tachet
fun(
  x = tachet,
  id.vars = c(
    "Group",
    "Family",
    "Subfamily",
    "Genus",
    "Species",
    "Taxa"
  ),
  region = "EU_tachet"
)

# AUS
fun(
  x = Trait_AUS,
  id.vars = c(
    "unique_id",
    "Order",
    "Family",
    "Genus",
    "Species"
  ),
  region = "Australia"
) %>%
  .[grepl("(?i)Habi", Trait), ]


# NZ
# Needs some further preprocessing
fun(
  x = Trait_NZ,
  id.vars = c(
    "Taxon",
    "Order",
    "Family",
    "Genus",
    "Species"
  ),
  region = "New Zealand"
)

# NOA New
fun(
  x = Trait_Noa_new,
  id.vars = c(
    "unique_id",
    "order",
    "family",
    "genus",
    "species"
  ),
  region = "North America Laura T"
)

# NOA old
fun(
  x = Trait_Noa,
  id.vars = c(
    "unique_id",
    "Order",
    "Family",
    "Genus",
    "Species"
  ),
  region = "North America Vieira"
)

#### Harmonized traits ####
dt_harmonized_traits <- data.table(
  Voltinism = c(
    "Semivoltine",
    "Univoltine",
    "Bi/Multivoltine",
    NA,
    NA,
    NA
  ),
  Size = c(
    "Small",
    "Medium",
    "Large",
    NA,
    NA,
    NA
  ),
  Locomotion = c(
    "Swimmer",
    "Crawler",
    "Burrower",
    "Sessil",
    NA,
    NA
  ),
  Respiration = c(
    "Cutaneous/Tegument",
    "Gills",
    "Plastron & Spiracle",
    NA,
    NA,
    NA
  ),
  Feeding = c(
    "Shredder",
    "Collector-gatherer",
    "Collector-filterer",
    "Herbivore",
    "Predator",
    "Parasite"
  ),
  Bodyform = c(
    "Streamlined",
    "Flattened",
    "Cylindrical",
    "Spherical",
    NA,
    NA
  )
)

formattable(dt_harmonized_traits, 
            align =c("c","c","c","c","c","c"))