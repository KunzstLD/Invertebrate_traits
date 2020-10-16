Trait_NZ[, taxa := coalesce(Species,Genus,Family,Order)]

# families with parasites in AUS
Trait_NZ[Family %in% c("Erpobdellidae",
                       "Glossiphoniidae",
                       "Ornithobdellidae",
                       "Richardsonianidae",
                       "Corallanidae",
                       "Cirolanidae"),]

# Check worms and leeches in NZ
# Rhynchobdellida
# Arguloida (parasitic crustaceans)
# Gordioidea (parasitic worms)
# Arhynchobdellida
# "Branchiobdellida" (few cases live as parasites)
# "Unionida" (mussels)
# "Mermithida"
Trait_NZ[Order %in% c("Rhynchobdellida", 
                      "Arguloida",
                      "Gordioidea",
                      "Arhynchobdellida", 
                      "Unionida",
                      "Mermithida"), ]

# critical species: "Alboglossiphonia heteroclita"
# -> Predator



# AUS
Trait_AUS <- readRDS(
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_harmonized.rds"
  )
)
parasite_taxa_AUS <- Trait_AUS[feed_parasite == 1, coalesce(species,
                                                            genus,
                                                            family,
                                                            order)]

Trait_NZ[taxa %in% parasite_taxa_AUS,]

# NOA
Trait_Noa_new <- readRDS(file = file.path(data_cleaned,
                                          "North_America",
                                          "Traits_US_LauraT_pp_harmonized.rds"))

parasite_taxa_NOA <-
  Trait_Noa_new[feed_parasite > 0, coalesce(species, genus,
                                            family, order)]
Trait_NZ[taxa %in% parasite_taxa_NOA, ]

# EU
Trait_EU <-
  readRDS(file = file.path(data_cleaned, "EU", "Trait_EU_pp_harmonized.rds"))

parasite_taxa_EU <-
  Trait_EU[feed_parasite > 0, coalesce(species, genus, family, order)]

Trait_EU[feed_parasite > 0, unique(order)]

# Scimyzidae and Ephydridae potentially parasites (Sciomyzidae 
# in gastropods):
# From Resh and Carde: 
# These flies are also known as shore flies, and
# most taxa are associated with aquatic habitats. This is another taxonomically
# rich family of Diptera (nearly 2000 species) and one of the most diverse 
# in feeding habits. 
# Larvae are consumers of decaying organic matter (e.g., Discocerina), 
# secondary stem borers of damaged plants (e.g., Typopsilopa), 
# primary herbivores (e.g., Hydrellia), generalist feeders of algae (e.g., Scatella), 
# specialist consumers of algae and cyanophytes (e.g., Hyadina),
# diatom specialists (e.g., Parydra), predators (e.g., Ochthera),
# and consumers of spider eggs (e.g., Trimerina)
Trait_NZ[taxa %in% parasite_taxa_EU, ]
Trait_EU[family %in% c("Ephydridae", "Sciomyzidae"), ]
