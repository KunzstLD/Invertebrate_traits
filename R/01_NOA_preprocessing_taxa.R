# ------------------------------------------------------------------------- 
#### Preprocessing Noa Traits Pt. 1 ####

# retrieve taxonomical information
# TODO incorporate information from Laura

# -------------------------------------------------------------------------


# Load US DB
Trait_Noa <- read_excel(file.path(
  data_in, "North_America", "6138 inverttraitstable.xls"
),
sheet = 1
)
setDT(Trait_Noa)

# remove rows with just NA's
Trait_Noa <- Trait_Noa[rowSums(is.na(Trait_Noa)) != ncol(Trait_Noa), ]

# all dae entries in Taxon are the same in col family
# Trait_Noa[grepl(".*dae", Taxon), .(which(Family != Taxon))]
# Trait_Noa[grepl(".*dae", Taxon)][c(35, 36, 37, 189), .(Family, Taxon)]
# all spp. taxa in Taxon column have an entry in Genus
# all(Trait_Noa[Taxon %like% "spp.", .(Genus = !is.na(Genus))]$Genus)
Trait_Noa[, Species := ifelse(Taxon %like% "spp.|sp.", NA, Taxon)]
Trait_Noa[, Species := ifelse(Species %like% ".+\\s.+", Species, NA)]

# few families/order in Taxon left
Trait_Noa[is.na(Species) & is.na(Genus) & is.na(Family) &
  grepl(".+nae", Taxon), Family := Taxon]
Trait_Noa[is.na(Species) & is.na(Genus) & is.na(Family), Order := Taxon]

# Change col order
setcolorder(x = Trait_Noa, neworder = c("TraitRecord_ID", "Species", "Genus", "Family", "Taxon"))

# we ignore these that have solely traits for Adults
Trait_Noa <- Trait_Noa[!grepl("ADULT.*", Adult), ]

# save raw for harmonization later (uses partly the raw data to extract information
# from the comment section)
saveRDS(Trait_Noa, file = file.path(data_in, "North_America", "Inverttraitstable_raw.rds"))

# retrieve information on order
family_Noa <-
  get_ids(sci_com = na.omit(unique(Trait_Noa$Family)),
          db = "gbif",
          rows = 1)
family_gbif <- data.table(id_family = family_Noa$gbif, 
           family = names(family_Noa$gbif))
Trait_Noa[family_gbif,
          id_family := i.id_family,
          on = c(Family = "family")]
order_Noa <- cbind(classification(
  id = Trait_Noa[!is.na(id_family) & !duplicated(id_family), ]$id_family,
  db = "gbifid"
))[c("family", "order")]
setDT(order_Noa)
Trait_Noa[order_Noa,
  Order := i.order,
  on = c(Family = "family")
]

# check order for (sub) families taxizie couldn't recognize
Trait_Noa[is.na(id_family) & grepl("Dicosmoecinae", Family), Order := "Trichoptera"]
Trait_Noa[is.na(id_family) & grepl("Pseudostenophylacinae", Family), Order := "Trichoptera"]
Trait_Noa[is.na(id_family) & grepl("Donaciinae", Family), Order := "Coleoptera"]
Trait_Noa[is.na(id_family) & grepl("Tanypodinae", Family), Order := "Diptera"]
Trait_Noa[is.na(id_family) & grepl("Diamesinae", Family), Order := "Diptera"]

# fix a few wrong orders (Diplostraca was assigned)
Trait_Noa[grepl("Cyzicidae|Leptestheriidae|Limnadiidae", Family), Order := "Spinicaudata"]
Trait_Noa[grepl("Lynceidae", Family), Order := "Onychura"]
Trait_Noa[grepl("Caenestheriidae", Family), Order := "Branchiopoda"]

# Taxonomical corrections
# Parapoynx & Petrophilia
Trait_Noa[Genus %in% c("Parapoynx",
                       "Petrophila"), Family := "Crambidae"]

# Rossiana
Trait_Noa[Genus == "Rossiana", Family := "Rossianidae"]

# Sperchonopsis
Trait_Noa[Genus == "Sperchonopsis", Family := "Sperchonidae"]

# Corbiculidae
Trait_Noa[Family == "Corbiculidae", Order := "Venerida"]

# del id_family column
Trait_Noa[, id_family := NULL]

# save
saveRDS(
  object = Trait_Noa,
  file = file.path(
    data_cleaned,
    "North_America",
    "Traits_US_taxa_pp.rds"
  )
)
fwrite(
  x = Trait_Noa,
  file = file.path(
    data_cleaned,
    "North_America",
    "Traits_US_taxa_pp.csv"
  )
)

