
# ------------------------------------------------------------------------- 
#### Preprocessing NZ Traits ####

# cleaning & retrieval of taxonomical information
# There are trait data, probably origninating from Annika Wagenhoff, that have 
# entries like 0,33 or 0,66 -> Likely that those are already
# normalized values. Mostly at Order level, will not be considered
# Trait_NZ[`ADUORLAR_Adult or larva` > 2 & `ADUORLAR_Adult or larva` < 3, 
#          .(Order, Family, `Genus or higher`, `ADUORLAR_Adult or larva`)]

# -------------------------------------------------------------------------
# TODO add a more detailed description 

# load latest NZ DB
Trait_NZ <- read_excel(
  file.path(data_in, 
            "NZ",
            "Copy of NZ trait database April 2017 BJS.xlsx"),
  sheet = 1,
  range = "A4:BK581"
)
setDT(Trait_NZ)

# read header
header <- read_excel(
  file.path(data_in, "NZ",
            "Copy of NZ trait database April 2017 BJS.xlsx"),
  sheet = 1,
  range = "E2:BK2"
)

# change col names
setnames(Trait_NZ, names(Trait_NZ)[5:63],
         paste0(names(header), "_", names(Trait_NZ)[5:63]))
setnames(Trait_NZ, "Taxon/trait modality", "Taxon")

# del crude group entries 
Trait_NZ[grepl("group", Taxon), Taxon := sub("\\-group", "", Taxon)]

# create species column -> grep only entries with two words
Trait_NZ[grepl("[A-z]\\s.+", Taxon), `:=`(Species = Taxon, Taxon = NA)]

# Taxon col: move dae 
Trait_NZ[grepl(".*dae", Taxon), `:=`(Family = Taxon, Taxon = NA)]

# Check with Family and Order col -> take care for NAs in col
Trait_NZ[!is.na(Taxon) & !is.na(Family) &
           (Taxon %in% Order | Taxon %in% Family), Taxon := NA]

# Check the rest of Taxon entries with Taxize to identify taxonomic level
tax_level <- get_ids(names = Trait_NZ[!is.na(Taxon)]$Taxon, db = "gbif", 
                     rows = 1)
Trait_NZ[!is.na(Taxon), GBIFID := tax_level$gbif]
obj <- classification(Trait_NZ[!is.na(GBIFID)]$GBIFID, db = "gbif")
obj <- cbind(obj)
setDT(obj)

# merge back on Genus
Trait_NZ[obj[!is.na(genus)],
         `:=`(`Genus or higher` = i.genus,
              Family = i.family,
              Order = i.order),
         on = c(Taxon = "genus")]

# few Taxon entries need manual editing 
Trait_NZ[grepl("Bivalvia", Taxon), `:=` (Taxon = NA, Order = Taxon)]
Trait_NZ[grepl("Annelida", Taxon), `:=` (Taxon = NA, Order = Taxon)]
Trait_NZ[grepl("Zygoptera", `Genus or higher`),
         `:=`(Taxon = NA,
              `Genus or higher` = NA,
              Order = Taxon)]

# Chironominae,Chironomini, Macropelopiini, Tanytarsini tribes of 
# Chironomidae
Trait_NZ[grepl("Chironominae|Chironomini|Macropelopiini|Tanytarsini", 
               Taxon), Family := "Chironomidae"]
# Anisoptera
Trait_NZ[grepl("Anisoptera", Taxon), Order := "Anisoptera"]
# Odonata
Trait_NZ[grepl("Odonata", Taxon), Order := "Odonata"]
# Hirudinea
Trait_NZ[grepl("Hirudinea", Taxon), Order := "Hirudinea"]
# remove -group from a few taxa 
Trait_NZ[grepl(".*group", Species), Species := sub("\\-group", "", Species)]
# Some entries have another name in () which seems to be part of the actual name of this species and doesn't bother us 

# Species & Genus col: move hydropsyche entries in Genus col
Trait_NZ[grepl("Hydropsyche\\s\\(.+\\)$", Species), `:=`(`Genus or higher` = Species, 
                                                         Species = NA)]
# check Genus or higher column -> create pure Genus col
# Family entries moved to Family col
Trait_NZ[grepl(".*dae", `Genus or higher`), `:=`(Family = `Genus or higher`, 
                                                 `Genus or higher` = NA)]
# Compare Genus with Order colum
Trait_NZ[`Genus or higher` %in% Order, `Genus or higher` := NA ]

# Compare Family with Order column
Trait_NZ[Family %in% Order, Family := NA]

# order 
setorder(Trait_NZ, Species, `Genus or higher`,
         Family, Order, na.last = TRUE)

# change col name of Genus, remove Taxon
# The Order column also contains taxa with a higher (i.e. class,...)
# resolution, but has been named liked this for consistency with the 
# other trait databases
setnames(Trait_NZ, c("Genus or higher", "Order"), c("Genus", "Order"))

# col order
setcolorder(Trait_NZ, neworder = c("Order", "Family", "Genus", "Species", "Taxon"))

# few taxonomic information is still missing 
miss_names <- get_ids(names = Trait_NZ[is.na(Genus) &
                                         is.na(Family) & !is.na(Species),]$Species,
                      db = "gbif")
Trait_NZ[is.na(Genus) &
           is.na(Family) & !is.na(Species), missing_taxa := miss_names$gbif]

obj <- classification(Trait_NZ[!is.na(missing_taxa)]$missing_taxa, db = "gbif")
obj <- cbind(obj)
setDT(obj)

# merge back
Trait_NZ[obj, `:=`(Genus = i.genus,
                   Family = i.family,
                   Order = i.order),
         on = c(Species = "species")]
# del GBIFID & missing_taxa col
Trait_NZ[, c("GBIFID", "missing_taxa") := NULL]

# Add missing family & Order information for Hydropsyche
Trait_NZ[grepl("Hydropsyche", Genus), `:=`(Family = "Hydropsychidae", 
                                           Order = "Trichoptera")]
Trait_NZ[grepl("Lumbriculus", Genus), Family := "Lumbriculidae", ]
Trait_NZ[grepl("Stylodrilus", Genus), Family := "Lumbriculidae", ]

# Annika (Wagenhoff's?)
miss_taxa <-
  get_ids(Trait_NZ[grep("family or composite for matching with Annika's db",
                        Order), ]$Family,
          db = "gbif")
Trait_NZ[grep("family or composite for matching with Annika's db",
              Order), GBIFID := miss_taxa$gbif]
obj <- classification(Trait_NZ[!is.na(GBIFID),]$GBIFID, db = "gbif")
obj <- cbind(obj)
setDT(obj)

# merge back on Family
Trait_NZ[obj,
         `:=`(Order = i.order),
         on = c(Family = "family")]

# del GBIFID col
Trait_NZ[, GBIFID := NULL]

# What remains from  "family or composite for matching with Annika's db" is ignored
# It seems that they are also not part of the new version of this database

#### Further taxonomical fixes ####

# Hirudinea
Trait_NZ[grepl("Glossiphoniidae", Family), Order := "Rhynchobdellida"]

# Mollusca is a phylum
# Hydridellinae unclear atm -> difficult to find information
# Planorbidae, Melanopsidae, Physidae, Ancylidae & Latiidae currently unranked
Trait_NZ[grepl("Tateidae", Family), Order := "Littorinimorpha"]

# Oligochaeta subclass? or order?
# Naididae & Tubificidae the same actually
Trait_NZ[grepl("Naididae|Haplotaxidae", Family), Order := "Haplotaxida"]
Trait_NZ[grepl("Tubificidae", Family), Family := "Naididae"]
Trait_NZ[grepl("Phreodrilidae", Family), Order := "Tubificida"]
Trait_NZ[grepl("Lumbriculidae", Family), Order := "Lumbriculida"]
Trait_NZ[grepl("Eiseniella", Genus), `:=`(Family = "Lumbriculidae",
                                          Order = "Lumbriculida")]
# Crustacea
# some information is noted down in a crazy fashion
Trait_NZ[grepl("Amphipoda", Family), `:=`(
  Order = sub("(.*)(/)(.*)", "\\1", Family),
  Family = sub("(.*)(/)(.*)", "\\3", Family)
)]
Trait_NZ[grepl("Amphipoda", Family), `:=`(Genus = NA, 
                                          Family = NA)]
Trait_NZ[grepl("(Decapoda)(/)(.*)", Family), `:=`(
  Order = sub("(.*)(/)(.*)", "\\1", Family),
  Family = sub("(.*)(/)(.*)", "\\3", Family)
)]

# Tanaidacea seems to be an order!
Trait_NZ[grepl("Tanaidacea", Family), Order := "Tanaidae"] 
# Few entries are actually an orders or higher taxonomicla units
Trait_NZ[grepl("Cladocera", Family), `:=`(Genus = NA, 
                                          Family = NA, 
                                          Order = "Cladocera")]
Trait_NZ[grepl("Isopoda", Family), `:=`(Genus = NA, 
                                        Family = NA, 
                                        Order = "Isopoda")]
# Ostracoda is actually a class
Trait_NZ[grepl("Ostracoda", Family), `:=`(Genus = NA, 
                                          Family = NA, 
                                          Order = "Ostracoda")]
# Coepoda is a subclass
Trait_NZ[grepl("Copepoda", Family), `:=`(Genus = NA, 
                                         Family = NA, 
                                         Order = "Copepoda")]
Trait_NZ[grepl("Mysidae", Family), Order := "Mysida"]
Trait_NZ[grepl("Phreatogammaridae", Family), Order := "Amphipoda"]

# Syncarida is a superorder
Trait_NZ[grepl("Syncarida", Family), `:=`(Genus = NA, 
                                          Family = NA, 
                                        Order = "Syncarida")]
Trait_NZ[grepl("Phreatiocidae", Family), Order := "Isopoda"]

# Hydridae,NA
Trait_NZ[grepl("Hydridae", Order), `:=`(Order = "Anthoathecata",
                                        Family = "Hydridae")]
# NA entries are Lymnaeidae -> currently unranked
Trait_NZ[is.na(Order), Order := "Mollusca"]

# save
saveRDS(Trait_NZ, 
        file = file.path(data_cleaned, 
                         "NZ", 
                         "Trait_NZ_taxa_pp.rds"))