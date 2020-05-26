
# ------------------------------------------------------------------------- 
#### Preprocessing NZ Traits ####
# ------------------------------------------------------------------------- 
Trait_NZ <- read_excel(
  file.path(data_in,
            "NZ",
            "nz_trait_database_v19_2_18+CM.xlsx"),
  sheet = 1,
  range = "A4:BK499"
)
setDT(Trait_NZ)

# read header
header <- read_excel(
  file.path(data_in, "NZ",
            "nz_trait_database_v19_2_18+CM.xlsx"),
  sheet = 1,
  range = "E3:BK3"
)

# change col names
setnames(Trait_NZ, names(Trait_NZ)[5:63],
         paste0(names(header), "_", names(Trait_NZ)[5:63]))
setnames(Trait_NZ, "Taxon/trait modality", "Taxon")

# create unique_id for possible duplicates
Trait_NZ[, unique_id := 1:nrow(Trait_NZ)]

# create species column: grep only entries with two words or more
Trait_NZ[grepl("[A-z]\\s.+", Taxon), `:=`(Species = Taxon, Taxon = NA)]

# Taxon col: search for family names
# Trait_NZ[grepl(".*dae", Taxon), .(Taxon, `Genus or Higher`, Family, Order)]
# put in Family col if not already there
# entry 21 is Hydra (which is actually a Genus)
Trait_NZ[grepl(".*dae", Taxon), `:=`(Family = ifelse(Taxon == Family, Family, Taxon),
                                     Taxon = NA)]

# Check for family names in Order col 
Trait_NZ[grepl(".*dae", Order), `:=`(`Genus or Higher` = "Hydra",
                                     Family = "Hydridae",
                                     Order = "Anthoathecata")]

# family names with "/"
Trait_NZ[grepl("\\/", Family) &
           grepl(".*oda", Family),
         `:=`(
           Phylum = "Arthropoda",
           Class = "Malacostraca",
           Order = sub("(.+)(\\/)(.+)", "\\1", Family),
           Family = sub("(.+)(\\/)(.+)", "\\3", Family)
         )]
# Trait_NZ[grepl("Amphipoda", Family), `:=`(
#   Class = Order,
#   Order = Family,
#   Family = NA,
#   `Genus or Higher` = NA
# )]

# other Families with "oda" in name
Trait_NZ[grepl(".*oda$", Family) &
           Order %in% "Crustacea", 
         `:=`(
             Phylum = "Arthropoda",
             Class = "Malacostraca",
             Order = Family,
             Family = NA,
             `Genus or Higher` = NA
           )]
# Cladocera wrongly in family col
Trait_NZ[grepl("Cladocera", Family), `:=`(Phylum = "Cladocera",
                                          Class = "Branchiopoda",
                                          Order = Family,
                                          Family = NA)]
# Syncarida wrongly in family col
Trait_NZ[grepl("Syncarida", Family), `:=`(
  Phylum = "Arthropoda",
  Class = "Malacostraca",
  Order = Family,
  Family = NA,
  `Genus or Higher` = NA
)]

# Tanaidacea wrongly in family col
Trait_NZ[grepl("Tanaidacea", Family), `:=`(
  Phylum = "Arthropoda",
  Class = "Malacostraca",
  Order = Family,
  Family = "Tanaidae"
)]

# subfamilies deleted
Trait_NZ[grepl("\\/", Family), Family := sub("(.+)(\\/)(.+)", "\\1", Family)]

# "nae"
# Trait_NZ[grepl(".*nae$", Family), .(Order, Family, `Genus or Higher`, Species)]$Family %>% 
#   unique(.) %>% 
#   query_google(.)
Trait_NZ[Family %in% c("Orthocladiinae",
                       "Diamesinae",
                       "Podonominae",
                       "Tanypodinae"), Family := "Chironomidae"]
# Hydridellinae is a subfamily -> seems to be unranked on family level
# left like this in DB

# few taxa with "Oligochaeta" as family assigned
# Lumbriculus variegatus
# Stylodrilus heringianus
Trait_NZ[Species %in% c("Lumbriculus variegatus", 
                        "Stylodrilus heringianus"), 
         `:=`(Family = "Lumbriculidae", 
              Class = "Clitellata",
              Phylum = "Annelida")]
# Eiseniella
Trait_NZ[`Genus or Higher` %in% "Eiseniella" , 
         `:=`(
           Family = "Lumbricidae", 
           Class = "Clitellata",
           Phylum = "Annelida"
         )]

# Genus and higher col: 
# rm taxa that are already in family or order column
# Trait_NZ[`Genus or Higher` == Family |
#            `Genus or Higher` == Order, .(Species, `Genus or Higher`, Taxon, Family, Order)]
Trait_NZ[`Genus or Higher` == Family |
           `Genus or Higher` == Order, `Genus or Higher` := NA]

# check for subfamilies
# Trait_NZ[`Genus or Higher` %like% ".*nae", ]
Trait_NZ[`Genus or Higher` %in% c("Orthocladiinae",
                                  "Diamesinae",
                                  "Podonominae",
                                  "Tanypodinae"), `Genus or Higher` := NA]

# Check the Genus or higher entries with Taxize to identify taxonomic level
# one ambiguous entry: Gordioida -> actually Nematomorpha
# taxa <- Trait_NZ[!is.na(`Genus or Higher`), ] %>% 
#   .[!duplicated(`Genus or Higher`), `Genus or Higher`]
# tax_level <- get_ids(names = taxa,
#                      db = "gbif", 
#                      rows = 1)
# Trait_NZ[!is.na(`Genus or Higher`) &
#            !duplicated(`Genus or Higher`), GBIFID := tax_level$gbif]
# obj <- classification(Trait_NZ[!is.na(GBIFID)]$GBIFID, db = "gbif")
# obj <- cbind(obj)
# setDT(obj)
# obj[is.na(genus_id), ]
Trait_NZ[grepl("Gordioida", `Genus or Higher`), `Genus or Higher` := NA]

# rename
setnames(Trait_NZ,
         old = "Genus or Higher",
         new = "Genus")

# Order column:
# Few entries are actually on Order but also occur in 
# Family col (probably unranked on Family-level)
Trait_NZ[Family == Order, Family := NA]

# Some entries that are actually on class or phylum level:
# Trait_NZ[!is.na(Order) & !duplicated(Order), .(Phylum,
#                                                Class,
#                                                Order,
#                                                Family,
#                                                `Genus or Higher`,
#                                                Species)]
# query_google(c("Acarina", "Bryozoa", "Crustacea", "Hirudinea", "Mollusca",
#                "Nematoda", "Nematomorpha", "Nemerta", "Platyhelminthes",
#                "Polychaeta", "Tardigrada"))
# Class:
Trait_NZ[Order %in% "Acarina", 
         `:=`(Phylum = "Arthropoda", 
           Class = Order,
           Order = NA)]
Trait_NZ[Order %in% "Polychaeta", 
         `:=`(Phylum = "Annelida", 
              Class = Order,
              Order = NA)]
Trait_NZ[Order %in% "Hirudinea", `:=`(Class = Order,
                                      Order = "Rhynchobdellida")]

# Phylum - just entries on order level:
Trait_NZ[Order %in% c("Bryozoa",
                      "Nematoda",
                      "Nematomorpha",
                      "Platyhelminthes",
                      "Tardigrada"), `:=`(Phylum = Order,
                                          Order = NA)]

# Phylum - entries on lower-levels than order:
Trait_NZ[Order %in% "Crustacea" & Family %in% "Mysidae",
         `:=`(Phylum = "Arthropoda",
           Class = "Malacostraca",
           Order = "Mysida")]

Trait_NZ[Order %in% "Crustacea" & Family %in% "Phreatiocidae",
         `:=`(Phylum = "Arthropoda",
              Class = "Malacostraca",
              Order = "Mysida")]

Trait_NZ[Order %in% "Crustacea" & Family %in% "Phreatogammaridae",
         `:=`(Phylum = "Arthropoda",
              Class = "Malacostraca",
              Order = "Amphipoda")]

# rm not used cols
Trait_NZ[, Taxon := NULL]

# colum order
setcolorder(Trait_NZ,
            neworder = c("Phylum", "Class", "Order", "Family", "Genus", "Species"))

# some columns have numerical and categorical values
Filter(is.character, Trait_NZ) %>% Hmisc::describe(.)
# transform to numeric:
# 3 buds
# 3 sexual
# 3 vivip
cols <- c("SINGLE_single individual",
          "HERMA_hermaphrodism",
          "SUBMERGED_submerged")
Trait_NZ[, (cols) := lapply(.SD, function(y)
  gsub("\\sbuds|\\ssexual|\\svivip", "", y)),
  .SDcols = cols]

# ambiguous entries:
# ?
# 2?
# 3?
# 3 polyps ?
# deleted through conversion to numeric
cols <- names(Filter(is.character , Trait_NZ[, -c("Phylum", "Class", "Order", "Family",
                                          "Genus", "Species")]))
Trait_NZ[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

# ------------------------------------------------------------------------- 
#### Handle duplicates ####
# ------------------------------------------------------------------------- 

# no duplicates on species level:
# Trait_NZ[!is.na(Species) & duplicated(Species), ]

# no duplicates on genus-level:
Trait_NZ[is.na(Species) & !is.na(Genus), ] %>% 
  .[duplicated(Genus), ]

# few duplicated entries on family level (used to be different subfamilies of
# Chironomidae)
# Often values are the same across a trait
# range  [0-3]
# if not the same mean is taken
# Trait_NZ[grepl("Chironomidae", Family) & is.na(Genus) & is.na(Species), ]

# trait cols
cols <- grep("(?i)unique_id|species|genus|family|order|class|phylum",
             names(Trait_NZ),
             value = TRUE,
             invert = TRUE)

Trait_NZ[is.na(Species) & is.na(Genus) & !is.na(Family),
         (cols) := lapply(.SD, mean), .SDcols = cols]

# rm duplicated families
ids <- Trait_NZ[is.na(Species) & is.na(Genus) & !is.na(Family), ] %>% 
  .[duplicated(Family), unique_id]
Trait_NZ <- Trait_NZ[!unique_id %in% ids, ]

# save
saveRDS(Trait_NZ, 
        file = file.path(data_cleaned, 
                         "NZ", 
                         "Trait_NZ_taxa_pp.rds"))
