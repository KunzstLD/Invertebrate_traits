
# reference cols
ref_cols <- grep("(?i)ref.+", names(Trait_AUS), value = TRUE)

# search terms 
search <- c("Merrit.+Cummins.+1986" = 70,
            "Merrit.+Cummins.+1978" = 236,
            "Poff.+2006" = 261)

# create lf of reference data from AUS trait dataset
Trait_AUS <-
  fread(file.path(data_in,
                  "Australia",
                  "Australian_macroinv_trait_database_final.csv"))

ref_data <- melt(Trait_AUS[, c("Species", "Genus", "Family", "Order", ref_cols), with = FALSE], 
     measure.vars = ref_cols)

#### Feeding mode ####

# References in search are used in NOA dataset
# check if these families were used in the NOA dataset for trait aggregations
# than compare 
ref_data[value %in% search, ]

# Trait NOA table
Trait_Noa_new <- fread(file.path(data_in, "North_America", "Trait_table_LauraT.csv"))
cols <- c("Accepted_Name", "Genus", "Family", "Order", "Study_Citation_abbrev", "Study_Citation")

refs_Noa <- Trait_Noa_new[, ..cols]

# might use the same source for feeding, but not really sure (according to the trait
# table description Trophic Relations of Aquatic Insects is used in Noa)
Trait_Noa_new[Study_Citation_abbrev %like% "Merrit.+Cummins" & !is.na(Feed_prim_abbrev), ]


#### Body size ####
# Check body size data, as these look very similar between NOA and AUS!
# basically, almost all references in the AUS DB availabile for size are
# assigned at family-level
# TODO few references need to be fixed!
body_size_refs <- ref_data[variable %like% "size", unique(value)] %>% 
  strsplit(. , ";|,") %>% 
  unlist() %>% 
  as.numeric() %>% 
  table()

body_size_refs[order(-body_size_refs)]

# most frequent references: 2, 36, 22, 16
ref_data[variable %like% "size" & value %like% "22|16" & !is.na(Family), ]

ref_data[variable %like% "size" & !is.na(value) & !is.na(Genus), ]

# could still check if these occur in NOA (which is rather unlikely)
# 2 - Williams W. D. (1980). Australian Freshwater Life. (MacMillan Books: Melbourne.)
# 36 - Hawking J. H. and Smith, F.H. (1997). Colour guide to invertebrates of Australian inland waters. Albury: CRCFE
# 22- Cartwright D. (1954). Preliminary guide to the identification of late instar larvae of Australian ecnomidae, philopotamidae and tasmiidae. Thurgoona: CRCFE
# 16 - St.Clair R. M. (1997). Preliminary guide to the identification of late instar larvae of Australian philorheithridae, calamoceratidae and helicopsychidae. Melbourne: CRCFE
Trait_Noa_new[Study_Citation_abbrev %like% "Williams|Hawking|Cartwright|St.*Clair", ]




