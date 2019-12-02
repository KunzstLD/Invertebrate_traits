
# _________________________________________________________________________ 
####  Aggregation of traits AUS ####
# _________________________________________________________________________ 

# read in harmonized & preprocessed EU Trait DB
Trait_AUS <- readRDS(
  file = file.path(
    data_cleaned,
    "Australia",
    "Trait_AUS_harmonized.rds"
  )
)

# _________________________________________________________________________ 
#### Normalization ####
# All trait states of one trait are divided by the row sum
# Hence, trait affinities are represented as "%" or ratios 
# _________________________________________________________________________ 
Trait_AUS <- normalize_by_rowSum(
  x = Trait_AUS,
  non_trait_cols = c("unique_id",
                     "species",
                     "genus",
                     "family",
                     "order")
)

# test how complete trait sets are 
completeness_trait_data(
  x = Trait_AUS,
  non_trait_cols = c("unique_id",
                     "species",
                     "genus",
                     "family",
                     "order")
)

# Subset to relevant traits 
Trait_AUS <- Trait_AUS[, .SD,
                       .SDcols = names(Trait_AUS) %like% "locom|feed|resp|volt|bf|size|dev|unique_id|species|genus|family|order"]

# just return rows where for each trait there is an observation 
Trait_AUS <- na.omit(Trait_AUS,
                     cols = names(Trait_AUS[, -c("unique_id", 
                                                 "species", 
                                                 "genus", 
                                                 "family", 
                                                 "order")]))

# Subset to interesting orders, maybe include Amphipoda & Venerida
# at a later stage
Trait_AUS <- Trait_AUS[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera"
), ]

# rm unique id col
Trait_AUS[, unique_id := NULL]

# _________________________________________________________________________
#### Aggregation of traits ####
# - Aggregation: 
 # Median to Genus/ 
 # Mode for Family 
# -> differing amount of values to calculate value for each modality
# sum(table(Trait_AUS[grepl("Chirono.*", family)]$temp_eurytherm))

# Poff et al. 2006: most common genus-level modality assigned (genus level
# trait data with family-level monitoring data)
# which(table(Trait_AUS$genus) == max(table(Trait_AUS$genus), na.rm = TRUE))
# _________________________________________________________________________

# create name pattern to choose traits
trait_col <- names(Trait_AUS[, -c("family",
                                  "genus",
                                  "species",
                                  "order")])

# subset so that no NA values occur in Species data (otherwise all NA entries are viewed as a group &
# aggregated as well)
Trait_AUS_genus <- Trait_AUS[!is.na(species), lapply(.SD, median), 
                             .SDcols = trait_col, 
                             by = genus]
# merge family information 
Trait_AUS_genus[Trait_AUS[!is.na(species), ], 
                `:=`(family = i.family,
                     order = i.order),
                on = "genus"]

# bind with data resolved on Genus level 
Trait_AUS_genus <-
  rbind(Trait_AUS_genus, Trait_AUS[is.na(species) &
                                     !is.na(genus),], fill = TRUE)
# _________________________________________________________________________
#### Aggregate to family level ####
# take mode if duplicates, otherwise maximum
# test <- Trait_AUS_genus[, lapply(.SD, Mode, na.rm = TRUE), 
#                .SDcols = names(Trait_AUS_genus) %like% "^temp", 
#                by = "family"]
# Trait_fam <- Trait_AUS_genus[, c(lapply(.SD, function(y) {
#   if (length(unique(y)) == length(y) & length(y) > 1) {
#     max(y)
#   } else {
#     Mode(x = y)
#   }
# }), .N),
# .SDcols = names(Trait_AUS_genus) %like% pat_traitname,
# by = family]
# _________________________________________________________________________
Trait_fam <- Trait_AUS_genus[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y)
    # e.g. in case (0,0,3)
  } else if (Mode(y) == 0 & !all((y) == 0))  {
    Mode(y[y != 0])
  }
  else{
    Mode(y)
  }
}), .N),
.SDcols = trait_col,
by = "family"]

# merge back information on order 
Trait_fam[Trait_AUS_genus, 
          `:=`(order = i.order), 
          on = "family"]

# del unnecessary information 
Trait_fam[, N := NULL]

# filter for taxa resolved on family level that are not yet respresented in the 
# aggregated dataset (Trait_fam)
taxa_famlvl <- Trait_AUS[is.na(species) &
                           is.na(genus) &
                           !(family %in% Trait_fam$family) &
                           !is.na(family), ] %>%
  .[!duplicated(family), ]

# rbind with trait data resolved on family level
Trait_AUS_agg <- rbind(taxa_famlvl[, -c("species", "genus")], 
                       Trait_fam)
# save
saveRDS(object = Trait_AUS_agg,
        file = file.path(data_out, "Trait_AUS_agg.rds"))
