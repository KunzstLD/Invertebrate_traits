
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

# Subset to relevant traits & orders
Trait_AUS <- Trait_AUS[, .SD,
                       .SDcols = names(Trait_AUS) %like%
                         "locom|feed|resp|volt|size|bf|dev|unique_id|species|genus|family|order"] %>%
  .[order %in% c(
    "Ephemeroptera",
    "Hemiptera",
    "Odonata",
    "Trichoptera",
    "Coleoptera",
    "Plecoptera",
    "Diptera"
  ),]

# just return rows where for each trait there is an observation 
Trait_AUS <- na.omit(Trait_AUS,
                     cols = names(Trait_AUS[, -c("unique_id", 
                                                 "species", 
                                                 "genus", 
                                                 "family", 
                                                 "order",
                                                 "bf_cylindrical",
                                                 "bf_flattened",
                                                 "bf_spherical",
                                                 "bf_streamlined")]))
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

# get names of trait columns
trait_col <- names(Trait_AUS[, -c("family",
                                  "genus",
                                  "species",
                                  "order")])

# subset so that no NA values occur in Species data (otherwise all NA entries are viewed as a group &
# aggregated as well)
Trait_AUS_genus <- Trait_AUS[!is.na(species), lapply(.SD, median, na.rm = TRUE), 
                             .SDcols = trait_col, 
                             by = genus]

# merge family information 
Trait_AUS_genus[Trait_AUS[!is.na(species),],
                `:=`(family = i.family,
                     order = i.order),
                on = "genus"]

# bind with data resolved on Genus level
Trait_AUS_genus <-
  rbind(Trait_AUS_genus, Trait_AUS[is.na(species) &
                                     !is.na(genus),
                                   -c("species")] %>%
          .[!genus %in% Trait_AUS_genus$genus, ] %>%
          .[duplicated(genus),])

# split Body form data
# & aggregate BF information on family level 
Trait_AUS_genus_bf <- Trait_AUS_genus[!is.na(bf_flattened),]
Trait_AUS_genus  <- Trait_AUS_genus[is.na(bf_flattened), -c("bf_cylindrical",
                                                            "bf_flattened",
                                                            "bf_spherical",
                                                            "bf_streamlined")]
bf_col <- c("bf_flattened",
            "bf_cylindrical",
            "bf_spherical",
            "bf_streamlined")

Trait_AUS_genus_bf <- Trait_AUS_genus_bf[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y, na.rm = TRUE)
    # e.g. in case (0,0,3)
  } else if (Mode(y, na.rm = TRUE) == 0 & !all((y) == 0))  {
    Mode(y[y != 0], na.rm = TRUE)
  }
  else{
    Mode(y, na.rm = TRUE)
  }
})),
.SDcols = bf_col,
by = "family"]

Trait_AUS_genus_bf[Trait_AUS,
                   `:=`(order = i.order),
                   on = "family"]

# bring together with dataset compiled with BF traits from mdfrc, Leon Metzeling, Ben Kefford
# & Verena Schreiner
BF_AUS <- fread(file.path(
  data_missing,
  "Missing_traits_AUS",
  "BF",
  "AUS_BF_missing_final.csv"
)) %>%
  na.omit(.)
BF_AUS <- rbind(BF_AUS, Trait_AUS_genus_bf)

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
trait_col <- names(Trait_AUS_genus[, -c("family",
                                        "genus",
                                        "order")])

Trait_fam <- Trait_AUS_genus[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y, na.rm = TRUE)
    # e.g. in case (0,0,3)
  } else if (Mode(y, na.rm = TRUE) == 0 & !all((y) == 0))  {
    Mode(y[y != 0], na.rm = TRUE)
  }
  else{
    Mode(y, na.rm = TRUE)
  }
}), .N),
.SDcols = trait_col,
by = "family"]

# merge back information on order 
Trait_fam[Trait_AUS_genus, 
          `:=`(order = i.order), 
          on = "family"]

# filter for taxa resolved on family level that are not yet respresented in the 
# aggregated dataset (Trait_fam)
taxa_famlvl <- Trait_AUS[is.na(species) &
                           is.na(genus) &
                           !(family %in% Trait_fam$family) &
                           !is.na(family), ] %>%
  .[!duplicated(family), ]

# rbind with trait data resolved on family level
Trait_AUS_agg <- rbind(taxa_famlvl[, -c("species", "genus")], 
                       Trait_fam, fill = TRUE)

# merge BF information back
Trait_AUS_agg[BF_AUS, 
              `:=`(bf_flattened = i.bf_flattened,
                   bf_spherical = i.bf_spherical,
                   bf_cylindrical = i.bf_cylindrical,
                   bf_streamlined = i.bf_streamlined),
              on = "family"]

# filter taxa out where BF information was not availabile 
Trait_AUS_agg <-
  Trait_AUS_agg[!(
    is.na(bf_cylindrical) & is.na(bf_flattened) &
      is.na(bf_spherical) & is.na(bf_streamlined)
  ),]

# rm N column
Trait_AUS_agg[, N := NULL]

# save
saveRDS(object = Trait_AUS_agg,
        file = file.path(data_out, "Trait_AUS_agg.rds"))