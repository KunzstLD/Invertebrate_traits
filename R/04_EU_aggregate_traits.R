# =========================================================================
#### Aggregation of traits EU ####
# =========================================================================

# read in harmonized, preprocessed & already normalized EU Trait DB
Trait_EU <-
  readRDS(file = file.path(data_cleaned, "EU", "Trait_EU_pp_harmonized.rds"))

# check completeness of trait data
completeness_trait_data(x = Trait_EU,
                        non_trait_cols =
                          c("order",
                            "family",
                            "genus",
                            "species"))

# Choose traits &
# restrict to certain orders
Trait_EU <- Trait_EU[, .SD,
                     .SDcols = names(Trait_EU) %like% "locom|feed|resp|volt|size|bf|dev|species|genus|family|order"] %>%
  .[order %in% c(
    "Ephemeroptera",
    "Hemiptera",
    "Odonata",
    "Trichoptera",
    "Coleoptera",
    "Plecoptera",
    "Diptera"
  ), ]

# just return rows where for each trait there is an observation
Trait_EU <- na.omit(Trait_EU,
  cols = names(Trait_EU[, -c("species", "genus", "family", "order")])
)

# _______________________________________________________________________
#### Aggregate to genus level ####
# _______________________________________________________________________

# create name pattern to choose traits
trait_col <- names(Trait_EU[, -c("family",
                                 "genus",
                                 "species",
                                 "order")])
# First aggregation step 
Trait_EU_genus <- Trait_EU[!is.na(species), lapply(.SD, median, na.rm = TRUE),
                           .SDcols = trait_col,
                           by = genus]

# merge family & order information
Trait_EU_genus[Trait_EU,
               `:=`(family = i.family,
                    order = i.order),
               on = "genus"]

# bind date on genus & family level back
Trait_EU_genus <-
  rbind(Trait_EU_genus,
        Trait_EU[is.na(species) &
                   !is.na(genus),
                 -c("species")] %>%
          .[!genus %in% Trait_EU_genus$genus, ])


# _______________________________________________________________________
#### Aggregate on family level ####
# take mode if duplicates, otherwise maximum
# test <- Trait_EU_genus[, lapply(.SD, Mode, na.rm = TRUE), 
#                .SDcols = names(Trait_EU_genus) %like% "^temp", 
#                by = "family"]
# Trait_fam <- Trait_EU_genus[, c(lapply(.SD, function(y) {
#   if (length(unique(y)) == length(y) & length(y) > 1) {
#     max(y)
#   } else {
#     Mode(x = y)
#   }
# }), .N),
# .SDcols = names(Trait_EU_genus) %like% pat_traitname,
# by = family]
# _______________________________________________________________________
Trait_fam <- Trait_EU_genus[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y)
    # e.g. in case (0,0,3)
  } else if (Mode(y) == 0 & !all((y) == 0))  {
    Mode(y[y != 0])
  }
  else{
    Mode(y)
  }
})),
.SDcols = trait_col,
by = family]

# merge back information on order 
Trait_EU_agg <- Trait_fam[Trait_EU_genus,
                          `:=`(order = i.order),
                          on = "family"]

# TODO: family information in tachet not completely covered in agg. freshwaterecol
# BF information missing for few families
# 6 of thoses 11 taxa are covered in NOA DB
# Taxa_famlvl <-
# tachet[is.na(species) & is.na(genus) & !is.na(family),] %>%
  # .[!family %in% Trait_fam$family,] %>%
# .[, .SD, .SDcols = names(tachet) %like% "locom|feed|resp|volt|size|dev|bf|species|genus|family|order"] %>%
# .[order %in% c(
#   "Ephemeroptera",
#   "Hemiptera",
#   "Odonata",
#   "Trichoptera",
#   "Coleoptera",
#   "Plecoptera",
#   "Diptera"
# ), ]

# feed_parasite:
# Trait_EU_agg[feed_parasite != 0,]

# cache as RDS object
saveRDS(object = Trait_EU_agg, file = file.path(data_out, "Trait_EU_agg.rds"))
