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
                     .SDcols = names(Trait_EU) %like% "locom|feed|resp|volt|size|dev|species|genus|family|order"] %>%
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
Trait_EU <- na.omit(Trait_EU,
                    cols = names(Trait_EU[, -c("species", "genus", "family", "order")]))

# _______________________________________________________________________
#### Aggregate to genus level ####
# _______________________________________________________________________

# Add BF data from PUP on species level
# Using Philippe Polateras classification
body_form_pup <- fread(file.path(data_missing, 
                                "Body_form_EU_NOA", 
                                "body_form_polatera_EU_NOA.csv"))

# change "," to "." and convert bf columns to numeric
cols <- c("streamlined", "flattened", "cylindrical", "spherical")
body_form_pup[, (cols) := lapply(.SD, function(y) {
  sub("\\,", ".", y) %>%
    as.numeric(.)
}),
.SDcols = cols]

# subset to EU data
bf_EU <- body_form_pup[grepl("EU.*", array),]

# merge on species level
Trait_EU[bf_EU[!is.na(species), .(flattened,
                                  spherical,
                                  cylindrical,
                                  streamlined,
                                  species)],
         `:=`(
           bf_flattened = i.flattened,
           bf_spherical = i.spherical,
           bf_cylindrical = i.cylindrical,
           bf_streamlined = i.streamlined
         ),
         on = "species"]

# Rm two taxa with no BF information 
Trait_EU <- Trait_EU[!is.na(bf_flattened), ]

# create name pattern to choose traits
trait_col <- names(Trait_EU[, -c("family",
                                  "genus",
                                  "species",
                                  "order")])
# First aggregation step 
Trait_EU_genus <- Trait_EU[, lapply(.SD, median, na.rm = TRUE),
                           .SDcols = trait_col,
                           by = genus]

# merge family & order information 
Trait_EU_genus[Trait_EU,
               `:=`(family = i.family,
                    order = i.order),
               on = "genus"]

# load & complement with Tachet data on genus level
# subset to relevant traits &
# restrict to relevant orders
tachet <- readRDS(file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds"))
tachet <- tachet[, .SD,
    .SDcols = names(tachet) %like% "locom|feed|resp|volt|size|dev|species|genus|family|order"] %>%
  .[order %in% c(
    "Ephemeroptera",
    "Hemiptera",
    "Odonata",
    "Trichoptera",
    "Coleoptera",
    "Plecoptera",
    "Diptera"
  ), ]

# merge PUP BF traits to tachet on genus & family level
# only take complete trait data on genus level 
# that are not present in Trait_EU (almost all from tachet)
tachet_genus <- tachet[!is.na(genus) & is.na(species),] %>%
  .[!genus %in% Trait_EU_genus$genus, ] %>%
  .[!duplicated(genus), -c("species")] %>%
  na.omit(.)

# merge on genus level
tachet_genus[bf_EU[is.na(species) & !is.na(genus), .(flattened,
                                               spherical,
                                               cylindrical,
                                               streamlined,
                                               genus)],
       `:=`(
         bf_flattened = i.flattened,
         bf_spherical = i.spherical,
         bf_cylindrical = i.cylindrical,
         bf_streamlined = i.streamlined
       ),
       on = "genus"]

# bind trait data
Trait_EU_genus <-  rbind(Trait_EU_genus, tachet_genus)

# BF information for Molanna missing
Trait_EU_genus <- Trait_EU_genus[!is.na(bf_spherical), ]

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

# family information in freshwaterecol already covered
# Trait_EU[is.na(species) & is.na(genus) & !is.na(family), ] %>%
#   .[!family %in% Trait_fam$family, -c("species", "genus")]

# TODO: family information in tachet not completely covered in agg. freshwaterecol
# tachet <- readRDS(file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds"))
# Taxa_famlvl <-
#   tachet[is.na(species) & is.na(genus) & !is.na(family),] %>%
#   .[!family %in% Trait_fam$family,] %>%
#   .[, .SD, .SDcols = names(tachet) %like% "locom|feed|resp|volt|size|dev|species|genus|family|order"] %>% 
#   .[order %in% c(
#     "Ephemeroptera",
#     "Hemiptera",
#     "Odonata",
#     "Trichoptera",
#     "Coleoptera",
#     "Plecoptera",
#     "Diptera"
#   ), ]
# Trait_EU_agg <- rbind(Taxa_famlvl, Trait_fam) 

# feed_parasite:
# Trait_EU_agg[feed_parasite != 0,]

# cache as RDS object
saveRDS(object = Trait_EU_agg, file = file.path(data_out, "Trait_EU_agg.rds"))
