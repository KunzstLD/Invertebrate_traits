# =========================================================================
#### Aggregation of traits EU ####
# =========================================================================

# read in harmonized, preprocessed & already normalized EU Trait DB
Trait_EU <- readRDS(file = file.path(data_cleaned, "EU", "Trait_EU_pp_harmonized.rds"))

# get trait names & create pattern for subset
trait_names_pattern <-
  names(Trait_EU[, -c("family",
                      "genus",
                      "species",
                      "order")]) %>%
  sub("\\_.*|\\..*", "", .) %>%
  unique() %>%
  paste0("^", .)

# test how complete trait sets are 
output <- matrix(ncol = 2, nrow = length(trait_names_pattern))
for (i in seq_along(trait_names_pattern)) {
  # vector containing either 0 (no NAs) or a number (> 0) meaning that all
  # entries for this trait contained NA
  vec <-
    Trait_EU[, apply(.SD, 1, function(y)
      base::sum(is.na(y))),
      .SDcols = names(Trait_EU) %like% trait_names_pattern[[i]]]
  
  # How complete is the dataset for each individual trait?
  output[i, ] <-
    c((length(vec[vec == 0]) / nrow(Trait_EU))  %>% `*` (100) %>% round(),
      trait_names_pattern[[i]])
}
# output

# Choose traits
Trait_EU <- Trait_EU[, .SD,
                     .SDcols = names(Trait_EU) %like% "locom|feed|resp|volt|ovip|size|dev|species|genus|family|order"]

# just return rows where for each trait there is an observation 
Trait_EU <- na.omit(Trait_EU,
                    cols = names(Trait_EU[, -c("species", "genus", "family", "order")]))

# restrict to certain orders
Trait_EU <- Trait_EU[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera"
), ]

# _______________________________________________________________________
#### Aggregate to genus level ####
# _______________________________________________________________________

# create name pattern to choose traits
trait_col <- names(Trait_EU[, -c("family",
                                  "genus",
                                  "species",
                                  "order")])
# First aggregation step 
Trait_EU_genus <- Trait_EU[, lapply(.SD, median), 
                           .SDcols = trait_col, 
                           by = genus]
# merge family & order information 
Trait_EU_genus[Trait_EU, 
               `:=`(family = i.family,
                    order = i.order),
               on = "genus"]

# load & complement with Tachet data on genus level
tachet <- readRDS(file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds"))

# subset to relevant traits
tachet <- tachet[, .SD, 
                 .SDcols = names(tachet) %like% "locom|feed|resp|volt|ovip|size|dev|species|genus|family|order"]

# restrict to certain orders
tachet <- tachet[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera"
), ]

# only take complete trait data on genus level not already present in Trait_EU (almost all from tachet)
tachet <- tachet[!is.na(genus) &
                  is.na(species) &
                 !genus %in% Trait_EU_genus$genus, ] %>%
  .[, -c("species")] %>%
  na.omit(.)

# bind trait data
Trait_EU_genus <-  rbind(Trait_EU_genus, tachet) 

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

# family information in tachet is already covered in agg. freshwaterecol
# except for Sparganophilidae, whose information is - however - not complete
# tachet <- readRDS(file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds"))
# tachet[grepl("Sparganophilidae", family), ] %>% 
#   .[, .SD, .SDcols = names(tachet) %like% "locom|feed|resp|volt|ovip|size|dev"]

# merge back information on order 
Trait_EU_agg <- Trait_fam[Trait_EU_genus,
                          `:=`(order = i.order),
                          on = "family"]
# think about feed_parasite?
Trait_EU_agg[feed_parasite != 0,]

# del dev_no_insect
Trait_EU_agg[, dev_no_insect := NULL]

# cache as RDS object
saveRDS(object = Trait_EU_agg, file = file.path(data_out, "Trait_EU_agg.rds"))