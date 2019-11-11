
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
# _________________________________________________________________________ 

# get trait names & create pattern for subset
trait_names_pattern <-
  names(Trait_AUS[, -c("unique_id",
                       "family",
                       "genus",
                       "species",
                       "order")]) %>%
  sub("\\_.*|\\..*", "", .) %>%
  unique() %>%
  paste0("^", .)

# loop for normalization (trait categories for each trait sum up to 1) 
for(cols in trait_names_pattern) {
  
  # get row sum for a specific trait
  Trait_AUS[, rowSum := apply(.SD, 1, sum),
            .SDcols = names(Trait_AUS) %like% cols]
  
  # get column names for assignment
  col_name <- names(Trait_AUS)[names(Trait_AUS) %like% cols]
  
  Trait_AUS[, (col_name) := lapply(.SD, function(y) {
    round(y / rowSum, digits = 2)
  }),
  .SDcols = names(Trait_AUS) %like% cols]
}
Trait_AUS[, rowSum := NULL]

# test how complete trait sets are 
output <- matrix(ncol = 2, nrow = length(trait_names_pattern))

for (i in seq_along(trait_names_pattern)) {
  # vector containing either 0 (no NAs) or a number (> 0) meaning that all
  # entries for this trait contained NA
  vec <-
    Trait_AUS[, apply(.SD, 1, function(y)
      base::sum(is.na(y))),
      .SDcols = names(Trait_AUS) %like% trait_names_pattern[[i]]]
  
  # How complete is the dataset for each individual trait?
  output[i, ] <-
    c((length(vec[vec == 0]) / nrow(Trait_AUS))  %>% `*` (100) %>% round(),
      trait_names_pattern[[i]])
}
# Gives % coverage for each trait
# output

# Subset to relevant traits 
Trait_AUS <- Trait_AUS[, .SD,
                       .SDcols = names(Trait_AUS) %like% "locom|feed|resp|volt|ovip|size|dev|unique_id|species|genus|family|order"]

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
Trait_AUS_resol_fam <- Trait_AUS[is.na(species) & 
                                   is.na(genus) & 
                                   !(family %in% Trait_fam$family) &
                                   !is.na(family), ]
# condense duplicates in Trait_AUS_resol_fam
Trait_AUS_resol_fam <-
  condense_dupl_numeric(
    trait_data = Trait_AUS_resol_fam,
    col_with_dupl_entries = "family",
    non_trait_cols = c("species", "genus", "family", "order")
  )

# rbind with trait data resolved on family level
Trait_AUS_agg <- rbind(Trait_AUS_resol_fam[, -c("species", "genus")], 
                       Trait_fam)

# del dev_no_insect to be consistent with NZ DB
Trait_AUS_agg[, dev_no_insect := NULL]

# save
saveRDS(object = Trait_AUS_agg,
        file = file.path(data_out, "Trait_AUS_agg.rds"))
