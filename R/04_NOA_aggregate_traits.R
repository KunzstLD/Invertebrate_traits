
# _________________________________________________________________________
#### Aggregation of traits NOA #### 
# TODO: Check and simplify Aggregation functions
# _________________________________________________________________________

# read in pp_harmonized
Trait_Noa <- readRDS(file = file.path(data_cleaned,
                                      "North_America", 
                                      "Traits_US_pp_harmonized.rds"))
# _________________________________________________________________________
#### Normalization ####
# First step is normalizing of the trait values to a range of [0 - 1] by
# dividing for a given trait each value for a trait state by the sum of all 
# trait states
# Then trait data are subsetted to taxa that have complete 
# information on all traits
# _________________________________________________________________________

# get trait names & create pattern for subset
trait_names_pattern <-
  names(Trait_Noa[, -c("unique_id",
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
  Trait_Noa[, rowSum := apply(.SD, 1, sum),
                .SDcols = names(Trait_Noa) %like% cols]
  
  # get column names for assignment
  col_name <- names(Trait_Noa)[names(Trait_Noa) %like% cols]
  
  Trait_Noa[, (col_name) := lapply(.SD, function(y) {
    round(y / rowSum, digits = 2)
  }),
  .SDcols = names(Trait_Noa) %like% cols]
}

# del unnecessary columns
Trait_Noa[, c("rowSum") := NULL]

# test how complete trait sets are 
output <- matrix(ncol = 2, nrow = length(trait_names_pattern))

for (i in seq_along(trait_names_pattern)) {
  # vector containing either 0 (no NAs) or a number (> 0) meaning that all
  # entries for this trait contained NA
  vec <-
    Trait_Noa[, apply(.SD, 1, function(y)
      base::sum(is.na(y))),
      .SDcols = names(Trait_Noa) %like% trait_names_pattern[[i]]]
  
  # How complete is the dataset for each individual trait?
  output[i, ] <-
    c((length(vec[vec == 0]) / nrow(Trait_Noa))  %>% `*` (100) %>% round(),
      trait_names_pattern[[i]])
}
# Gives % coverage for each trait
output

# Subset to relevant traits 
Trait_Noa <- Trait_Noa[, .SD,
                       .SDcols = names(Trait_Noa) %like% "locom|feed|resp|volt|ovip|size|dev|unique_id|species|genus|family|order"]

# just return rows where for each trait there is an observation 
Trait_Noa <- na.omit(Trait_Noa,
                     cols = names(Trait_Noa[, -c("unique_id", 
                                                 "species", 
                                                 "genus", 
                                                 "family", 
                                                 "order")]))

# Subset to interesting orders: Venerida & Amphipoda?
# Trichoptera, Venerida, Coleoptera, Plecoptera, Diptera, Amphipoda
Trait_Noa <- Trait_Noa[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera"
), ]

# _________________________________________________________________________
#### First aggregation step ####
# _________________________________________________________________________

# create vector with trait names
trait_col <- names(Trait_Noa[, -c("unique_id",
                                  "family",
                                  "genus",
                                  "species",
                                  "order")])

# aggregate species data to genus level via median
Trait_Noa_genus <- Trait_Noa[!is.na(species), lapply(.SD, median), 
                             .SDcols = trait_col, 
                             by = "genus"]

# merge family information back 
Trait_Noa_genus[Trait_Noa[!is.na(species), ],
                `:=`(family = i.family, 
                     order = i.order),
                on = "genus"]

# rbind with Trait data resolved on genus level
Trait_Noa_genus <-
  rbind(Trait_Noa_genus, Trait_Noa[is.na(species) & !is.na(genus),
                                   -c("unique_id", "species")])

# _________________________________________________________________________
#### Aggregate to family level ####
# take mode if duplicates, otherwise maximum
# test <- Trait_Noa_genus[, lapply(.SD, Mode, na.rm = TRUE), 
#                .SDcols = names(Trait_Noa_genus) %like% "^temp", 
#                by = "family"]
# Trait_Noa_fam <- Trait_Noa_genus[, c(lapply(.SD, function(y) {
#   if (length(unique(y)) == length(y) & length(y) > 1) {
#     max(y)
#   } else {
#     Mode(x = y)
#   }
# }), .N),
# .SDcols = names(Trait_Noa_genus) %like% pat_traitname,
# by = "family"]
# _________________________________________________________________________
Trait_Noa_agg <- Trait_Noa_genus[, c(lapply(.SD, function(y) {
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

# merge information on order back
Trait_Noa_agg[Trait_Noa,
              `:=`(order = i.order),
              on = "family"]

# Taxa resolved on family level already present in aggregated dataset
Trait_Noa[is.na(species) & is.na(genus),] %>% 
  .[!family %in% Trait_Noa_agg$family, ]

# del unnecessary information 
Trait_Noa_agg[, N := NULL]

# save
saveRDS(object = Trait_Noa_agg, 
        file = file.path(data_out, "Trait_Noa_agg.rds"))
