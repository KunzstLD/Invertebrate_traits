# _________________________________________________________________________
#### Aggregation of traits NOA data from Laura T #### 
# TODO: Check and simplify Aggregation functions
# _________________________________________________________________________

# read in pp_harmonized
Trait_Noa_new <- readRDS(file = file.path(data_cleaned,
                                      "North_America",
                                      "Traits_US_LauraT_pp_harmonized.rds"))

# Check completeness of trait data
# just return rows where for each trait there is an observation 
# oviposition is clearly the bottleneck!
completeness_trait_data(x = Trait_Noa_new,
                        non_trait_cols = c("unique_id",
                                           "species",
                                           "genus",
                                           "family",
                                           "order"))

# subset of traits (remove certain columns?) 
# currently, oviposition not used
Trait_Noa_new <- Trait_Noa_new[,.SD,
                                .SDcols = names(Trait_Noa_new) %like%
                                "locom|feed|resp|volt|bf|size|dev|unique_id|species|genus|family|order"]

Trait_Noa_new <- na.omit(Trait_Noa_new,
                         cols = names(Trait_Noa_new[, - c("unique_id", 
                                                          "species", 
                                                          "genus", 
                                                          "family", 
                                                          "order")]))
# _________________________________________________________________________
#### First aggregation step ####
# _________________________________________________________________________

# create vector with trait names
trait_col <- names(Trait_Noa_new[, - c("unique_id",
                                      "family",
                                      "genus",
                                      "species",
                                      "order")])

# aggregate species data to genus level via median
Trait_Noa_new_genus <-
  Trait_Noa_new[!is.na(species), lapply(.SD, median, na.rm = TRUE),
                .SDcols = trait_col, by = "genus"]

# merge family information back 
Trait_Noa_new_genus[Trait_Noa_new[!is.na(species),],
                `:=`(family = i.family,
                     order = i.order),
                     on = "genus"]

# rbind with Trait data resolved on genus level 
# (only for genera not present in Trait_Noa_new_genus)
Trait_Noa_new_genus <-
  rbind(Trait_Noa_new_genus,
        Trait_Noa_new[is.na(species) &
                        !is.na(genus),
                      -c("unique_id", "species")] %>% 
          .[!genus %in% Trait_Noa_new_genus$genus, ])

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
Trait_Noa_new_agg <- Trait_Noa_new_genus[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y)
    # e.g. in case (0,0,3)
  } else if (Mode(y) == 0 & !all((y) == 0)) {
    Mode(y[y != 0])
  }
  else {
    Mode(y)
  }
})),
.SDcols = trait_col,
by = "family"]

# merge information on order back
Trait_Noa_new_agg[Trait_Noa_new,
              `:=`(order = i.order),
              on = "family"]

# Taxa resolved on family level are not present in aggregated dataset 
Taxa_famlvl <- Trait_Noa_new[is.na(species) & is.na(genus),] %>%
  .[!family %in% Trait_Noa_new_agg$family, - c("unique_id", "species", "genus")] %>% 
  .[!duplicated(family), ]

# bind aggregated data and Taxa resol. on family level together 
Trait_Noa_new_agg <- rbind(Trait_Noa_new_agg, Taxa_famlvl)

# save
saveRDS(object = Trait_Noa_new_agg,
        file = file.path(data_out, "Trait_Noa_agg.rds"))
