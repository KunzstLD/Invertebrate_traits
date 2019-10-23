
# _________________________________________________________________________
#### Aggregation of traits NOA #### 
# TODO: Check and simplify Aggregation functions
# _________________________________________________________________________

# read in pp_harmonized
Trait_Noa <- readRDS(file = file.path(data_cleaned,
                                      "North_America", 
                                      "Traits_US_pp_harmonized.rds"))
# select trait col
trait_col <-
  grep(
    "unique_id|species|genus|family|order",
    names(Trait_Noa),
    invert = TRUE,
    value = TRUE
  )

# test how complete trait sets are
name_vec <- sub("\\_.*", "", trait_col) %>% unique()
output <- matrix(ncol = 2, nrow = length(name_vec))

for (i in seq_along(name_vec)) {
  vec <- Trait_Noa[, base::sum(.SD) == 0,
                   .SDcols = names(Trait_Noa) %like% name_vec[i],
                   by = 1:nrow(Trait_Noa)]$V1
  
  # percentage of how many entries per trait lack information
  output[i, ] <-
    c(round((sum(vec) / nrow(Trait_Noa)) * 100), name_vec[i])
}
output

# just return rows where for each trait there is an observation 
data <- get_complete_trait_data(
  trait_data = Trait_Noa,
  non_trait_col = c("unique_id",
                    "species",
                    "genus",
                    "family",
                    "order")
)
# lapply(data, nrow)

# merge datasats together
Trait_Noa <- Reduce(merge, data[c("locom",
                                  "feed",
                                  "resp",
                                  "volt",
                                  "ovip",
                                  "size",
                                  "dev")])
# Availabile traits:
# "locom",
# "feed",
# "resp",
# "ph",
# "temp",
# "volt",
# "ovip",
# "size", 
# "stage"

# Subset to interesting orders: Ephemeroptera, Hemiptera, Odonata, 
# Trichoptera, Venerida, Coleoptera, Plecoptera, Diptera, Amphipoda
Trait_Noa <- Trait_Noa[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Venerida",
  "Coleoptera",
  "Plecoptera",
  "Diptera",
  "Amphipoda"
), ]

# _________________________________________________________________________
#### First aggregation step ####
# _________________________________________________________________________

# create name pattern for relevant traits
pat_traitname <- paste(trait_col, collapse = "|")

# aggregate species data to genus level via median
Trait_Noa_genus <- Trait_Noa[!is.na(species), lapply(.SD, median), 
                             .SDcols = names(Trait_Noa) %like% pat_traitname, 
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
.SDcols = names(Trait_Noa_genus) %like% pat_traitname,
by = "family"] 

# merge information on order back
Trait_Noa_agg[Trait_Noa,
              `:=`(order = i.order),
              on = "family"]

# del unnecessary information 
Trait_Noa_agg[, N := NULL]

# Taxa resolved on family level already present in aggregated dataset
Trait_Noa_agg[family %in% Trait_Noa[is.na(species) & is.na(genus),]$family, ]

# merge back information on order
Trait_Noa_agg[Trait_Noa,
              `:=`(order = i.order),
              on = "family"]
# save
saveRDS(object = Trait_Noa_agg, file = file.path(data_out, "Trait_Noa_agg.rds"))