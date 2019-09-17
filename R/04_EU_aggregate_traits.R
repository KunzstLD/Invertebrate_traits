# =========================================================================
#### Aggregation of traits EU ####
# =========================================================================

# read in harmonized & preprocessed EU Trait DB
Trait_EU <- readRDS(file = file.path(data_cleaned, "EU", "Trait_EU_pp_harmonized.rds"))

# get trait columns
trait_col <-
  grep(
    "unique_id|species|genus|family|order",
    names(Trait_EU),
    invert = TRUE,
    value = TRUE
  )

# subset so that only traits with complete information are retained -> test how complete trait sets are
name_vec <- sub("\\_.*", "", trait_col)
name_vec <- unique(name_vec)
output <- matrix(ncol = 2, nrow = length(name_vec))

# percentage of how many entries per trait lack information
for (i in seq_along(name_vec)) {
  vec <- Trait_EU[, base::sum(.SD) == 0,
                    .SDcols = names(Trait_EU) %like% name_vec[i],
                    by = 1:nrow(Trait_EU)]$V1
  
  output[i,] <-
    c(round((sum(vec) / nrow(Trait_EU)) * 100), name_vec[i])
}
output

# just return rows where for each trait there is an observation 
# use get_complete_trait_data function
data <- get_complete_trait_data(trait_data = Trait_EU)

# lapply(data, nrow)
Trait_EU <- Reduce(merge, data[c("locom",
                                 "feed",
                                 "resp",
                                 "volt",
                                 "ovip",
                                 "size")])
# possible traits: "locom", "feed", "resp", "ph", "temp", "volt", "ovip", "size","stage"

#### Aggregate to genus level ####

# create name pattern to choose traits
pat_traitname <- paste(trait_col, collapse = "|")

# First aggregation step -> Median
Trait_EU_genus <- Trait_EU[, lapply(.SD, median), 
                           .SDcols = names(Trait_EU) %like% pat_traitname, 
                           by = genus]

# merge family information 
Trait_EU_genus[Trait_EU, 
               `:=`(family = i.family,
                    order = i.order),
               on = "genus"]

# load & complement with Tachet data on genus level
tachet <- readRDS(file.path(data_cleaned, "EU", "Trait_Tachet_pp_harmonized.rds"))

# only take complete trait data (almost all from tachet)
data_tachet <-
  get_complete_trait_data(trait_data = tachet[!is.na(genus) &
                                                is.na(species) &
                                                !genus %in% Trait_EU_genus$genus,])

tachet <- Reduce(merge, data_tachet[c("locom",
                                      "feed",
                                      "resp",
                                      "volt",
                                      "ovip",
                                      "size")])
# define col names
cols <- names(Trait_EU_genus)[names(Trait_EU_genus) %like% pat_traitname]

# bind trait data
Trait_EU_genus <- 
  rbind(Trait_EU_genus, 
        tachet[, -"species"]
  ) 

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
}), .N),
.SDcols = names(Trait_EU_genus) %like% pat_traitname,
by = family]

# family information in tachet is already covered in agg. freshwaterecol
# except for Sparganophilidae, whose information is - however - not complete

# merge back information on order 
Trait_EU_agg <- Trait_fam[Trait_EU_genus,
                          `:=`(order = i.order),
                          on = "family"]
# del not used columns
Trait_EU_agg[, N := NULL]

#### Add information on pattern of development ####
# Holometabolous or hemimetabolous?
hemimetabola <- c(
  "Ephemeroptera",
  "Odonata",
  "Plecoptera",
  "Grylloblattodea",
  "Orthoptera",
  "Phasmatodea",
  "Zoraptera",
  "Embioptera",
  "Dermaptera",
  "Mantodea",
  "Blattodea",
  "Isoptera",
  "Thyssanoptera",
  "Hemiptera",
  "Phthriptera",
  "Psocoptera"
)

holometabola <- c(
  "Coleoptera",
  "Streptsiptera",
  "Raphidioptera",
  "Megaloptera",
  "Neuroptera",
  "Diptera",
  "Mecoptera",
  "Siphonoptera",
  "Lepidoptera",
  "Trichoptera",
  "Hymenoptera"
)

Trait_EU_agg[, pattern_of_development := ifelse(
  order %in% hemimetabola,
  "hemimetabolous",
  ifelse(order %in% holometabola,
         "holometabolous",
         "no_insect")
)] 

# cache as RDS object
saveRDS(object = Trait_EU_agg, file = file.path(data_out, "Trait_EU_agg.rds"))
