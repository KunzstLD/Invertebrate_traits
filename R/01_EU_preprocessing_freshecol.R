# ------------------------------------------------------------------------------
#### Preprocessing of freshwaterecology DB and incorporation of Tachet data ####
# ------------------------------------------------------------------------------

# Prep freshecol DB ---------------------------------------------------
dat_EU <- fread(
  file.path(
    data_in,
    "EU",
    "macroinvertebrate_EUR_complete.csv"
  )
)

# Subset to relevant traits
dat_EU <- dat_EU[, .SD, .SDcols = names(dat_EU) %like%
                   "order|family|genus|species|^ph|feed|loco|resp|disp|stage|volt|rep|temp"]

# Voltinism trait contains thermal conditions, change every string to 1
col <- grep("volt", names(dat_EU), value = TRUE)
dat_EU[, (col) := lapply(.SD, function(y) {
  ifelse(!is.na(y), 1, NA)
}), .SDcols = names(dat_EU) %like% "volt"]

# dissem traits contain -99 -> change to NA
# col <- grep("dissem", names(dat_EU), value = TRUE)
# dat_EU[, (col) := lapply(.SD, function(y) {
#   ifelse(y == -99, NA, y)
# }), .SDcols = names(dat_EU) %like% "dissem"]

# get trait columns
cols <- grep("order|family|genus|species|unique_id", names(dat_EU), 
             value = TRUE, invert = TRUE)
dat_EU[, (cols) := lapply(.SD, as.numeric), 
       .SDcols = !names(dat_EU) %like% "order|family|genus|species|unique_id"]

# transform all NA values to 0 
for (j in cols){
  data.table::set(dat_EU, which(is.na(dat_EU[[j]])),j,0)
}

# Dispersal Data from Sarremejane -----------------------------------------
# disp_EU <-
#   fread(
#     file = file.path(data_in, "Europe", "Flying_propensity.csv"),
#     na.strings = ""
#   )
# 
# # fly propensity NA to medium
# disp_EU[grepl("NA", Flying_propensity), Flying_propensity := "Medium"]
# 
# # turn propensity trait into wide format
# disp_EU <- dcast(
#   data = disp_EU[!is.na(Flying_propensity)],
#   formula = Taxon  ~ Flying_propensity,
#   fun.aggregate = length,
#   value.var = "Flying_propensity"
# )
# 
# # filter for "dae"
# # ** -> ony family level identification possible, average of traits for all genera
# # in the given family
# disp_EU[grepl(".*dae", Taxon), `:=`(Family = sub("\\*\\*|[[:space:]]\\*\\*", "",
#                                                  Taxon),
#                                     Taxon = NA_character_)]
# # spp. to Genus
# disp_EU[grepl("spp\\.", Taxon), `:=`(Genus = sub("[[:space:]]spp\\.", "", Taxon),
#                                      Taxon = NA_character_)]
# 
# # * -> trait information from phylogenetically closest genera
# disp_EU[grepl("\\*", Taxon), Taxon := sub("\\*","",Taxon)]
# 
# # del group in Taxon
# disp_EU[grepl("group", Taxon), Taxon := sub("[[:space:]]group","", Taxon)]
# 
# # rename
# setnames(disp_EU, "Taxon", "Species")
# 
# # change col type to numeric for Medium, Strong, Weak
# disp_EU[, c("Medium", "Strong", "Weak") := lapply(.SD, as.numeric),
#         .SDcols = c("Medium", "Strong", "Weak")]


# Check duplicates ---------------------------------------------------
# Have accidentially some entries been copied that contain the same information?
# dupl_EU <- dat_EU_upd[species %in% dat_EU_upd[duplicated(species), ]$species, ]
# 
# # unique spec names to iterate over 
# unique_spec <- unique(dupl_EU$species)
# 
# for(taxa in unique_spec){
#   
#   df_1 <- dupl_EU[species %in% taxa, ][1,]
#   df_2 <- dupl_EU[species %in% taxa, ][2,]
#   
#   # anti join using all columns as join variables
#   not_shared_rows <- anti_join(df_1[, -c("unique_id"), with = FALSE],
#                               df_2[, -c("unique_id"), with = FALSE],
#                               by = names(df_1[, -c("unique_id"), with = FALSE])) %>%
#                     nrow()
#   
#   print(data.frame(taxa = taxa, 
#                     nr_not_shared_rows = not_shared_rows))
# }
# mini test
# test_1 <- data.frame(x = c(0), y = (1), z = c("taxa1"))
# test_2 <- data.frame(x = c(1), y = (1), z = c("taxa1"))
# 
# anti_join(test_1,
#           test_2,
#           by = names(test_1))
# YES! -> few entries have been copied and can therefore be removed
dat_EU <- dat_EU[!duplicated(species), ]

# save as RDS
# saveRDS(object = dat_EU,
#         file = file.path(data_cleaned, "EU", "Trait_Freshecol_pp.rds"))
