# ------------------------------------------------------------------------------
#### Preprocessing of freshwaterecology DB ####
# ------------------------------------------------------------------------------

# read in
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

# get trait columns
cols <- grep("order|family|genus|species|unique_id", names(dat_EU), 
             value = TRUE, invert = TRUE)
dat_EU[, (cols) := lapply(.SD, as.numeric), 
       .SDcols = !names(dat_EU) %like% "order|family|genus|species|unique_id"]

# transform all NA values to 0 
# needed for harmonization later
for (j in cols){
  data.table::set(dat_EU, which(is.na(dat_EU[[j]])),j,0)
}

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

# taxonomical corrections
# Microhydra sowerbyi is actually a freshwater jellyfish
# family: Olindiidae, order: Hydrozoa
dat_EU[grepl("Microhydra sowerbyi", species), `:=`(family = "Olindiidae",
                                                   order = "Hydrozoa")]

# save as RDS
saveRDS(object = dat_EU,
        file = file.path(data_cleaned, "EU", "Trait_Freshecol_pp.rds"))
