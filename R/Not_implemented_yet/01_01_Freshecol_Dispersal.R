# Dispersal Data from Sarremejane -----------------------------------------

# dissem traits contain -99 -> change to NA
# col <- grep("dissem", names(dat_EU), value = TRUE)
# dat_EU[, (col) := lapply(.SD, function(y) {
#   ifelse(y == -99, NA, y)
# }), .SDcols = names(dat_EU) %like% "dissem"]


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