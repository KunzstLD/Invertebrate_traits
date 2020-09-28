# ------------------------------------------------------------------------------------
#### Put raw data from Freshwaterecology.info DB 2018 Macroinvertebrates together ####

# getting links to the taxadbexport files files
# manually exportet from website
filelinks <- list.files(
  path = file.path(data_in, "EU", "Raw_freshecol"),
  pattern = "taxadbexport",
  full.names = TRUE
)

# Read in
output_db <- replicate(length(filelinks), data.frame())

for (i in seq_along(filelinks)) {

  # fetch line where actual DB starts
  ind <- grep("Taxon", readLines(filelinks[i]))

  # read in
  db <- read.table(filelinks[i], skip = ind, sep = ";", header = TRUE, na.strings = (""), fill = TRUE)

  # put into list
  output_db[[i]] <- db
}

# bind
freshecol_db <- do.call(rbind, output_db)

# read in those files where traits had to be downloaded in single bits
filelinks_1 <- list.files(data_in, pattern = "diptera|coleoptera", full.names = TRUE, ignore.case = TRUE)

#
output_db1 <- replicate(length(filelinks_1), data.frame())

for (i in seq_along(filelinks_1)) {

  # fetch line where actual DB starts
  ind <- grep("Taxon", readLines(filelinks_1[i]))

  # read in
  db <- read.table(filelinks_1[i], skip = ind, sep = ";", header = TRUE, na.strings = (""), fill = TRUE)

  # put into list
  output_db1[[i]] <- db
}

# bind
freshecol_db1 <- do.call(cbind, output_db1[1:3])
freshecol_db2 <- do.call(cbind, output_db1[4:6])

# rename taxa column
names(freshecol_db)[1] <- "Taxon"
names(freshecol_db1)[1] <- "Taxon"
names(freshecol_db2)[1] <- "Taxon"

# delete EU/X.* columns
freshecol_db <- freshecol_db[, !names(freshecol_db) == "EU"]
freshecol_db1 <- freshecol_db1[, !names(freshecol_db1) %like% "EU.*|X.*"]
freshecol_db2 <- freshecol_db2[, !names(freshecol_db2) %like% "EU.*|X.*"]

# bind with freshecol
freshecol_db_final <- rbind(freshecol_db, freshecol_db1, freshecol_db2)

# delete Family names
freshecol_db_final <- freshecol_db_final[-grep("[[:upper:]]{3}", freshecol_db_final$Taxon), ]

# Delete rows with just NA values
ind <- apply(freshecol_db1[, -1], MARGIN = 1, function(y) all(is.na(y)))
freshecol_db1 <- freshecol_db1[!ind, ]

# Original
# write
# write.csv(
#   freshecol_db_final,
#   "/home/kunz/Dokumente/Trait DB/Europe/Freshwater_info/Freshwaterecol_Aug_2018.csv"
# )

# save
saveRDS(
  object = freshecol_db_final,
  file = file.path(
    data_in,
    "EU",
    "Freshwaterecol_Aug_2018.rds"
  )
)
