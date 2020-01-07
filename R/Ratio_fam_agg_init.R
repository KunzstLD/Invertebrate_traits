# How many families end up in the final
# aggregated trait datasets compared to the initial
# (not aggregated) trait datasets?
# This gives us insight how much and how consistent
# information is covered in these trait databases


# _______________________________________________
#### Load initial & aggregated final dataset ####
# _______________________________________________

# initial datasets
files_init <- dir(data_cleaned,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "harmonized"
)

# create list with initial datasets
datasets_init <- lapply(files_init, readRDS)

# get file names
file_names <- sub("(\\/.*)(\\/.*)(\\/)(.*)(\\.rds)", "\\4", files_init)

# assign names to list
for (i in seq_along(file_names)) {
  names(datasets_init)[[i]] <- file_names[[i]]
}

EU_init <- datasets_init[c(".Trait_EU_pp_harmonized", ".Trait_Tachet_pp_harmonized")]
NOA_init <- datasets_init[c(".Trait_US_LauraT_pp_harmonized", "Trait_US_pp_harmonized")]

# TODO: Check Zeallot
# c(AUS_init, NZ_init) %<-% c(datasets_init$.Trait_AUS_harmonized,
#                             datasets_init$.Trait_NZ_pp_harmonized)

str(datasets_init$.Trait_AUS_harmonized)

# aggregated datasets
files_agg <- list.files(path = file.path(data_out), pattern = ".*\\.rds")
datasets_agg <-
  lapply(seq_along(files_agg), function(i)
    readRDS(file.path(data_out, files_agg[[i]])))

# assign names for each list element
for (i in seq_along(files_agg)) {
  names(datasets_agg)[[i]] <- files_agg[[i]]
}

# _______________________________________________
#### Calcule coverage of families per region ####
# _______________________________________________

# aggregated datasets
cov_agg <- rbindlist(lapply(datasets_agg, function(y) {
  y[order %in% c(
    "Ephemeroptera",
    "Hemiptera",
    "Odonata",
    "Trichoptera",
    "Coleoptera",
    "Plecoptera",
    "Diptera"
  ), .(family, order)]
}),
idcol = "file"
) %>%
  .[, .(family, .N), by = .(file, order)]

# EU
cov_EU_init <- rbindlist(lapply(EU_init, function(y)
  y[order %in% c(
    "Ephemeroptera",
    "Hemiptera",
    "Odonata",
    "Trichoptera",
    "Coleoptera",
    "Plecoptera",
    "Diptera"
  ), .(family, order)]),
  idcol = "file") %>%
  .[!duplicated(family), .(file, family, .N),
    by = order]

cov_agg_EU <- cov_agg[grepl("Trait_EU.*", file), ] %>%
  .[!duplicated(order), -("family")]

cov_agg_EU[cov_EU_init,
  `:=`(N_init = i.N),
  on = "order"
] %>%
  .[, .(file,
        order,
        Number_agg_families = N,
        Number_initial_families = N_init,
        taxonomical_coverage = N / N_init
  )]

# NoA

#AUS & NZ