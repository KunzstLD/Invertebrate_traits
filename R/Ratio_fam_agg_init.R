# How many families end up in the final
# preprocessed trait datasets compared to the initial
# trait datasets?
# This gives us insight how much and how consistent
# information is covered in these trait databases
# Notice: trait information gets not lost during aggregation
# TODO: Identify missing families

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
file_names <- sub("(\\.\\/.*)(\\/.*)(\\/)(.*)(\\.rds)", "\\4", files_init)

# assign names to list
for (i in seq_along(file_names)) {
  names(datasets_init)[[i]] <- file_names[[i]]
}

# create individual datasets from list
EU_init <- datasets_init[c("Trait_EU_pp_harmonized", "Trait_Tachet_pp_harmonized")]
NOA_init <- datasets_init[c("Traits_US_LauraT_pp_harmonized", "Traits_US_pp_harmonized")]
c(AUS_init, NZ_init) %<-% list(datasets_init$.Trait_AUS_harmonized,
                            datasets_init$.Trait_NZ_pp_harmonized)

# load aggregated datasets
files_agg <- dir(path = data_out,
                 recursive = TRUE, 
                 full.names = TRUE, 
                 pattern = ".*\\.rds")

datasets_agg <- lapply(files_agg, readRDS)

# get file names
file_names_agg <- sub("(\\.\\/.*)(\\/)(.*)(\\.rds)", "\\3", files_agg)

# assign names for each list element
for (i in seq_along(file_names_agg)) {
  names(datasets_agg)[[i]] <- file_names_agg[[i]]
}

# _______________________________________________
#### Calcule coverage of families per region ####
# _______________________________________________

# aggregated datasets
# count families per order
# Restricted to orders from Trait profiles project 
cov_agg <- rbindlist(lapply(datasets_agg, function(y) {
  y[, .(family, order)]
}),
idcol = "file"
) %>%
  .[, .(family, .N), by = .(file, order)]

#### EU ####
# prepare aggregated data for EU
cov_agg_EU <- cov_agg[grepl("Trait_EU.*", file), ] %>%
  .[!duplicated(order), -("family")]

# EU initial data
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

# Calculate taxonomical coverage EU
cov_agg_EU[cov_EU_init,
  `:=`(N_init = i.N),
  on = "order"
] %>%
  .[, `:=`(
    file = file,
    order = order,
    Number_agg_families = N,
    Number_initial_families = N_init,
    taxonomical_coverage = N / N_init
  )]

#### NOA ####
# prepare aggregated data NOA
cov_agg_NOA <- cov_agg[grepl("Trait_Noa.*", file), ] %>%
  .[!duplicated(order), -("family")]

# NOA initial data
cov_init_NOA <- rbindlist(lapply(NOA_init, function(y)
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

# Calculate taxonomical coverage NOA
cov_agg_NOA[cov_init_NOA,
  `:=`(N_init = i.N),
  on = "order"
] %>%
  .[, `:=`(
    file = file,
    order = order,
    Number_agg_families = N,
    Number_initial_families = N_init,
    taxonomical_coverage = N / N_init
  )]

#### AUS ####
# prepare aggregated data NOA
cov_agg_AUS <- cov_agg[grepl("Trait_AUS.*", file), ] %>%
  .[!duplicated(order), -("family")]

# NOA initial data
cov_init_AUS <- AUS_init[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera"
), .(family, order)] %>%
  .[!duplicated(family), .(family, .N),
    by = order
  ]

# Calculate taxonomical coverage NOA
cov_agg_AUS[cov_init_AUS,
  `:=`(N_init = i.N),
  on = "order"
] %>%
  .[, `:=`(
    file = file,
    order = order,
    Number_agg_families = N,
    Number_initial_families = N_init,
    taxonomical_coverage = N / N_init
  )]

 #### NZ ####
cov_agg_NZ <- cov_agg[grepl("Trait_NZ.*", file), ] %>%
  .[!duplicated(order), -("family")]

# NOA initial data
cov_init_NZ <- NZ_init[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera"
), .(family, order)] %>%
  .[!duplicated(family), .(family, .N),
    by = order
  ]

# Calculate taxonomical coverage NOA
cov_agg_NZ[cov_init_NZ,
  `:=`(N_init = i.N),
  on = "order"
] %>%
  .[, `:=`(
    file = file,
    order = order,
    Number_agg_families = N,
    Number_initial_families = N_init,
    taxonomical_coverage = N / N_init
  )]

# bring all datasets together
tax_coverage <- rbind(
  cov_agg_EU, cov_agg_NOA,
  cov_agg_AUS, cov_agg_NZ
)

# convert to wide table
data.table::dcast(tax_coverage[, .(file, order, taxonomical_coverage)],
                  formula = file ~ order,
                  value.var = "taxonomical_coverage")