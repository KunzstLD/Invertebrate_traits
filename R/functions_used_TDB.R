# _________________________________________________________________________
# Collection of functions used in Trait Analysis
# _________________________________________________________________________

# _________________________________________________________________________
# Preprocessing  & data cleaning
# _________________________________________________________________________

#### Load multiple rds files in a list ####
load_data <- function(path, pattern) {
  files <- list.files(path = path, pattern = pattern)
  data <- lapply(files, function(y) readRDS(file = file.path(path, y)))
  data <- setNames(data, files)
  data
}

#### Cleaning text (colnames) ####
text_with_underscore <- function(text) {
  ind <- grep("[[:space:]]|\\,|\\;|\\.|\\(|\\)|\\-", text)
  text[ind] <- gsub("\\,|\\;|\\.|\\(|\\)|\\-", "", text[ind])
  text[ind] <- gsub("[[:space:]]", "_", text[ind])
  return(text)
}

#### Capitalize the first letter ####
simpleCap <- function(x) {
  s <- tolower(x)
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

#### Google search ####
query_google <- function(x) {
  invisible(lapply(x,
                   function(y) {
                     utils::browseURL(url = paste0("https://google.com/search?q=", y))
                   }))
}

#### coalesce implementation when x is zero ####
coalesce_with_zero <- function(...) {
  Reduce(
    function(x, y) {
      i <- which(x == 0)
      x[i] <- y[i]
      x
    },
    list(...)
  )
}

#### coalescing join ####
coalesce_join <- function(x,
                          y,
                          by = NULL,
                          suffix = c(".x", ".y"),
                          join = NULL,
                          ...) {
  # transform to tible -> join needs tibble
  if (any(class(x) %in% "data.table")) {
    x <- as_tibble(x)
  }
  if (any(class(y) %in% "data.table")) {
    y <- as_tibble(y)
  }

  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output/ # colnames that both sets share
  cols <- union(names(x), names(y))

  # fetch those col that contain .x or .y (those won't be in the original sets, are
  # created by the join)
  to_coalesce <- names(joined)[!names(joined) %in% cols]

  # extract suffix -> just the number of their characters will be important
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]

  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- purrr::map_dfc(to_coalesce, ~ dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  # get col from joined & coalesced, but only those in x (relevant col from y are in coalesced)
  dplyr::bind_cols(joined, coalesced)[names(x)]

  # In original version -> modify by when different keys have been used
  # dplyr::bind_cols(joined, coalesced)[cols]
}

# Testing coalesce join
# zeros from Tachet DB get transformed to NAs -> should be no problem
# Tachet DB: missing information for taxon & variable -> just 0
# When information for a particular trait is availabile, NA values in this trait are assumend to be 0
# test:
# tachet_test <- tachet[1, .(volt_uni, volt_semi, volt_bi, stage_egg, Ph_6_t, Species_merge)]
# dat_EU_test <- dat_EU[grep("Spongilla lacustris", species), .(volt_uni, volt_semi,
#                                                               volt_bi, stage_egg, ph_acidic,
#                                                               species)]
# str(tachet_test)
# str(dat_EU_test)
# test <- coalesce_join(x = dat_EU_test, y = tachet_test, by = c("species" = "Species_merge"),
#                       join = dplyr::left_join)
# nrow(test)

#### coalescing join using colesce_with_zero ####
coalesce_join_with_zero <- function(x,
                                    y,
                                    by = NULL,
                                    suffix = c(".x", ".y"),
                                    join = NULL,
                                    ...) {
  # transform to tible -> join needs tibble
  if (any(class(x) %in% "data.table")) {
    x <- as_tibble(x)
  }
  if (any(class(y) %in% "data.table")) {
    y <- as_tibble(y)
  }

  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output/ # colnames that both sets share
  cols <- union(names(x), names(y))

  # fetch those col that contain .x or .y (those won't be in the original sets, are
  # created by the join)
  to_coalesce <- names(joined)[!names(joined) %in% cols]

  # extract suffix -> just the number of their characters will be important
  suffix_used <-
    suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]

  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <-
    purrr::map_dfc(to_coalesce, ~ coalesce_with_zero(
      joined[[paste0(.x, suffix[1])]],
      joined[[paste0(.x, suffix[2])]]
    ))
  names(coalesced) <- to_coalesce

  # get col from joined & coalesced, but only those in x (relevant col from y are in coalesced)
  dplyr::bind_cols(joined, coalesced)[names(x)]

  # In original version -> modify by when different keys have been used
  # dplyr::bind_cols(joined, coalesced)[cols]
}
# test
# test <- data.table(x = c(0,0,1), y = c(1,1,2), join = c("A", "B", "C"))
# test_2 <-  data.table(x = c(1,2,1), y = c(2,2,2), z = c(1,1,2), join = c("A", "B", "C"))
# coalesce_join_with_zero(x = test, y = test_2,
#                         by = "join",
#                         join = dplyr::left_join)


#### Fetch duplicate taxa ####
# Returns dataset (ordered) only with the duplicated entries
# requires:
# x is col in quotes
# dat is data.frame/data.table
fetch_dupl <- function(data, col) {
  # fetch rows with duplicates
  n_occur <- data.frame(table(data[[col]]))
  dupl <- n_occur[n_occur$Freq > 1, "Var1"]
  output <- data[data[[col]] %in% dupl, ]
  # order and return output
  output[order(output[[col]]), ]
}

#### Aggregate duplicate taxa entries ####


# Simple aggregation function for DB that in itself consist of
# multiple DBs (e.g. Australian trait DB)
# There, duplicates originate from the different original databases
# and often produce entries like (1, 0) where 
aggr_dupl_multDB <- function(y) {
  if(sum(y) == 0)
    0
  else
    mean(y[y != 0])
}

# NOTICE: Not used anymore, far to complex for what they were 
# actually for
# Duplicate taxa with numeric values are condensed:
# The Mode is taken when the same taxa occurs multiple times
# If all values are distinct, the median is taken.
# See also the documentation of "condense_dupl_numeric_agg"
# requires:
# dataset
# column with duplicates
# all non trait columns
# condense_dupl_numeric <- function(trait_data, col_with_dupl_entries, non_trait_cols) {
# 
#   #
#   if (nrow(trait_data[duplicated(get(col_with_dupl_entries)), ]) == 0) {
#     stop("Input data has no duplicates. Check for duplicates first before applying this function.")
#   }
# 
#   # subset to duplicate taxa
#   dupl_taxa <-
#     trait_data[
#       duplicated(get(col_with_dupl_entries)),
#       unique(get(col_with_dupl_entries))
#     ]
# 
#   # create data.tables with and without duplicates
#   dupl <- trait_data[get(col_with_dupl_entries) %in% dupl_taxa, ]
#   without_dupl <-
#     trait_data[!(get(col_with_dupl_entries) %in% dupl_taxa), ]
# 
#   # transform duplicates into long format
#   # function should have the possibility to choose id.vars (non trait columns)
#   dupl_lf <- melt(
#     data = dupl,
#     id.vars = non_trait_cols
#   )
# 
#   # Actual condensation of trait values
#   # rather take Mode but throw out zero? instead of median in second ifelse() statement
#   dupl_lf[, `:=`(value = ifelse(
# 
#     # is TRUE when NOT all values are distinct
#     length(value) != length(unique(value)),
#     #  is TRUE when not just zeros are present
#     ifelse(
#       !identical(value[value != 0], numeric(0)),
#       Mode(value[value != 0], na.rm = TRUE),
#       Mode(value, na.rm = TRUE)
#     ),
#     # just distinct values
#     # zeros are dropped -> are actually not observed values
#     ifelse(y == 0,
#       y,
#       median(y[y != 0], na.rm = TRUE)
#     )
#   )),
#   by = variable
#   ]
# 
#   # create formula for dcast
#   formula <-
#     paste0(paste0(non_trait_cols, collapse = "+"), "~", "variable") %>%
#     as.formula()
# 
#   # convert back to long format
#   dupl <- dcast(
#     data = dupl_lf,
#     formula = formula,
#     fun.aggregate = mean
#   )
# 
#   # bind duplicates and non duplicates back
#   data <- rbind(dupl, without_dupl)
#   return(data)
# }
# 
# # Core aggregation function from the function "condese_dupl_numeric"
# # Aggregates duplicates so that the most common value is taken
# # In case just distinct values are present the median is taken
# # Zeros are filtered out if values different from zero exist
# # Problematic for cases like:
# # Mode(c(1, 1, 2, 2, 3)) compared to Mode(c(2, 2, 1, 1, 3))
# # Can be used within data.table
# condense_dupl_numeric_agg <- function(y) {
#   ifelse(
# 
#     # is TRUE when NOT all values are distinct
#     length(y) != length(unique(y)),
#     #  is TRUE when not just zeros are present
#     ifelse(
#       !identical(y[y != 0], numeric(0)),
#       Mode(y[y != 0], na.rm = TRUE),
#       Mode(y, na.rm = TRUE)
#     ),
#     # just distinct values
#     # zeros are dropped -> are actually not observed values
#     ifelse(sum(y) == 0,
#       y,
#       median(y[y != 0], na.rm = TRUE)
#     )
#   )
# }
# Testing:
# test <- data.table(x = c("A", "A", "B", "D", "D", "D"),
#                    y = c(1, 0, 5, 0.5, 0.5, NA))
# test[, lapply(.SD, condense_dupl_numeric_agg), .SDcols = "y",
#      by = .(x)]


#### Get only complete trait data ####
# NOTICE: This function can be used when certain trait states of a given trait
# have only zeros (!) as entries. Is not needed anymore since now subsetting
# is done with NA

# Get complete trait data from a dataset with traits and taxa information
# (meaning at least one value different from zero)
# Gives a list with as many datasets as traits are presented
# Can be merged together using Reduce!
# works atm without considering NAs
# non trait column need to be manually defined by user
# get_complete_trait_data <- function(trait_data, non_trait_col) {
#
#   # pattern of non trait col names
#   non_trait_col_pat <- paste0("(?i)", paste0(non_trait_col, collapse = "|"))
#
#   # create name vector
#   name_vec <-
#     grep(
#       non_trait_col_pat,
#       names(trait_data),
#       invert = TRUE,
#       value = TRUE
#     ) %>%
#     sub("\\_.*", "", .) %>%
#     unique()
#
#   # create output matrix
#   output <- matrix(ncol = 2, nrow = length(name_vec))
#
#   data <- list()
#   for (i in seq_along(name_vec)) {
#     row <- trait_data[, base::sum(.SD) > 0,
#                       .SDcols = names(trait_data) %like% name_vec[i],
#                       by = 1:nrow(trait_data)]$V1
#
#     #
#     data[[i]] <- trait_data[row, .SD,
#                             .SDcols = names(trait_data) %like%
#                               paste(c(
#                                 name_vec[i],
#                                 non_trait_col
#                               ),
#                               collapse = "|")]
#     names(data)[[i]] <- name_vec[[i]]
#   }
#   return(data)
# }
## Data were processed and merged together with Reduce
# data <- get_complete_trait_data(
#   trait_data = Trait_Noa_new,
#   non_trait_col = c("unique_id",
#                     "species",
#                     "genus",
#                     "family",
#                     "order")
# )
# # lapply(data, nrow)
# Trait_Noa_new <- Reduce(merge, data[c("locom",
#                                       "feed",
#                                       "resp",
#                                       "volt",
#                                       "size",
#                                       "dev")])

#### create individual pattern of trait name (not category!) ####
# i.e. feed_herbivore, feed_shredder -> feed
# TODO: Add unit test
create_pattern_ind <- function(x, non_trait_cols) {
  pat <- paste0(non_trait_cols, collapse = "|")
  # get trait names & create pattern for subset
  trait_names_pattern <-
    grep(pat, names(x), value = TRUE, invert = TRUE) %>%
    sub("\\_.*|\\..*", "", .) %>%
    unique() %>%
    paste0("^", .)
  return(trait_names_pattern)
}

#### Normalization of trait scores ####
# All trait states of one trait are divided by their row sum
# Hence, trait affinities are represented as "%" or ratios
normalize_by_rowSum <- function(x, non_trait_cols, na.rm) {
  # get trait names & create pattern for subset
  trait_names_pattern <- create_pattern_ind(x = x,
                                            non_trait_cols = non_trait_cols)
  
  # loop for normalization (trait categories for each trait sum up to 1)
  for (cols in trait_names_pattern) {
    # get row sum for a specific trait
    x[, rowSum := apply(.SD, 1, sum, na.rm = na.rm),
      .SDcols = names(x) %like% cols]
    
    # get column names for assignment
    col_name <- names(x)[names(x) %like% cols]
    
    # divide values for each trait state by
    # the sum of trait state values
    x[, (col_name) := lapply(.SD, function(y) {
      round(y / rowSum, digits = 2)
    }),
    .SDcols = names(x) %like% cols]
  }
  # del rowSum column
  x[, rowSum := NULL]
  return(x)
}

#### check for completeness of trait dataset ####
completeness_trait_data <- function(x, non_trait_cols) {
  trait_names_pattern <- create_pattern_ind(
    x = x,
    non_trait_cols = non_trait_cols
  )

  # test how complete trait sets are
  output <- matrix(ncol = 2, nrow = length(trait_names_pattern))
  for (i in seq_along(trait_names_pattern)) {
    # vector containing either 0 (no NAs) or a number (> 0) meaning that all
    # entries for this trait contained NA
    vec <-
      x[, apply(.SD, 1, function(y) {
        base::sum(is.na(y))
      }),
      .SDcols = names(x) %like% trait_names_pattern[[i]]
      ]

    # How complete is the dataset for each individual trait?
    output[i, ] <-
      c(
        (length(vec[vec == 0]) / nrow(x)) %>% `*`(100) %>% round(),
        trait_names_pattern[[i]]
      )
  }
  return(output)
}


# _________________________________________________________________________
# Trait Aggregation
# Mode
# when there are no duplicate values, mode returns the first value!
# ________________________________________________________________________
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}