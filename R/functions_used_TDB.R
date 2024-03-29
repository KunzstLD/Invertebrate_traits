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

#### create individual pattern of trait name (not category!) ####
# i.e. feed_herbivore, feed_shredder -> feed
# TODO: Add unit test
create_pattern_ind <- function(x, non_trait_cols) {
  if (missing(non_trait_cols)) {
    trait_names_pattern <- sub("\\_.*|\\..*", "", names(x)) %>%
      unique() %>%
      paste0("^", .)
  } else{
    pat <- paste0(non_trait_cols, collapse = "|")
    # get trait names & create pattern for subset
    trait_names_pattern <-
      grep(pat, names(x), value = TRUE, invert = TRUE) %>%
      sub("\\_.*|\\..*", "", .) %>%
      unique() %>%
      paste0("^", .)
  }
  trait_names_pattern
}

#### Normalization of trait scores ####
# All trait states of one trait are divided by their row sum
# Hence, trait affinities are represented as "%" or ratios
# this works for traits named: "groupingfeature_trait"
normalize_by_rowSum <- function(x, 
                                non_trait_cols, 
                                na.rm = TRUE) {
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
      y / rowSum
    }),
    .SDcols = names(x) %like% cols]
  }
  # del rowSum column
  x[, rowSum := NULL]
  return(x)
}

# For binary coded traits!
# Check for a given grouping feature if taxa have multiple
# traits assigned
# taxonomic columns must be written in small letters
taxa_not_normalized <- function(x, pattern) {
  ind <- which(rowSums(x[, .SD,
                         .SDcols = patterns(pattern)]) > 1)
  x[ind, .SD, .SDcols = patterns(paste0(pattern, "|species|genus|family|order"))]
}



#### Check for completeness of trait dataset ####
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

#### Check of trait values ####
# Are there any unexpected trait values?
# Gives rowSums per grouping feature
# Should be ideally applied to normalised data
# Values are 0 or maximum rowSum value
check_trait_values <- function(Trait_DB) {
  pat <- create_pattern_ind(Trait_DB[, -c("species",
                                          "genus",
                                          "family",
                                          "order")])
  names(pat) <- pat
  
  otp <- list()
  for (i in names(pat)) {
    test <- Trait_DB[, apply(.SD, 1, function(y)
      length(y[!is.na(y)])), .SDcols = patterns(pat[[i]])]
    otp[[i]] <- unique(test)
  }
  otp
}

# _________________________________________________________________________
#### Trait Aggregation ####
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

# direct aggregation to family level
direct_agg <- function(trait_data,
                       non_trait_cols, 
                       method,
                       na.rm = TRUE) {
  # get names of trait columns
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_col <- grep(pat, names(trait_data), value = TRUE, invert = TRUE)
  
  # aggregate to family-level
  # subset so that no NA values occur in data
  # (otherwise all NA entries are viewed as a group &
  # aggregated as well)
  agg_data <- trait_data[!is.na(family),
                         lapply(.SD, method, na.rm = na.rm),
                         .SDcols = trait_col,
                         by = "family"
                         ]
  
  # merge information on order back
  agg_data[trait_data,
           `:=`(order = i.order),
           on = "family"
           ]
  agg_data
}
