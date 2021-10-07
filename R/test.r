# 
aus_old <- readRDS(file.path(data_cleaned, "Australia", "Trait_AUS_agg_old.rds"))

aus_old[!family %in% Trait_AUS_agg$family, ]


# Testing a function for finding thoses families with excess zeros ----
# Consider for median aggregation!
test <- Trait_Noa_new[family == "Leptoceridae", .SD,
    .SDcols = patterns("size|species|genus|family|order")
]

# Create a function out of this
test_excess_zeros <- function(trait_data,
                              trait_pattern) {
    family <- na.omit(trait_data[, .SD,
        .SDcols = patterns(paste0(trait_pattern, "|family"))
    ]) %>%
        .[, lapply(.SD, function(y) (sum(y == 0) / .N) > 0.5),
            by = .(family)
        ] %>%
        .[, all(.SD), .SDcols = patterns(trait_pattern), by = .(family)] %>%
        .[V1 == 1, family]

    list(
        "families" = family,
        "trait" = trait_pattern
    )
}

for (pat in c("feed", "size")) {
    test_excess_zeros(
        trait_data = Trait_Noa_new,
        trait = pat
    ) %>% print()
}



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
