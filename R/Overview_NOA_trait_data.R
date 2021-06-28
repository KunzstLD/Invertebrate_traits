# __________________________________________________________________________________________________
# Harmonized trait DB
# __________________________________________________________________________________________________
Trait_Noa_new <- readRDS(file = file.path(data_cleaned,
                                          "North_America",
                                          "Traits_US_LauraT_pp_harmonized.rds"))
names(Trait_Noa_new)
# feeding mode
# locomotion (crawler, burrower, clinger, ...)
# size
# respiration 
# voltinism
# oviposition
# body form

# Check completeness of trait data
# just return rows where for each trait there is an observation 
# oviposition is clearly the bottleneck!
completeness_trait_data(x = Trait_Noa_new,
                        non_trait_cols = c("unique_id",
                                           "species",
                                           "genus",
                                           "family",
                                           "order"))

# Information on species-level: 2414
Trait_Noa_new[!is.na(species), ]
# Information on genus-level: 1163
Trait_Noa_new[is.na(species) & !is.na(genus), ]
# Information on family-level: 176
Trait_Noa_new[is.na(species) & is.na(genus) & !is.na(family), ]

# How many taxa per order?
Trait_Noa_new[, .N, by = order] %>% 
  ggplot(., aes(x = as.factor(order),
                              y = N)) +
  geom_pointrange(aes(ymin = 0,
                      ymax = N)) +
  #scale_y_continuous(trans = 'log10') +
  labs(x = "Order", y = "Number of taxa per order") +
  ggtitle("Overview taxa harmonized NA trait database") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(
      family = "Roboto Mono",
      size = 14,
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.text.y = element_text(family = "Roboto Mono",
                               size = 14),
    legend.title = element_text(family = "Roboto Mono",
                                size = 16),
    legend.text = element_text(family = "Roboto Mono",
                               size = 14),
    strip.text = element_text(family = "Roboto Mono",
                              size = 14),
    panel.grid = element_blank()
  )

# __________________________________________________________________________________________________
# Twardochleb DB
# see Paper: https://onlinelibrary.wiley.com/doi/10.1111/geb.13257
# __________________________________________________________________________________________________

# __________________________________________________________________________________________________
# Vieira DB
# __________________________________________________________________________________________________
Trait_Vieira <- readRDS(file = file.path(data_cleaned,
                                         "North_America",
                                         "Traits_US_pp.rds"))
# Information on species-level: 1761
Trait_Vieira[!is.na(Species), ]
# Information on genus-level: 924
Trait_Vieira[is.na(Species) & !is.na(Genus), ]
# Information on family-level: 163
Trait_Vieira[is.na(Species) & is.na(Genus) & !is.na(Family), ]

# How many taxa per order?
Trait_Vieira[, .N, by = Order] %>% 
  ggplot(., aes(x = as.factor(Order),
                y = N)) +
  geom_pointrange(aes(ymin = 0,
                      ymax = N)) +
  #scale_y_continuous(trans = 'log10') +
  labs(x = "Order", y = "Number of taxa per order") +
  ggtitle("Overview taxa Vieira trait database") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(
      family = "Roboto Mono",
      size = 14,
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.text.y = element_text(family = "Roboto Mono",
                               size = 14),
    legend.title = element_text(family = "Roboto Mono",
                                size = 16),
    legend.text = element_text(family = "Roboto Mono",
                               size = 14),
    strip.text = element_text(family = "Roboto Mono",
                              size = 14),
    panel.grid = element_blank()
  )






