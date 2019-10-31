# _____________________________________________________________________
#### Calculate "affinity scores" based on frequency ####
# Frequency of each trait state is converted into a probability
# Code is from Laura Twardochleb and just slightly changed by me
# TODO:Improve Code by using DT
# _____________________________________________________________________

library(data.table)
library(dplyr)
library(tidyr)

# read in
Trait_Noa_new <- fread(file.path(data_in, "North_America", "Trait_table_LauraT.csv"))

# select a few columns
Subset_Noa_new <- Trait_Noa_new[, .(Voltinism_abbrev, Thermal_pref, Habit_prim, Habit_sec,
                                    Genus, Family, Order)]

# set "Other" as category to NA
for(j in names(Subset_Noa_new)) {
  data.table::set(Subset_Noa_new,
                  which(Subset_Noa_new[[j]] %in% "Other (specify in comments)"),
                  j,
                  NA)
}

mode_fn <-
  function(x) {
    ifelse(any(!is.na(x)), names(which.max(table(x))), as.character(NA))
  }

# trait mode according to Laura
Subset_Noa_new %>% 
  filter(!is.na(Genus)) %>%
  group_by(Genus) %>% 
  summarize_all(mode_fn)

# reshape table into long format
keycol=c("Trait_group")
valuecol=c("Trait")
gathercol <- c(
  "Habit_prim",
  "Habit_sec",
  "Thermal_pref",
  "Voltinism_abbrev"
)

# long format
traits_long<- Subset_Noa_new  %>% 
  filter(!is.na(Genus)) %>% 
  gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments

# calculate frequency of trait state occurrence per genus and trait
# convert data back to wide format
traits_long %>%
  filter(!is.na(Trait)) %>%
  group_by(Genus, Trait_group, Trait) %>%
  tally() %>%
  mutate(Percent = n / sum(n)) %>% 
  dcast(., Genus ~ Trait+ Trait_group)# %>% 
#  filter(grepl("Acentrella", Genus))





