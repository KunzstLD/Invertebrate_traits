# Invertebrate traits

This repo contains scripts for data preparation and trait harmonisation of trait data of aquatic invertebrates from different continents and regions based on trait databases developed for these continents and regions. Corresponding publication:

*Tackling inconsistencies in freshwater invertebrate trait databases: Harmonising across continents and aggregating taxonomic resolution*

doi:

[https://doi.org/10.1111/fwb.13840](https://doi.org/10.1111/fwb.13840)

*Stefan Kunz, Ben J. Kefford, Astrid Schmidt-Kloiber, Christoph D. Matthaei, Philippe Usseglio-Polatera, Wolfram Graf, N. LeRoy Poff, Leon Metzeling, Laura Twardochleb, Charles P. Hawkins, Ralf B. Schäfer*



For a detailed description please refer to the publication.


## Content overview

### - /Cleaned_data

Contains the preprocessed and harmonised trait datasets for each continent and region as *rds file*.

* "Trait_continent/region_preproc.rds": preprocessed and cleaned trait dataset (e.g. handling of duplicates, addition of taxonomic nomenclature, etc.)
* "Trait_continent/region_harmonised.rds": harmonised trait datasets

### - /Paper

Contains TeX files for the paper

### - /R

Contains R scripts for preprocessing and harmonisation. The raw data could not be provided but can be obtained from the respective providers of the trait databases (e.g., https://www.freshwaterecology.info/ ).

All R Scripts beginning starting with *02_* or higher can be reproduced with the data provided in **-Cleaned data**.
