# title: "importCountyEsts"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# We read in the entire dataset of US-wide county energy estimates. (It's large.)
Updated_county_energy_estimates <- read_csv("Updated-county-energy-estimates-fixed.gz",
                                            show_col_types = FALSE)
# Replace missing values with 0. 
Updated_county_energy_estimates$MECS_FT[is.na(Updated_county_energy_estimates$MECS_FT)] <- 0

# Let's read in some NY population stats by county
NYS_population_by_county <- read_delim("NYS population by county.tsv", 
     "\t", escape_double = FALSE, 
     trim_ws = TRUE, skip = 1, show_col_types = FALSE)
