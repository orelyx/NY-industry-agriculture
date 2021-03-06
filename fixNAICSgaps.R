# title: "fixNAICSgaps.R"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

uncodedAgricultureRows <- RegionEnergyEsts %>% 
  filter(is.na(NAICS) && (IND_SECTOR == "Agriculture")) %>%
  select(-NAICS) %>% 
  mutate(NAICS = 119100)

# The NREL IEDB dataset contains some records in which the NAICS code given is 
# "11193 & 11194 & 11199" or "1125 & 1129". These are peculiar combinations:
# 11193 - Sugarcane FarmingT 
# 11194 - Hay FarmingT 
# 11199 - All Other Crop FarmingT 
# 1125 - AquacultureT 
# 1129 - Other Animal ProductionT 
# Not knowing what to make of these combinations, we just replace the codes with 
# our synthesized code 119100 - Agriculture (unspecified). 

# Find rows containing an NAICS value we want to replace, 
# and insert the replacement value. 
fixRows <- function(df, bad, fixed) {
  df %>% 
    filter((NAICS == bad) & (MMBTU_TOTAL >= 0.000001)) %>% 
    mutate(NAICS = fixed)
}

# Replace NAICS values 236, 237, and 238 with their synthetic 4-digit 
# replacements. This allows us to do 4-digit slices without missing 
# energy use for these NAICS categories. 
fixedRows236 <- fixRows(RegionEnergyEsts, 236, 2369)
fixedRows237 <- fixRows(RegionEnergyEsts, 237, 2378)
fixedRows238 <- fixRows(RegionEnergyEsts, 238, 2388)

# Pare off the rows we don't want; replace them with the rows we do. 
RegionEnergyEsts <- RegionEnergyEsts %>%
  filter(!((NAICS %in% c(236, 237, 238)) & (MMBTU_TOTAL >= 0.0000000001))) %>%
  bind_rows(fixedRows236, fixedRows237, fixedRows238) %>%
  filter(!is.na(NAICS)) %>%       # will be replaced with uncodedAgricultureRows
  bind_rows(uncodedAgricultureRows) %>%
  arrange(YEAR, County, NAICS)
