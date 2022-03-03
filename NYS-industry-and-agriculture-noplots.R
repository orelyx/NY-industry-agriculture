##################################################################################
# title: "NYS-industry-and-agriculture-noplots.R"
# author: "Eric Koski"
# date: "2/16/2022"
#
# A lightweight variant of NYS-industry-and-agriculture.R which executes the main 
# processing steps but bypasses generating plots or tables. Can be run non-
# interactively, in which case it simply generates the output datasets for New 
# York State as a whole. 
#
#     Copyright (c) 2021, 2022 by Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/.

knitr::opts_chunk$set(echo = FALSE, eval = TRUE)
source("setup.R")
outputFormat <- ""    # rmarkdown::all_output_formats(knitr::current_input())[[1]]
source("utilFunctions.R")
source("conversions.R")

AllRegionsCounties <- list(
  "Western New York" = c("Niagara", "Erie", "Chautauqua", "Cattaraugus", "Allegany"),
  "Finger Lakes" = c("Monroe", "Orleans", "Genesee", "Wyoming", "Livingston", "Ontario", "Yates",  "Wayne",   "Seneca"),
  "Southern Tier" = c("Steuben", "Schuyler", "Chemung", "Tompkins", "Tioga", "Chenango", "Broome", "Delaware"),
  "Central New York" = c("Cortland", "Cayuga", "Onondaga", "Oswego", "Madison"),
  "North Country" = c("St. Lawrence", "Lewis", "Jefferson", "Hamilton", "Essex", "Clinton", "Franklin"),
  "Mohawk Valley" = c("Oneida", "Herkimer", "Fulton", "Montgomery", "Otsego", "Schoharie"),
  "Capital District" = c("Albany", "Columbia", "Greene", "Warren", "Washington", "Saratoga", "Schenectady", "Rensselaer"),
  "Hudson Valley" = c("Sullivan", "Ulster", "Dutchess", "Orange", "Putnam", "Rockland", "Westchester"),
  "New York City" = c("New York", "Bronx", "Queens", "Kings", "Richmond"),
  "Long Island" = c("Nassau", "Suffolk")
)

CountyRegions <- NULL

for (r in names(AllRegionsCounties)) {
  for (c in AllRegionsCounties[[r]]) {
    CountyRegions <- CountyRegions %>%
      bind_rows(tibble(County = c,
                       Region = r))
  }
}

regions <- unique(CountyRegions$Region)
numberOfRegions <- length(regions)

AllRegionsCounties[["New York State"]] <- c(unname(unlist(AllRegionsCounties)))

if (interactive()) {
  Region <- names(AllRegionsCounties)[
    tryCatch(menu(names(AllRegionsCounties), graphics = TRUE, title = "Select region                                   "),
             error = function (e) {
             })]
  if (length(Region) == 0) {
    message("Invalid selection -- defaulting to New York State.")
    Region <- "New York State"
  }
} else {
  Region <- "New York State"
}

RegionCounties <- sort(AllRegionsCounties[[Region]])

numberOfCounties <- length(RegionCounties)

# Data sources

# The industrial and agricultural economic sectors have been recognized as being especially 
# challenging to address in climate change mitigation activities due to the diversity of 
# activities and processes involved [@faisCriticalRoleIndustrial2016]. Recognizing these 
# challenges, a research group at the US National Renewable Energy Laboratory has sought to 
# develop efficient and reliable techniques for estimating energy use in these sectors 
# through the development of the NREL Industrial Emissions Tool (IET) [@mcmillanIndustryEnergyTool2018]. 
# In addition, they have used the IET to develop and publish a dataset of industrial and 
# agricultural GHG energy use statistics broken down to the level of individual counties, 
# NAICS activity codes [@ombNORTHAMERICANINDUSTRY2017], and fuel types used for energy generation 
# for the entire United States, the NREL Industrial Energy Data Book (IEDB) 
# [@mcmillanNRELIndustryenergydatabook2019], published through the 
# [NREL data catalogue](https://data.nrel.gov/submissions/122). The analyses presented 
# here use the IEDB in conjunction with publicly-available tables of 
# [County FIPS codes](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697) 
# [@usdanrcsCountyFIPSCodes2019] and [2017 NAICS codes](https://www.census.gov/eos/www/naics/2017NAICS/2017_NAICS_Structure.xlsx) 
# [@ombNORTHAMERICANINDUSTRY2017]. 
# 
# The energy use statistics in [@mcmillanNRELIndustryenergydatabook2019] are drawn from a 
# variety of sources. Facilities with large amounts of greenhouse gas emissions are required 
# to report their emissions under the US EPA's Greenhouse Gas Reporting Program (GHGRP) 
# [@u.s.environmentalprotectionagencyGreenhouseGasReporting2014]. These reported quantities 
# are used directly. To obtain emissions estimates for the far more numerous smaller emitters 
# in the manufacturing, agricultural, mining, and construction sectors, data are combined from 
# 
# - the EPA's Manufacturing Energy Consumption Survey (MECS) [@eiaManufacturingEnergyConsumption2019] 
# - the US Energy Information Administration's EIA Form-923 data on electricity use [@eiaFormEIA923Detailed2018]
# - the US Department of Agriculture's Agriculture Survey [@usdaUSDANationalAgricultural2019][@usdaUSDANASSQuickStats2019] and Census of Agriculture [@usdaUSDANationalAgricultural2019a]
# - the US Census Bureau's Economic Census [@uscensusbureauEconomicCensus2019] and County Business Patterns (CBP) dataset [@uscensusbureauCountyBusinessPatterns2019]
# 
# in order to first estimate the relationship between facility size and emissions for each 
# economic sector; these estimates are combined with the numbers and sizes (employment, 
# fuel and lubricant cost data, etc.) of emissions-generating facilities to obtain GHG 
# emissions estimates [@mcmillanIndustryEnergyTool2018]. 

source("importCountyEsts.R")

source("NAICSitems.R")

# Because of its reliance on census data available only after a time-lag of about three years, 
# the NREL IEDB provides energy use data only through calendar year 2016. It is likely that any 
# Greenhouse Gas Inventory would be similarly limited for similar reasons; for instance, New York State's 
# Greenhouse Gas Inventory for years 1990-2016 [@nyserdaNewYorkState2019] was not published until July 2019. 

# The NREL dataset identifies counties only by FIPS code. We get the corresponding county-names 
# and add them to the dataset along with the NAICS sector names and descriptions. Then we filter 
# to just the nine counties of the selected region.

County_FIPS_codes <- read_delim("County FIPS codes.txt", 
    "\t", escape_double = FALSE, col_names = FALSE, 
    trim_ws = TRUE) %>% 
  transmute(COUNTY_FIPS = X1, County = X2, State = X3)

NYcountyEnergyEsts <- Updated_county_energy_estimates %>% 
  filter(STATE == "NEW YORK") %>%                       # Keep only the NEW YORK rows
  left_join(County_FIPS_codes, by = "COUNTY_FIPS") %>%  # Add county names
  # We have to do some finagling here. The NREL IEDB dataset contains some records in which 
  # the NAICS code given is "11193 & 11194 & 11199" or "1125 & 1129". These result in NAs
  # when we convert them to numeric; we replace them with synthesized codes in fixNAICSgaps.R. 
  mutate(across(NAICS, ~suppressWarnings(as.numeric(.)))) %>%
  left_join(NAICS_Descriptions_2017, by = "NAICS") %>%  # Add NAICS code names and 
                                                        # descriptions
  mutate(across(County, ~str_replace(., "St Lawrence", "St. Lawrence")))

RegionEnergyEsts <- NYcountyEnergyEsts %>% 
  filter(County %in% RegionCounties)

# The NREL dataset contains some rows with missing MMBTU_TOTAL values; these result in NAs. 
# Replace the NAs with 0s. 
RegionEnergyEsts[["MMBTU_TOTAL"]][
    which(is.na(RegionEnergyEsts[["MMBTU_TOTAL"]]))] <- 0

# The NAICS code's numerical structure has some gaps representing flaws in the hierarchy; for 
# instance, there are no 1-digit or 2-digit codes for 'Manufacturing' (should be 3) or 'Trade, 
# Transportation, and Warehousing' (should be 4). We synthesize the codes we will need, giving 
# them names and descriptions respecting their positions relative to the pre-existing native 
# NAICS sector definitions. 
source("synthesizeNAICScodes.R")

# Next we merge in the synthesized codes, and make sliced versions of the NAICS dataset 
# containing only 1-, 2-, 3-, or 4-digit codes.
source("makeSlicedNAICSs.R")

source("fixNAICSgaps.R")

source("finishNRELdatasets.R")
source("EPA/getEPAemissionsFactors.R")

# Energy use

source("fuelTypeDefinitions.R")
# Read in our table of fuel type definitions, and render the table showing them

# Compute GHG emissions datasets from energy use by fuel type and EPA emission factors. 
# Prepare a simple table of CO2, CH4, N2O emissions per mmBTU for our eight fuel types. 
source("GHGemissions.R")

source("GHGanalysis.R")
# Sector analysis: manufacturing, agriculture, construction, and mining-related emissions

# Save our master NAICS table with our fixups, trimming off unneeded columns
write_tsv(NAICS_Descriptions_2017 %>% 
            select(-ends_with("dig")), "NAICS_Descriptions_2017.csv.gz")
saveRDS(NAICS_Descriptions_2017 %>% 
          select(-ends_with("dig")), "NAICS_Descriptions_2017.rds")

if (!dir.exists(str_c("./", Region))) {
  dir.create(str_c("./", Region))
}

# These full and summary datasets are useful to look at with other tools, so save them now. 
# write_tsv(RegionEnergyEsts, "RegionEnergyEsts.tsv.gz")    # Some files are huge, so compress 'em. 
# Note that .rds files are already compressed, sparing us the need to do it. 
write_tsv(RegionEnergyPerFuelYear_1dig, str_c("./", Region, "/RegionEnergyPerFuelYear_1dig.csv"))
write_tsv(RegionEnergyPerFuelYear_2dig, str_c("./", Region, "/RegionEnergyPerFuelYear_2dig.csv"))
write_tsv(RegionEnergyPerFuelYear_3dig, str_c("./", Region, "/RegionEnergyPerFuelYear_3dig.csv.gz"))
write_tsv(RegionEnergyPerFuelYear_4dig, str_c("./", Region, "/RegionEnergyPerFuelYear_4dig.csv.gz"))
saveRDS(RegionEnergyPerFuelYear_1dig, str_c("./", Region, "/RegionEnergyPerFuelYear_1dig.rds"))
saveRDS(RegionEnergyPerFuelYear_2dig, str_c("./", Region, "/RegionEnergyPerFuelYear_2dig.rds"))
saveRDS(RegionEnergyPerFuelYear_3dig, str_c("./", Region, "/RegionEnergyPerFuelYear_3dig.rds"))
saveRDS(RegionEnergyPerFuelYear_4dig, str_c("./", Region, "/RegionEnergyPerFuelYear_4dig.rds"))

write_tsv(GHGemissionsPerCountyFuelYear1dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear1dig.csv"))
write_tsv(GHGemissionsPerCountyFuelYear2dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear2dig.csv"))
write_tsv(GHGemissionsPerCountyFuelYear3dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear3dig.csv.gz"))
write_tsv(GHGemissionsPerCountyFuelYear4dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear4dig.csv.gz"))
saveRDS(GHGemissionsPerCountyFuelYear1dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear1dig.rds"))
saveRDS(GHGemissionsPerCountyFuelYear2dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear2dig.rds"))
saveRDS(GHGemissionsPerCountyFuelYear3dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear3dig.rds"))
saveRDS(GHGemissionsPerCountyFuelYear4dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYear4dig.rds"))

write_tsv(GHGemissionsPerCountyFuelYearWider1dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider1dig.csv"))
write_tsv(GHGemissionsPerCountyFuelYearWider2dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider2dig.csv"))
write_tsv(GHGemissionsPerCountyFuelYearWider3dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider3dig.csv.gz"))
write_tsv(GHGemissionsPerCountyFuelYearWider4dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider4dig.csv.gz"))
saveRDS(GHGemissionsPerCountyFuelYearWider1dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider1dig.rds"))
saveRDS(GHGemissionsPerCountyFuelYearWider2dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider2dig.rds"))
saveRDS(GHGemissionsPerCountyFuelYearWider3dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider3dig.rds"))
saveRDS(GHGemissionsPerCountyFuelYearWider4dig, str_c("./", Region, "/GHGemissionsPerCountyFuelYearWider4dig.rds"))


