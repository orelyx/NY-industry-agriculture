################################################################################
# title: "NYS-industry-and-agriculture.R"
# author: "Eric Koski"
# date: "2/16/2022"
#
# Produces the same R objects as are produced in rendering 
# Generic-report-industry-and-agriculture.Rmd, but doesn't actually knit the 
# document. Useful when you want to create the table and plot objects (and rendered 
# plots) of the document for manipulation in the Rstudio global environment. Can 
# be run non-interactively, in which case it simply generates datasets for New 
# York State as a whole; otherwise, prompts for the region for which datasets are 
# desired. 
#
#     Copyright (c) 2022 Orebed Analytics LLC under MIT License; see LICENSE.md. 
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

# Tools and methods

# The analyses and illustrations presented in this report were prepared using the R programming language [@rcoreteamLanguageEnvironmentStatistical2019] and the powerful associated collection of tools for data analysis and visualization [@wickhamGgplot2ElegantGraphics2016][@wickhamWelcomeTidyverse2019a]. The report itself is prepared using an R facility known as Rmarkdown [@allairejjRmarkdownDynamicDocuments2019], in which a single file or collection of files contains both the text of a document such as this one and the code (which needn't only be R code) used to generate the analysis it presents. Management of the document and code as a single unit permits the use of the rich, capable version control tools available to software developers and ensures that the document in its final form is reproducible. In use of Rmarkdown, the code used to generate elements such as figures and tables can be presented interleaved with the document text as desired, in the form of 'code chunks' such as the example below. A companion document to this one presents all of the code used in preparing the document in this form, along with text describing the data processing and interpretation. Interestingly, little of the code used in preparing this document would need to change in order to prepare similar analyses for any other region in New York State. 

# I had been using countyNames.Rmd as a child document here, but ran into an Rstudio deficiency that 
# makes debugging the document harder: the "Run All Chunks Above" and "Run Current Chunk" icons 
# shown in the upper right corner of the chunk don't work when the chunk is a child document. This 
# is a longstanding issue: see https://community.rstudio.com/t/making-child-code-chunks-execute-
# by-clicking-run-current-chunk/12907
# and https://stackoverflow.com/questions/48764918/rmarkdown-running-child-chunks-from-inside-
# rstudio/48777264. 

# The NREL dataset identifies counties only by FIPS code. We get the corresponding county-names 
# and add them to the dataset along with the NAICS sector names and descriptions. Then we filter 
# to just the nine counties of the Genesee/Finger Lakes Region.

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

# One of the benefits of using the NREL IEDB is the insight it provides into changes in 
# energy use patterns that have occurred in recent years. 
source("energyUsePerCountyYearplot.R")

source("energyUsePerCountyYeartab.R")
# Render our summary table

source("energyUsePerFuelYearplot.R")
# Render a simple area plot of energy use per fuel type per year. Since this isn't broken 
# down by county, we can render this even if we're analyzing the entire state. 

source("energyUsePerFuelYeartab.R")
# Render our summary table. Also usable for the entire state. 

# Figure \ref{fig:energyUsePerFuelYearplot} presents the same total energy use shown above, 
# but this time broken down by fuel type. The decline in the use of coal is especially striking. 
# Since there is only a modest increase at most in the use of other fuels, the coal decline seems 
# likely to be a result of changes in economic activity rather than of fuel-switching. Use of 
# natural gas increases from `r firstNaturalGasPct`% in `r as.character(earliestYear)` to 
# `r lastNaturalGasPct`% in `r as.character(latestYear)`; use of other fuels changes only modestly. 

source("fuelTypeDefinitions.R")
# Read in our table of fuel type definitions, and render the table showing them

# Table \ref{tab:fuelTypeDefinitions} provides definitions of the fuel types used in the NREL 
# IEDB and in this document, based on definitions provided by the US Energy Information Agency 
# [@eiaGlossaryEnergyInformation2019][@eiaPetroleumOtherLiquids]. *Net electricity* in most 
# cases refers to energy purchased from grid suppliers, but could refer increasingly to on-site 
# renewable electricity generation in future years. 

# source("countiesLinePlot.R")
# Render a line plot of energy/fuel use per county per year -- no longer used

source("countyAreaPlot.R")
# Render an analogous area plot. 

source("regionPlot1.R")
# Render a plot for the entire region, showing fuel use for each two-digit *non-manufacturing* NAICS sector

source("regionPlot2.R")
# Render a plot for the entire region, showing fuel use for each three-digit *manufacturing* NAICS sector

source("regionPlot3.R")

source("energyBySector3digLYtab.R")
# Render table showing latest year's energy use by sector and fuel type for the entire region

source("energyBySector3digxCountyLYtab.R")
# Render table showing latest year's energy use by sector and county for the entire region


# Greenhouse gas emissions

# Compute GHG emissions datasets from energy use by fuel type and EPA emission factors. 
# Prepare a simple table of CO2, CH4, N2O emissions per mmBTU for our eight fuel types. 
source("GHGemissions.R")

source("emissionFactorSummarytab.R")

# Table \ref{tab:emissionFactorSummarytab} summarizes the emission factors used for these fuel types.

## Results and analysis

source("GHGsByCountyYearplot.R")

# Figure \ref{fig:GHGsByCountyYearplot} provides a summary view of estimated annual greenhouse 
# gas emissions for the region, stratified by county; the same data appear in 
# Table \ref{tab:GHGsByCountyYeartabs}. 

source("GHGsByCountyYeartabs.R") 

source("GHGsByCountyPerCapitaplot.R")

source("GHGsByCountyPerCapitatab.R") 

source("GHGsByFuelYearplot.R") 

source("GHGanalysis.R")

## Sector analysis

source("GHGsectorYearplot.R")

# Figure \ref{fig:GHGsectorYearplot} provides an overview of GHG emissions by economic sector for the region. 

source("GHGsectortabs.R")

# The more detailed breakdowns by NAICS category in the following sub-sections 
# yield greater insight into the sources of these emissions. For each of the four sectors, 
# the following pages provide a plot showing the relative emissions quantities for 
# sub-categories of each sector, and the distribution of these emissions across the counties 
# in the region. 

### Manufacturing

source("GHGManufacturingplot.R")

source("GHGmanufacturingtab.R")

### Agriculture

source("GHGAgricultureplot.R")

source("GHGAgriculturetab.R")

### Construction

source("GHGConstructionplot.R")

source("GHGConstructiontab.R")

### Mining

source("GHGMiningplot.R")

source("GHGMiningtab.R")

source("EPA/emissionFactorstabs.R")

source("countySectorLYFueltab.R")

source("countySectorYearFueltab.R")

source("GHGcountySectorLYFueltab.R")

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
