# title: "finishNRELDatasets.R"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# A sorted vector of fuel names will be useful. 
fuelNames <- c("Natural gas", "Diesel", "Net electricity", "LPG-NGL", 
               "Residual fuel oil", "Coal", "Coke and breeze", "Other")

# Add the 1-, 2-, 3-, and 4-digit abbreviated NAICS codes, and the corresponding 
# names and descriptions
RegionEnergyEsts <- RegionEnergyEsts %>%
  rename(`Fuel type` = MECS_FT) %>%
  mutate(across(`Fuel type`, ~str_replace_all(., c("_" = " ", "LPG NGL" = "LPG-NGL")))) %>%
  mutate(NAICS1dig = trimInt(NAICS, 1)) %>%
  mutate(NAICS2dig = trimInt(NAICS, 2)) %>%
  mutate(NAICS3dig = trimInt(NAICS, 3)) %>%
  mutate(NAICS4dig = trimInt(NAICS, 4)) 
RegionEnergyEsts <- RegionEnergyEsts %>% 
  left_join(NAICS_Descriptions1dig, by = "NAICS1dig") %>%
  left_join(NAICS_Descriptions2dig, by = "NAICS2dig") %>%
  left_join(NAICS_Descriptions3dig, by = "NAICS3dig") %>%
  left_join(NAICS_Descriptions4dig, by = "NAICS4dig") 

# We keep the abbreviated NAICS codes in the summary dataframes because 
# other uses of the data might want them. We don't carry the NAICS 
# descriptions around because they're extremely bulky. 

# Aggregate the energy use for each 1-digit abbreviated NAICS code
RegionEnergyPerFuelYear1dig <- RegionEnergyEsts %>% 
  group_by(YEAR, County, NAICS1dig, NAICSname1dig, `Fuel type`) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
RegionEnergyPerFuelYear_1dig <-pivot_wider(
  RegionEnergyPerFuelYear1dig, 
  names_from = `Fuel type`, 
  values_from = MMBTU)

# Aggregate the energy use for each 2-digit abbreviated NAICS code
RegionEnergySumm2dig <- RegionEnergyEsts %>% 
  group_by(YEAR, County, NAICS2dig, NAICSname2dig, `Fuel type`) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
RegionEnergyPerFuelYear_2dig <- pivot_wider(RegionEnergySumm2dig, 
                                               names_from = `Fuel type`, 
                                               values_from = MMBTU)

# Aggregate the energy use for each 3-digit abbreviated NAICS code
RegionEnergySumm3dig <- RegionEnergyEsts %>% 
  group_by(YEAR, County, NAICS3dig, NAICSname3dig, `Fuel type`) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
RegionEnergyPerFuelYear_3dig <- pivot_wider(RegionEnergySumm3dig, 
                                               names_from = `Fuel type`, 
                                               values_from = MMBTU)

# Aggregate the energy use for each 4-digit abbreviated NAICS code
RegionEnergySumm4dig <- RegionEnergyEsts %>% 
  group_by(YEAR, County, NAICS4dig, NAICSname4dig, `Fuel type`) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
RegionEnergyPerFuelYear_4dig <- pivot_wider(RegionEnergySumm4dig, 
                                            names_from = `Fuel type`, 
                                            values_from = MMBTU)

# The pivot_wider()s created NAs in the columns for individual fuels, so let's clean 
# them up by nonchalantly replacing them with 0s as before. 
for (fuel in fuelNames) {
  RegionEnergyPerFuelYear_1dig[[fuel]][
    which(is.na(RegionEnergyPerFuelYear_1dig[[fuel]]))] <- 0
  RegionEnergyPerFuelYear_2dig[[fuel]][
    which(is.na(RegionEnergyPerFuelYear_2dig[[fuel]]))] <- 0
  RegionEnergyPerFuelYear_3dig[[fuel]][
    which(is.na(RegionEnergyPerFuelYear_3dig[[fuel]]))] <- 0
  RegionEnergyPerFuelYear_4dig[[fuel]][
    which(is.na(RegionEnergyPerFuelYear_4dig[[fuel]]))] <- 0
}

# Summarize to a dataset aggregating the use of each fuel for each county and year
# (dropping any NAICS breakdown). This is used for both line and area plots below. 
# (Otherwise we would do this in the code chunk for the single plot based on this 
# dataset.)
RegionEnergyPerCountyYearFuel <- RegionEnergyPerFuelYear_2dig %>% 
  group_by(YEAR, County) %>% 
  summarize(across(all_of(fuelNames), ~sum(., na.rm = TRUE)))

RegionEnergyPerFuelYear_1dig <- ungroup(RegionEnergyPerFuelYear_1dig)
RegionEnergyPerFuelYear_2dig <- ungroup(RegionEnergyPerFuelYear_2dig)
RegionEnergyPerFuelYear_3dig <- ungroup(RegionEnergyPerFuelYear_3dig)
RegionEnergyPerFuelYear_4dig <- ungroup(RegionEnergyPerFuelYear_4dig)
RegionEnergyPerCountyYearFuel <- ungroup(RegionEnergyPerCountyYearFuel)

earliestYear <- min(RegionEnergyPerFuelYear_3dig$YEAR)
latestYear <- max(RegionEnergyPerFuelYear_3dig$YEAR)

# We're going to want a 3-digit dataframe for the most recent year, whichever it is. 
RegionEnergyPerFuelLY_3dig <- filter(RegionEnergyPerFuelYear_3dig,
                                        YEAR == latestYear)

# Let's make a 4-digit dataframe for the most recent year while we're at it. 
RegionEnergyPerFuelLY_4dig <- filter(RegionEnergyPerFuelYear_4dig,
                                        YEAR == latestYear)

# We're going to want fuelNames sorted by total use of each fuel type. 
# This version is for the entire period covered in the dataset. 
fuelTotals <- NULL
for (col in fuelNames) {
  fuelTotals[col] <- sum(RegionEnergyPerFuelYear_3dig[col], na.rm = TRUE)
}
fuelTotals <- sort(fuelTotals, decreasing = FALSE)

# We're going to want a list of fuel names sorted in increasing order of 
# total energy amount per fuel. fuelTotals is already sorted accordingly, 
# so we'll use its list of names. 
fuelNames <- names(fuelTotals)
fuelNamesFctr <- factor(fuelNames, levels = fuelNames, labels = fuelNames)

# We need to redo this calculation with just the latest year's data. 
# We're going to want fuelNames sorted by total use of each fuel type. 
# We end our vars' names with "LY" for latest year. 
fuelTotalsLY <- NULL
fuelNamesLY <- names(fuelTotals)        # fuelTotals from finishNRELdatasets.Rmd
for (col in fuelNamesLY) {
  fuelTotalsLY[col] <- sum(RegionEnergyPerFuelLY_3dig[col], na.rm = TRUE)
}
fuelTotalsLY <- sort(fuelTotalsLY, decreasing = TRUE)
fuelNamesLY <- names(fuelTotalsLY)
fuelNamesLYfctr <- factor(fuelNamesLY, levels = fuelNamesLY, labels = fuelNamesLY)

countyTotalsLY <- NULL
countyNamesLY <- countyNames <- unique(RegionEnergyPerFuelLY_3dig$County)
countySummary <- RegionEnergyPerFuelLY_3dig %>%
  group_by(County) %>%
  summarize_at(vars(fuelNamesLY), sum) %>%
  mutate(countySums = rowSums(select(., !!!syms(fuelNamesLY)))) %>%
  select(County, countySums)

for (cty in countySummary$County) {
  countyTotalsLY[cty] <- filter(countySummary, County == cty)$countySums
}
countyTotalsLY <- sort(desc(countyTotalsLY))
countyNamesLY <- names(countyTotalsLY)

if (Region == "New York State") {
  regionTotalsLY <- NULL
  regionNamesLY <- regionNames <- unique(CountyRegions$Region)
  regionSummary <- RegionEnergyPerFuelLY_3dig %>%
    left_join(CountyRegions, by = "County") %>%
    group_by(Region) %>%
    summarize(across(all_of(fuelNamesLY), sum)) %>%
    mutate(regionSums = rowSums(select(., !!!syms(fuelNamesLY)))) %>%
    select(Region, regionSums)
  
  for (reg in regionSummary$Region) {
    regionTotalsLY[reg] <- filter(regionSummary, Region == reg)$regionSums
  }
  regionTotalsLY <- sort(desc(regionTotalsLY))
  regionNamesLY <- names(regionTotalsLY)
}

# Let's give ourselves a vector of population by year for the region. 
# Bound to come in handy for per capita stats. 
RegionPopulation <- NULL
for (i in seq(earliestYear, latestYear, by = 1)) {
  RegionPopulation[[as.character(i)]] <- sum(
    mcmapply(function(county, year) 
      dfLookup(NYS_population_by_county, "County", county, year), 
      RegionCounties, as.character(i), mc.cores = 6))
}

countiesSummary <- RegionEnergyEsts %>%
  group_by(County) %>%
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE)) %>% 
  arrange(MMBTU)
counties <- countiesSummary$County

# energyUseSummary is used in energyUsePerCountyYearplot.Rmd and energyUsePerCountyYeartab.Rmd. 
energyUseSummary <- RegionEnergyEsts %>%
  group_by(County, YEAR) %>%
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(Year = YEAR) %>%
  select(-YEAR)

energyUseSummary$County <- factor(energyUseSummary$County, levels = counties, ordered = TRUE)
