# title: "finishNRELDatasets.R"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# A sorted vector of fuel names will be useful. 
fuelNames <- c("Natural_gas", "Diesel", "Net_electricity", "LPG_NGL", 
               "Residual_fuel_oil", "Coal", "Coke_and_breeze", "Other")

# Add the 1-, 2-, 3-, and 4-digit abbreviated NAICS codes, and the corresponding 
# names and descriptions
GFLcountyEnergyEsts <- GFLcountyEnergyEsts %>%
  mutate(NAICS1dig = trimInt(NAICS, 1)) %>%
  mutate(NAICS2dig = trimInt(NAICS, 2)) %>%
  mutate(NAICS3dig = trimInt(NAICS, 3)) %>%
  mutate(NAICS4dig = trimInt(NAICS, 4)) 
GFLcountyEnergyEsts <- GFLcountyEnergyEsts %>% 
  left_join(NAICS_Descriptions1dig, by = "NAICS1dig") %>%
  left_join(NAICS_Descriptions2dig, by = "NAICS2dig") %>%
  left_join(NAICS_Descriptions3dig, by = "NAICS3dig") %>%
  left_join(NAICS_Descriptions4dig, by = "NAICS4dig") 

# We keep the abbreviated NAICS codes in the summary dataframes because 
# other uses of the data might want them. We don't carry the NAICS 
# descriptions around because they're extremely bulky. 

# Aggregate the energy use for each 1-digit abbreviated NAICS code
GFLcountyEnergyPerFuelYear1dig <- GFLcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS1dig, NAICSname1dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
GFLcountyEnergyPerFuelYear_1dig <-pivot_wider(
  GFLcountyEnergyPerFuelYear1dig, 
  names_from = MECS_FT, 
  values_from = MMBTU)

# Aggregate the energy use for each 2-digit abbreviated NAICS code
GFLcountyEnergySumm2dig <- GFLcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS2dig, NAICSname2dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
GFLcountyEnergyPerFuelYear_2dig <- pivot_wider(GFLcountyEnergySumm2dig, 
                                               names_from = MECS_FT, 
                                               values_from = MMBTU)

# Aggregate the energy use for each 3-digit abbreviated NAICS code
GFLcountyEnergySumm3dig <- GFLcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS3dig, NAICSname3dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
GFLcountyEnergyPerFuelYear_3dig <- pivot_wider(GFLcountyEnergySumm3dig, 
                                               names_from = MECS_FT, 
                                               values_from = MMBTU)

# Aggregate the energy use for each 4-digit abbreviated NAICS code
GFLcountyEnergySumm4dig <- GFLcountyEnergyEsts %>% 
  group_by(YEAR, County, NAICS4dig, NAICSname4dig, MECS_FT) %>% 
  summarize(MMBTU = sum(MMBTU_TOTAL, na.rm = TRUE))

# Make the fuel types columns
GFLcountyEnergyPerFuelYear_4dig <- pivot_wider(GFLcountyEnergySumm4dig, 
                                               names_from = MECS_FT, 
                                               values_from = MMBTU)

# The pivot_wider()s created NAs in the columns for individual fuels, so let's clean 
# them up by nonchalantly replacing them with 0s as before. 
for (fuel in fuelNames) {
  GFLcountyEnergyPerFuelYear_1dig[[fuel]][
    which(is.na(GFLcountyEnergyPerFuelYear_1dig[[fuel]]))] <- 0
  GFLcountyEnergyPerFuelYear_2dig[[fuel]][
    which(is.na(GFLcountyEnergyPerFuelYear_2dig[[fuel]]))] <- 0
  GFLcountyEnergyPerFuelYear_3dig[[fuel]][
    which(is.na(GFLcountyEnergyPerFuelYear_3dig[[fuel]]))] <- 0
  GFLcountyEnergyPerFuelYear_4dig[[fuel]][
    which(is.na(GFLcountyEnergyPerFuelYear_4dig[[fuel]]))] <- 0
}

# These full and summary datasets are useful to look at with other tools, so save them now. 
# write_tsv(GFLcountyEnergyEsts, "GFLcountyEnergyEsts.tsv.gz")    # It's huge, so compress it. 
write_tsv(GFLcountyEnergyPerFuelYear_1dig, "GFLcountyEnergyPerFuelYear_1dig.csv")
write_tsv(GFLcountyEnergyPerFuelYear_2dig, "GFLcountyEnergyPerFuelYear_2dig.csv")
write_tsv(GFLcountyEnergyPerFuelYear_3dig, "GFLcountyEnergyPerFuelYear_3dig.csv.gz")
write_tsv(GFLcountyEnergyPerFuelYear_4dig, "GFLcountyEnergyPerFuelYear_4dig.csv.gz")

# Summarize to a dataset aggregating the use of each fuel for each county and year
# (dropping any NAICS breakdown). This is used for both line and area plots below. 
# (Otherwise we would do this in the code chunk for the single plot based on this 
# dataset.)
GFLenergyPerCountyYearFuel <- GFLcountyEnergyPerFuelYear_2dig %>% 
  group_by(YEAR, County) %>% 
  summarize(Diesel = sum(Diesel, na.rm = TRUE), 
            LPG_NGL = sum(LPG_NGL, na.rm = TRUE), 
            Net_electricity = sum(Net_electricity, na.rm = TRUE), 
            Coal = sum(Coal, na.rm = TRUE), 
            Natural_gas = sum(Natural_gas, na.rm = TRUE), 
            Coke_and_breeze = sum(Coke_and_breeze, na.rm = TRUE), 
            Residual_fuel_oil = sum(Residual_fuel_oil, na.rm = TRUE), 
            Other = sum(Other, na.rm = TRUE))

GFLcountyEnergyPerFuelYear_1dig <- ungroup(GFLcountyEnergyPerFuelYear_1dig)
GFLcountyEnergyPerFuelYear_2dig <- ungroup(GFLcountyEnergyPerFuelYear_2dig)
GFLcountyEnergyPerFuelYear_3dig <- ungroup(GFLcountyEnergyPerFuelYear_3dig)
GFLcountyEnergyPerFuelYear_4dig <- ungroup(GFLcountyEnergyPerFuelYear_4dig)
GFLenergyPerCountyYearFuel <- ungroup(GFLenergyPerCountyYearFuel)

earliestYear <- min(GFLcountyEnergyPerFuelYear_3dig$YEAR)
latestYear <- max(GFLcountyEnergyPerFuelYear_3dig$YEAR)

# We're going to want a 3-digit dataframe for the most recent year, whichever it is. 
GFLcountyEnergyPerFuelLY_3dig <- filter(GFLcountyEnergyPerFuelYear_3dig,
                                        YEAR == latestYear)

# Let's make a 4-digit dataframe for the most recent year while we're at it. 
GFLcountyEnergyPerFuelLY_4dig <- filter(GFLcountyEnergyPerFuelYear_4dig,
                                        YEAR == latestYear)

# We're going to want fuelNames sorted by total use of each fuel type. 
# This version is for the entire period covered in the dataset. 
fuelTotals <- NULL
for (col in fuelNames) {
  fuelTotals[col] <- sum(GFLcountyEnergyPerFuelYear_3dig[col], na.rm = TRUE)
}
fuelTotals <- sort(fuelTotals, decreasing = FALSE)

# We're going to want a list of fuel names sorted in increasing order of 
# total energy amount per fuel. fuelTotals is already sorted accordingly, 
# so we'll use its list of names. 
fuelNames <- names(fuelTotals)
fuelNamesFctr <- factor(fuelNames, levels = fuelNames, labels = fuelNames)

# We need to redo this calculation with just the 2016 data. 
# We're going to want fuelNames sorted by total use of each fuel type. 
# We end our vars' names with "LY" for latest year. 
fuelTotalsLY <- NULL
fuelNamesLY <- names(fuelTotals)        # fuelTotals from finishNRELdatasets.Rmd
for (col in fuelNamesLY) {
  fuelTotalsLY[col] <- sum(GFLcountyEnergyPerFuelLY_3dig[col], na.rm = TRUE)
}
fuelTotalsLY <- sort(fuelTotalsLY, decreasing = TRUE)
fuelNamesLY <- names(fuelTotalsLY)
fuelNamesLYfctr <- factor(fuelNamesLY, levels = fuelNamesLY, labels = fuelNamesLY)

countyTotalsLY <- NULL
countyNamesLY <- countyNames <- unique(GFLcountyEnergyPerFuelLY_3dig$County)
countySummary <- GFLcountyEnergyPerFuelLY_3dig %>%
  group_by(County) %>%
  summarize_at(vars(fuelNamesLY), sum) %>%
  mutate(countySums = rowSums(select(., !!!syms(fuelNamesLY)))) %>%
  select(County, countySums)

for (cty in countySummary$County) {
  countyTotalsLY[cty] <- filter(countySummary, County == cty)$countySums
}
countyTotalsLY <- sort(desc(countyTotalsLY))
countyNamesLY <- names(countyTotalsLY)

# Let's give ourselves a vector of population by year for the GFL region. 
# Bound to come in handy for per capita stats. 
GFLpopulation <- NULL
for (i in seq(earliestYear, latestYear, by = 1)) {
  GFLpopulation[[as.character(i)]] <- sum(
    mapply(function(county, year) 
      dfLookup(NYS_population_by_county, "County", county, year), 
      GFLcounties, as.character(i)))
}
