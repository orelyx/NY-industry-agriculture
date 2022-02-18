# title: "GHGemissions.Rmd"
# author: "Eric Koski"
# date: "12/31/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Prepare a simple table of CO2, CH4, N2O emissions per mmBTU for our eight fuel types. 

maxCores <- 12

Annual_Coal_Report_excerpt <- read_delim("Annual Coal Report excerpt.tsv", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE, 
                                         skip = 1) %>%
  select(-starts_with("..."))

coalStatsLY <- Annual_Coal_Report_excerpt %>%
  filter(Year == latestYear) %>%
  select(`Bituminous Coal`, `Subbituminous Coal`, Lignite, Anthracite, Total) %>%
  mutate(Stat = "Production, short tons") %>% 
  select(Stat, everything())

productionPct <- tibble(Stat = "Production by weight (short tons)",
                        `Bituminous Coal` = 100 *
                          coalStatsLY$`Bituminous Coal` / coalStatsLY$Total,
                        `Subbituminous Coal` = 100 * 
                          coalStatsLY$`Subbituminous Coal` / coalStatsLY$Total,
                        Lignite = 100 * 
                          coalStatsLY$Lignite / coalStatsLY$Total,
                        Anthracite = 100 * 
                          coalStatsLY$Anthracite / coalStatsLY$Total,
                        Total = 100)

coalHeatContent <- 
  tibble(Stat = "Heat content (mmBTU)",
         `Bituminous Coal` = 
           coalStatsLY$`Bituminous Coal` * 
           dfLookup(EmissionFactorsSolidFuels, "Fuel Type",
                    "Bituminous Coal", "Heat Content"),
         `Subbituminous Coal` = 
           coalStatsLY$`Subbituminous Coal` *
           dfLookup(EmissionFactorsSolidFuels, "Fuel Type",
                    "Sub-bituminous Coal", "Heat Content"),
         Lignite = 
           coalStatsLY$Lignite *
           dfLookup(EmissionFactorsSolidFuels, "Fuel Type",
                    "Lignite", "Heat Content"),
         Anthracite = 
           coalStatsLY$Anthracite *
           dfLookup(EmissionFactorsSolidFuels, "Fuel Type",
                    "Anthracite", "Heat Content")) %>%
  mutate(Total = `Bituminous Coal` + `Subbituminous Coal` + 
           Lignite + Anthracite)

heatContentPct <- tibble(Stat = "Heat content percentage",
                         `Bituminous Coal` = 100 * 
                           coalHeatContent$`Bituminous Coal` /
                           coalHeatContent$Total,
                         `Subbituminous Coal` = 100 * 
                           coalHeatContent$`Subbituminous Coal` /
                           coalHeatContent$Total,
                         Lignite = 100 * 
                           coalHeatContent$Lignite / coalHeatContent$Total,
                         Anthracite = 100 * 
                           coalHeatContent$Anthracite / coalHeatContent$Total,
                         Total = 100)

coalStatsLY <- coalStatsLY %>%
  bind_rows(productionPct, coalHeatContent, heatContentPct)

# The coal used in the US is of various types. For the year 2016, coal production by weight was
# 44.6% bituminous, 45.3% sub-bituminous, 9.8% lignite, and less than 0.3% anthracite by weight, or
# 55% bituminous, 38% sub-bituminous, 6.8% lignite, and less than 0.3% anthracite by heat content.
# The EPA emission factors include a set of emission values for "Mixed (Industrial Sector)" which
# are used below in computing GHG emissions from coal. These are very close to the weighted 
# averages of the emission values for the four coal types.[@eiaAnnualCoalReport2019]

CoalFactors <- tibble(
  FuelName = "Coal", 
  CO2 = dfLookup(EmissionFactorsSolidFuels, 
                 "Fuel Type", 
                 c("Mixed", "Industrial Sector"),
                 c("CO2", "mmBtu")),
  CH4 = dfLookup(EmissionFactorsSolidFuels, 
                 "Fuel Type", 
                 c("Mixed", "Industrial Sector"),
                 c("CH4", "mmBtu")),
  N2O = dfLookup(EmissionFactorsSolidFuels, 
                 "Fuel Type", 
                 c("Mixed", "Industrial Sector"),
                 c("N2O", "mmBtu")))

# In addition to coke derived from coal, US petroleum refineries synthesize significant 
# amounts of petroleum coke; however, nearly all of this 'petcoke' is exported rather than 
# being used domestically [@eiaInternationalEnergyOutlook2016]. 

CokeFactors <- tibble(
  FuelName = "Coke_and_breeze", 
  CO2 = dfLookup(EmissionFactorsSolidFuels, 
                 "Fuel Type", 
                 c("Coal Coke"),
                 c("CO2", "mmBtu")),
  CH4 = dfLookup(EmissionFactorsSolidFuels, 
                 "Fuel Type", 
                 c("Coal Coke"),
                 c("CH4", "mmBtu")),
  N2O = dfLookup(EmissionFactorsSolidFuels, 
                 "Fuel Type", 
                 c("Coal Coke"),
                 c("N2O", "mmBtu")))

# Most diesel fuel used in the US is what is known as "Grade No.2-D diesel fuel", where the "No.2" 
# refers to the fuel's level of density and viscosity. Grade No.2-D diesel fuel is very similar 
# in composition to what the industry classifies as No.2 fuel oil. The EPA emission factors 
# don't specify values for diesel fuel specifically, so the values for No.2 fuel oil are used. 
# [@chevroncorp.DieselfueltechreviewPdf]

DieselFactors <- tibble(
  FuelName = "Diesel", 
  CO2 = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Distillate Fuel Oil No. 2"),
                 c("CO2", "mmBtu")),
  CH4 = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Distillate Fuel Oil No. 2"),
                 c("CH4", "mmBtu")),
  N2O = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Distillate Fuel Oil No. 2"),
                 c("N2O", "mmBtu")))

# The term "residual fuel oil" as defined applies to both of what are classified as No.5 and 
# No.6 residual fuel oils. No.5 residual fuel oil is evidently used mostly as a fuel for 
# naval and commercial ships; accordingly, we use the emission factors for No.6 residual fuel 
# oil which has a variety of onshore uses [@eiaPETROLEUMOTHERLIQUIDS]. 

ResidualFuelOilFactors <- tibble(
  FuelName = "Residual_fuel_oil", 
  CO2 = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Residual Fuel Oil No. 6"),
                 c("CO2", "mmBtu")),
  CH4 = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Residual Fuel Oil No. 6"),
                 c("CH4", "mmBtu")),
  N2O = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Residual Fuel Oil No. 6"),
                 c("N2O", "mmBtu")))

# The fuel type "LPG-NGL" would appear from its name to apply to two categories of fuels: 
# "Liquefied Petroleum Gases" and "Natural Gas Liquids". However, the EIA definitions don't 
# seem to clearly distinguish the two categories; both are composed primarily of liquefied 
# propane and butane [@eiaPETROLEUMOTHERLIQUIDS]. Accordingly, the analysis below uses the EPA's
# emission factors for "Liquefied Petroleum # Gases (LPG)" for this fuel type.

LPG_NGL_Factors <- tibble(
  FuelName = "LPG_NGL", 
  CO2 = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Liquefied Petroleum Gases"),
                 c("CO2", "mmBtu")),
  CH4 = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Liquefied Petroleum Gases"),
                 c("CH4", "mmBtu")),
  N2O = dfLookup(EmissionFactorsLiquidFuels, 
                 "Fuel Type", 
                 c("Liquefied Petroleum Gases"),
                 c("N2O", "mmBtu")))

# The natural gas fuel type is clearly delineated and has specified emission factors; these are 
# used below. 

NaturalGasFactors <- tibble(
  FuelName = "Natural_gas", 
  CO2 = dfLookup(EmissionFactorsGaseousFuels, 
                 "Fuel Type", 
                 c("Natural Gas"),
                 c("CO2", "mmBtu")),
  CH4 = dfLookup(EmissionFactorsGaseousFuels, 
                 "Fuel Type", 
                 c("Natural Gas"),
                 c("CH4", "mmBtu")),
  N2O = dfLookup(EmissionFactorsGaseousFuels, 
                 "Fuel Type", 
                 c("Natural Gas"),
                 c("N2O", "mmBtu")))

# For net electricity we use the emission factors provided by the EPA in 
# [@epaEmissionFactorsGreenhouse2018] for the Northeast Power Coordinating Council's Upstate 
# NY region. Note that the EPA table gives emissions 
# for all three GHGs in kg/MWh; we want to convert to kg/ or g/mmBTU.

# eGRIDlookup(): returns a vector of emission factors for the 
eGRIDlookup <- function(subregions, pollutant) {
  EFs <- NULL
  for (sr in subregions) {
    EFs[[sr]] <- dfLookup(EmissionFactorsGridElectricity, 
                                 "eGRID Subregion", 
                                 sr,
                                 c("Total Output", pollutant))
  }
  unname(unlist(EFs))
}

subregions <- c("NYUP", "NYCW", "NYLI")

NetElectricityFactors <- tibble(
  FuelName = "Net_electricity",
  subregion = subregions,
  CO2 = eGRIDlookup(subregions, "CO2") * MWhPerMMBTU,        # kg/mmBTU
  CH4 = eGRIDlookup(subregions, "CH4") * 1000 * MWhPerMMBTU, # kg -> g
  N2O = eGRIDlookup(subregions, "N2O") * 1000 * MWhPerMMBTU) # kg -> g

# Other fuels for the region are almost entirely wood-based biomass fuels, based on statistics 
# for New York State as a whole [@eiaNewYorkState2020]. Modest quantities of wind and 
# hydrolelectric power are also generated for on-site industrial use. Like the latter, biomass 
# is considered for this analysis to have no greenhouse gas emissions, since emitted carbon 
# was earlier absorbed from the atmosphere through photosynthesis (recognizing that this may 
# be an oversimplification; see for instance Costanza et al  
# [@costanzaBioenergyProductionForest2017]. Changes in carbon sequestration capacity due to 
# the conversion from wild forest to harvested commercial forest should be accounted for 
# under land use change. 

# Table shows industrial power generation from renewable sources for NY state. 
annual_generation_state <- read_excel("annual_generation_state.xls", 
                                      sheet = "Net_Generation_1990-2018 Final", 
                                      skip = 1)

OtherFuelFactors <- tibble(
  FuelName = "Other", CO2 = 0, CH4 = 0, N2O = 0)

# Now (after all that) assemble our consolidated table of GHG emission factors!
GHGfactors <- bind_rows(CoalFactors, CokeFactors, 
                        DieselFactors, LPG_NGL_Factors, 
                        NaturalGasFactors, # NetElectricityFactors,
                        OtherFuelFactors, ResidualFuelOilFactors)

# ... and add CO2e emission factors based on Global Warming Potentials. 

# CH4gwp <- dfLookup(GlobalWarmingPotentials, "gas", "CH4", "GWP")
# N2Ogwp <- dfLookup(GlobalWarmingPotentials, "gas", "N2O", "GWP")

GWP100 <- c("CO2" = 1, "CH4" = 25, "N2O" = 298)
GWP20 <- c("CO2" = 1, "CH4" = 87, "N2O" = 268)

emissionColumns <- c("CO2", "CH4", "N2O", "CO2e100", "CO2e20")

# Invoking the 20-year GWP by itself doesn't change the effective emission factors very much. 
# The big difference comes from including upstream emissions from natural gas infrastructure; 
# these are not included here (yet). 

# Add GHG factor for CO2-equivalent emissions based on 100-year Global Warming Potential. 
GHGfactors <- GHGfactors %>%
  mutate(CO2e100 = CO2 + (CH4 * GWP100["CH4"] / 1000) + (N2O * GWP100["N2O"] / 1000)) %>% # CO2e in kgs
  mutate(CO2e20 = CO2 + (CH4 * GWP20["CH4"] / 1000) + (N2O * GWP20["N2O"] / 1000))

NetElectricityFactors <- NetElectricityFactors %>%
  mutate(CO2e100 = CO2 + (CH4 * GWP100["CH4"] / 1000) + (N2O * GWP100["N2O"] / 1000)) %>% # CO2e in kgs
  mutate(CO2e20 = CO2 + (CH4 * GWP20["CH4"] / 1000) + (N2O * GWP20["N2O"] / 1000))

GHGemissionsCountyYear <- RegionEnergyEsts %>%
  mutate(eGRIDregion = ifelse((County %in% NYCcounties) | (County == "Westchester"), "NYCW",
                              ifelse(County %in% c("Nassau", "Suffolk"), "NYLI", "NYUP"))) %>%
  left_join(GHGfactors, by = c("MECS_FT" = "FuelName")) %>%
  left_join(NetElectricityFactors, 
            by = c("MECS_FT" = "FuelName", "eGRIDregion" = "subregion"),
            suffix = c(".x", ".y")) 

for(col in emissionColumns) {
  GHGemissionsCountyYear <- GHGemissionsCountyYear %>%
    mutate(!!sym(col) := ifelse(is.na(!!sym(str_c(col, ".x"))), 
                               !!sym(str_c(col, ".y")), 
                               !!sym(str_c(col, ".x"))))
}

GHGemissionsCountyYear <- GHGemissionsCountyYear %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  mutate(CO2kg = MMBTU_TOTAL * CO2,
         CH4kg = MMBTU_TOTAL * CH4,
         N2Okg = MMBTU_TOTAL * N2O,
         CO2e100kg = MMBTU_TOTAL * CO2e100,
         CO2e20kg = MMBTU_TOTAL * CO2e20)

# Now aggregate the emissions for each abbreviated NAICS code, for 1, 2, 3, and 4-digit 
# abbreviated codes. 
GHGemissionsPerCountyFuelYear1dig <- GHGemissionsCountyYear %>%
  group_by(YEAR, County, NAICS1dig, NAICSname1dig, MECS_FT) %>%
  summarize(CO2kg = sum(CO2kg, na.rm = TRUE),
            CH4kg = sum(CH4kg, na.rm = TRUE),
            N2Okg = sum(N2Okg, na.rm = TRUE),
            CO2e100kg = sum(CO2e100kg, na.rm = TRUE)) %>%
  ungroup()
            
GHGemissionsPerCountyFuelYear2dig <- GHGemissionsCountyYear %>%
  group_by(YEAR, County, NAICS2dig, NAICSname2dig, MECS_FT) %>%
  summarize(CO2kg = sum(CO2kg, na.rm = TRUE),
            CH4kg = sum(CH4kg, na.rm = TRUE),
            N2Okg = sum(N2Okg, na.rm = TRUE),
            CO2e100kg = sum(CO2e100kg, na.rm = TRUE)) %>%
  ungroup()

GHGemissionsPerCountyFuelYear3dig <- GHGemissionsCountyYear %>%
  group_by(YEAR, County, NAICS3dig, NAICSname3dig, MECS_FT) %>%
  summarize(CO2kg = sum(CO2kg, na.rm = TRUE),
            CH4kg = sum(CH4kg, na.rm = TRUE),
            N2Okg = sum(N2Okg, na.rm = TRUE),
            CO2e100kg = sum(CO2e100kg, na.rm = TRUE)) %>%
  ungroup()

GHGemissionsPerCountyFuelYear4dig <- GHGemissionsCountyYear %>%
  group_by(YEAR, County, NAICS4dig, NAICSname4dig, MECS_FT) %>%
  summarize(CO2kg = sum(CO2kg, na.rm = TRUE),
            CH4kg = sum(CH4kg, na.rm = TRUE),
            N2Okg = sum(N2Okg, na.rm = TRUE),
            CO2e100kg = sum(CO2e100kg, na.rm = TRUE)) %>%
  ungroup()

CO2fraction <- sum(GHGemissionsCountyYear$CO2kg) / sum(GHGemissionsCountyYear$CO2e100kg)

GHGemissionsPerFuelYear <- GHGemissionsPerCountyFuelYear1dig %>%
  group_by(YEAR, MECS_FT) %>%
  summarize(CO2kg = sum(CO2kg, na.rm = TRUE),
            CH4kg = sum(CH4kg, na.rm = TRUE),
            N2Okg = sum(N2Okg, na.rm = TRUE),
            CO2e100kg = sum(CO2e100kg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Year = YEAR, `Fuel Type` = MECS_FT) %>%
  select(-YEAR, -MECS_FT) %>%
  select(`Fuel Type`, Year, everything())

# Make a version of the table that includes Net electricity, to render later
GHGfactors_tbl <- GHGfactors %>%
  bind_rows(NetElectricityFactors %>%
              mutate(across(FuelName, ~str_c(., " (", subregion, ")")))) %>%
  select(-subregion)

# Reorganize for GHGsByCountyYearplot.R and following

countiesGHGtotals <- GHGemissionsPerCountyFuelYear1dig %>%
  group_by(County) %>%
  summarize(CO2e100tonnes = sum(CO2e100kg, na.rm = TRUE) / 1000) %>% 
  arrange(CO2e100tonnes) 

GHGcounties <- countiesGHGtotals$County

countiesGHGsummary <- GHGemissionsPerCountyFuelYear1dig %>%
  group_by(County, YEAR) %>%
  summarize(CO2e100tonnes = sum(CO2e100kg, na.rm = TRUE) / 1000) %>% 
  arrange(CO2e100tonnes) %>%
  mutate(Year = YEAR) %>%
  select(-YEAR)

countiesGHGsummary$County <- factor(countiesGHGsummary$County, 
                                    levels = GHGcounties, 
                                    ordered = TRUE)

# Reformat for GHGsByCountyYeartabs.R and following

countiesGHGsummaryWider <- ungroup(countiesGHGsummary) %>%
  arrange(Year) %>%
  pivot_wider(names_from = Year, values_from = CO2e100tonnes) %>%
  bind_rows(summarise_all(., ~(if(is.numeric(.)) sum(.) else "Totals"))) %>%
  mutate(sum = rowSums(select(., starts_with("20")))) %>%
  arrange(sum) %>%
  select(-sum)

# Now compute GHGs by county per capita, for GHGsByCountyPerCapitaplot.R and following

GHGsbyCountyPerCapita <- countiesGHGsummaryWider 

for(col in names(GHGsbyCountyPerCapita)) {
  if (!is.na(suppressWarnings(as.integer(col)))) {
    GHGsbyCountyPerCapita <- GHGsbyCountyPerCapita %>%
      mutate(!!as.symbol(col) := 
               mapply(function(name, tonnes) 
               { tonnes / 
                   ifelse(name == "Totals", 
                          RegionPopulation[[col]],
                          dfLookup(NYS_population_by_county, "County", name, col)) },
               County, !!as.symbol(col)))
  }
}

GHGsbyCountyPerCapita <- 
  bind_rows(GHGsbyCountyPerCapita %>% 
              filter(County != "Totals") %>%
              arrange(desc(!!sym(as.character(latestYear)))),
            GHGsbyCountyPerCapita %>% 
              filter(County == "Totals"))

