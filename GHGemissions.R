# title: "GHGemissions.Rmd"
# author: "Eric Koski"
# date: "12/31/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Prepare a simple table of CO2, CH4, N2O emissions per mmBTU for our eight fuel types. 

Annual_Coal_Report_excerpt <- read_delim("Annual Coal Report excerpt.tsv", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE, 
                                         skip = 1) %>%
  select(-starts_with("..."))

coalStats2016 <- Annual_Coal_Report_excerpt %>%
  filter(Year == 2016) %>%
  select(`Bituminous Coal`, `Subbituminous Coal`, Lignite, Anthracite, Total) %>%
  mutate(Stat = "Production, short tons") %>% 
  select(Stat, everything())

productionPct <- tibble(Stat = "Production by weight (short tons)",
                        `Bituminous Coal` = 100 *
                          coalStats2016$`Bituminous Coal` / coalStats2016$Total,
                        `Subbituminous Coal` = 100 * 
                          coalStats2016$`Subbituminous Coal` / coalStats2016$Total,
                        Lignite = 100 * 
                          coalStats2016$Lignite / coalStats2016$Total,
                        Anthracite = 100 * 
                          coalStats2016$Anthracite / coalStats2016$Total,
                        Total = 100)

coalHeatContent <- 
  tibble(Stat = "Heat content (mmBTU)",
         `Bituminous Coal` = 
           coalStats2016$`Bituminous Coal` * 
           dfLookup(EmissionFactorsSolidFuels, "Fuel Type",
                    "Bituminous Coal", "Heat Content"),
         `Subbituminous Coal` = 
           coalStats2016$`Subbituminous Coal` *
           dfLookup(EmissionFactorsSolidFuels, "Fuel Type",
                    "Sub-bituminous Coal", "Heat Content"),
         Lignite = 
           coalStats2016$Lignite *
           dfLookup(EmissionFactorsSolidFuels, "Fuel Type",
                    "Lignite", "Heat Content"),
         Anthracite = 
           coalStats2016$Anthracite *
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

coalStats2016 <- coalStats2016 %>%
  bind_rows(productionPct, coalHeatContent, heatContentPct)

# The coal used in the US is of various types. For the year 2016, coal production by weight was
# 44.6% bituminous, 45.3% sub-bituminous, 9.8% lignite, and less than 0.3% anthracite by weight, or
# 55% bituminous, 38% sub-bituminous, 6.8% lignite, and less than 0.3% anthracite by heat content.
# The EPA emission factors include a set of emission values for "Mixed (Industrial Sector)" which
# are used below in computing GHG emissions from coal. These are very close to the weighted 
# averages of the emission values for the four coal types.[@eiaAnnualCoalReport2019]

CoalFactors <- tibble(
  fuelName = "Coal", 
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
  fuelName = "Coke_and_breeze", 
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
  fuelName = "Diesel", 
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
# No.6 residual fuel oils. No.5 residual fuel oil is evidently used mostly as a fuel for naval and 
# naval and commercial ships; accordingly, we use the emission factors for No.6 residual fuel 
# oil which has a variety of onshore uses [@eiaPETROLEUMOTHERLIQUIDS]. 

ResidualFuelOilFactors <- tibble(
  fuelName = "Residual_fuel_oil", 
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
  fuelName = "LPG_NGL", 
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
  fuelName = "Natural_gas", 
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
# NY region, which contains the entire GFL region. Note that the EPA table gives emissions 
# for all three GHGs in kg/MWh; we want to convert to kg/ or g/mmBTU. 

NetElectricityFactors <- tibble(
  fuelName = "Net_electricity", 
  CO2 = dfLookup(EmissionFactorsGridElectricity, 
                 "eGRID Subregion", 
                 c("NYUP"),
                 c("Total Output", "CO2")) * MWhPerMMBTU,        # kg/mmBTU
  CH4 = dfLookup(EmissionFactorsGridElectricity, 
                 "eGRID Subregion", 
                 c("NYUP"),
                 c("Total Output", "CH4")) * 1000 * MWhPerMMBTU, # kg -> g
  N2O = dfLookup(EmissionFactorsGridElectricity, 
                 "eGRID Subregion", 
                 c("NYUP"),
                 c("Total Output", "N2O")) * 1000 * MWhPerMMBTU) # kg -> g

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
  fuelName = "Other", CO2 = 0, CH4 = 0, N2O = 0)

# Now (after all that) assemble our consolidated table of GHG emission factors!
GHGfactors <- bind_rows(CoalFactors, CokeFactors, 
                        DieselFactors, LPG_NGL_Factors, 
                        NaturalGasFactors, NetElectricityFactors,
                        OtherFuelFactors, ResidualFuelOilFactors)

# ... and add CO2e emission factors based on Global Warming Potentials. 

CH4gwp <- dfLookup(GlobalWarmingPotentials, "gas", "CH4", "GWP")
N2Ogwp <- dfLookup(GlobalWarmingPotentials, "gas", "N2O", "GWP")

# Add GHG factor for CO2-equivalent emissions based on 100-year Global Warming Potential. 
GHGfactors <- GHGfactors %>%
  mutate(CO2e100 = CO2 + (CH4 * CH4gwp / 1000) + N2O * N2Ogwp / 1000) # CO2e in kgs

# Compute GHG emissions based on fuel/energy use and emission factors. This takes several 
# minutes to run. Such fun!
GHGemissionsCountyYear <- GFLcountyEnergyEsts %>%
  mutate(CO2kg = MMBTU_TOTAL * as.double(
           lapply(MECS_FT, function(x) 
             dfLookup(GHGfactors, "fuelName", x, "CO2$")))) %>% # '$' is latex for end of string
  mutate(CH4kg = MMBTU_TOTAL * as.double(
           lapply(MECS_FT, function(x) 
             dfLookup(GHGfactors, "fuelName", x, "CH4$")))) %>%
  mutate(N2Okg = MMBTU_TOTAL * as.double(
           lapply(MECS_FT, function(x) 
             dfLookup(GHGfactors, "fuelName", x, "N2O$")))) %>%
  mutate(CO2e100kg = MMBTU_TOTAL * as.double(
           lapply(MECS_FT, function(x) 
             dfLookup(GHGfactors, "fuelName", x, "CO2e"))))     # N.B. "CO2" is a substring!

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
