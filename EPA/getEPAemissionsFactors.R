# title: "getEPAemissionsFactors.R"
# author: "Eric Koski"
# date: "12/13/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

library(readxl)
library(tidyverse)
library(stringi)

# Since this file is source()d, it's executed in the environment of the calling 
# script file, so we need to make explicit where to look for the .xlsx file. 
EPAemissionsFactorsPathname <- "EPA/EPA_emission_factors_march_2018.xlsx"

# Stationary combustion emission factors
EmissionFactorsSolidFuels_units <- read_excel(EPAemissionsFactorsPathname, 
    sheet = "SolidFuels", n_max = 1, .name_repair = "unique")
EmissionFactorsSolidFuels <- read_excel(EPAemissionsFactorsPathname, 
    sheet = "SolidFuels", col_names = FALSE, 
    skip = 2, .name_repair = "minimal") 
names(EmissionFactorsSolidFuels) <- 
  str_c(names(EmissionFactorsSolidFuels_units),
        c(",\n"),
        slice(EmissionFactorsSolidFuels_units, 1)) %>%
  str_replace_all("[.]{3}[0-9]{1}", "")
names(EmissionFactorsSolidFuels)[1] <- names(EmissionFactorsSolidFuels_units)[1]
EmissionFactorsSolidFuels <- EmissionFactorsSolidFuels %>% 
  filter(!is.na(!!as.symbol(names(EmissionFactorsSolidFuels)[3])))

EmissionFactorsLiquidFuels_units <- read_excel(EPAemissionsFactorsPathname, 
                                               sheet = "LiquidFuels", n_max = 1, .name_repair = "unique")
EmissionFactorsLiquidFuels <- read_excel(EPAemissionsFactorsPathname, 
                                         sheet = "LiquidFuels", col_names = FALSE, 
                                         skip = 2, .name_repair = "minimal") 
names(EmissionFactorsLiquidFuels) <- 
  str_c(names(EmissionFactorsLiquidFuels_units),
        c(",\n"),
        slice(EmissionFactorsLiquidFuels_units, 1)) %>%
  str_replace_all("[.]{3}[0-9]{1}", "")
names(EmissionFactorsLiquidFuels)[1] <- names(EmissionFactorsLiquidFuels_units)[1]
EmissionFactorsLiquidFuels <- EmissionFactorsLiquidFuels %>% 
  filter(!is.na(!!as.symbol(names(EmissionFactorsLiquidFuels)[3])))

EmissionFactorsGaseousFuels_units <- read_excel(EPAemissionsFactorsPathname, 
                                                sheet = "GaseousFuels", n_max = 1, .name_repair = "unique")
EmissionFactorsGaseousFuels <- read_excel(EPAemissionsFactorsPathname, 
                                          sheet = "GaseousFuels", col_names = FALSE, 
                                          skip = 2, .name_repair = "minimal") 
names(EmissionFactorsGaseousFuels) <- 
  str_c(names(EmissionFactorsGaseousFuels_units),
        c(",\n"),
        slice(EmissionFactorsGaseousFuels_units, 1)) %>%
  str_replace_all("[.]{3}[0-9]{1}", "")
names(EmissionFactorsGaseousFuels)[1] <- names(EmissionFactorsGaseousFuels_units)[1]
EmissionFactorsGaseousFuels <- EmissionFactorsGaseousFuels %>% 
  filter(!is.na(!!as.symbol(names(EmissionFactorsGaseousFuels)[3])))
# print(EmissionFactorsGaseousFuels)

# Grid electricity emission factors

EmissionFactorsGridElectricity <- 
  read_excel(EPAemissionsFactorsPathname, 
             sheet = "Electricity", 
             col_names = FALSE, 
             skip = 3)

GridElectricityNames <- 
  read_excel(EPAemissionsFactorsPathname, 
             sheet = "Electricity", 
             col_names = FALSE, 
             n_max = 3)

names(EmissionFactorsGridElectricity)[2:4] <- str_c(
  stri_replace_all_fixed(slice(GridElectricityNames, 1)[2], "Emission Factors", ""),
  slice(GridElectricityNames, 2)[2:4],
  ", ",
  slice(GridElectricityNames, 3)[2:4])

names(EmissionFactorsGridElectricity)[5:7] <- str_c(
  stri_replace_all_fixed(slice(GridElectricityNames, 1)[5], "Emission Factors", ""),
  slice(GridElectricityNames, 2)[5:7],
  ", ",
  slice(GridElectricityNames, 3)[5:7])

names(EmissionFactorsGridElectricity)[1] <- slice(GridElectricityNames, 2)[[1]]

# Convert pounds to kg
EmissionFactorsGridElectricity[2:7] <- 
  mapply(function(x)(x * kgPerLb), 
         EmissionFactorsGridElectricity[2:7])

names(EmissionFactorsGridElectricity)[2:7] <-
  mapply(function(x)(stri_replace_all_fixed(x, "lb", "kg")),
         names(EmissionFactorsGridElectricity)[2:7])

MobileCombustionCO2 <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "MobileCombustionCO2")

CH4andN2OforNonRoadVehicles <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "CH4andN2OforNon-RoadVehicles")

# Add on an un-latex-ified column to facilitate lookups. 
GlobalWarmingPotentials <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "GlobalWarmingPotentials") %>%
  mutate(gas = stri_replace_all_regex(Gas, "[_$]", "")) %>%
  select(Gas, contains("GWP"), gas)


GWPsForBlendedRefrigerants <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "GWPsForBlendedRefrigerants")

CH4andN2OforOnRoadGasolineVehicles <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "CH4andN2OforOn-RoadGasolineVehicles")

CH4andN2OforOnRoadDieselandAlternativeFuelVehicles <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "CH4andN2OforOn-RoadDieselandAlternativeFuelVehicles")

CH4andN2OforNonRoadVehicles <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "CH4andN2OforNon-RoadVehicles")

TransportationAndDistribution <- read_excel(
  EPAemissionsFactorsPathname, 
  sheet = "TransportationAndDistribution")

SteamAndHeat <- read_excel(
  EPAemissionsFactorsPathname, 
    sheet = "SteamAndHeat")

BusinessTravelAndEmployeeCommuting <- read_excel(
  EPAemissionsFactorsPathname, 
    sheet = "BusinessTravelAndEmployeeCommuting ")
