# title: "regionPlot2.R"
# author: "Eric Koski"
# date: "12/13/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
#
# Summarize to a dataset aggregating the use of each fuel for each 3-dig sector and 
# year (dropping any breakdown by county). This is used for the area plot below. 
# Filter out non-manufacturing sectors, which were already plotted in regionPlot1. 

RegionEnergyPerMfgSector3dYearFuel <- RegionEnergyPerFuelYear_3dig %>% 
  filter((NAICS3dig > 299) & (NAICS3dig < 400)) %>%
  group_by(YEAR, NAICS3dig, NAICSname3dig) %>% 
  summarize(Diesel = sum(Diesel, na.rm = TRUE), 
            LPG_NGL = sum(LPG_NGL, na.rm = TRUE), 
            Net_electricity = sum(Net_electricity, na.rm = TRUE), 
            Coal = sum(Coal, na.rm = TRUE), 
            Natural_gas = sum(Natural_gas, na.rm = TRUE), 
            Coke_and_breeze = sum(Coke_and_breeze, na.rm = TRUE), 
            Residual_fuel_oil = sum(Residual_fuel_oil, na.rm = TRUE), 
            Other = sum(Other, na.rm = TRUE),
            .groups = "drop") 

# Make our own copy of RegionEnergyPerMfgSector3dYearFuel with columns added 
# to represent upper and lower bounds for area plots. 
RegionEnergyPerMfgSector3dYearFuelBounds <- ungroup(RegionEnergyPerMfgSector3dYearFuel) 

for (fuel in fuelNames) {
  if (fuel == fuelNames[1])
  {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    RegionEnergyPerMfgSector3dYearFuelBounds <- 
      RegionEnergyPerMfgSector3dYearFuelBounds %>%
      mutate(!!name1 := seq(0, 0, length.out = 
                              length(RegionEnergyPerMfgSector3dYearFuelBounds[[fuel]])))
    lastFuel <- fuel
  } else {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    name2 <- str_c(lastFuel, "Upper")
    RegionEnergyPerMfgSector3dYearFuelBounds <- RegionEnergyPerMfgSector3dYearFuelBounds %>%
      mutate(!!name1 := RegionEnergyPerMfgSector3dYearFuelBounds[[name2]])
  }
  name1 <- as.symbol(str_c(fuel, "Upper"))
  name2 <- str_c(fuel, "Lower")
  RegionEnergyPerMfgSector3dYearFuelBounds <- RegionEnergyPerMfgSector3dYearFuelBounds %>%
    mutate(!!name1 := RegionEnergyPerMfgSector3dYearFuelBounds[[name2]] + 
             RegionEnergyPerMfgSector3dYearFuelBounds[[fuel]])
  lastFuel <- fuel
}

# Some sector names have "T" appended to them in the original dataset. These originally 
# meant something but just seem distracting here. 
RegionEnergyPerMfgSector3dYearFuelBounds <- RegionEnergyPerMfgSector3dYearFuelBounds %>%
  arrange(NAICS3dig) %>%
  mutate_at(c("NAICSname3dig"), function(x) {stri_replace_last_regex(x, "T$", "")}) %>%
  mutate_at(c("NAICSname3dig"), function(x) {
    stri_replace_all_regex(x, "Electrical Equipment", "Electric Eqpt.")}) %>%
  mutate_at(c("NAICSname3dig"), function(x) {
    stri_replace_all_regex(x, "Component Manufacturing", "Component Mfg.")})

# We'll have to chart the data for the industry sectors separately in two groups, 
# because there are so many of them. 
RegionEnergyPerMfgSector3dYearFuelBounds1 <- RegionEnergyPerMfgSector3dYearFuelBounds %>%
  filter(NAICS3dig < 325)
RegionEnergyPerMfgSector3dYearFuelBounds2 <- RegionEnergyPerMfgSector3dYearFuelBounds %>%
  filter(NAICS3dig > 324)

# Make the first faceted area plot. Note use of "!!as.symbol" -- a useful idiom as it 
# allows us to synthesize column names rather than having to give them all explicitly. 
regionsMfg3dAreaPlot1 <- ggplot(RegionEnergyPerMfgSector3dYearFuelBounds1) +
  # ggtitle("Annual energy use by county and fuel type") +
  facet_wrap(~NAICSname3dig, ncol = 2, scales = "free_y") +
  theme(strip.text = element_text(size = 8)) +
  ylab("Annual energy use, MMBTU (Millions of BTU)") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = c("#e6194B", "#000075", "#3cb44b", "#9A6324", 
                               "#4363d8", "#f58231", "#42d4f4", "#f032e6", 
                               "#469990", "#800000", "#ffe119", "#aaffc3", 
                               "#ffe119", "#e6beff"),
                    name = "Fuel type", 
                    # ... and order the legend keys the way we want them!
                    breaks = rev(str_replace_all(fuelNames, "_", " "))) + 
  scale_y_continuous(labels = scientific_10) 

for (fuel in fuelNames) {
  regionsMfg3dAreaPlot1 <- regionsMfg3dAreaPlot1 +
                geom_ribbon(aes(YEAR, 
                    ymin = !!as.symbol(str_c(fuel, "Lower")), 
                    ymax = !!as.symbol(str_c(fuel, "Upper")), 
                    fill = !!quo_name(str_replace_all(fuel, "_", " "))), alpha = 0.6)
}

show(regionsMfg3dAreaPlot1)

