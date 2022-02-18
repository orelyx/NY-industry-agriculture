# title: "energyUsePerFuelYearplot"
# author: "Eric Koski"
# date: "12/16/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Summarize to a dataset aggregating the use of each fuel for each 
# year (dropping any breakdown by county). This is used for the area plot below. 

RegionEnergyPerYearFuel <- RegionEnergyPerFuelYear_3dig %>% 
  group_by(YEAR) %>% 
  summarize_at(vars(all_of(fuelNames)), sum, na.rm = TRUE) 

# Make our own copy of RegionEnergyPerYearFuel with columns added 
# to represent upper and lower bounds for area plots. 
RegionEnergyPerYearFuelBounds <- ungroup(RegionEnergyPerYearFuel) 

for (fuel in fuelNames) {
  if (fuel == fuelNames[1])
  {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    RegionEnergyPerYearFuelBounds <- 
      RegionEnergyPerYearFuelBounds %>%
      mutate(!!name1 := seq(0, 0, length.out = 
                              length(RegionEnergyPerYearFuelBounds[[fuel]])))
    lastFuel <- fuel
  } else {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    name2 <- str_c(lastFuel, "Upper")
    RegionEnergyPerYearFuelBounds <- RegionEnergyPerYearFuelBounds %>%
      mutate(!!name1 := RegionEnergyPerYearFuelBounds[[name2]])
  }
  name1 <- as.symbol(str_c(fuel, "Upper"))
  name2 <- str_c(fuel, "Lower")
  RegionEnergyPerYearFuelBounds <- RegionEnergyPerYearFuelBounds %>%
    mutate(!!name1 := RegionEnergyPerYearFuelBounds[[name2]] + 
             RegionEnergyPerYearFuelBounds[[fuel]])
  lastFuel <- fuel
}

# Make the area plot. Note use of "!!as.symbol" -- a useful idiom as it 
# allows us to synthesize column names rather than having to give them all explicitly. 
energyPerYearFuelPlot <- ggplot(RegionEnergyPerYearFuelBounds) +
  # ggtitle("Annual energy use by county and fuel type") +
  ylab("Annual energy use, MMBTU (Millions of BTU)") +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank()) +
  # scale_fill_hue(name = "Fuel type", h = c(30, 360) + 15) +
  # scale_fill_manual(values = c("#1a1980", "#bf8c26", 
  #                              "#800d59", "#1ab3ff", "#1a801a", "#ff0000", 
  #                              "#3333ff", "#bbff33", "#bf0000"),
  #                              name = "Fuel type") +
  scale_fill_manual(values = c("#e6194B", "#000075", "#3cb44b", "#9A6324", 
                               "#4363d8", "#f58231", "#43d4f4", "#f032e6", 
                               "#469990", "#800000", "#ffe119", "#aaffc3", 
                               "#ffe119", "#e6beff"),
                    name = "Fuel type", 
                    # ... and order the legend keys the way we want them!
                    breaks = rev(str_replace_all(fuelNames, "_", " ")),
                    guide = guide_legend(override.aes = list(alpha = 0.6))) +
  scale_y_continuous(labels = scientific_10) +
  scale_x_continuous(breaks = seq(min(RegionEnergyPerYearFuel$YEAR),
                                  max(RegionEnergyPerYearFuel$YEAR), 
                                  by = 1))

for (fuel in fuelNames) {
  energyPerYearFuelPlot <- energyPerYearFuelPlot +
    geom_ribbon(aes(YEAR, 
                    ymin = !!as.symbol(str_c(fuel, "Lower")), 
                    ymax = !!as.symbol(str_c(fuel, "Upper")), 
                    fill = !!quo_name(str_replace_all(fuel, "_", " "))), alpha = 0.6)
}

show(energyPerYearFuelPlot)
