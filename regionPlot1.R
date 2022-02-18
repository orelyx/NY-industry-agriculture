# title: "regionPlot1.R"
# author: "Eric Koski"
# date: "12/13/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Summarize to a dataset aggregating the use of each fuel for each 2-dig sector and 
# year (dropping any breakdown by county). This is used for the area plot below. 
# Filter out manufacturing sectors; we will plot them separately. 

RegionEnergyPerNonMSector3dYearFuel <- RegionEnergyPerFuelYear_3dig %>% 
  filter((NAICS3dig < 300) | (NAICS3dig > 399)) %>%
  group_by(YEAR, NAICSname3dig) %>% 
  summarize(Diesel = sum(Diesel, na.rm = TRUE), 
            LPG_NGL = sum(LPG_NGL, na.rm = TRUE), 
            Net_electricity = sum(Net_electricity, na.rm = TRUE), 
            Coal = sum(Coal, na.rm = TRUE), 
            Natural_gas = sum(Natural_gas, na.rm = TRUE), 
            Coke_and_breeze = sum(Coke_and_breeze, na.rm = TRUE), 
            Residual_fuel_oil = sum(Residual_fuel_oil, na.rm = TRUE), 
            Other = sum(Other, na.rm = TRUE),
            .groups = "drop") 

# Make our own copy of RegionEnergyPerNonMSector3dYearFuel with columns added 
# to represent upper and lower bounds for area plots. 
RegionEnergyPerNonMSector3dYearFuelBounds <- ungroup(RegionEnergyPerNonMSector3dYearFuel) 

for (fuel in fuelNames) {
  if (fuel == fuelNames[1])
  {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    RegionEnergyPerNonMSector3dYearFuelBounds <- 
      RegionEnergyPerNonMSector3dYearFuelBounds %>%
      mutate(!!name1 := seq(0, 0, length.out = 
                              length(RegionEnergyPerNonMSector3dYearFuelBounds[[fuel]])))
    lastFuel <- fuel
  } else {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    name2 <- str_c(lastFuel, "Upper")
    RegionEnergyPerNonMSector3dYearFuelBounds <- RegionEnergyPerNonMSector3dYearFuelBounds %>%
      mutate(!!name1 := RegionEnergyPerNonMSector3dYearFuelBounds[[name2]])
  }
  name1 <- as.symbol(str_c(fuel, "Upper"))
  name2 <- str_c(fuel, "Lower")
  RegionEnergyPerNonMSector3dYearFuelBounds <- RegionEnergyPerNonMSector3dYearFuelBounds %>%
    mutate(!!name1 := RegionEnergyPerNonMSector3dYearFuelBounds[[name2]] + 
             RegionEnergyPerNonMSector3dYearFuelBounds[[fuel]])
  lastFuel <- fuel
}

# Some sector names have "T" appended to them in the original dataset. These originally 
# meant something but just seem distracting here. 
RegionEnergyPerNonMSector3dYearFuelBounds <- RegionEnergyPerNonMSector3dYearFuelBounds %>%
  mutate_at(c("NAICSname3dig"), function(x) {stri_replace_last_regex(x, "T$", "")})

# Make the faceted area plot. Note use of "!!as.symbol" -- a useful idiom as it 
# allows us to synthesize column names rather than having to give them all explicitly. 
regionsNonM3dAreaPlot <- ggplot(RegionEnergyPerNonMSector3dYearFuelBounds) +
  # ggtitle("Annual energy use by county and fuel type") +
  facet_wrap(~NAICSname3dig, ncol = 2, scales = "free_y") +
  theme(strip.text = element_text(size = 8)) +
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
  scale_y_continuous(labels = scientific_10) 

for (fuel in fuelNames) {
  regionsNonM3dAreaPlot <- regionsNonM3dAreaPlot +
    geom_ribbon(aes(YEAR, 
                    ymin = !!as.symbol(str_c(fuel, "Lower")), 
                    ymax = !!as.symbol(str_c(fuel, "Upper")), 
                    fill = !!quo_name(str_replace_all(fuel, "_", " "))), alpha = 0.6)
}

# Below doesn't seem to work -- why not?
# regionsNonM3dAreaPlot <- regionsNonM3dAreaPlot +
#   guides(fill = guide_legend(override.aes = list(alpha = 0.6)))

show(regionsNonM3dAreaPlot)
# dput(regionsNonM3dAreaPlot, "regionsNonM3dAreaPlot.txt", control = "all")
