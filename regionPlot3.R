# title: "regionPlot3.R"
# author: "Eric Koski"
# date: "12/13/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
#

# Make the second faceted area plot. 
regionsMfg3dAreaPlot2 <- ggplot(RegionEnergyPerMfgSector3dYearFuelBounds2) +
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
                    breaks = rev(fuelNames)) + 
  scale_y_continuous(labels = scientific_10) 

for (fuel in fuelNames) {
  regionsMfg3dAreaPlot2 <- regionsMfg3dAreaPlot2 +
    geom_ribbon(aes(YEAR, 
                    ymin = !!as.symbol(str_c(fuel, "Lower")), 
                    ymax = !!as.symbol(str_c(fuel, "Upper")), 
                    fill = !!quo_name(fuel)), alpha = 0.6)
}

show(regionsMfg3dAreaPlot2)
