# title: "countiesLinePlot.R"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
#
# Make a line-plot of the aggregated fuel use
if (Region != "New York State") {
  
  countiesLinePlot <- ggplot(RegionEnergyPerCountyYearFuel) +
    facet_wrap(~County, ncol = 2, scales = "free_y") +
    ylab("Annual energy use, MMBTU") +
    scale_color_hue(name = "Fuel type", h = c(30, 360) + 15) +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(labels = scientific_10)
  
  for (fuel in fuelNames) {
    countiesLinePlot <- countiesLinePlot +
      geom_line(aes(YEAR, 
                    !!as.symbol(fuel), 
                    color = !!quo_name(str_replace_all(fuel, "_", " "))))
  }
  
  show(countiesLinePlot)
}

