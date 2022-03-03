# title: "energyUsePerCountyYearplot.Rmd"
# author: "Eric Koski"
# date: "12/13/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Render a simple stacked-bar chart of energy use per county per year. 

if ((outputFormat == "pdf_document") & (Region != "New York State")) {
  energyUseSummaryplot <- ggplot(energyUseSummary) +
    scale_fill_manual(values = c(
      "#e6194B", "#000075", "#3cb44b", "#9A6324", 
      "#4363d8", "#f58231", "#43d4f4", "#f032e6", 
      "#469990", "#800000", "#ffe119", "#aaffc3", 
      "#ffe119", "#e6beff"),
      name = "County",
      breaks = counties) +
    scale_y_continuous(breaks = seq(0, 1e8, by = 1e7),
                       minor_breaks = seq(0, 1e8, by = 2e6),
                       labels = scientific_10) +
    ylab("Annual energy use, MMBTU (Millions of BTU)") +
    scale_x_continuous(breaks = seq(2000, 2040, by = 1),
                       minor_breaks = seq(2000, 2040, by = 1)) +
    theme(axis.title.x = element_blank()) +
    geom_col(aes(Year, MMBTU, fill = County))
  
  show(energyUseSummaryplot)
}


