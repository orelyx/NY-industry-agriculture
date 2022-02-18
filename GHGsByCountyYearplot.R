# title: "GHGsByCountyYearplot.R"
# author: "Eric Koski"
# date: "1/2/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

maxSum <- max((countiesGHGsummary %>%
                 group_by(Year) %>% 
                 summarize(across(CO2e100tonnes, sum)))$CO2e100tonnes)

leftTicks <- axisTicks(c(0, 1.17 * maxSum), 
                       log = FALSE, nint = 6)

floorLog <- floor(log10(max(leftTicks)))
if((max(leftTicks) / (10^floorLog)) >= 7) {
  minorIntvl <- 10^floorLog
} else if((max(leftTicks) / (10^floorLog)) >= 4.5) {
  minorIntvl <- 10^floorLog /2
} else if((max(leftTicks) / (10^floorLog)) >= 2) {
  minorIntvl <- 10^floorLog / 4
} else {
  minorIntvl <- 10^(floorLog - 1)
}

if (Region != "New York State") {
  countiesGHGsummaryplot <- ggplot(countiesGHGsummary) +
    scale_fill_manual(values = c(
      "#e6194B", "#000075", "#3cb44b", "#9A6324", 
      "#4363d8", "#f58231", "#43d4f4", "#f032e6", 
      "#469990", "#800000", "#ffe119", "#aaffc3", 
      "#ffe119", "#e6beff"),
      name = "County",
      breaks = GHGcounties) +
    scale_y_continuous(breaks = leftTicks,
                       minor_breaks = seq(0, max(leftTicks), by = minorIntvl),
                       labels = scientific_10) +
    ylab(TeX('CO$_2$-equivalent emissions, metric tons CO$_2$')) +
    scale_x_continuous(breaks = seq(2000, 2040, by = 1),
                       minor_breaks = seq(2000, 2040, by = 1)) +
    theme(axis.title.x = element_blank()) +
    geom_col(aes(Year, CO2e100tonnes, fill = County))
  
    show(countiesGHGsummaryplot)
}

