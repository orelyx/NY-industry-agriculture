# title: "GHGsectorYearplot.R"
# author: "Eric Koski"
# date: "1/8/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

GHGemissionsSectorYearSummaryplot <- GHGemissionsSectorYearSummary
GHGemissionsSectorYearSummaryplot$Sector <- 
  factor(GHGemissionsSectorYearSummary$Sector, levels = sectorOrder)

GHGemissionsSectorYearSummaryplot <- GHGemissionsSectorYearSummaryplot %>%
  arrange(Sector)

maxSum <- max((GHGemissionsSectorYearSummaryplot %>%
             group_by(Year) %>% 
             summarize(across(CO2e100mt, sum)))$CO2e100mt)

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

GHGsectorYearplot <- ggplot(GHGemissionsSectorYearSummaryplot) +
  scale_fill_manual(values = c(
    "#9A6324", "#3cb44b", "#63B8FF", "#ffa500"),
    name = "Sector",
    breaks = sectorOrder
    ) +
  scale_y_continuous(breaks = leftTicks,
                     minor_breaks = seq(0, max(leftTicks), by = minorIntvl),
    labels = scientific_10) +
  ylab(bquote(atop(paste(CO[2], "-equivalent emissions,"), 
                   paste("metric tons ", CO[2])))) +
  scale_x_continuous(breaks = seq(2000, 2040, by = 1),
                     minor_breaks = seq(2000, 2040, by = 1)) +
  theme(axis.title.x = element_blank()) +
  geom_col(aes(Year, CO2e100mt, fill = Sector))

show(GHGsectorYearplot)
  
