# title: "GHGConstructionplot.R"
# author: "Eric Koski"
# date: "1/7/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

leftTicks <- axisTicks(c(0, 1.17 * max(GHGemissionsConstrDetailHighest$CO2e100mt)), 
                       log = FALSE, nint = 5)

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

GHGConstructionforplot <- GHGemissionsConstrDetailHighest %>%
  mutate(secondAxisVals = CO2eCumFract * max(leftTicks))

xlabels <- as.character(GHGConstructionforplot$NAICS4dig[
  seq(1:length(GHGConstructionforplot$NAICS4dig))])
xlabels[length(GHGConstructionforplot$NAICS4dig)] <- "rest"

GHGConstructionplot <- ggplot(GHGConstructionforplot) +
  scale_color_manual(values = c("#333333"), name = "") +
  scale_fill_manual(values = c("#63B8FF"), name = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.margin = margin(1, 1, 1, 1, "mm"),
        legend.position = c(0.75, 0.5),
        legend.title = element_blank(),
        legend.spacing = unit(1, "mm"),
        legend.box.background = element_blank()) +
  scale_x_continuous(breaks = seq(0, nrow(GHGConstructionforplot)-1, by=1), 
                     minor_breaks = NULL,
                     labels = xlabels) +
  scale_y_continuous(breaks = leftTicks,
                     minor_breaks = seq(0, max(leftTicks), by = minorIntvl), 
                     labels = scientific_10,
                     sec.axis = sec_axis(~ . * (100 / max(leftTicks)), 
                                         name = "Cumulative percentage")) +
  xlab("Category NAICS code (see table)") +
  ylab(bquote(atop(paste(CO[2], "-equivalent emissions,"), 
                   paste("metric tons ", CO[2])))) +
  geom_col(aes(seq(0, nrow(GHGConstructionforplot)-1, by=1), 
               CO2e100mt, fill = "emissions")) +
  geom_line(aes(seq(0, nrow(GHGConstructionforplot)-1, by=1), 
                secondAxisVals, color = "percentage")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2))


show(GHGConstructionplot)
