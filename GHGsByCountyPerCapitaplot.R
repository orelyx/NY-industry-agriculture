# title: "GHGsByCountyPerCapitaplot.R"
# author: "Eric Koski"
# date: "1/3/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# library(dplyr)
# library(purrr)
# library(rlang)

GHGsbyCountyPerCapita <- countiesGHGsummaryWider 

for(col in names(GHGsbyCountyPerCapita)) {
  if (!is.na(suppressWarnings(as.integer(col)))) {
  GHGsbyCountyPerCapita <- GHGsbyCountyPerCapita %>%
    mutate(!!as.symbol(col) := 
      mapply(function(name, tonnes) 
        { tonnes / 
          ifelse(name == "Totals", 
                 RegionPopulation[[col]],
                 dfLookup(NYS_population_by_county, "County", name, col)) },
        County, !!as.symbol(col)))
  }
}

GHGsbyCountyPerCapita <- 
  bind_rows(GHGsbyCountyPerCapita %>% 
              filter(County != "Totals") %>%
              # arrange(desc(`2016`)),
              arrange(desc(!!sym(as.character(latestYear)))),
            GHGsbyCountyPerCapita %>% 
              filter(County == "Totals"))

GHGcounties_pc <- GHGsbyCountyPerCapita$County

GHGsbyCountyPerCapitaLonger <- GHGsbyCountyPerCapita %>%
  pivot_longer(starts_with("20"), 
               names_to = "Year", 
               values_to = "CO2ePerCapitaTonnes") %>%
  group_by(County)

if (Region != "New York State") {
  GHGsbyCountyPerCapitaLongerplot <- 
    ggplot(GHGsbyCountyPerCapitaLonger,
           aes(Year, CO2ePerCapitaTonnes, color = County)) +
    scale_y_continuous(limits = c(0, ceiling(1.17 * max(GHGsbyCountyPerCapitaLonger$CO2ePerCapitaTonnes))),
                       breaks = seq(0, 32, by=1),
                       minor_breaks = seq(0, 32, by=0.2)) +
    xlab(NULL) + ylab(NULL) +
    scale_color_manual(values = c(
      # orange        maroon2     plum        darkblue      
      #                                     
      "#ffa500",    "#7f0000",  "#dda0dd",  "#00008b",      
      
      # medium-       red         darkslate-  laserlemon     
      # turquoise                 gray                             
      "#48d1cc",    "#ff0000",  "#2f4f4f",  "#1e90ff",    
      
      # mediumspring- dark-       blue        fuchsia           
      # green         salmon
      "#00fa9a",    "#e9967a",  "#0000ff",  "#ff00ff", 
      
      # olive         chartreuse  dodgerblue  deep-
      #                                       pink
      "#808000",    "#7fff00",  "#ffff54","#ff1493"),
      name = "County",
      breaks = GHGcounties_pc
    ) +
    geom_point() +
    geom_line(aes(group = County))
  
  show(GHGsbyCountyPerCapitaLongerplot)
}

