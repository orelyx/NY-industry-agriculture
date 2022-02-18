# title: "GHGsectortabs.R"
# author: "Eric Koski"
# date: "1/8/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# library(tidyverse)

if (Region == "New York State") {
  # Table with 1-digit sectors across the top, counties down the left
  GHGsectorCountyLY <- GHGemissionsLYSectorCountySummary
  GHGsectorCountyLY$Sector <- factor(GHGsectorCountyLY$Sector, levels = rev(sectorOrder))
  GHGsectorCountyLYtotals <- GHGsectorCountyLY %>%
    group_by(Sector) %>% 
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    pivot_wider(names_from = Sector,
                values_from = CO2e100mt,
                values_fill = list(CO2e100mt = 0)) %>%
    bind_cols(County = "New York State totals") %>%
    mutate(`Total` = sum(c_across(all_of(c("Manufacturing", "Agriculture", "Construction", "Mining")))))
  GHGsectorCountyLY <- GHGsectorCountyLY %>%
    mutate(theLevel = sapply(Sector, function(x) { which(x == sectorOrder) })) %>%
    arrange(desc(theLevel)) %>%
    select(-theLevel) %>%
    pivot_wider(names_from = Sector,
                values_from = CO2e100mt,
                values_fill = list(CO2e100mt = 0)) %>%
    rowwise() %>%
    mutate(`Total` = sum(c_across(all_of(c("Manufacturing", "Agriculture", "Construction", "Mining"))))) %>%
    ungroup() %>%
    select(-Year) %>%
    bind_rows(GHGsectorCountyLYtotals) %>%
    mutate(across(c("Manufacturing", "Agriculture", "Construction", "Mining", "Total"), 
                  ~formatC(., format = "f", digits = 0, big.mark = ",")))
  
  if (outputFormat == "pdf_document") {
    GHGsectorCountyLY %>%
      kable(escape = FALSE, digits = 0, booktabs = TRUE, longtable = TRUE,
          align = c("lrrrrr"),
          linesep = c(rep.int(c(rep.int("", 4), "\\addlinespace"), (numberOfCounties - 1) %/% 5), "\\midrule"),
          caption = 
            tableCaption(str_c(as.character(latestYear), " CO$_2$-equivalent GHG emissions per sector and county, metric tons CO$_2$"))) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"),
                  position = "center",
                  font_size = 10)
  }
} else {
  # Since the number of counties is small enough, we can use columns for counties and 
  # rows for sectors; this is just a concise summary
  GHGsectorCountyLY <- GHGemissionsLYSectorCountySummary %>%
    arrange(County) %>%
    pivot_wider(id_cols = Sector, 
                names_from = County,
                values_from = CO2e100mt,
                values_fill = list(CO2e100mt = 0)) %>%
    select(Sector, sort(GHGcounties)) 
  
  GHGsectorCountyLY$Sector <- factor(GHGsectorCountyLY$Sector, levels = rev(sectorOrder))
  
  GHGsectorCountyLY <- GHGsectorCountyLY %>%
    arrange(Sector) %>%
    mutate(CountyDist = "", Total = 0)
  
  for (i in 1:nrow(GHGsectorCountyLY)) {
    row <- slice(GHGsectorCountyLY, i)
    row$Total <- sum(select(row, one_of(countyNames)))
    countyGHGs <- select(row, one_of(sort(GHGcounties)))
    if (sum(countyGHGs) < 0.00001) {
      GHGsectorCountyLY[i,]$CountyDist <- ""
    } else {
      GHGfractions <- lapply(countyGHGs, function(x) (x/row$Total))
      GHGsectorCountyLY[i,]$CountyDist <- 
        sparkline(yspikes = c(unlist(GHGfractions), 0), 
                  width=8, 
                  bottomline = FALSE)
    }
  }
  
  GHGsectorCountyLY <- GHGsectorCountyLY %>%
    mutate(`County distribution` = CountyDist) %>%
    select(Sector, `County distribution`, everything(), -CountyDist, -Total) 
  
  for (cty in countyNames) {
    GHGsectorCountyLY <- GHGsectorCountyLY %>%
      mutate(!!sym(cty) := formatC(!!sym(cty), format = "d", big.mark = ","))
  }
  
  if (outputFormat == "pdf_document") {
    GHGsectorCountyLY %>%
      kable(caption = 
              tableCaption(str_c(as.character(latestYear), " CO$_2$-equivalent GHG emissions per sector and county, metric tons CO$_2$")),
            digits = 0,
            format = "latex", 
            align = "llrrrrrrrrr", 
            booktabs = TRUE, escape = FALSE,
            linesep = c("\\addlinespace[1mm]")) %>%
      column_spec(1, width = "6.3em") %>%
      column_spec(2, width = "5.3em") %>%
      column_spec(3:(numberOfCounties + 2), width = ifelse(numberOfCounties <= 6, "5em", "4.1em")) %>%
      kable_styling(bootstrap_options = c("hover", "condensed"),
                    latex_options = c("hold_position"),
                    position = "center",
                    font_size = ifelse(numberOfCounties <= 6, 9, 
                                       ifelse(numberOfCounties <= 8, 8, 7.6)))
  }
}