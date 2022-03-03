# title: "energyUsePerFuelYeartab.R"
# author: "Eric Koski"
# date: "12/16/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

RegionEnergyPerYearFueltab <- RegionEnergyPerFuelYear_3dig %>% 
  group_by(YEAR) %>% 
  summarize_at(vars(rev(all_of(fuelNames))), sum, na.rm = TRUE) %>%
  arrange(YEAR) %>%
  mutate(Year = as.character(YEAR)) %>%
  select(-YEAR) %>%
  select(Year, everything()) %>%
  bind_rows(summarise_all(., ~(if(is.numeric(.)) sum(.) else "Totals"))) %>%
  mutate(`Annual totals` = rowSums(select(., !!!syms(all_of(fuelNames))))) 

earliestRow <- filter(RegionEnergyPerYearFueltab, Year == min(Year))
latestRow <- filter(RegionEnergyPerYearFueltab, Year == max(Year))

firstNaturalGasPct <- 
  formatC(round(100 * (earliestRow$`Natural gas` / earliestRow$`Annual totals`)),
          format = "d")
lastNaturalGasPct <- 
  formatC(round(100 * (latestRow$`Natural gas` / latestRow$`Annual totals`)),
          format = "d")

RegionEnergyPerYearFueltab <- RegionEnergyPerYearFueltab %>%
  mutate_if(is.numeric, ~formatC(as.integer(round(.)), big.mark = ","))

names(RegionEnergyPerYearFueltab) <- 
  str_replace_all(names(RegionEnergyPerYearFueltab), "_", " ")

# Render the table
if (outputFormat == "pdf_document") {
  RegionEnergyPerYearFueltab %>%
    kable(escape = FALSE, digits = 0, booktabs = TRUE, longtable = TRUE,
          align = c("crrrrrrrrr"),
          linesep = c(""),
          caption = tableCaption(
            "Annual industrial and agricultural energy use (millions of BTU) by fuel type")) %>%
    row_spec(length(RegionEnergyPerYearFueltab$Year) - 1, hline_after = TRUE) %>%
    column_spec(1, width = "3em") %>%
    column_spec(2, width = "4.2em") %>%
    column_spec(3, width = "4.4em") %>%
    column_spec(4, width = "4.4em") %>%
    column_spec(5, width = "4.4em") %>%
    column_spec(6, width = "4.4em") %>%
    column_spec(7, width = "3.9em") %>%
    column_spec(8, width = "3.9em") %>%
    column_spec(9, width = "4.4em") %>%
    column_spec(10, width = "5em") %>%
    kable_styling(position = "center",
                  font_size = 8.6)
}

