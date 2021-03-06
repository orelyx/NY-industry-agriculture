# title: "GHGmanufacturingtab.R"
# author: "Eric Koski"
# date: "1/6/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

if (outputFormat == "pdf_document") {
  GHGemissionsMfgDetailHighest %>%
  select(contains("NAICS"), CO2e100mt, CountyDist, HighestCounties) %>%
  mutate(CO2e100mt = formatC(CO2e100mt, format = "d", big.mark = ",")) %>%
  kable(caption = tableCaption("Manufacturing categories with highest CO$_2$-equivalent GHG emissions"),
        col.names = c("NAICS", 
                      "Manufacturing category", 
                      "CO$_2$-equivalent emissions, tonnes",
                      "County distribution", "Principal counties"),
        digits = 0,
        format = "latex", 
        align = "rlrll", 
        booktabs = TRUE, escape = FALSE,
        linesep = c("")) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  font_size = 9.2,
                  latex_options = c("hold_position")) %>%
    column_spec(1, width = "2.3em") %>%
    column_spec(2, width = "21em") %>%
    column_spec(3, width = "7.9em") %>%
    column_spec(4, width = "5.4em") %>%
    column_spec(5, width = "11.5em")
}