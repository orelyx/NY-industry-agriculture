# title: "GHGMiningtab.R"
# author: "Eric Koski"
# date: "1/6/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. a

if (outputFormat == "pdf_document") {
  GHGemissionsMiningDetailHighest %>%
    select(contains("NAICS"), CO2e100mt, CountyDist, HighestCounties) %>%
    mutate(CO2e100mt = formatC(CO2e100mt, format = "d", big.mark = ",")) %>%
    kable(caption = 
            tableCaption("Mining categories with CO$_2$-equivalent GHG emissions"),
          col.names = c("NAICS", 
                        "Mining category", 
                        "CO$_2$-equivalent emissions, tonnes",
                        "County distribution", "Principal counties"),
          digits = 0,
          format = "latex", 
          align = "rlrll", 
          booktabs = TRUE, escape = FALSE,
          linesep = c("")) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  font_size = 9.6,
                  latex_options = c("hold_position")) %>%
    column_spec(1, width = "2.3em") %>%
    column_spec(2, width = "16.7em") %>%
    column_spec(3, width = "7.9em") %>%
    column_spec(4, width = "5.4em") %>%
    column_spec(5, width = "11em") 
}

