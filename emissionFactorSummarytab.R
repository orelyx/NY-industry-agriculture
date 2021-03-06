# title: "emissionFactorSummarytab.R"
# author: "Eric Koski"
# date: "1/2/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
# library(tidyverse)
# library(knitr)
# library(rmarkdown)

emissionFactorSummary <- GHGfactors_tbl %>%
  transmute(`Fuel type` = 
              stri_replace_all_fixed(
                stri_replace_all_fixed(FuelName, "_", " "), 
                "LPG NGL", "LPG-NGL"),
            `CO$_2$, kg per mmBTU` = CO2, 
            `CH$_4$, g per mmBTU` = CH4, 
            `N$_2$O, g per mmBTU` = N2O,
            `CO$_2$-equivalent emissions, kgCO$_2$ per mmBTU` = CO2e100) 

if (outputFormat == "pdf_document") {
  emissionFactorSummary %>%
  kable(escape = FALSE, digits = 2, booktabs = TRUE,
        align = c("lrrrr"),
        linesep = c("\\addlinespace[1mm]"),
        caption = tableCaption(
          "Summary: emission factors for NREL fuel types")) %>%
    column_spec(1, width = "10em") %>%
    column_spec(2, width = "6em") %>%
    column_spec(3, width = "6em") %>%
    column_spec(4, width = "6em") %>%
    column_spec(5, width = "12em") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"),
                  latex_options = c("hold_position"),
                  position = "center",
                  font_size = 10)
}
