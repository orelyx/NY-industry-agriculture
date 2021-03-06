# title: "fuelTypeDefinitions.R"
# author: "Eric Koski"
# date: "12/17/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

Standard_fuel_types <- read_excel("Standard fuel types.xlsx")

Standard_fuel_types <- filter(Standard_fuel_types, !is.na(`Fuel type`))

theLink <- text_spec("The EIA Glossary", 
                     link = "https://www.eia.gov/tools/glossary/",
                     format = "latex")

if (outputFormat == "pdf_document") {
  Standard_fuel_types %>% 
    kable(caption = tableCaption("Standard fuel types"),
          format = "latex", 
          booktabs = TRUE, escape = FALSE,
          linesep = c("\\addlinespace[4pt]")) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  latex_options = c("hold_position"),
                  position = "center",
                  protect_latex = TRUE,
                  font_size = 10) %>%
    footnote(general = str_c("Source: ", theLink),
             general_title = "",
             escape = FALSE) %>%
    column_spec(1, width = "5em") %>%
    column_spec(2, width = "44em") 
}
