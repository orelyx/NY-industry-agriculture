# title: "countySectorYearFueltab.R"
# author: "Eric Koski"
# date: "12/11/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
# Here we render a rather lengthy table showing how energy use in each county 
# and 1-digit NAICS changes year-by-year, per fuel. A lot of effort went into 
# getting the pdf version to look right and fit in a page-width. 

# We're going to fuss a lot with the dataset we use to render the table, 
# so let's make our own copy. We won't use NAICS1dig. 
RegionSectorYearFuel_tab <- select(RegionEnergyPerFuelYear_1dig, -NAICS1dig)

# We want the initial columns ordered County, Sector, Year, then fuels in decreasing 
# order of total use. 
RegionSectorYearFuel_tab <- RegionSectorYearFuel_tab %>% 
  rename(Year = YEAR) %>%
  select(County, NAICSname1dig, Year, !!!vars(rev(fuelNames))) 

RegionSectorYearFuel_tab <- RegionSectorYearFuel_tab %>%
  arrange(County, NAICSname1dig, Year) %>%
  rename('Sector (NAICS)' = NAICSname1dig) %>%
  # Convert numbers to strings with thousands separators
  mutate_at(fuelNames, ~formatC(as.integer(round(.)), big.mark = ","))

# Make the fuel names friendlier and more consistent
names(RegionSectorYearFuel_tab) <- 
  str_replace_all(names(RegionSectorYearFuel_tab), "_", " ")

# How precisely we render the table depends on whether we're rendering to HTML or pdf. 
if (outputFormat == "pdf_document") {
  RegionSectorYearFuel_tab %>%
    kable(escape = FALSE, digits = 0, longtable = TRUE,
          align = c("llcrrrrrrrr"),
          caption = tableCaption(
            "Annual energy use (millions of BTU) by county and industry sector"),
          booktabs = TRUE, linesep = c("", "", "", "", "", "", "\\addlinespace")) %>%
    add_header_above(c(" " = 3, "Fuel types" = 8)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  latex_options = c("hold_position", "repeat_header"),
                  position = "center",
                  font_size = 8, fixed_thead = TRUE) %>%
    column_spec(1, width = "4em") %>%
    column_spec(2, width = "8em") %>%
    column_spec(3, width = "1.8em") %>%
    column_spec(4, width = "4em") %>%
    column_spec(5, width = "3.8em") %>%
    column_spec(6, width = "4em") %>%
    column_spec(7, width = "3.5em") %>%
    column_spec(8, width = "4em") %>%
    column_spec(9, width = "4em") %>%
    column_spec(10, width = "4em") %>%
    column_spec(11, width = "4em")
} # else {                           # probably "html_document"
#   RegionSectorYearFuel_tab %>%
#     kable(escape = FALSE, digits = 0, longtable = TRUE,
#           align = c("llcrrrrrrrr"),
#           caption = "Annual energy use (millions of BTU) by county and industry sector") %>%
#     add_header_above(c(" " = 3, "Fuel types" = 8)) %>%
#     kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                   latex_options = c("hold_position", "repeat_header"),
#                   position = "center",
#                   font_size = 11, fixed_thead = TRUE) %>%
#     column_spec(2, width = "8em")
# }
