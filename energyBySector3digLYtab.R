# title: "energyBySector3digLYtab.R"
# author: "Eric Koski"
# date: "12/14/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# We're going to fuss a lot with the dataset we use to render the table, 
# so let's make our own copy, filtered to contain only the latest year's data.  
energyBySector3digLY_tab <- RegionEnergyPerFuelLY_3dig

# Summarize across all counties 
energyBySector3digLY_tab <- energyBySector3digLY_tab %>%
  select(-County, -YEAR) %>%                                  # delete unused columns
  group_by(NAICS3dig, NAICSname3dig) %>%
  summarize_at(vars(fuelNamesLY), sum, na.rm = TRUE) %>%      # summarize for all fuel types
  mutate(`Sector totals` = sum(!!!syms(fuelNamesLY))) %>%     # add each sector's total energy use
  ungroup()                                             

energyBySector3digLY_tab <- energyBySector3digLY_tab %>%
  arrange(desc(`Sector totals`)) %>%                         # sort descending by sector totals
  mutate(NAICSnameNum3dig = str_c(NAICS3dig, ". ",           # combine columns and scrub off 'T's 
                                  stri_replace_all_regex(NAICSname3dig, "T$", ""))) %>%
  rename(`Sector (NAICS)` = NAICSnameNum3dig) %>%
  select(-NAICSname3dig, -NAICS3dig) %>%                     # remove columns now superfluous
  select(`Sector (NAICS)`, everything()) %>%                 # move sector to far left
  # Add summary row at the bottom
  bind_rows(summarise_all(., ~(if(is.numeric(.)) sum(.) else "Totals"))) %>%
  # Convert numbers to strings with thousands separators
  mutate_if(is.numeric, ~formatC(as.integer(round(.)), big.mark = ","))

# Make the fuel names friendlier and more consistent
names(energyBySector3digLY_tab) <- 
  str_replace_all(names(energyBySector3digLY_tab), "_", " ")

# How precisely we render the table depends on whether we're rendering to HTML or pdf. 
if (outputFormat == "pdf_document") {
  energyBySector3digLY_tab %>%
    kable(escape = FALSE, digits = 0, longtable = TRUE,
          align = c("lrrrrrrrrr"),
          caption = tableCaption(
            str_c(as.character(latestYear), 
                  " energy use (millions of BTU) by industry sector and fuel type")),
          booktabs = TRUE, linesep = c("")) %>%
    add_header_above(c(" " = 1, "Fuel types" = 8)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  latex_options = c("repeat_header", "striped", "hold_position"),
                  position = "center",
                  font_size = 8, fixed_thead = TRUE,
                  stripe_color = honeydew) %>%
    row_spec(nrow(energyBySector3digLY_tab) - 1,
             hline_after = TRUE) %>%
    row_spec(nrow(energyBySector3digLY_tab),
             background = "#FFFFFF") %>%
    column_spec(1, width = "11.5em") %>%
    column_spec(2, width = "4.2em") %>%
    column_spec(3, width = "4.2em") %>%
    column_spec(4, width = "4em") %>%
    column_spec(5, width = "4em") %>%
    column_spec(6, width = "3.8em") %>%
    column_spec(7, width = "3.8em") %>%
    column_spec(8, width = "3.8em") %>%
    column_spec(9, width = "4.4em") %>%
    column_spec(10, width = "4.2em") 
} # else {                           # probably "html_document"
#   energyBySector3digLY_tab %>%
#     kable(escape = FALSE, digits = 0, longtable = TRUE,
#           align = c("lrrrrrrrr"),
#           caption = "2016 energy use (millions of BTU) by county and industry sector") %>%
#     add_header_above(c(" " = 1, "Fuel types" = 8)) %>%
#     kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                   latex_options = c("hold_position", "repeat_header"),
#                   position = "center",
#                   font_size = 11, fixed_thead = TRUE) %>%
#     column_spec(2, width = "8em")
# }
