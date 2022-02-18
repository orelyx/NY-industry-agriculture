# title: "energyBySector3digxCountyLYtab.R"
# author: "Eric Koski"
# date: "12/14/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
# We're going to fuss a lot with the dataset we use to render the table, 
# so let's make our own copy, filtered to contain only the latest year's data. 
namesFrom <- if (Region == "New York State") { 
  "Region" 
} else { 
  "County" 
}

energyBySector3digxAreaLY_tab <- RegionEnergyPerFuelLY_3dig %>%
  filter(YEAR == max(YEAR)) %>%
  mutate(allFuels = rowSums(select(., !!!syms(fuelNamesLY)))) %>%
  select(YEAR, County, NAICS3dig, NAICSname3dig, allFuels) %>%
  left_join(CountyRegions, by = "County") %>%
  select(-YEAR) %>%
  pivot_wider(names_from = all_of(namesFrom), values_from = allFuels)

# pivot_wider creates spurious NAs; replace them with 0s. 
if (Region == "New York State") {
  for (reg in unique(CountyRegions$Region)) {
    energyBySector3digxAreaLY_tab[[reg]][
      which(is.na(energyBySector3digxAreaLY_tab[[reg]]))] <- 0
  }
} else {
  for (cty in countyNames) {
    energyBySector3digxAreaLY_tab[[cty]][
      which(is.na(energyBySector3digxAreaLY_tab[[cty]]))] <- 0
  }
}

if (Region == "New York State") {
  energyBySector3digxAreaLY_tab <- energyBySector3digxAreaLY_tab %>%
    select(-County) %>%
    ungroup() %>%
    group_by(NAICS3dig, NAICSname3dig) %>%
    summarize(across(all_of(regionNamesLY), sum)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(`Sector totals` = sum(c_across(all_of(regionNamesLY)))) %>%
    ungroup()
  areaNamesLY <- regionNamesLY
  numberOfAreas <- numberOfRegions
} else {
  energyBySector3digxAreaLY_tab <- energyBySector3digxAreaLY_tab %>%
    rowwise() %>%
    mutate(`Sector totals` = sum(c_across(all_of(countyNamesLY)))) %>%
    ungroup()
  areaNamesLY <- countyNamesLY 
  numberOfAreas <- numberOfCounties
}

energyBySector3digxAreaLY_tab <- energyBySector3digxAreaLY_tab %>%
  arrange(desc(`Sector totals`)) %>%                         # sort descending by sector totals
  mutate(NAICSnameNum3dig = str_c(NAICS3dig, ". ",           # combine columns and scrub off 'T's 
                                  stri_replace_all_regex(NAICSname3dig, "T$", ""))) %>%
  rename(`Sector (NAICS)` = NAICSnameNum3dig) %>%
  select(-NAICSname3dig, -NAICS3dig) %>%
  select(`Sector (NAICS)`, !!!syms(areaNamesLY),           # move sector to far left
         `Sector totals`) %>%                                # and reorder columns
  # Add summary row at the bottom
  bind_rows(summarise_all(., ~(if(is.numeric(.)) sum(.) else "Totals"))) %>%
  # Convert numbers to strings with thousands separators
  mutate_if(is.numeric, ~formatC(as.integer(round(.)), big.mark = ","))

# How precisely we render the table depends on whether we're rendering to HTML or pdf. 
if (outputFormat == "pdf_document") {
  energyBySector3digxAreaLY_tab %>%
    kable(escape = FALSE, digits = 0,
          align = glue_collapse(str_c(c("l", rep.int("r", numberOfAreas + 1)))),
          caption = tableCaption(
            str_c(as.character(max(RegionEnergyPerFuelLY_3dig$YEAR)), 
                  " energy use (millions of BTU) by industry sector and ", 
                  if (namesFrom == "Region") { "region" } else { "county" })),
          booktabs = TRUE, longtable = TRUE, linesep = c("")) %>%
    add_header_above(
      if (namesFrom == "County") {
        c(" " = 1, "County" = numberOfAreas)
      } else {
        c(" " = 1, "Region" = numberOfAreas)
      }) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  latex_options = c("repeat_header", "striped"),   # "hold_position", 
                  position = "center",
                  font_size = ifelse(numberOfAreas > 9, 6.8, 
                                     ifelse(numberOfAreas > 6, 7.3, 
                                            ifelse(numberOfAreas > 4, 8, 9))), 
                  fixed_thead = TRUE,
                  stripe_color = honeydew) %>%
    row_spec(nrow(energyBySector3digxAreaLY_tab) - 1,
             hline_after = TRUE) %>%
    row_spec(nrow(energyBySector3digxAreaLY_tab),
             background = "#FFFFFF") %>%
    column_spec(1, width = ifelse(numberOfAreas > 4, "12em", "18em")) %>%
    column_spec(2:(numberOfAreas + 1), 
                width = ifelse(numberOfAreas > 9, "3.8em", 
                               ifelse(numberOfAreas > 6,  "4.0em", "4.4em"))) %>%
    column_spec(numberOfAreas + 2, width = "4.6em")
} # else {                           # probably "html_document"
#   energyBySector3digxAreaLY_tab %>%
#     kable(escape = FALSE, digits = 0, longtable = TRUE,
#           align = c("lrrrrrrrr"),
#           caption = "2016 energy use (millions of BTU) by county and industry sector") %>%
#     add_header_above(c(" " = 1, "Fuel types" = 8)) %>%
#     kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                   latex_options = c("hold_position", "repeat_header", "striped"),
#                   position = "center",
#                   font_size = 11, fixed_thead = TRUE) %>%
#     column_spec(2, width = "8em")
# }
