# title: "countySectorLYFueltab.R"
# author: "Eric Koski"
# date: "12/11/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
# We're going to fuss a lot with the dataset we use to render the table, 
# so let's make our own copy. We won't use NAICS1dig. 
RegionSectorLYFuel_tab <- RegionEnergyPerFuelLY_4dig

# We want the initial columns ordered County, Sector
RegionSectorLYFuel_tab <- RegionSectorLYFuel_tab %>% 
  select(County, NAICS4dig, NAICSname4dig, everything()) %>%
  select(-YEAR)

RegionSectorLYFuel_tab <- RegionSectorLYFuel_tab %>%
  arrange(County, NAICS4dig) %>%
  # combine columns and scrub off 'T's
  mutate(NAICSnameNum4dig = str_c(NAICS4dig, ". ",           
                                  stri_replace_all_regex(NAICSname4dig, "T$", ""))) %>%
  rename(`County / Sector (NAICS)` = NAICSnameNum4dig) %>%
  # remove superfluous columns
  select(-NAICSname4dig, -NAICS4dig) %>%     
  # reorder columns
  select(County, `County / Sector (NAICS)`, !!!syms(fuelNamesLY)) %>% 
  mutate_at(vars(all_of(fuelNames)), ~formatC(as.integer(round(.)), big.mark = ","))

# Make the fuel names friendlier and more consistent
names(RegionSectorLYFuel_tab) <- 
  str_replace_all(names(RegionSectorLYFuel_tab), "_", " ")

aList <- NULL
countyList <- 
  if (TRUE) {
    for (county in unique(RegionSectorLYFuel_tab$County)) {
      aList[[county]] <- sum(RegionSectorLYFuel_tab$County == county)
    }
    aList
  }
# print(countyList)

rowList <- NULL
for (i in 1:(length(countyList) - 1)) {
  rowList[[i]] <- sum(unlist(countyList)[1:i])
}
# print(rowList)

stripeIndex <- NULL
for (i in 1:length(countyList)) {
  start <- ifelse(i == 1, 2, unlist(rowList)[i-1] + 2)
  stripeIndex <- c(stripeIndex, seq(start, start + unlist(countyList)[i] - 2, by = 2))
}
# stripeIndex

RegionSectorLYFuel_tab <- RegionSectorLYFuel_tab %>%
  select(-County)

# How precisely we render the table depends on whether we're rendering to HTML or pdf. 
if (outputFormat == "pdf_document") {
  RegionSectorLYFuel_tab %>%
    kable(escape = FALSE, digits = 0, longtable = TRUE,
          align = c("lrrrrrrrr"), 
          caption = tableCaption(str_c(
            as.character(max(RegionEnergyPerFuelLY_4dig$YEAR)),
            " energy use (millions of BTU) by county and industry sector")),
          booktabs = TRUE, linesep = c("")) %>%
    add_header_above(c(" " = 1, "Fuel types" = 8)) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  latex_options = c("striped", "hold_position", "repeat_header"),
                  position = "center",
                  font_size = 8, fixed_thead = TRUE,
                  stripe_index = stripeIndex,
                  stripe_color = honeydew) %>%
    pack_rows(index = countyList, hline_after = TRUE, 
              indent = FALSE, latex_gap_space = "0.75em") %>%
    row_spec(unlist(rowList), hline_after = TRUE, ) %>%
    column_spec(1, width = "16em") %>%
    column_spec(2, width = "4em") %>%
    column_spec(3, width = "4em") %>%
    column_spec(4, width = "4em") %>%
    column_spec(5, width = "3.5em") %>%
    column_spec(6, width = "4em") %>%
    column_spec(7, width = "4em") %>%
    column_spec(8, width = "3.7em") %>%
    column_spec(9, width = "4.4em")
} # else {                           # probably "html_document"
#   RegionSectorLYFuel_tab %>%
#     kable(escape = FALSE, digits = 0, longtable = TRUE,
#           align = c("llrrrrrrrr"),
#           caption = "Annual energy use (millions of BTU) by county and industry sector") %>%
#     add_header_above(c(" " = 2, "Fuel types" = 8)) %>%
#     kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                   latex_options = c("hold_position", "repeat_header"),
#                   position = "center",
#                   font_size = 11, fixed_thead = TRUE) %>%
#     column_spec(2, width = "8em")
# }
