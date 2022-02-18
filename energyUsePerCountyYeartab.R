# title: "energyUsePerCountyYeartab.R"
# author: "Eric Koski"
# date: "12/13/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# We share energyUseSummary with energyUsePerCountyYearplot.Rmd. It is already 
# grouped by County and Year. 
# pivot_wider so we have one column per year. 
energyUseSummaryWider <- pivot_wider(ungroup(energyUseSummary),
                                     names_from = Year, 
                                     values_from = MMBTU) %>%
  bind_rows(summarise_all(., ~(if(is.numeric(.)) sum(.) else "Totals"))) %>%
  mutate(sum = rowSums(select(., starts_with("20")))) %>%
  arrange(ifelse(Region == "New York State", desc(sum), sum)) %>%
  select(-sum)

# Convert numeric values to character, so we can use "," as a thousands separator. 
for(col in names(energyUseSummaryWider)) {
  if (is.numeric(energyUseSummaryWider[col][[1]])) {
    energyUseSummaryWider <- energyUseSummaryWider %>%
      mutate((!!as.symbol(col)) := formatC(as.integer(round(!!as.symbol(col))), 
                                           big.mark = ","))
  }
}

# Render the table
if (outputFormat == "pdf_document") {
  energyUseSummaryWider %>%
    kable(escape = FALSE, digits = 0, booktabs = TRUE, longtable = TRUE,
          align = c("l", rep.int("r", (latestYear - earliestYear) + 1)),
          linesep = 
            if(Region == "New York State") {
              c(rep.int(c(rep.int("", 4), "\\addlinespace"), (numberOfCounties - 1) %/% 5), "\\midrule")
            } else {
              c(rep.int("", numberOfCounties - 1), "\\midrule")
            },
          caption = tableCaption( 
            "Annual industrial and agricultural energy use (millions of BTU) by county")) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"),
                  position = "center",
                  font_size = 10)
}

