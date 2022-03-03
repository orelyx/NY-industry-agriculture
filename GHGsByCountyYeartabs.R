# title: "GHGsByCountyYeartabs.R"
# author: "Eric Koski"
# date: "1/3/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Make a copy we can format for display. 
countiesGHGsummaryWidertab <- countiesGHGsummaryWider

# Convert numeric values to character, so we can use "," as a thousands separator. 
for(col in names(countiesGHGsummaryWidertab)) {
  if (is.numeric(countiesGHGsummaryWidertab[col][[1]])) {
    countiesGHGsummaryWidertab <- countiesGHGsummaryWidertab %>%
      mutate((!!as.symbol(col)) := formatC(as.integer(round(!!as.symbol(col))), 
                                           big.mark = ","))
  }
}

if (outputFormat == "pdf_document") {
  countiesGHGsummaryWidertab %>%
    kable(escape = FALSE, digits = 0, booktabs = TRUE, longtable = TRUE,
          align = c("lrrrrrrr"),
          linesep = 
            if(Region == "New York State") {
              c(rep.int(c(rep.int("", 4), "\\addlinespace"), (numberOfCounties) %/% 5), 
                rep.int("", (numberOfCounties - 1) %% 5), 
                "\\midrule")
            } else {
              c(rep.int("", numberOfCounties - 1), "\\midrule")
            },
          caption = tableCaption( 
            "Annual agricultural/industrial CO$_2$-equivalent emissions (metric tons) by county")) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"),
                  position = "center",
                  font_size = 10)
} 

