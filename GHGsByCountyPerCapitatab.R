# title: "GHGsByCountyPerCapitatab.R"
# author: "Eric Koski"
# date: "1/4/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Now let's do the table
if (outputFormat == "pdf_document") {
  GHGsbyCountyPerCapita %>%
    kable(escape = FALSE, digits = 2, booktabs = TRUE, longtable = TRUE,
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
            "CO$_2$-equivalent agricultural/industrial GHG emissions per capita, metric tons")) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"),
                  position = "center",
                  font_size = 11)
} 

