# title: "NAICSitems"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

NAICSdesc = "https://www.census.gov/naics/2017NAICS/2017_NAICS_Descriptions.xlsx";

if (!file.exists("2017_NAICS_Descriptions.xlsx")) {
  download.file(NAICSdesc, "2017_NAICS_Descriptions.xlsx", mode = "wb")
}

X2017_NAICS_Descriptions <- suppressSnark(read_excel("2017_NAICS_Descriptions.xlsx"))

# Give the columns more meaningful names, and mark rows as native to distinguish them 
# from rows we will synthesize below. 
NAICS_Descriptions_2017 <- transmute(X2017_NAICS_Descriptions, 
                                     NAICS = suppressWarnings(ifelse(is.na(as.integer(Code)), 0, as.integer(Code))), 
                                     NAICSname = Title, 
                                     NAICSdescr = Description,
                                     origin = "native") %>%
  filter(NAICS > 0) %>%
  mutate(across(NAICSname, ~str_remove_all(., "T$")))
