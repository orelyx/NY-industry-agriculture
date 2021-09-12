# title: "NAICSitems"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

X2017_NAICS_Descriptions <- suppressSnark(read_excel("2017-NAICS-Descriptions.xlsx"))

# Give the columns more meaningful names, and mark rows as native to distinguish them 
# from rows we will synthesize below. 
NAICS_Descriptions_2017 <- transmute(X2017_NAICS_Descriptions, 
                                     NAICS = as.integer(Code), 
                                     NAICSname = Title, 
                                     NAICSdescr = Description,
                                     origin = "native")
