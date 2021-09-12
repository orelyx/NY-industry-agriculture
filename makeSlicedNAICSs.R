# title: "makeSlicedNAICSs.R"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

NAICS_Descriptions_2017 <- NAICS_Descriptions_2017 %>% 
  filter(!is.na(NAICS)) %>% 
  bind_rows(NAICS_Addrows1dig) %>%
  bind_rows(NAICS_Addrows2dig) %>%
  bind_rows(NAICS_Addrows3dig) %>%
  bind_rows(NAICS_Addrows4dig) %>% 
  arrange(as.character(NAICS))

# Add columns with NAICS code values trimmed to max 1, 2, or 3 digits
NAICS_Descriptions_2017 <- NAICS_Descriptions_2017 %>% 
  mutate(NAICS1dig = trimInt(NAICS, 1)) %>%  
  mutate(NAICS2dig = trimInt(NAICS, 2)) %>% 
  mutate(NAICS3dig = trimInt(NAICS, 3)) %>% 
  mutate(NAICS4dig = trimInt(NAICS, 4)) 

# Make a table of the one-digit abbreviated NAICS codes
NAICS_Descriptions1dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS1dig) %>%
  transmute(NAICS1dig = NAICS1dig, NAICSname1dig = NAICSname, 
            NAICSdescr1dig = NAICSdescr) %>%
  arrange(NAICS1dig) %>%
  distinct()

# Make a table of the two-digit abbreviated NAICS codes
NAICS_Descriptions2dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS2dig) %>%
  filter(str_length(as.character(NAICS)) == 2) %>%
  transmute(NAICS2dig = NAICS2dig, NAICSname2dig = NAICSname, 
            NAICSdescr2dig = NAICSdescr) %>%
  arrange(NAICS2dig) %>%
  distinct()

# Make an analogous table of 3-digit abbreviated NAICS codes
NAICS_Descriptions3dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS3dig) %>%
  filter(str_length(as.character(NAICS)) == 3) %>%
  transmute(NAICS3dig = NAICS3dig, 
            NAICSname3dig = NAICSname, 
            NAICSdescr3dig = NAICSdescr) %>%
  arrange(NAICS3dig) %>%
  distinct()

# Make an analogous table of 4-digit abbreviated NAICS codes
NAICS_Descriptions4dig <- NAICS_Descriptions_2017 %>%
  filter(NAICS == NAICS4dig) %>%
  filter(str_length(as.character(NAICS)) == 4) %>%
  transmute(NAICS4dig = NAICS4dig, 
            NAICSname4dig = NAICSname, 
            NAICSdescr4dig = NAICSdescr) %>%
  arrange(NAICS4dig) %>%
  distinct()

