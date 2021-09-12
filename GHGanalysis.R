# title: "GHGanalysis.Rmd"
# author: "Eric Koski"
# date: "1/4/2020"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# library(glue)
# library(ltxsparklines)

GHGemissions2016industryDetail <- GHGemissionsPerCountyFuelYear4dig %>%
  filter(YEAR == "2016") %>%
  group_by(County, NAICS4dig, NAICSname4dig) %>%
  summarize(CO2e100mt = sum(CO2e100kg) / 1000,
            .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(id_cols = c(NAICS4dig, NAICSname4dig),
              names_from = County, 
              values_from = CO2e100mt,
              values_fill = list(CO2e100mt = 0)) %>%
  mutate(CO2e100mt = rowSums(select(., -contains("NAICS")))) %>%
  mutate(NAICSname4dig = stri_replace_first_regex(NAICSname4dig, "T$", "")) %>%
  arrange(desc(CO2e100mt))

GHGemissions2016_2dig <- GHGemissionsPerCountyFuelYear2dig %>%
  filter(YEAR == "2016") %>%
    group_by(NAICS2dig, NAICSname2dig) %>%
  summarize(CO2e100mt = sum(CO2e100kg) / 1000,
            .groups = "drop") %>%
  ungroup()

GHGemissions2016_3dig <- GHGemissionsPerCountyFuelYear3dig %>%
  filter(YEAR == "2016") %>%
    group_by(NAICS3dig, NAICSname3dig) %>%
  summarize(CO2e100mt = sum(CO2e100kg) / 1000,
            .groups = "drop") %>%
  ungroup()

GHGemissionsSectorSummary <- GHGemissionsPerCountyFuelYear2dig %>%
  ungroup() %>%
  mutate(Sector = map_chr(NAICS2dig,    # not lapply() because we want a character vector, 
                         function(x)    # not a list
                           switch(as.character(x),
                                  "11" = "Agriculture",
                                  "21" = "Mining",
                                  "23" = "Construction",
                                  "31" = "Manufacturing",
                                  "32" = "Manufacturing",
                                  "33" = "Manufacturing"))) %>%
  mutate(Year = YEAR) %>% 
  select(Sector, Year, County, CO2e100kg, -YEAR) %>% 
  group_by(Sector, Year, County) %>%
  summarize(CO2e100mt = sum(CO2e100kg / 1000)) %>%
  ungroup()

GHGemissionsSectorYearSummary <- GHGemissionsSectorSummary %>%
  group_by(Sector, Year) %>%
  summarize(CO2e100mt = sum(CO2e100mt)) %>%
  ungroup()

GHGemissions2016SectorCountySummary <- GHGemissionsSectorSummary %>%
  filter(Year == 2016) 

GHGemissions2016sectorSummary <- bind_rows(
  filter(GHGemissions2016_3dig, NAICS3dig %in% c(111, 112)) %>%
    mutate(sector="Agriculture") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup(),
  filter(GHGemissions2016_3dig, NAICS3dig %in% c(211, 212, 213)) %>%
    mutate(sector="Mining") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup(),
  filter(GHGemissions2016_3dig, NAICS3dig %in% c(236, 237, 238)) %>%
    mutate(sector="Construction") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup(),
  filter(GHGemissions2016_2dig, NAICS2dig %in% c(31, 32, 33)) %>%
    mutate(sector="Manufacturing") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup()) %>%
  mutate(percentage = 100 * CO2e100mt / sum(GHGemissions2016_2dig$CO2e100mt))

sectorOrder <- GHGemissions2016sectorSummary %>%
  arrange(CO2e100mt) %>%
  select(sector)

sectorOrder <- sectorOrder[[1]]

################# MANUFACTURING ##############################################

GHGemissionsManufacturingDetail <- GHGemissions2016industryDetail %>%
  filter((NAICS4dig >= 3000) & (NAICS4dig < 4000)) %>%
  arrange(desc(CO2e100mt)) %>%
  mutate(CO2eCumFract = cumsum(CO2e100mt) / sum(CO2e100mt)) %>%
  select(contains("NAICS"), contains("CO2e"), everything())

MfgTotals <- NULL
for (n in countyNames) {
  MfgTotals[[n]] <- sum(GHGemissionsManufacturingDetail[[n]])
}
MfgTotals <- sort(unlist(MfgTotals), decreasing = TRUE)
GHGemissionsManufacturingDetail <- select(GHGemissionsManufacturingDetail, contains("NAICS"), contains("CO2e"), !!names(MfgTotals))

GHGemissionsMfgDetailHighest <- 
  slice(GHGemissionsManufacturingDetail, 
        1:(nrow(filter(GHGemissionsManufacturingDetail, CO2eCumFract < 0.86)) + 1))

MfgRemaining <- list("NAICS4dig" = 0, 
                     "NAICSname4dig" = "Remaining manufacturing categories",
                     "CO2e100mt" = sum(GHGemissionsManufacturingDetail$CO2e100mt) - 
                       sum(GHGemissionsMfgDetailHighest$CO2e100mt),
                     "CO2eCumFract" = 1.0)
for (n in names(MfgTotals)) {
  MfgRemaining[[n]] <- sum(GHGemissionsManufacturingDetail[[n]]) - 
    sum(GHGemissionsMfgDetailHighest[[n]])
}

GHGemissionsMfgDetailHighest <- bind_rows(GHGemissionsMfgDetailHighest, 
                                          MfgRemaining)

GHGemissionsMfgDetailHighest <- GHGemissionsMfgDetailHighest %>%
  mutate(CountyDist = "", HighestCounties = "")

for (i in 1:nrow(GHGemissionsMfgDetailHighest)) {
  row <- slice(GHGemissionsMfgDetailHighest, i)
  countyGHGs <- select(row, one_of(countyNames))
  countyGHGs <- countyGHGs[,order(as_vector(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsMfgDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsMfgDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    GHGemissionsMfgDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= 0.1)], sep = ", ")
    GHGemissionsMfgDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = GHGfractions, 
                width=8, 
                bottomline = FALSE)
  }
}

################# AGRICULTURE ##############################################

GHGemissionsAgricultureDetail <- GHGemissions2016industryDetail %>%
  filter((NAICS4dig >= 1000) & (NAICS4dig < 2000)) %>%
  arrange(desc(CO2e100mt)) %>%
  mutate(CO2eCumFract = cumsum(CO2e100mt) / sum(CO2e100mt)) %>%
  select(contains("NAICS"), contains("CO2e"), everything())

AgTotals <- NULL
for (n in countyNames) {
  AgTotals[[n]] <- sum(GHGemissionsAgricultureDetail[[n]])
}
AgTotals <- sort(unlist(AgTotals), decreasing = TRUE)
GHGemissionsAgricultureDetail <- select(GHGemissionsAgricultureDetail, contains("NAICS"), contains("CO2e"), !!names(AgTotals))

GHGemissionsAgDetailHighest <- 
  slice(GHGemissionsAgricultureDetail, 
        1:(nrow(filter(GHGemissionsAgricultureDetail, CO2eCumFract < 0.98)) + 1))

AgRemaining <- list("NAICS4dig" = 0, 
                     "NAICSname4dig" = "Remaining agriculture categories",
                     "CO2e100mt" = sum(GHGemissionsAgricultureDetail$CO2e100mt) - 
                       sum(GHGemissionsAgDetailHighest$CO2e100mt),
                     "CO2eCumFract" = 1.0)
for (n in names(AgTotals)) {
  AgRemaining[[n]] <- sum(GHGemissionsAgricultureDetail[[n]]) - 
    sum(GHGemissionsAgDetailHighest[[n]])
}

GHGemissionsAgDetailHighest <- bind_rows(GHGemissionsAgDetailHighest, 
                                         AgRemaining)

GHGemissionsAgDetailHighest <- GHGemissionsAgDetailHighest %>%
  mutate(CountyDist = "", HighestCounties = "")

for (i in 1:nrow(GHGemissionsAgDetailHighest)) {
  row <- slice(GHGemissionsAgDetailHighest, i)
  countyGHGs <- select(row, one_of(countyNames))
  countyGHGs <- countyGHGs[,order(as_vector(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsAgDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsAgDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    GHGemissionsAgDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= 0.1)], sep = ", ")
    GHGemissionsAgDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = GHGfractions, 
                width=8, 
                bottomline = FALSE)
  }
}

################# CONSTRUCTION ##############################################

GHGemissionsConstructionDetail <- GHGemissions2016industryDetail %>%
  filter((NAICS4dig >= 2300) & (NAICS4dig < 2400)) %>%
  arrange(desc(CO2e100mt)) %>%
  mutate(CO2eCumFract = cumsum(CO2e100mt) / sum(CO2e100mt)) %>%
  select(contains("NAICS"), contains("CO2e"), everything())

ConstrTotals <- NULL
for (n in countyNames) {
  ConstrTotals[[n]] <- sum(GHGemissionsConstructionDetail[[n]])
}
ConstrTotals <- sort(unlist(ConstrTotals), decreasing = TRUE)
GHGemissionsConstructionDetail <- select(GHGemissionsConstructionDetail, contains("NAICS"), contains("CO2e"), !!names(ConstrTotals))

GHGemissionsConstrDetailHighest <- 
  slice(GHGemissionsConstructionDetail, 
        1:(nrow(filter(GHGemissionsConstructionDetail, CO2eCumFract < 0.98)) + 1))

ConstrRemaining <- list("NAICS4dig" = 0, 
                     "NAICSname4dig" = "Remaining construction categories",
                     "CO2e100mt" = sum(GHGemissionsConstructionDetail$CO2e100mt) - 
                       sum(GHGemissionsConstrDetailHighest$CO2e100mt),
                     "CO2eCumFract" = 1.0)
for (n in names(ConstrTotals)) {
  ConstrRemaining[[n]] <- sum(GHGemissionsConstructionDetail[[n]]) - 
    sum(GHGemissionsConstrDetailHighest[[n]])
}

GHGemissionsConstrDetailHighest <- bind_rows(GHGemissionsConstrDetailHighest, 
                                         ConstrRemaining)

GHGemissionsConstrDetailHighest <- GHGemissionsConstrDetailHighest %>%
  mutate(CountyDist = "", HighestCounties = "")

for (i in 1:nrow(GHGemissionsConstrDetailHighest)) {
  row <- slice(GHGemissionsConstrDetailHighest, i)
  countyGHGs <- select(row, one_of(countyNames))
  countyGHGs <- countyGHGs[,order(as_vector(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsConstrDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsConstrDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    GHGemissionsConstrDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= 0.1)], sep = ", ")
    GHGemissionsConstrDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = GHGfractions, 
                width=8, 
                bottomline = FALSE)
  }
}


################# MINING ##############################################

GHGemissionsMiningDetail <- GHGemissions2016industryDetail %>%
  filter((NAICS4dig >= 2000) & (NAICS4dig < 2200)) %>%
  arrange(desc(CO2e100mt)) %>%
  mutate(CO2eCumFract = cumsum(CO2e100mt) / sum(CO2e100mt)) %>%
  select(contains("NAICS"), contains("CO2e"), everything())

MiningTotals <- NULL
for (n in countyNames) {
  MiningTotals[[n]] <- sum(GHGemissionsMiningDetail[[n]])
}
MiningTotals <- sort(unlist(MiningTotals), decreasing = TRUE)
GHGemissionsMiningDetail <- select(GHGemissionsMiningDetail, contains("NAICS"), contains("CO2e"), !!names(MiningTotals))

GHGemissionsMiningDetailHighest <- 
  slice(GHGemissionsMiningDetail, 
        1:(nrow(filter(GHGemissionsMiningDetail, CO2eCumFract < 0.98)) + 1))

MiningRemaining <- list("NAICS4dig" = 0, 
                     "NAICSname4dig" = "Remaining mining categories",
                     "CO2e100mt" = sum(GHGemissionsMiningDetail$CO2e100mt) - 
                       sum(GHGemissionsMiningDetailHighest$CO2e100mt),
                     "CO2eCumFract" = 1.0)
for (n in names(MiningTotals)) {
  MiningRemaining[[n]] <- sum(GHGemissionsMiningDetail[[n]]) - 
    sum(GHGemissionsMiningDetailHighest[[n]])
}

GHGemissionsMiningDetailHighest <- bind_rows(GHGemissionsMiningDetailHighest, 
                                          MiningRemaining)

GHGemissionsMiningDetailHighest <- GHGemissionsMiningDetailHighest %>%
  mutate(CountyDist = "", HighestCounties = "")

for (i in 1:nrow(GHGemissionsMiningDetailHighest)) {
  row <- slice(GHGemissionsMiningDetailHighest, i)
  countyGHGs <- select(row, one_of(countyNames))
  countyGHGs <- countyGHGs[,order(as_vector(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsMiningDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsMiningDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    GHGemissionsMiningDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= 0.1)], sep = ", ")
    GHGemissionsMiningDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = GHGfractions, 
                width=8, 
                bottomline = FALSE)
  }
}

