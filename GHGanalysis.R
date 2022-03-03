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

GHGemissionsLYindustryDetail <- GHGemissionsPerCountyFuelYear4dig %>%
  filter(YEAR == max(YEAR)) %>%
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

GHGemissionsLY_2dig <- GHGemissionsPerCountyFuelYear2dig %>%
  filter(YEAR == max(YEAR)) %>%
    group_by(NAICS2dig, NAICSname2dig) %>%
  summarize(CO2e100mt = sum(CO2e100kg) / 1000,
            .groups = "drop") %>%
  ungroup()

GHGemissionsLY_3dig <- GHGemissionsPerCountyFuelYear3dig %>%
  filter(YEAR == max(YEAR)) %>%
    group_by(NAICS3dig, NAICSname3dig) %>%
  summarize(CO2e100mt = sum(CO2e100kg) / 1000,
            .groups = "drop") %>%
  ungroup()

GHGemissionsSectorSummary <- GHGemissionsPerCountyFuelYear2dig %>%
  ungroup() %>%
  left_join(tibble(NAICS2dig = c(11, 21, 23, 31, 32, 33),
                   Sector = c("Agriculture", "Mining", "Construction", 
                              rep.int("Manufacturing", 3))), by = "NAICS2dig") %>%
  mutate(Year = YEAR) %>% 
  select(Sector, Year, County, CO2e100kg, -YEAR) %>% 
  group_by(Sector, Year, County) %>%
  summarize(CO2e100mt = sum(CO2e100kg / 1000)) %>%
  ungroup()

GHGemissionsSectorYearSummary <- GHGemissionsSectorSummary %>%
  group_by(Sector, Year) %>%
  summarize(CO2e100mt = sum(CO2e100mt)) %>%
  ungroup()

GHGemissionsLYSectorCountySummary <- GHGemissionsSectorSummary %>%
  filter(Year == max(Year)) 

GHGemissionsLYsectorSummary <- bind_rows(
  filter(GHGemissionsLY_3dig, NAICS3dig %in% c(111, 112)) %>%
    mutate(sector="Agriculture") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup(),
  filter(GHGemissionsLY_3dig, NAICS3dig %in% c(211, 212, 213)) %>%
    mutate(sector="Mining") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup(),
  filter(GHGemissionsLY_3dig, NAICS3dig %in% c(236, 237, 238)) %>%
    mutate(sector="Construction") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup(),
  filter(GHGemissionsLY_2dig, NAICS2dig %in% c(31, 32, 33)) %>%
    mutate(sector="Manufacturing") %>%
    group_by(sector) %>%
    summarize(CO2e100mt = sum(CO2e100mt)) %>%
    ungroup()) %>%
  mutate(percentage = 100 * CO2e100mt / sum(GHGemissionsLY_2dig$CO2e100mt))

sectorOrder <- GHGemissionsLYsectorSummary %>%
  arrange(CO2e100mt) %>%
  select(sector)

sectorOrder <- sectorOrder[[1]]

maxListedCounties <- 4

################# MANUFACTURING ##############################################

GHGemissionsManufacturingDetail <- GHGemissionsLYindustryDetail %>%
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
        1:(min(14 + ifelse(numberOfCounties < 4, 6,
                           ifelse(numberOfCounties < 10, 2, 0)), 
               nrow(filter(GHGemissionsManufacturingDetail, CO2eCumFract < 0.84))) + 1))

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
# browser() ###############
for (i in 1:nrow(GHGemissionsMfgDetailHighest)) {
  row <- slice(GHGemissionsMfgDetailHighest, i)
  countyGHGs <- select(row, one_of(countyNames))
  countyGHGs <- countyGHGs[,order(unlist(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsMfgDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsMfgDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    qtl <- 1 - min(1, maxListedCounties / length(unlist(GHGfractions)))
    thrsh <- min(0.1, (quantile(unlist(GHGfractions), probs = qtl) + 0.00001))
    GHGemissionsMfgDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= thrsh)], sep = ", ")
    GHGemissionsMfgDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = c(unlist(GHGfractions)[1:min(12, length(GHGfractions))], 0), 
                width=8, 
                bottomline = FALSE)
  }
}

################# AGRICULTURE ##############################################

GHGemissionsAgricultureDetail <- GHGemissionsLYindustryDetail %>%
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
  countyGHGs <- countyGHGs[,order(unlist(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsAgDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsAgDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    qtl <- 1 - min(1, maxListedCounties / length(unlist(GHGfractions)))
    thrsh <- min(0.1, (quantile(unlist(GHGfractions), probs = qtl) + 0.00001))
    GHGemissionsAgDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= thrsh)], sep = ", ")
    GHGemissionsAgDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = c(unlist(GHGfractions)[1:min(12, length(GHGfractions))], 0), 
                width=8, 
                bottomline = FALSE)
  }
}

################# CONSTRUCTION ##############################################

GHGemissionsConstructionDetail <- GHGemissionsLYindustryDetail %>%
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
  countyGHGs <- countyGHGs[,order(unlist(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsConstrDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsConstrDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    qtl <- 1 - min(1, maxListedCounties / length(unlist(GHGfractions)))
    thrsh <- min(0.1, (quantile(unlist(GHGfractions), probs = qtl) + 0.00001))
    GHGemissionsConstrDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= thrsh)], sep = ", ")
    GHGemissionsConstrDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = c(unlist(GHGfractions)[1:min(12, length(GHGfractions))], 0), 
                width=8, 
                bottomline = FALSE)
  }
}


################# MINING ##############################################

GHGemissionsMiningDetail <- GHGemissionsLYindustryDetail %>%
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
  countyGHGs <- countyGHGs[,order(unlist(-countyGHGs[1,]))]
  if (sum(countyGHGs) < 0.00001) {
    GHGemissionsMiningDetailHighest[i,]$HighestCounties <- ""
    GHGemissionsMiningDetailHighest[i,]$CountyDist <- ""
  } else {
    GHGfractions <- lapply(countyGHGs, function(x) (x/row$CO2e100mt))
    qtl <- 1 - min(1, maxListedCounties / length(unlist(GHGfractions)))
    thrsh <- min(0.1, (quantile(unlist(GHGfractions), probs = qtl) + 0.00001))
    GHGemissionsMiningDetailHighest[i,]$HighestCounties <- 
      glue_collapse(names(countyGHGs)[which(GHGfractions >= thrsh)], sep = ", ")
    GHGemissionsMiningDetailHighest[i,]$CountyDist <- 
      sparkline(yspikes = c(unlist(GHGfractions)[1:min(12, length(GHGfractions))], 0), 
                width=8, 
                bottomline = FALSE)
  }
}

