# title: "GHGsByFuelYearplot.R"
# author: "Eric Koski"
# date: "1/4/2020"

GHGemissionsPerFuel <- GHGemissionsPerFuelYear %>%
  group_by(`Fuel Type`) %>%
  summarize(CO2e100mt = sum(CO2e100kg, na.rm = TRUE) / 1000) %>%
  ungroup() %>%
  arrange(CO2e100mt)

fuelTypesSorted <- GHGemissionsPerFuel$`Fuel Type`

GHGemissionsFractionsPerFuelByYear <- GHGemissionsPerFuelYear %>%
  group_by(Year) %>%
  mutate(across(CO2e100kg, ~(. / sum(.)))) %>%
  select(Year, `Fuel Type`, CO2e100kg)

initialCoalFraction <- (GHGemissionsFractionsPerFuelByYear %>%
                          filter(Year == earliestYear) %>%
                          filter(`Fuel Type` == "Coal"))$CO2e100kg
finalCoalFraction <- (GHGemissionsFractionsPerFuelByYear %>%
                          filter(Year == latestYear) %>%
                          filter(`Fuel Type` == "Coal"))$CO2e100kg

GHGemissionsPerFuelYearBounds <- GHGemissionsPerFuelYear %>%
  mutate(CO2e100mt = CO2e100kg / 1000) %>%
  pivot_wider(id_cols = Year,
              names_from = `Fuel Type`,
              values_from = CO2e100mt)

for (fuel in fuelTypesSorted) {
  if (fuel == fuelTypesSorted[1])
  {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    GHGemissionsPerFuelYearBounds <- 
      GHGemissionsPerFuelYearBounds %>%
      mutate(!!name1 := seq(0, 0, length.out = 
                              length(GHGemissionsPerFuelYearBounds[[fuel]])))
    lastFuel <- fuel
  } else {
    name1 <- as.symbol(str_c(fuel, "Lower"))
    name2 <- str_c(lastFuel, "Upper")
    GHGemissionsPerFuelYearBounds <- GHGemissionsPerFuelYearBounds %>%
      mutate(!!name1 := GHGemissionsPerFuelYearBounds[[name2]])
  }
  name1 <- as.symbol(str_c(fuel, "Upper"))
  name2 <- str_c(fuel, "Lower")
  GHGemissionsPerFuelYearBounds <- GHGemissionsPerFuelYearBounds %>%
    mutate(!!name1 := GHGemissionsPerFuelYearBounds[[name2]] + 
             GHGemissionsPerFuelYearBounds[[fuel]])
  lastFuel <- fuel
}

# Now, the plot

GHGemissionsPerFuelYearplot <- ggplot(GHGemissionsPerFuelYearBounds) +
  ylab(NULL) +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = c("#e6194B", "#000075", "#3cb44b", "#9A6324", 
                               "#4363d8", "#f58231", "#43d4f4", "#f032e6", 
                               "#469990", "#800000", "#ffe119", "#aaffc3", 
                               "#ffe119", "#e6beff"),
                    name = "Fuel type", 
                    # ... and order the legend keys the way we want them!
                    breaks = rev(str_replace_all(fuelTypesSorted, "_", " ")),
                    guide = guide_legend(override.aes = list(alpha = 0.6))) +
  scale_x_continuous(breaks = seq(earliestYear, latestYear, by=1)) +
  scale_y_continuous(labels = scientific_10, minor_breaks = seq(0, 5.0e6, by=1.0e5))

for (fuel in fuelTypesSorted) {
  GHGemissionsPerFuelYearplot <- GHGemissionsPerFuelYearplot +
    geom_ribbon(aes(Year, 
                    ymin = !!as.symbol(str_c(fuel, "Lower")), 
                    ymax = !!as.symbol(str_c(fuel, "Upper")), 
                    fill = !!quo_name(str_replace_all(fuel, "_", " "))), alpha = 0.6)
}

show(GHGemissionsPerFuelYearplot)
