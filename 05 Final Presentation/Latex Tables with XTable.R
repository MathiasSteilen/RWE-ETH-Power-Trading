library(tidyverse)
library(xtable)

tibble(
  "Data Type" = c(
    "Auction Price",
    "Transmission Capacity",
    "Day-Ahead Prices",
    "Load",
    "Wind/Solar Generation Forecast",
    "Generation (all types)",
    "Water Reservoirs",
    "Cross-Border Flows",
    "Cross-Border Capacity Forecast"
  ),
  "Source" = c(
    "JAO",
    "JAO",
    "ENTSOE",
    "ENTSOE",
    "ENTSOE",
    "ENTSOE",
    "ENTSOE",
    "ENTSOE",
    "ENTSOE"
  ),
  "Unit" = c(
    "EUR",
    "MW",
    "EUR",
    "EUR",
    "MW",
    "MW",
    "MWh",
    "MW",
    "MW"
  ),
  "Country/Direction" = c(
    "CH-DE and DE-CH",
    "CH-DE and DE-CH",
    "DE, CH, FR, IT, AT",
    "DE, CH, FR, IT, AT",
    "DE, CH, FR, IT, AT",
    "DE, CH, FR, IT, AT",
    "DE, CH, FR, IT, AT",
    "DE, CH, FR, IT, AT",
    "DE, CH, FR, IT, AT"
  )
) |> 
  xtable(caption = "Categories of data retrieved",
         label = "Test") |> 
  print.xtable(include.rownames = F)