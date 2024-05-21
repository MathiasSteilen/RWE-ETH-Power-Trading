library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

df = read_csv(
  "../../00 Data Retrieval and Cleaning/0_df_predictive_ch_spot_price.csv"
)

df |> 
  transmute(
    date,
    .pred = lag(day_ahead_price_ch, n = 24),
    actual = day_ahead_price_ch
  ) |> 
  filter(year(date) >= 2024) |> 
  write_csv("holdout_predictions.csv")
