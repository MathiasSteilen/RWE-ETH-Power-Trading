library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

df = read_csv(
  "../../00 Data Retrieval and Cleaning/0_df_predictive_dech_auction_price.csv"
)

df |> 
  transmute(
    date,
    .pred = lag(auction_price_de_ch, n = 24),
    actual = auction_price_de_ch
  ) |> 
  filter(year(date) >= 2024) |> 
  write_csv("holdout_predictions.csv")
