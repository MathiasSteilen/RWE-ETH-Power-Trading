library(tidyverse)
library(tidymodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

input_files = c(
  "../00 Data Retrieval and Cleaning/0_df_predictive_chde_auction_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_chde_auction_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_predictive_dech_auction_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_dech_auction_price.csv"
)

output_folders = c(
  "./1 - XGBoost - Prediction - CHDE - Data Predictive/",
  "./1 - XGBoost - Prediction - CHDE - Data Theoretical/",
  "./1 - XGBoost - Prediction - DECH - Data Predictive/",
  "./1 - XGBoost - Prediction - DECH - Data Theoretical/"
)

target_vars = c(
  "auction_price_ch_de",
  "auction_price_ch_de",
  "auction_price_de_ch",
  "auction_price_de_ch"
)

# OOS Predictions ----

preds = bind_rows(
  read_csv(paste0(output_folders[1], "holdout_predictions.csv")) |> 
    mutate(
      country = "CHDE",
      data_type = "Predictive"
    ),
  read_csv(paste0(output_folders[2], "holdout_predictions.csv")) |> 
    mutate(
      country = "CHDE",
      data_type = "Theoretical"
    ),
  read_csv(paste0(output_folders[3], "holdout_predictions.csv")) |> 
    mutate(
      country = "DECH",
      data_type = "Predictive"
    ),
  read_csv(paste0(output_folders[4], "holdout_predictions.csv")) |> 
    mutate(
      country = "DECH",
      data_type = "Theoretical"
    )
)

# Fix values to zero at zero
preds = preds |> 
  mutate(.pred = ifelse(.pred < 0, 0, .pred))

## Time Series ----
preds |> 
  pivot_longer(c(.pred, actual)) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  facet_grid(country ~ data_type)


## Tukey Anscombe ----
preds |> 
  mutate(res = actual - .pred) |> 
  ggplot(aes(.pred, res)) +
  geom_point(alpha = 0.5) +
  facet_wrap(country ~ data_type, scales = "free") +
  geom_smooth()

## Metrics ----
eval_metrics = metric_set(rsq, rmse, mae, rmse)

preds |> 
  group_by(country, data_type) |> 
  eval_metrics(truth = actual, estimate = .pred) |> 
  ggplot(aes(data_type, .estimate)) +
  geom_col() +
  facet_wrap(country ~ .metric, scales = "free")
