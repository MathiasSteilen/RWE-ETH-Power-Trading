library(tidyverse)
library(tidymodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

input_files = c(
  "../00 Data Retrieval and Cleaning/0_df_predictive_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_predictive_de_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_de_spot_price.csv"
)

output_folders = c(
  "./1 - XGBoost - Prediction - CH - Data Predictive/",
  "./1 - XGBoost - Prediction - CH - Data Theoretical/",
  "./1 - XGBoost - Prediction - DE - Data Predictive/",
  "./1 - XGBoost - Prediction - DE - Data Theoretical/"
)

target_vars = c(
  "day_ahead_price_ch",
  "day_ahead_price_ch",
  "day_ahead_price_de",
  "day_ahead_price_de"
)

# OOS Predictions ----

preds = bind_rows(
  read_csv(paste0(output_folders[1], "holdout_predictions.csv")) |> 
    mutate(
      country = "CH",
      data_type = "Predictive"
    ),
  read_csv(paste0(output_folders[2], "holdout_predictions.csv")) |> 
    mutate(
      country = "DE",
      data_type = "Predictive"
    ),
  read_csv(paste0(output_folders[3], "holdout_predictions.csv")) |> 
    mutate(
      country = "CH",
      data_type = "Theoretical"
    ),
  read_csv(paste0(output_folders[4], "holdout_predictions.csv")) |> 
    mutate(
      country = "DE",
      data_type = "Theoretical"
    )
)

preds

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