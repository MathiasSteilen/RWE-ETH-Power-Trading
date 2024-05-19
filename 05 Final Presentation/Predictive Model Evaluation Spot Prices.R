library(tidyverse)
library(tidymodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

input_files = c(
  "../00 Data Retrieval and Cleaning/0_df_predictive_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_predictive_de_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_de_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_predictive_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_predictive_de_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_de_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_predictive_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_ch_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_predictive_de_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_theoretical_de_spot_price.csv",
  "../00 Data Retrieval and Cleaning/0_df_final_ml_predictive.csv"
)

output_folders = c(
  "../01 Task 1 - Spot Price/1 - BASELINE - Prediction - CH/",
  "../01 Task 1 - Spot Price/1 - BASELINE - Prediction - DE/",
  "../01 Task 1 - Spot Price/1 - BASELINE - Prediction - CH/",
  "../01 Task 1 - Spot Price/1 - BASELINE - Prediction - DE/",
  "../01 Task 1 - Spot Price/1 - XGBoost - Prediction - CH - Data Predictive/",
  "../01 Task 1 - Spot Price/1 - XGBoost - Prediction - CH - Data Theoretical/",
  "../01 Task 1 - Spot Price/1 - XGBoost - Prediction - DE - Data Predictive/",
  "../01 Task 1 - Spot Price/1 - XGBoost - Prediction - DE - Data Theoretical/",
  "../01 Task 1 - Spot Price/1 - NBEATS - Prediction - CH - Data Predictive/",
  "../01 Task 1 - Spot Price/1 - NBEATS - Prediction - CH - Data Theoretical/",
  "../01 Task 1 - Spot Price/1 - NBEATS - Prediction - DE - Data Predictive/",
  "../01 Task 1 - Spot Price/1 - NBEATS - Prediction - DE - Data Theoretical/",
  "../01 Task 1 - Spot Price/1 - NHITS - Prediction - CH - Data Predictive/"
)

data_type = c(
  "Predictive",
  "Predictive",
  "Theoretical",
  "Theoretical",
  "Predictive",
  "Theoretical",
  "Predictive",
  "Theoretical",
  "Predictive",
  "Theoretical",
  "Predictive",
  "Theoretical",
  "Predictive"
)

target_vars = c(
  "day_ahead_price_ch",
  "day_ahead_price_de",
  "day_ahead_price_ch",
  "day_ahead_price_de",
  "day_ahead_price_ch",
  "day_ahead_price_ch",
  "day_ahead_price_de",
  "day_ahead_price_de",
  "day_ahead_price_ch",
  "day_ahead_price_ch",
  "day_ahead_price_de",
  "day_ahead_price_de",
  "day_ahead_price_ch"
)

model = c(
  "Baseline",
  "Baseline",
  "Baseline",
  "Baseline",
  "XGBoost",
  "XGBoost",
  "XGBoost",
  "XGBoost",
  "NBEATS",
  "NBEATS",
  "NBEATS",
  "NBEATS",
  "NHITS"
)

# OOS Predictions ----

for (i in 1:length(input_files)){
  
  new_file = read_csv(paste0(output_folders[i], "holdout_predictions.csv")) |> 
    mutate(
      target = case_when(
        target_vars[i] == "day_ahead_price_ch" ~ "Spot CH",
        target_vars[i] == "day_ahead_price_de" ~ "Spot DE",
        TRUE ~ NA
      ),
      data_type = data_type[i],
      model = model[i]
    )
  
  if (i == 1){
    preds = new_file
  }
  else {
    preds = preds |> bind_rows(new_file)
  }
  
}

preds

## Time Series ----
preds |> 
  pivot_longer(c(.pred, actual)) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  facet_wrap(~ target + data_type + model, scales = "free", ncol = 4) +
  scale_colour_manual(values = c("firebrick", "grey50"))


## Tukey Anscombe ----
preds |> 
  mutate(res = actual - .pred) |> 
  ggplot(aes(.pred, res)) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ target + data_type + model, scales = "free", ncol = 4) +
  geom_smooth() 

## Metrics ----
eval_metrics = metric_set(rsq, rmse, mae, rmse)

preds |> 
  group_by(target, data_type, model) |>
  eval_metrics(truth = actual, estimate = .pred) |> 
  unique() |> 
  select(-c(.estimator)) |> 
  pivot_wider(names_from = .metric, values_from = .estimate)
