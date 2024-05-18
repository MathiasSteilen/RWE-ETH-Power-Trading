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

data_type = c(
  "Predictive",
  "Theoretical",
  "Predictive",
  "Theoretical"
)

target_vars = c(
  "auction_price_ch_de",
  "auction_price_ch_de",
  "auction_price_de_ch",
  "auction_price_de_ch"
)

model = c(
  "XGBoost",
  "XGBoost",
  "XGBoost",
  "XGBoost"
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
  ggplot(aes(target, .estimate, fill = model)) +
  geom_col(position = "dodge") +
  facet_wrap( ~ data_type + .metric, scales = "free")
