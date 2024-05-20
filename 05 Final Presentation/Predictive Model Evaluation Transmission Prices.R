library(tidyverse)
library(tidymodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

# Each Model needs
# "model": name the model
# "target": name the target
# "data_type": name the data type (theoretical, predictive)

preds = bind_rows(
  # Baselines  -----------------------------------------
  read_csv("../02 Task 2 - Transmission Price/1 - BASELINE - Prediction - CHDE/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - BASELINE - Prediction - CHDE/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - BASELINE - Prediction - DECH/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - BASELINE - Prediction - DECH/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "auction_price_de_ch",
      data_type = "Theoretical"
    ),
  
  ## FastTS models -----------------------------------------
  read_csv("../02 Task 2 - Transmission Price/1 - fastTS - Prediction - CHDE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - fastTS - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - fastTS - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - fastTS - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "auction_price_de_ch",
      data_type = "Theoretical"
    ),
  
  # LightGBM  -----------------------------------------
  read_csv("../02 Task 2 - Transmission Price/1 - LGBM - Prediction - CHDE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - LGBM - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - LGBM - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - LGBM - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "auction_price_de_ch",
      data_type = "Theoretical"
    ),
  
  # NBEATS  -----------------------------------------
  read_csv("../02 Task 2 - Transmission Price/1 - NBEATS - Prediction - CHDE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - NBEATS - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - NBEATS - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - NBEATS - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "auction_price_de_ch",
      data_type = "Theoretical"
    ),
  
  # XGBoost  -----------------------------------------
  read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - CHDE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "auction_price_de_ch",
      data_type = "Theoretical"
    )
)

preds

# Limit all preds at zero
preds = preds |> 
  mutate(.pred = ifelse(.pred < 0, 0, .pred))


## Metrics ----
eval_metrics = metric_set(rmse, mae, rsq)

preds |> 
  group_by(target, data_type, model) |>
  eval_metrics(truth = actual, estimate = .pred) |> 
  unique() |> 
  select(-c(.estimator)) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  arrange(data_type, target, rmse)

preds |> 
  group_by(target, data_type, model) |> 
  mutate(target = ifelse(str_detect(target, "ch_de"), "CH_DE", "DE_CH")) |> 
  eval_metrics(truth = actual, estimate = .pred) |> 
  ggplot(aes(target, .estimate, fill = model)) +
  geom_col(position = "dodge") +
  facet_wrap( ~ data_type + .metric, scales = "free")

## Time Series ----
preds |> 
  filter(target == "auction_price_ch_de") |> 
  filter(data_type == "Theoretical") |> 
  filter(model %in% c("LightGBM", "fastTS", "Baseline")) |> 
  pivot_longer(c(.pred, actual)) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  facet_wrap(~ model, scales = "free", ncol = 1) +
  scale_colour_manual(values = c("firebrick", "grey50"))


preds |> 
  pivot_longer(c(.pred, actual)) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  facet_wrap(~ target + data_type + model, scales = "free", ncol = 4) +
  scale_colour_manual(values = c("firebrick", "grey50"))


## Tukey Anscombe ----
preds |> 
  filter(target == "auction_price_ch_de") |> 
  filter(data_type == "Theoretical") |> 
  filter(model %in% c("LightGBM", "fastTS", "Baseline")) |> 
  mutate(res = actual - .pred) |> 
  ggplot(aes(.pred, res)) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ model, scales = "free", ncol = 4) +
  geom_smooth() 
  