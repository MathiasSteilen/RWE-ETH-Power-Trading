library(tidyverse)
library(tidymodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

# Each Model needs
# "model": name the model
# "target": name the target
# "data_type": name the data type (theoretical, predictive)

preds = bind_rows(
  # Baselines  -----------------------------------------
  read_csv("../01 Task 1 - Spot Price/1 - BASELINE - Prediction - CH/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - BASELINE - Prediction - CH/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - BASELINE - Prediction - DE/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - BASELINE - Prediction - DE/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "Baseline",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    ),
  
  ## FastTS models -----------------------------------------
  read_csv("../01 Task 1 - Spot Price/1 - fastTS - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - fastTS - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - fastTS - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - fastTS - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date = dmy_hm(date), .pred, actual,
      model = "fastTS",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    ),
  
  # LightGBM  -----------------------------------------
  read_csv("../01 Task 1 - Spot Price/1 - LGBM - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - LGBM - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - LGBM - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - LGBM - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "LightGBM",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    ),
  
  # NBEATS  -----------------------------------------
  read_csv("../01 Task 1 - Spot Price/1 - NBEATS - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - NBEATS - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - NBEATS - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - NBEATS - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "NBEATS",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    ),
  
  # ARIMA  -----------------------------------------
  read_csv("../01 Task 1 - Spot Price/1 - ARIMAX - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - ARIMAX - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - ARIMAX - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - ARIMAX - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    ),
  
  
  # XGBoost  -----------------------------------------
  read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "XGBoost",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    ),
  
  # GARCH  -----------------------------------------
  
  read_csv("../01 Task 1 - Spot Price/1 - GARCH - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    mutate(date = with_tz(date, tzone = "UTC")) %>% 
    mutate(actual = acutal) %>% 
    select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - GARCH - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    mutate(date = with_tz(date, tzone = "UTC")) %>% 
    mutate(actual = acutal) %>%
    select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - GARCH - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    mutate(date = with_tz(date, tzone = "UTC")) %>% 
    mutate(actual = acutal) %>%
    select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - GARCH - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    mutate(date = with_tz(date, tzone = "UTC")) %>% 
    mutate(actual = acutal) %>%
    select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    )
)


## Metrics ----
eval_metrics = metric_set(rmse, mae, rsq)

preds <- preds |> 
  group_by(target, data_type, model) |>
  eval_metrics(truth = actual, estimate = .pred) |> 
  unique() |> 
  select(-c(.estimator)) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  arrange(data_type, target, rmse)


write.csv(preds, "metricsPrices.csv")

## Time Series ----
preds |> 
  pivot_longer(c(.pred, actual)) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  facet_wrap(~ target + data_type + model, scales = "free", ncol = 4) +
  scale_colour_manual(values = c("firebrick", "grey50"))

preds |> 
  filter(target == "day_ahead_price_ch") |> 
  filter(model %in% c("LightGBM", "Baseline")) |> 
  filter(data_type == "Theoretical") |> 
  pivot_longer(c(.pred, actual)) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  facet_wrap(~ model, scales = "free", ncol = 4) +
  scale_colour_manual(values = c("firebrick", "grey50"))

fit$residuals


## Tukey Anscombe ----
preds |> 
  mutate(res = actual - .pred) |> 
  ggplot(aes(.pred, res)) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ target + data_type + model, scales = "free", ncol = 4) +
  geom_smooth() 






