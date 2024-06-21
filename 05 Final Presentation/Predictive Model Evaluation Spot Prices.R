library(tidyverse)
library(yardstick)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
theme_set(
  theme_light() +
    theme(
      plot.title = element_text(face="bold", size=14),
      plot.subtitle = element_text(size = 10, colour = "grey50")
    )
)

# Each Model needs
# "model": name the model
# "target": name the target
# "data_type": name the data type (theoretical, predictive)

df_dates<-read_csv("../01 Task 1 - Spot Price/1 - BASELINE - Prediction - CH/holdout_predictions.csv",)
correcterd_dates<-df_dates$date

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
      date, .pred, actual,
      model = "fastTS",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - fastTS - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "fastTS",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - fastTS - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
      model = "fastTS",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - fastTS - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    transmute(
      date, .pred, actual,
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
    mutate(date = correcterd_dates) %>%
    # mutate(actual = acutal) %>% 
    # select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_ch",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - GARCH - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    mutate(date = correcterd_dates) %>%
    # mutate(actual = acutal) %>%
    # select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_ch",
      data_type = "Theoretical"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - GARCH - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    mutate(date = correcterd_dates) %>%
    # mutate(actual = acutal) %>%
    # select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_de",
      data_type = "Predictive"
    ),
  read_csv("../01 Task 1 - Spot Price/1 - GARCH - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    mutate(date = correcterd_dates) %>%
    # mutate(actual = acutal) %>%
    # select(-"acutal") %>% 
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "day_ahead_price_de",
      data_type = "Theoretical"
    )
)


## Metrics ----
eval_metrics <- metric_set(rmse, mae, rsq)

metrics <- preds |> 
  group_by(target, data_type, model) |>
  eval_metrics(truth = actual, estimate = .pred) |> 
  unique() |> 
  select(-c(.estimator)) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  arrange(data_type, target, rmse)

write.csv(metrics, "metricsPrices.csv")

# Calculate the spreads
price_spread <- preds %>%
  filter(target %in% c("day_ahead_price_ch", "day_ahead_price_de")) %>%
  pivot_wider(names_from = target, values_from = c(.pred, actual)) %>%
  mutate(
    pred_price_spread = .pred_day_ahead_price_ch - .pred_day_ahead_price_de,
    actual_price_spread = actual_day_ahead_price_ch - actual_day_ahead_price_de
  ) %>% select(-c(".pred_day_ahead_price_ch", ".pred_day_ahead_price_de", 
                  "actual_day_ahead_price_ch", "actual_day_ahead_price_de"))

metrics_price <- price_spread |> 
  group_by(data_type, model) |>
  eval_metrics(truth = actual_price_spread, estimate = pred_price_spread) |> 
  unique() |> 
  select(-c(.estimator)) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  arrange(data_type, target, rmse)

write.csv(metrics_price, "metricsSpread_prices.csv")


## Time Series ----
# preds |> 
#   pivot_longer(c(.pred, actual)) |> 
#   ggplot(aes(date, value, colour = name)) +
#   geom_line() +
#   facet_wrap(~ target + data_type + model, scales = "free", ncol = 4) +
#   scale_colour_manual(values = c("firebrick", "grey50"))


plot<-preds %>%
  filter(target == "day_ahead_price_ch") %>%
  filter(data_type == "Theoretical") %>%
  pivot_longer(c(.pred, actual), names_to = "name", values_to = "value") %>%
  mutate(line_label = case_when(
    name == "actual" & model == "LightGBM" ~ "Actual",
    name == ".pred" & model == "ARIMA" ~ "Predicted ARMA",
    name == ".pred" & model == "LightGBM" ~ "Predicted LightGBM",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = date, y = value, colour = line_label)) +
  geom_line(data = . %>% filter(name == "actual" & model == "LightGBM")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "ARIMA")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "LightGBM")) +
  scale_colour_manual(values = c("Actual" = "black", "Predicted ARMA" = "firebrick", "Predicted LightGBM" = "gray50")) +
  labs(y = "Price level") +
  theme(legend.position = "top", legend.box = "horizontal") +
  guides(colour = guide_legend(title = NULL))  
ggsave(plot=plot, filename = "plots_resultSection/day_ahead_price_ch - Theoretical.png", height=4, width = 8)

plot<-preds %>%
  filter(target == "day_ahead_price_ch") %>%
  filter(data_type == "Predictive") %>%
  pivot_longer(c(.pred, actual), names_to = "name", values_to = "value") %>%
  mutate(line_label = case_when(
    name == "actual" & model == "LightGBM" ~ "Actual",
    name == ".pred" & model == "ARIMA" ~ "Predicted ARMA",
    name == ".pred" & model == "LightGBM" ~ "Predicted LightGBM",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = date, y = value, colour = line_label)) +
  geom_line(data = . %>% filter(name == "actual" & model == "LightGBM")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "ARIMA")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "LightGBM")) +
  scale_colour_manual(values = c("Actual" = "black", "Predicted ARMA" = "firebrick", "Predicted LightGBM" = "gray50")) +
  labs(y = "Price level") +
  theme(legend.position = "top", legend.box = "horizontal") +
  guides(colour = guide_legend(title = NULL))  
ggsave(plot=plot, filename = "plots_resultSection/day_ahead_price_ch - Predictive.png", height=4, width = 8)


plot<-preds %>%
  filter(target == "day_ahead_price_de") %>%
  filter(data_type == "Theoretical") %>%
  pivot_longer(c(.pred, actual), names_to = "name", values_to = "value") %>%
  mutate(line_label = case_when(
    name == "actual" & model == "LightGBM" ~ "Actual",
    name == ".pred" & model == "GARCH" ~ "Predicted GARCH",
    name == ".pred" & model == "LightGBM" ~ "Predicted LightGBM",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = date, y = value, colour = line_label)) +
  geom_line(data = . %>% filter(name == "actual" & model == "LightGBM")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "GARCH")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "LightGBM")) +
  scale_colour_manual(values = c("Actual" = "black", "Predicted GARCH" = "firebrick", "Predicted LightGBM" = "gray50")) +
  labs(y = "Price level") +
  theme(legend.position = "top", legend.box = "horizontal") +
  guides(colour = guide_legend(title = NULL))  
ggsave(plot=plot, filename = "plots_resultSection/day_ahead_price_de - Theoretical.png", height=4, width = 8)

plot<-preds %>%
  filter(target == "day_ahead_price_de") %>%
  filter(data_type == "Predictive") %>%
  pivot_longer(c(.pred, actual), names_to = "name", values_to = "value") %>%
  mutate(line_label = case_when(
    name == "actual" & model == "LightGBM" ~ "Actual",
    name == ".pred" & model == "GARCH" ~ "Predicted GARCH",
    name == ".pred" & model == "LightGBM" ~ "Predicted LightGBM",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = date, y = value, colour = line_label)) +
  geom_line(data = . %>% filter(name == "actual" & model == "LightGBM")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "GARCH")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "LightGBM")) +
  scale_colour_manual(values = c("Actual" = "black", "Predicted GARCH" = "firebrick", "Predicted LightGBM" = "gray50")) +
  labs(y = "Price level") +
  theme(legend.position = "top", legend.box = "horizontal") +
  guides(colour = guide_legend(title = NULL))  
ggsave(plot=plot, filename = "plots_resultSection/day_ahead_price_de - Predictive.png", height=4, width = 8)






## Tukey Anscombe ----
preds |> 
  mutate(res = actual - .pred) |> 
  ggplot(aes(.pred, res)) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ target + data_type + model, scales = "free", ncol = 4) +
  geom_smooth() 


## Evaluation Plots by our in day ----
data_types = targets = preds |> 
  distinct(data_type) |> 
  pull()

targets = preds |> 
  distinct(target) |> 
  pull()

for (dt in data_types){
  for (t in targets){
    
    p = preds |> 
      mutate(hour = hour(date)) |> 
      filter(data_type == dt) |> 
      filter(target == t) |> 
      filter(! model %in% c("XGBoost")) |>  
      group_by(model, hour) |> 
      # MAPE to account for different absolute levels of target by hour
      summarise(metric = mean(abs(.pred - actual))/mean(actual)) |> 
      # summarise(metric = mean(abs(.pred - actual))/mean(actual)) |> 
      ggplot(aes(hour, metric, colour = model)) +
      geom_line() +
      labs(title = paste("Out-of-sample losses by hour in day"),
           subtitle = paste("Target:", t, "|", "Data Type:", dt, "data"),
           y = "MAPE",
           x = "Hour in Day") +
      scale_y_continuous(labels = percent_format()) +
      ggsci::scale_color_jama() +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())
    
    print(p)
    ggsave(
      plot = p,
      filename = paste0("./plots_hourinday/", t, "_", dt, ".png"), 
      height = 3, 
      width = 6,
      dpi = 250
    )
    
  }
}
