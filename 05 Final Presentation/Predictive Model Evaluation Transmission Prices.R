library(tidyverse)
library(yardstick)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

# Each Model needs
# "model": name the model
# "target": name the target
# "data_type": name the data type (theoretical, predictive)

cut <- as.POSIXct("2024-01-31 00:00:00", tz = "UTC")
df_dates<-read_csv("../02 Task 2 - Transmission Price/1 - BASELINE - Prediction - CHDE/holdout_predictions.csv",)
correcterd_dates<-df_dates$date


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
      date, .pred, actual,
      model = "fastTS",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - fastTS - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 

    transmute(
      date, .pred, actual,
      model = "fastTS",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - fastTS - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 

    transmute(
      date, .pred, actual,
      model = "fastTS",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - fastTS - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 

    transmute(
      date, .pred, actual,
      model = "fastTS",
      target = "auction_price_de_ch",
      data_type = "Theoretical"
    ),
  
  ## ARIMA -----------------------------------------
  read_csv("../02 Task 2 - Transmission Price/1 - ARIMAX - Prediction - CHDE - Data Predictive/holdout_predictions.csv",) |> 

    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - ARIMAX - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 

    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - ARIMAX - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 

    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - ARIMAX - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 

    transmute(
      date, .pred, actual,
      model = "ARIMA",
      target = "auction_price_de_ch",
      data_type = "Theoretical"
    ),
  
  ## GARCH -----------------------------------------
  read_csv("../02 Task 2 - Transmission Price/1 - GARCH - Prediction - CHDE - Data Predictive/holdout_predictions.csv",) |> 

    mutate(date = correcterd_dates) %>%
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "auction_price_ch_de",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - GARCH - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 

    mutate(date = correcterd_dates) %>%
    transmute(
      date, .pred, actual,
      model = "GARCH",
      target = "auction_price_ch_de",
      data_type = "Theoretical"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - GARCH - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 
    mutate(date = correcterd_dates) %>%
    transmute(date, .pred, actual,
      model = "GARCH",
      target = "auction_price_de_ch",
      data_type = "Predictive"
    ),
  read_csv("../02 Task 2 - Transmission Price/1 - GARCH - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 
    mutate(date = correcterd_dates) %>%
    transmute(date, .pred, actual,
      model = "GARCH",
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


# Limit all preds at zero
preds = preds |> 
  mutate(.pred = ifelse(.pred < 0, 0, .pred))



## Metrics ----
eval_metrics = metric_set(rmse, mae, rsq)

metrics<- preds |> 
  group_by(target, data_type, model) |>
  eval_metrics(truth = actual, estimate = .pred) |> 
  unique() |> 
  select(-c(.estimator)) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  arrange(data_type, target, rmse)

write.csv(metrics, "auctionPrices_metrics.csv")

## timeseries plots ----

plot<-preds %>%
  filter(target == "auction_price_ch_de") %>%
  filter(data_type == "Theoretical") %>%
  pivot_longer(c(.pred, actual), names_to = "name", values_to = "value") %>%
  mutate(line_label = case_when(
    name == "actual" & model == "LightGBM" ~ "Actual",
    name == ".pred" & model == "fastTS" ~ "Predicted fastTS",
    name == ".pred" & model == "LightGBM" ~ "Predicted LightGBM",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = date, y = value, colour = line_label)) +
  geom_line(data = . %>% filter(name == "actual" & model == "LightGBM")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "fastTS")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "LightGBM")) +
  scale_colour_manual(values = c("Actual" = "black", "Predicted fastTS" = "firebrick", "Predicted LightGBM" = "gray50")) +
  labs(y = "Price level") +
  theme(legend.position = "top", legend.box = "horizontal") +
  guides(colour = guide_legend(title = NULL))  
ggsave(plot=plot, filename = "plots_resultSection/auction_price_ch_de - Theoretical.png", height=4, width = 8)


plot<-preds %>%
  filter(target == "auction_price_ch_de") %>%
  filter(data_type == "Predictive") %>%
  pivot_longer(c(.pred, actual), names_to = "name", values_to = "value") %>%
  mutate(line_label = case_when(
    name == "actual" & model == "LightGBM" ~ "Actual",
    name == ".pred" & model == "fastTS" ~ "Predicted fastTS",
    name == ".pred" & model == "LightGBM" ~ "Predicted LightGBM",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = date, y = value, colour = line_label)) +
  geom_line(data = . %>% filter(name == "actual" & model == "LightGBM")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "fastTS")) +
  geom_line(data = . %>% filter(name == ".pred" & model == "LightGBM")) +
  scale_colour_manual(values = c("Actual" = "black", "Predicted fastTS" = "firebrick", "Predicted LightGBM" = "gray50")) +
  labs(y = "Price level") +
  theme(legend.position = "top", legend.box = "horizontal") +
  guides(colour = guide_legend(title = NULL))  
ggsave(plot=plot, filename = "plots_resultSection/auction_price_ch_de - Predictive.png", height=4, width = 8)

plot<-preds %>%
  filter(target == "auction_price_de_ch") %>%
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

ggsave(plot=plot, filename = "plots_resultSection/auction_price_de_ch - Theoretical.png", height=4, width = 8)

plot<-preds %>%
  filter(target == "auction_price_de_ch") %>%
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

ggsave(plot=plot, filename = "plots_resultSection/auction_price_de_ch - Predictive.png", height=4, width = 8)
