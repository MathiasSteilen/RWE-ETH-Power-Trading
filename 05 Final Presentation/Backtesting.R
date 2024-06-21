library(tidyverse)

# Read predictions ----

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
  # read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - CHDE - Data Predictive/holdout_predictions.csv",) |> 
  #   
  #   transmute(
  #     date, .pred, actual,
  #     model = "XGBoost",
  #     target = "auction_price_ch_de",
  #     data_type = "Predictive"
  #   ),
  # read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - CHDE - Data Theoretical/holdout_predictions.csv",) |> 
  #   
  #   transmute(
  #     date, .pred, actual,
  #     model = "XGBoost",
  #     target = "auction_price_ch_de",
  #     data_type = "Theoretical"
  #   ),
  # read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - DECH - Data Predictive/holdout_predictions.csv",) |> 
  #   
  #   transmute(
  #     date, .pred, actual,
  #     model = "XGBoost",
  #     target = "auction_price_de_ch",
  #     data_type = "Predictive"
  #   ),
  # read_csv("../02 Task 2 - Transmission Price/1 - XGBoost - Prediction - DECH - Data Theoretical/holdout_predictions.csv",) |> 
  #   
  #   transmute(
  #     date, .pred, actual,
  #     model = "XGBoost",
  #     target = "auction_price_de_ch",
  #     data_type = "Theoretical"
  #   )
) |> 
  bind_rows(
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
    # read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - CH - Data Predictive/holdout_predictions.csv",) |> 
    #   transmute(
    #     date, .pred, actual,
    #     model = "XGBoost",
    #     target = "day_ahead_price_ch",
    #     data_type = "Predictive"
    #   ),
    # read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - CH - Data Theoretical/holdout_predictions.csv",) |> 
    #   transmute(
    #     date, .pred, actual,
    #     model = "XGBoost",
    #     target = "day_ahead_price_ch",
    #     data_type = "Theoretical"
    #   ),
    # read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - DE - Data Predictive/holdout_predictions.csv",) |> 
    #   transmute(
    #     date, .pred, actual,
    #     model = "XGBoost",
    #     target = "day_ahead_price_de",
    #     data_type = "Predictive"
    #   ),
    # read_csv("../01 Task 1 - Spot Price/1 - XGBoost - Prediction - DE - Data Theoretical/holdout_predictions.csv",) |> 
    #   transmute(
    #     date, .pred, actual,
    #     model = "XGBoost",
    #     target = "day_ahead_price_de",
    #     data_type = "Theoretical"
    #   ),
    
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


backtest = preds |> 
  rename(pred = .pred) |> 
  pivot_longer(c(pred, actual), names_to = "value_type") |> 
  mutate(target = paste0(target, "_", value_type)) |> 
  select(-value_type) |> 
  pivot_wider(names_from = target, values_from = value) |> 
  arrange(data_type, model, date) |> 
  mutate(
    # Check in which direction would want to trade
    direction = case_when(
      day_ahead_price_ch_pred > day_ahead_price_de_pred  ~ "DECH",
      day_ahead_price_de_pred > day_ahead_price_ch_pred  ~ "CHDE",
      TRUE ~ NA
    ),
    # Check how large forecasted spread is
    forecasted_spread = case_when(
      direction == "DECH" ~ day_ahead_price_ch_pred - day_ahead_price_de_pred,
      direction == "CHDE" ~ day_ahead_price_de_pred - day_ahead_price_ch_pred,
      TRUE ~ NA
    ),
    # Check if trade could happen (predicted spread > predicted jao price)
    trade_intention = case_when(
      direction == "DECH" ~ forecasted_spread > auction_price_de_ch_pred,
      direction == "CHDE" ~ forecasted_spread > auction_price_ch_de_pred,
      TRUE ~ NA
    )
  ) |> 
  # Filter for the ones where trade intention exists
  filter(trade_intention) |> 
  mutate(
    # If trade is thought possible, bid 1 ct more competitive than forecast
    own_bid_jao = case_when(
      direction == "DECH" ~ auction_price_de_ch_pred + 0.01,
      direction == "CHDE" ~ auction_price_ch_de_pred + 0.01,
      TRUE ~ NA
    ),
    # Check first if JAO is won
    jao_won = case_when(
      direction == "DECH" & own_bid_jao > auction_price_de_ch_actual ~ TRUE,
      direction == "CHDE" & own_bid_jao > auction_price_ch_de_actual ~ TRUE,
      TRUE ~ FALSE
    )
  ) |> 
  # filter for the events where jao is won, else no bids in spot markets
  filter(jao_won) |> 
  # Bid for spot markets ISSUE
  # Big assumption: PROBLEM: can't make a smart bid here, because if you lose
  # out in the markets, need to buy in intraday, don't have data for that
  # hence assume we submit worst bids accepting every price
  # ALTERNATIVE: execute trade at actual prices (assume 1 MWh volume always)
  mutate(
    profit = case_when(
      direction == "DECH" ~ 
        (day_ahead_price_ch_actual - day_ahead_price_de_actual) -
        (own_bid_jao + 0.01),
      direction == "CHDE" ~ 
        (day_ahead_price_de_actual - day_ahead_price_ch_actual) -
        (own_bid_jao + 0.01),
      TRUE ~ FALSE
    )
  ) |> 
  group_by(model, data_type) |> 
  arrange(date) |> 
  # Calculate cumulative profit
  mutate(
    cum_profit = cumsum(profit)
  ) |> 
  ungroup()

backtest |> 
  ggplot(aes(date, cum_profit, colour = model)) +
  geom_line() +
  facet_wrap(~ data_type, ncol = 1) +
  ggsci::scale_colour_jama() +
  labs(title = "Out-of-sample trading simulation",
       y = "Cumulative Profit (EUR)") +
  theme(axis.title.x = element_blank())

ggsave(
  filename = "./plots_backtest/Backtest.png",
  dpi = 250,
  width = 6,
  height = 6
)

# Debugging of backtest ----

# Evaluation metric performance vs profit performance


# Did models trade in one direction particulary because they over-
# or underestimated something?
read_csv("../00 Data Retrieval and Cleaning/0_df_final_ml_theoretical.csv") |> 
  select(date, auction_price_ch_de, auction_price_de_ch, day_ahead_price_de,
         day_ahead_price_ch) |> 
  mutate(
    de_ch_spread = day_ahead_price_ch - day_ahead_price_de,
    ch_de_spread = day_ahead_price_de - day_ahead_price_ch
  ) |> 
  # select(date, contains("spread")) |> 
  # pivot_longer(-date) |> 
  ggplot(aes(de_ch_spread, y = after_stat(density))) +
  geom_histogram(fill = "dodgerblue", bins = 250) +
  geom_vline(xintercept = 0, lty = "dotted") +
  coord_cartesian(xlim = c(-200, 200)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Spread: CH Spot - DE Spot",
       x = "Spread (CH - DE)",
       y = "Density") +
  theme(legend.title = element_blank())

ggsave(
  filename = "./plots_backtest/Spread.png",
  dpi = 250,
  width = 5,
  height = 3
)

# read_csv("../00 Data Retrieval and Cleaning/0_df_final_ml_theoretical.csv") |> 
#   select(date, auction_price_ch_de, auction_price_de_ch, day_ahead_price_de,
#          day_ahead_price_ch) |>  
#   pivot_longer(-date) |> 
#   mutate(
#     country = case_when(
#       name == "auction_price_ch_de" ~ "CH_DE",
#       name == "auction_price_de_ch" ~ "DE_CH",
#       name == "day_ahead_price_de" ~ "DE",
#       name == "day_ahead_price_ch" ~ "CH",
#     ),
#     variable = case_when(
#       name == "auction_price_ch_de" ~ "JAO",
#       name == "auction_price_de_ch" ~ "JAO",
#       name == "day_ahead_price_de" ~ "SPOT",
#       name == "day_ahead_price_ch" ~ "SPOT",
#     ),
#     date = floor_date(date, "1d")
#   ) |> 
#   group_by(date, country, variable) |> 
#   summarise(value = mean(value, na.rm = T)) |> 
#   ungroup() |> 
#   ggplot(aes(date, value, colour = country)) +
#   geom_line() +
#   facet_wrap(~ variable, scales = "free_y", ncol = 1)

# Predicted vs actual JAO prices
preds |> 
  filter(data_type == "Theoretical") |> 
  mutate(date = floor_date(date, unit = "1d")) |> 
  group_by(date, model, target, data_type) |> 
  summarise(across(c(.pred, actual), mean)) |> 
  ungroup() |> 
  pivot_longer(c(.pred, actual)) |> 
  distinct() |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  facet_wrap(~  model + target + data_type, scales = "free_y", ncol = 4)

# Directional bias
backtest |> 
  count(data_type, direction, model) |> 
  pivot_wider(names_from = direction, values_from = n) |> 
  mutate(per = DECH/(CHDE + DECH)) |> 
  ggplot(aes(per, reorder(model, per))) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Directional Tendencies of Models",
       y = "", x = "Share of Times Trading DE to CH") +
  facet_wrap(~ data_type) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1))

ggsave(
  filename = "./plots_backtest/Directions.png",
  dpi = 250,
  width = 5,
  height = 3
)


# Relation of forecast accuracy with backtest performance
backtest |> 
  # filter(data_type == "Theoretical") |> 
  group_by(model, data_type, direction) |> 
  summarise(
    profit_min = min(profit),
    profit = sum(profit),
    profit_max = max(profit),
    
  ) |> 
  ungroup() |> 
  left_join(
    preds |> 
      group_by(model, data_type) |> 
      summarise(
        # metric = mean(abs(.pred - actual))/mean(actual)
        metric = sqrt(mean((actual - .pred)^2))
      ),
    by = c("model", "data_type")
  ) |> 
  ggplot(aes(metric, profit)) +
  geom_point(aes(colour = model)) +
  geom_smooth(method = "lm", se = F, ) +
  geom_hline(yintercept = 0, lty = "dotted") +
  labs(title = "Relation of Loss Metric to Backtest Performance",
       x = "RMSE across all target variables", y = "Profit (EUR)") +
  facet_wrap(~ data_type, ncol = 1) +
  theme(legend.title = element_blank())

ggsave(
  filename = "./plots_backtest/Metric vs Profit.png",
  dpi = 250,
  width = 6,
  height = 6
)

# Profits from directional trading
backtest |> 
  mutate(date = floor_date(date, unit = "1d")) |> 
  group_by(date, model, data_type, direction) |> 
  summarise(profit = sum(profit)) |>
  ungroup() |> 
  mutate(col = ifelse(profit > 0, "2", "1")) |> 
  ggplot(aes(date, profit, fill = col)) +
  geom_col() +
  labs(y = "Daily Profit (EUR)") +
  scale_fill_manual(values = c("firebrick", "forestgreen")) +
  facet_wrap(~ model + direction + data_type, ncol = 4) +
  theme(legend.position = "none",
        axis.title.x = element_blank())
