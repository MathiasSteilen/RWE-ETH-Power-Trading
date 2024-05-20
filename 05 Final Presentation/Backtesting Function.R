library(tidyverse)

backtest_predictions = function(
    jao_chde_predictions,
    jao_dech_predictions,
    de_predictions,
    ch_predictions
){
  
  preds = ch_predictions |> 
    rename(spot_ch = actual, spot_ch_pred = .pred) |> 
    left_join(
      de_predictions |>  
        rename(spot_de = actual, spot_de_pred = .pred) 
    ) |> 
    left_join(
      jao_chde_predictions |>  
        mutate(.pred = ifelse(.pred < 0, 0, .pred)) |> 
        rename(jao_chde = actual, jao_chde_pred = .pred) 
    ) |> 
    left_join(
      jao_dech_predictions |> 
        mutate(.pred = ifelse(.pred < 0, 0, .pred)) |> 
        rename(jao_dech = actual, jao_dech_pred = .pred) 
    )
  
  preds |> 
    mutate(
      # Check in which direction would want to trade
      direction = case_when(
        spot_ch_pred > spot_de_pred  ~ "DECH",
        spot_ch_pred < spot_de_pred  ~ "CHDE",
        TRUE ~ NA
      ),
      # Check how large forecasted spread is
      forecasted_spread = case_when(
        direction == "DECH" ~ spot_ch_pred - spot_de_pred,
        direction == "CHDE" ~ spot_de_pred - spot_ch_pred,
        TRUE ~ NA
      ),
      # Check if trade could happen (predicted spread > predicted jao price)
      trade_intention = case_when(
        direction == "DECH" ~ forecasted_spread > jao_dech_pred,
        direction == "CHDE" ~ forecasted_spread > jao_chde_pred,
        TRUE ~ NA
      )
    ) |> 
    # Filter for the ones where trade intention exists
    filter(trade_intention) |> 
    mutate(
      # If trade is thought possible, bid 1 ct more competitive than forecast
      own_bid_jao = case_when(
        direction == "DECH" ~ jao_dech_pred + 0.01,
        direction == "CHDE" ~ jao_chde_pred + 0.01,
        TRUE ~ NA
      ),
      # Check first if JAO is won
      jao_won = case_when(
        direction == "DECH" & own_bid_jao > jao_dech ~ TRUE,
        direction == "CHDE" & own_bid_jao > jao_chde ~ TRUE,
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
        direction == "DECH" ~ (spot_ch - spot_de) - jao_dech + 0.01,
        direction == "CHDE" ~ (spot_de - spot_ch) - jao_chde + 0.01,
        TRUE ~ FALSE
      )
    ) |> 
    # Calculate cumulative profit
    mutate(
      cum_profit = cumsum(profit)
    )
  
  
}
