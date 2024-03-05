library(tidyverse)

# Hourly
read_csv("actual_load_CH.csv") |> 
  rename(actual_load_ch = actual_load)

# Quarter hourly aggregated to hourly
read_csv("actual_load_AT.csv") |> 
  rename(actual_load_at = actual_load) |> 
  mutate(date = floor_date(date, "1h")) |> 
  group_by(date) |> 
  summarise(actual_load_at = mean(actual_load_at, na.rm = T))

# Spread in Day-Ahead vs. Capacity Price
list.files()

# Replicating the plot in the slides
spot_ch = read_csv("day_ahead_prices_CH.csv")
spot_de = read_csv("day_ahead_prices_DE_LU.csv")
capacity_dech = read_csv("jao_prices_DE-CH.csv")

spot_ch |> 
  rename(price_ch = price) |> 
  left_join(
    spot_de |> 
      rename(price_de = price), by = "date"
  ) |> 
  mutate(spread_ch_de = price_ch - price_de) |> 
  select(-contains("price")) |> 
  left_join(
    capacity_dech |> 
      select(delivery_begin_time_ch, price) |> 
      rename(capacity_de_ch = price),
    by = c("date" = "delivery_begin_time_ch")
  ) |> 
  filter(date >= ymd("2024-01-01")) |> 
  pivot_longer(-date) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  labs(title = "Replicating the Chart on Slide 25", y = "EUR", x = "") +
  theme_bw()


