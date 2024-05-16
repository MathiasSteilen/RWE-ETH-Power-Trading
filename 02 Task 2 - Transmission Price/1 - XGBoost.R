library(tidyverse)
library(tidymodels)
library(doParallel)
library(ggsci)
library(scales)
library(vip)

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

target_vars = c(
  "auction_price_ch_de",
  "auction_price_ch_de",
  "auction_price_de_ch",
  "auction_price_de_ch",
)

for (i in 1:length(input_files)){
  
  # Read right data set
  df = read_csv(input_files[i])
  
  # train and test split
  dt_train = df |> 
    filter(date >= ymd("2023-01-01")) |> 
    filter(date < ymd("2024-01-01"))
  
  dt_test = df |> 
    filter(date >= ymd("2024-01-01"))
  
  # Time Series Split for tuning
  initial_split = 0.5
  increment = 0.1
  train_perc = 0.7
  
  initial_train = floor(nrow(dt_train)*(initial_split+increment)*train_perc)
  initial_test = round(floor(nrow(dt_train) * initial_split * (1-train_perc)))
  
  folds = rolling_origin(
    dt_train, 
    initial = initial_train, 
    assess = initial_test,
    skip = round(increment * nrow(dt_train)),
    cumulative = T
  )
  
  # Recipe for preprocessing
  xg_rec <- recipe(formula(paste(target_vars[i], "~ .")), data = dt_train) |>
    update_role(date, new_role = "ID") |>
    step_zv(all_predictors()) |> 
    step_impute_mean(all_predictors()) |> 
    step_center(all_predictors()) |> 
    step_scale(all_predictors())
  
  # Model specification
  xg_spec <- boost_tree(mtry = tune(),
                        trees = tune(),
                        min_n = tune(),
                        tree_depth = tune(),
                        learn_rate = tune()) |>
    set_engine("xgboost") |>
    set_mode("regression")
  
  xg_wflow <- workflow() |> 
    add_recipe(xg_rec) |> 
    add_model(xg_spec)
  
  # Setting grid for hyperparameter tuning
  xg_grid <- grid_latin_hypercube(
    finalize(mtry(), dt_train),
    trees(),
    min_n(),
    tree_depth(),
    learn_rate(),
    size = 250
  )
  
  # Run grid search
  start_time = Sys.time()
  xg_tune <- tune_grid(object = xg_wflow,
                       grid = xg_grid,
                       resamples = folds)
  end_time = Sys.time()
  print(end_time - start_time)
  
  # Write results to csv
  tuning_results = xg_tune |> 
    collect_metrics()
  
  tuning_results |> 
    write_csv(paste0(output_folders[i], "tuning_grid_results_split.csv"))
  
  # Fit the best model
  best_combination = tuning_results |> 
    filter(.metric == "rmse") |> 
    arrange(mean) |> 
    head(1)
  
  xg_spec <- boost_tree(
    mtry = best_combination$mtry,
    trees = best_combination$trees,
    min_n = best_combination$min_n,
    tree_depth = best_combination$tree_depth,
    learn_rate = best_combination$learn_rate,
  ) |>
    set_engine("xgboost") |>
    set_mode("regression")
  
  xg_wflow <- workflow() |> 
    add_recipe(xg_rec) |> 
    add_model(xg_spec)
  
  xg_final_fit <- xg_wflow |> 
    fit(dt_train)
  
  # Write variable importance
  xg_final_fit |>
    extract_fit_parsnip() |>
    vip::vi() |> 
    write_csv(paste0(output_folders[i], "variable_importance_split.csv"))
  
  # Write predictions
  xg_final_fit |> 
    augment(dt_test) |> 
    select(date, day_ahead_price_de, .pred) |> 
    write_csv(paste0(output_folders[i],"holdout_predictions_split.csv"))
}

