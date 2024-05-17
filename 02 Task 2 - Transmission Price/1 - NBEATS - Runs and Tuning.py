import pandas as pd
import polars as pl
import plotnine as pn
import numpy as np
from tqdm.notebook import tqdm

import neuralforecast
from neuralforecast import NeuralForecast
from neuralforecast.models import NBEATS, NBEATSx, NHITS
from neuralforecast.auto import AutoNBEATS, AutoNHITS
from neuralforecast.losses.pytorch import MQLoss, DistributionLoss, MSE, MAE
from neuralforecast.tsdataset import TimeSeriesDataset
from neuralforecast.utils import AirPassengers, AirPassengersPanel, AirPassengersStatic

from sklearn.metrics import mean_absolute_error, mean_squared_error
import optuna
import os

input_files = [
  "../00 Data Retrieval and Cleaning/0_df_predictive_chde_auction_price.csv",
    "../00 Data Retrieval and Cleaning/0_df_theoretical_chde_auction_price.csv",
    "../00 Data Retrieval and Cleaning/0_df_predictive_dech_auction_price.csv",
    "../00 Data Retrieval and Cleaning/0_df_theoretical_dech_auction_price.csv"
]

output_folders = [
  "./1 - NBEATS - Prediction - CHDE - Data Predictive/",
  "./1 - NBEATS - Prediction - CHDE - Data Theoretical/",
  "./1 - NBEATS - Prediction - DECH - Data Predictive/",
  "./1 - NBEATS - Prediction - DECH - Data Theoretical/"
]

target_vars = [
  "auction_price_ch_de",
  "auction_price_ch_de",
  "auction_price_de_ch",
  "auction_price_de_ch"
]

# Run NBeats Tuning and Store Results
for i in range(len(input_files)):
    df = (
        pd.read_csv(input_files[i], parse_dates=["date"],
        )
        .rename(columns={target_vars[i]: "y", "date": "ds"})
        .assign(unique_id=target_vars[i])
    )

    df = df.filter(["unique_id", "ds", "y"])

    train_end = pd.Timestamp("2023-09-01").tz_localize("UTC")
    val_end = pd.Timestamp("2024-01-01").tz_localize("UTC")

    # Create splits
    df_train = df.query("ds < @val_end")
    # df_val = df.query("ds < @val_end").query("ds >= @train_end").head(500)
    df_test = df.query("ds >= @val_end")

    # Set how long should be forecasted
    HORIZON_SIZE = 38

    def objective(trial):
        # Define the hyperparameters to tune
        forecast_window = HORIZON_SIZE
        lookback_window = trial.suggest_int("lookback_window", 24, 2*7*24)
        learning_rate = trial.suggest_loguniform("learning_rate", 1e-5, 1e-1)
        batch_size = trial.suggest_categorical("batch_size", [4, 8, 16, 32, 64, 128, 256])

        # Create the NBEATS model
        model = NBEATS(
            h=forecast_window,
            input_size=lookback_window,
            loss=MSE(),
            max_steps=500,
            val_check_steps=10,
            early_stop_patience_steps=3,
            learning_rate=learning_rate,
            batch_size=batch_size,
        )

        # Create neuralforecast object
        nf = NeuralForecast(models=[model], freq="h")
        nf.fit(df=df_train, val_size=VALIDATION_SIZE)

        # Make predictions on the validation set and get validation loss
        val_pred_dfs = []
        df_val = df_train.tail(VALIDATION_SIZE)
        pred_timestamps = df_val.query("ds.dt.hour == 10")["ds"]

        for ts in pred_timestamps:
            start_ts = ts - pd.Timedelta(lookback_window, unit="hours")
            end_ts = ts
            pred_day = ts.date() + pd.Timedelta(1, unit="days")
            df_pred = df_train.query("@start_ts <= ds <= @end_ts")

            y_hat = nf.predict(df_pred).reset_index()
            y_hat = (
                y_hat.query("ds.dt.date == @pred_day")
                .rename(columns={"NBEATS": ".pred", "ds": "date"})
                .drop(columns="unique_id")
            )
            print(y_hat.shape)
            val_pred_dfs.append(y_hat)

        val_pred_df = (
            pd.concat(val_pred_dfs)
            .merge(df_val, how="left", left_on="date", right_on="ds")
            .drop(columns=["unique_id", "ds"])
            .rename(columns={"y": ".actual"})
            .dropna()
        )
        val_loss = mean_absolute_error(val_pred_df[".actual"], val_pred_df[".pred"])

        return val_loss

    # Set three months aside for validation
    VALIDATION_SIZE = 92*24

    # Create the study object and optimize the objective function
    study = optuna.create_study(direction="minimize")
    study.optimize(objective, n_trials=None, timeout=60*1)

    # Get the best hyperparameters
    best_params = study.best_params
    best_loss = study.best_value

    # Check if output folder exists, if not create it
    if not os.path.exists(output_folders[i]):
        os.makedirs(output_folders[i])
    # save hyperparameter tuning results
    study.trials_dataframe().to_csv(f"{output_folders[i]}/tuning_results.csv", index=False)

    # Train the model with the best hyperparameters
    # Create the NBEATS model
    model = NBEATS(
        h=38,
        input_size=best_params["lookback_window"],
        loss=MSE(),
        max_steps=500,
        val_check_steps=10,
        early_stop_patience_steps=3,
        learning_rate=best_params["learning_rate"],
        batch_size=best_params["batch_size"],
    )

    # Create neuralforecast object
    nf = NeuralForecast(models=[model], freq="h")
    nf.fit(df=df_train, val_size=VALIDATION_SIZE)

    # Make predictions on the test set and write to csv
    # Same approach of using predictions at 10 in the morning
    # for the entire next day
    test_pred_dfs = []
    pred_timestamps = df_test.query("ds.dt.hour == 10")["ds"]

    for ts in pred_timestamps:
        start_ts = ts - pd.Timedelta(best_params["lookback_window"], unit="hours")
        end_ts = ts
        pred_day = ts.date() + pd.Timedelta(1, unit="days")
        df_pred = df.query("@start_ts <= ds <= @end_ts")

        y_hat = nf.predict(df_pred).reset_index()
        y_hat = (
            y_hat.query("ds.dt.date == @pred_day")
            .rename(columns={"NBEATS": ".pred", "ds": "date"})
            .drop(columns="unique_id")
        )
        test_pred_dfs.append(y_hat)

    test_pred_df = (
        pd.concat(test_pred_dfs)
        .merge(df, how="left", left_on="date", right_on="ds")
        .drop(columns=["unique_id", "ds"])
        .rename(columns={"y": ".actual"})
        .dropna()
    )

    test_pred_df.to_csv(f"{output_folders[i]}/test_predictions.csv", index=False)
