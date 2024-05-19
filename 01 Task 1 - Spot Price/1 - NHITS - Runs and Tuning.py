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

from sklearn.model_selection import train_test_split, GridSearchCV, RandomizedSearchCV
from sklearn.impute import SimpleImputer
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer, KNNImputer
from sklearn.pipeline import FeatureUnion, make_pipeline, Pipeline
from sklearn.compose import ColumnTransformer, make_column_selector
from sklearn.preprocessing import (
    LabelEncoder,
    StandardScaler,
    OneHotEncoder,
    FunctionTransformer,
)
from sklearn.metrics import (
    r2_score,
    mean_absolute_error,
    mean_absolute_percentage_error,
    mean_squared_error,
)
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn import config_context
import optuna
import os
import holidays

df = pd.read_csv(
    "../00 Data Retrieval and Cleaning/0_df_final_ml_predictive.csv",
    parse_dates=["date"],
)

df = df.assign(
    day_ahead_price_de=lambda x: x["day_ahead_price_de"].shift(24),
    auction_price_de_ch=lambda x: x["auction_price_de_ch"].shift(24),
    auction_price_ch_de=lambda x: x["auction_price_ch_de"].shift(24),
)

df = df.assign(trend=lambda x: x.index, unique_id="spot_ch")


# Define the country (Switzerland)
country = "CH"

regional_holidays = holidays.CH(years=df.date.dt.year.unique().tolist())

holiday_df = pd.DataFrame(
    {
        "holiday_name": list(regional_holidays.values()),
        "holiday_date": list(regional_holidays.keys()),
    }
)

df = (
    df.assign(
        hour=lambda x: x.date.dt.hour + 1,
        month=lambda x: x.date.dt.month,
        quarter=lambda x: x.date.dt.quarter,
        wday=lambda x: x.date.dt.day_of_week + 1,
        weekend=lambda x: np.where(
            x.date.dt.day_name().isin(["Sunday", "Saturday"]), 1, 0
        ),
        work_hour=lambda x: np.where(
            x["hour"].isin(np.arange(17, 24).tolist() + np.arange(1, 5).tolist()), 0, 1
        ),
        week_hour=lambda x: x.date.dt.dayofweek * 24 + (x.date.dt.hour + 1),
        year=lambda x: x.date.dt.year,
        hour_counter=lambda x: np.arange(0, x.shape[0]),
    )
    .assign(day=lambda x: x.date.dt.date)
    .merge(holiday_df, how="left", left_on="day", right_on="holiday_date")
    .drop(["holiday_date", "day"], axis=1)
    .assign(
        holiday_name=lambda x: np.where(
            x["holiday_name"].isna(), "none", x["holiday_name"]
        )
    )
)


def sin_transformer(period):
    return FunctionTransformer(lambda x: np.sin(x / period * 2 * np.pi))


def cos_transformer(period):
    return FunctionTransformer(lambda x: np.cos(x / period * 2 * np.pi))


# hour in day
df["hour_sin"] = sin_transformer(24).fit_transform(df["hour"].astype(float))
df["hour_cos"] = cos_transformer(24).fit_transform(df["hour"].astype(float))

# hour in week
df["week_hour_sin"] = sin_transformer(168).fit_transform(df["week_hour"].astype(float))
df["week_hour_cos"] = cos_transformer(168).fit_transform(df["week_hour"].astype(float))

# month
df["month_sin"] = sin_transformer(12).fit_transform(df["month"].astype(float))
df["month_cos"] = cos_transformer(12).fit_transform(df["month"].astype(float))

# quarter
df["quarter_sin"] = sin_transformer(4).fit_transform(df["quarter"].astype(float))
df["quarter_cos"] = cos_transformer(4).fit_transform(df["quarter"].astype(float))

# weekday
df["wday_sin"] = sin_transformer(7).fit_transform(df["wday"].astype(float))
df["wday_cos"] = cos_transformer(7).fit_transform(df["wday"].astype(float))

df = df.drop(["hour", "month", "quarter", "wday", "week_hour"], axis=1)

manual_cols = ["trend", "unique_id"]
drop_cols = ["date", "day_ahead_price_ch"]

pipeline_cols = [
    col for col in df.drop(drop_cols, axis=1).columns if col not in manual_cols
]

num_cols = (
    df.drop(drop_cols, axis=1)
    .filter(pipeline_cols)
    .select_dtypes(include=np.number)
    .columns
)
cat_cols = (
    df.drop(drop_cols, axis=1)
    .filter(pipeline_cols)
    .select_dtypes(exclude=np.number)
    .columns
)

numeric_transformer = Pipeline(
    steps=[
        ("imputer", SimpleImputer()),
        #    ("scaler", StandardScaler())
    ]
)

categorical_transformer = Pipeline(
    steps=[
        ("imputer", SimpleImputer(strategy="most_frequent")),
        (
            "encoder",
            OneHotEncoder(sparse_output=False, handle_unknown="ignore"),
        ),
    ]
)

# Making column transformer where all transformers in the pipelines are included
preprocessor = ColumnTransformer(
    transformers=[
        ("numeric", numeric_transformer, num_cols),
        ("categorical", categorical_transformer, cat_cols),
    ],
    remainder="passthrough",
)


train_end = pd.Timestamp("2023-09-01").tz_localize("UTC")
val_end = pd.Timestamp("2024-01-01").tz_localize("UTC")

# Create splits
df_train = df.query("date < @val_end")
# df_val = df.query("date < @val_end").query("ds >= @train_end").head(500)
df_test = df.query("date >= @val_end")
X_train = df_train.drop(columns=["date", "day_ahead_price_ch"])
y_train = df_train["day_ahead_price_ch"]

X_test = df_test.drop(columns=["date", "day_ahead_price_ch"])
y_test = df_test["day_ahead_price_ch"]
fitted_preprocessor = preprocessor.fit(X_train)
X_train_preprocessed = pd.DataFrame(
    fitted_preprocessor.transform(X_train),
    columns=fitted_preprocessor.get_feature_names_out(),
)
X_test_preprocessed = pd.DataFrame(
    fitted_preprocessor.transform(X_test),
    columns=fitted_preprocessor.get_feature_names_out(),
)
# Replace prefixes in column names
new_cols = (
    X_train_preprocessed.columns.str.replace("numeric__", "")
    .str.replace("categorical__", "")
    .str.replace("remainder__", "")
)

# Assign new column names to the DataFrame
X_train_preprocessed.columns = new_cols
X_test_preprocessed.columns = new_cols

df_train = pd.concat(
    [
        df_train["date"].reset_index(drop=True),
        y_train.reset_index(drop=True),
        X_train_preprocessed.reset_index(drop=True),
    ],
    axis=1,
)

df_test = pd.concat(
    [
        df_test["date"].reset_index(drop=True),
        y_test.reset_index(drop=True),
        X_test_preprocessed.reset_index(drop=True),
    ],
    axis=1,
)

# Rename to NeuralForecast column convention
df_train = df_train.rename(columns={"day_ahead_price_ch": "y", "date": "ds"})
df_test = df_test.rename(columns={"day_ahead_price_ch": "y", "date": "ds"})

# Set how long should be forecasted
HORIZON_SIZE = 38


def objective(trial):
    # Define the hyperparameters to tune
    forecast_window = HORIZON_SIZE
    lookback_window = trial.suggest_int("lookback_window", 24, 2 * 7 * 24)
    learning_rate = trial.suggest_loguniform("learning_rate", 1e-5, 1e-1)
    batch_size = trial.suggest_categorical("batch_size", [4, 8, 16, 32, 64, 128, 256])

    # Create the model
    model = NHITS(
        h=38,
        input_size=lookback_window,
        loss=MSE(),
        scaler_type="standard",
        max_steps=500,
        val_check_steps=10,
        early_stop_patience_steps=4,
        learning_rate=learning_rate,
        batch_size=batch_size,
        futr_exog_list=df_train.drop(columns=["y", "unique_id", "ds"]).columns.tolist(),
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

        y_hat = nf.predict(futr_df=df_pred).reset_index()
        y_hat = (
            y_hat.query("ds.dt.date == @pred_day")
            .rename(columns={"NHITS": ".pred", "ds": "date"})
            .drop(columns="unique_id")
        )
        print(y_hat.shape)
        val_pred_dfs.append(y_hat)

    val_pred_df = (
        pd.concat(val_pred_dfs)
        .merge(df_val, how="left", left_on="date", right_on="ds")
        .drop(columns=["unique_id", "ds"])
        .rename(columns={"y": "actual"})
        .dropna()
    )
    val_loss = mean_squared_error(val_pred_df["actual"], val_pred_df[".pred"])

    return val_loss


# Set three months aside for validation
VALIDATION_SIZE = 92 * 24

# Create the study object and optimize the objective function
study = optuna.create_study(direction="minimize")
study.optimize(
    objective,
    n_trials=None,
    timeout=60 * 60 * 4
)

# Get the best hyperparameters
best_params = study.best_params
best_loss = study.best_value

# Check if output folder exists, if not create it
output_folder = "1 - NHITS - Prediction - CH - Data Predictive"
if not os.path.exists(output_folder):
    os.makedirs(output_folder)
# save hyperparameter tuning results
study.trials_dataframe().to_csv(f"{output_folder}/tuning_results.csv", index=False)

# Train the model with the best hyperparameters
# Create the model
model = NHITS(
    h=38,
    input_size=best_params["lookback_window"],
    loss=MSE(),
    max_steps=500,
    val_check_steps=5,
    early_stop_patience_steps=4,
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
pred_timestamps = df_test.query("ds.dt.hour == 10")["ds"].reset_index(drop=True)
df_full = pd.concat([df_train, df_test], axis=0).reset_index(drop=True).sort_values("ds")

for ts in pred_timestamps:
    start_ts = ts - pd.Timedelta(best_params["lookback_window"], unit="hours")
    end_ts = ts
    pred_day = ts.date() + pd.Timedelta(1, unit="days")
    df_pred = df_full.query("@start_ts <= ds <= @end_ts")
    
    y_hat = nf.predict(futr_df=df_pred).reset_index()
    y_hat = (
        y_hat.query("ds.dt.date == @pred_day")
        .rename(columns={"NHITS": ".pred"})
        .drop(columns="unique_id")
    )
    test_pred_dfs.append(y_hat)

test_pred_df = (
    pd.concat(test_pred_dfs)
    .merge(df_full[["ds", "y"]], how="left", left_on="ds", right_on="ds")
    .rename(columns={"y": "actual", "ds": "date"})
    .dropna()
)

test_pred_df.to_csv(f"{output_folder}/holdout_predictions.csv", index=False)
