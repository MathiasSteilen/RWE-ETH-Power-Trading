import pandas as pd
import polars as pl
import plotnine as pn
import numpy as np
from tqdm.notebook import tqdm

import lightgbm as lgb
from sklearn.linear_model import Lasso, ElasticNet, Ridge
from sklearn.model_selection import TimeSeriesSplit
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
    root_mean_squared_error,
)
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn import config_context
import optuna
from optuna.visualization import (
    plot_optimization_history,
    plot_param_importances,
    plot_parallel_coordinate,
)
import os
import holidays


input_files = [
    "../00 Data Retrieval and Cleaning/0_df_final_ml_predictive.csv",
    "../00 Data Retrieval and Cleaning/0_df_final_ml_theoretical.csv",
    "../00 Data Retrieval and Cleaning/0_df_final_ml_predictive.csv",
    "../00 Data Retrieval and Cleaning/0_df_final_ml_theoretical.csv",
]

output_folders = [
    "./1 - LGBM - Prediction - CHDE - Data Predictive/",
    "./1 - LGBM - Prediction - CHDE - Data Theoretical/",
    "./1 - LGBM - Prediction - DECH - Data Predictive/",
    "./1 - LGBM - Prediction - DECH - Data Theoretical/",
]

target_vars = [
    "auction_price_ch_de",
    "auction_price_ch_de",
    "auction_price_de_ch",
    "auction_price_de_ch",
]


# Run LGBM Training and Store Results
# Not running tuning anymore for time reasons
for i in range(len(input_files)):

    df = pd.read_csv(input_files[i], parse_dates=["date"])

    targets_to_lag = [
        "day_ahead_price_ch",
        "day_ahead_price_de",
        "auction_price_ch_de",
        "auction_price_de_ch",
    ]

    targets_to_lag = [var for var in targets_to_lag if var != target_vars[i]]

    # Need to lag the other three target variables by 24 hours, haven't done that yet:
    for var in targets_to_lag:
        df[var] = df[var].shift(24)

    # ### Include target lags
    #
    # - latest available price (last price of previous day)
    # - last two week prices at exact hour (14 additional cols)

    # Adding last available price from previous day
    df["day"] = df.date.dt.date
    df[f"{target_vars[i]}_last"] = (
        df.groupby("day")[target_vars[i]].transform("last").shift(24)
    )
    df = df.drop("day", axis=1)

    # Adding lags in 24h steps for the last two weeks
    lags = np.arange(24, 49).tolist() + (np.arange(3, 15) * 24).tolist()
    print(lags)
    for l in lags:
        df[f"{target_vars[i]}_lag_{l}"] = df[target_vars[i]].shift(l)

    # ### Additional Feature Generation
    #
    # - There might be a benefit of encoding cyclical calendar information
    # - Additionally: Holidays
    # Include a trend column:
    df = df.assign(trend=lambda x: x.index)

    # Define the country (Switzerland)
    country = "CH" if target_vars[i] == "auction_price_de_ch" else "DE"

    if country == "CH":
        regional_holidays = holidays.CH(years=df.date.dt.year.unique().tolist())
    else:
        regional_holidays = holidays.DE(years=df.date.dt.year.unique().tolist())

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
                x["hour"].isin(np.arange(17, 24).tolist() + np.arange(1, 5).tolist()),
                0,
                1,
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

    # ### Feature Engineering

    # Other:
    # - `date`: drop, can't feed into net
    #
    # Numerical:
    # - everything but `holiday_name`
    #
    # Categorical
    # - `holiday_name`: one-hot encode

    # #### Cyclical Encoding

    # - avoid issue with exploding feature space when one-hot encoding hundreds of levels in categorical vars
    # - puts end of cycle closer to beginning (End of Year is not that different from BOY)

    def sin_transformer(period):
        return FunctionTransformer(lambda x: np.sin(x / period * 2 * np.pi))

    def cos_transformer(period):
        return FunctionTransformer(lambda x: np.cos(x / period * 2 * np.pi))

    # hour in day
    df["hour_sin"] = sin_transformer(24).fit_transform(df["hour"].astype(float))
    df["hour_cos"] = cos_transformer(24).fit_transform(df["hour"].astype(float))

    # hour in week
    df["week_hour_sin"] = sin_transformer(168).fit_transform(
        df["week_hour"].astype(float)
    )
    df["week_hour_cos"] = cos_transformer(168).fit_transform(
        df["week_hour"].astype(float)
    )

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

    # #### `sklearn` Pipeline for Data Preparation
    manual_cols = ["trend", "unique_id"]
    drop_cols = ["date", target_vars[i]]

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
            # ("scaler", StandardScaler())
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

    # ### LGBM

    test_start = pd.Timestamp("2024-01-01").tz_localize("UTC")
    test_days = df.query("date >= @test_start").date.dt.date.unique()

    best_params = {
        "lookback_days": 28,
        "num_boost_rounds": 392,
        "learning_rate": 0.06909917442885449,
        "max_depth": 3,
        "num_leaves": 88,
        "lambda_l1": 4.025467191249312e-06,
        "lambda_l2": 1.2454961537826062e-06,
        "min_split_gain": 2.5434484381686483e-06,
    }

    lookback_days = best_params["lookback_days"]
    preds = []
    num_boost_rounds = best_params["num_boost_rounds"]

    # Define the parameter grid
    param = {
        "verbose": -1,
        "objective": "regression",
        "metric": "rmse",
        "learning_rate": best_params["learning_rate"],
        "max_depth": best_params["max_depth"],
        "num_leaves": best_params["num_leaves"],
        "lambda_l1": best_params["lambda_l1"],
        "lambda_l2": best_params["lambda_l2"],
        "min_split_gain": best_params["min_split_gain"],
    }

    for current_day in tqdm(test_days):
        train_start_day = current_day - pd.Timedelta(days=lookback_days)
        train_end_day = current_day - pd.Timedelta(days=1)

        df_train = df.query("@train_start_day <= date.dt.date <= @train_end_day")
        df_test = df.query("date.dt.date == @current_day")

        # Split data
        X_train = df_train.drop(columns=["date", target_vars[i]])
        y_train = df_train[target_vars[i]]

        X_test = df_test.drop(columns=["date", target_vars[i]])
        y_test = df_test[target_vars[i]]

        # Preprocess data
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

        # Create LightGBM dataset
        train_data = lgb.Dataset(
            X_train_preprocessed, label=y_train, free_raw_data=False
        )

        # Train the model
        bst = lgb.train(
            params=param,
            train_set=train_data,
            valid_sets=[train_data],
            num_boost_round=num_boost_rounds,
            # callbacks=[lgb.early_stopping(stopping_rounds=10)],
        )

        # Make predictions on validation set
        y_pred = bst.predict(X_test_preprocessed, num_iteration=bst.best_iteration)

        preds.extend(y_pred.tolist())

    preds = pd.DataFrame(
        {
            "date": df.query("date.dt.date in @test_days")["date"].to_list(),
            ".pred": preds,
            "actual": df.query("date.dt.date in @test_days")[
                target_vars[i]
            ].to_list(),
        }
    )

    if not os.path.exists(output_folders[i]):
        os.makedirs(output_folders[i])

    preds.to_csv(f"{output_folders[i]}/holdout_predictions.csv", index=False)
