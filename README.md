# ETH Zurich Statistics Lab: Collaborating with RWE on European Cross Border Power Trading ⚡

<p align="center">
  <img src="05 Final Presentation/06 - Solar Bathtub curve.png" height="300" />
</p>


This project models the differences in electricity prices between Germany and Switzerland, focusing on daily auctions that set prices and quantities for the next day’s electricity supply. The primary goal was to account for Joint Allocation Office (JAO) auction prices in trying to predict arbitrage opportunities in crossborder trading in the day-ahead markets.

## Data Sources and Preparation

Data was sourced from the ENTSOE Transparency Platform and the Joint Allocation Office (JAO), covering January 1, 2019, to February 1, 2024. This included auction prices, transmission capacity, day-ahead prices, load, weather forecasts, and more. The data, though relatively clean, required extensive joining and preparation for modeling.

## Modeling Approaches

Several models were employed:

- **ARMA with Exogenous Regressors (ARMAX)**: Combines autoregressive and moving average components with external regressors to predict electricity prices.
- **Sparsity Ranked Lasso with Partial Autocorrelation and Exogenous Variables (SRLPAx)**: Uses lasso regularization for effective handling of high-dimensional data and selection of significant predictors.
- **ARMAX-GARCH-X**: Combines ARMAX for mean predictions with GARCH-X for volatility modeling, suitable for financial data with volatility clustering.
- **Black Box Models (LightGBM, XGBoost)**: Less interpretable but often superior in predictive performance, serving as benchmarks.


## Findings and Performance

The main focus was on inference and explainable models, with predictive performance optimization being secondary. Key insights include:

- Most models effectively captured the patterns of Swiss and German day-ahead prices, though struggled with the complexity of JAO auction prices.
- No single model was universally superior, highlighting the need for task-specific model selection.
- ARMAX and ARMAX-GARCH-X models, while performant, required a priori variable selection due to computational complexity.
- The SRLPAx model was particularly useful in identifying important external predictors, contributing significantly to the understanding of relevant variables and data lag structures.

## Conclusion

The project highlights the importance of task-specific model selection and the balance between interpretability and predictive accuracy. For practical applications in day-ahead market forecasting and cross-border trading strategies, a hybrid approach leveraging both interpretable and black-box models may offer the most robust solution.

Future work could address computational restrictions to include more external regressors in ARMAX and ARMAX-GARCH-X models, potentially improving predictive performance and insights. Additionally, exploring more advanced machine learning models could enhance predictive power and trading opportunities, such as neural networks (ANN, LSTM).
