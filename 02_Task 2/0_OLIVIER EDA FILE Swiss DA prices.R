#EDA Swiss Day Ahead prices and several covariates


library(tidyverse)
library(ggsci)
library(scales)
library(tidymodels)
library(stats)
library(AMR)
library(corrplot)
set.seed(123)

# Default theme for charts
theme_set(
  theme_bw() +
    theme(  
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(
        face = "italic", size = 10, colour = "grey50"
      )
    )
)

data <- read.csv('data_cleaned_UTC.csv')
data= data[35010:nrow(data),] #only considering data since 2023
df <- data

df1 <- df %>% #response
  select(date, day_ahead_price_ch)

df2 <- df %>% #covariates
  select(-c(date, day_ahead_price_ch))

n <- nrow(df1)

df <- cbind(df1[1:(n-24),], df2[25:n, ])
rm(df1, df2, n)

colnames(df)


### EDA ###
summary(df)


############ Selecting most important features ############

# Calculate PEARSON correlations between predictors and response
correlation <- as.data.frame(cor(df[, -1], df$day_ahead_price_ch)) #Excluding data, as not numeric
correlation$index=1:nrow(correlation)

# Sort correlations in descending order
sorted_corr <- correlation[order(abs(correlation[,"V1"]), decreasing = T),]

# Select top 10 predictors for Swiss DA prices (exluding Swiss DA prices as variable)
top_10_corr_predictors <- sorted_corr[2:11,]
print(top_10_corr_predictors)

top_10_predictos_names = colnames(df[,-1])[top_10_corr_predictors$index]

library(corrplot)
library(car)
cor_names=c("day_ahead_price_ch", top_10_predictos_names)
scatterplotMatrix(df[,cor_names] )

#----------------------------------------------------------------------------
# Calculate RANK (SPEARMAN) correlations between predictors and response
correlation <- as.data.frame(cor(df[, -1], df$day_ahead_price_ch, method = "spearman")) #Excluding data, as not numeric
correlation$index=1:nrow(correlation)

# Sort correlations in descending order
sorted_corr <- correlation[order(abs(correlation[,"V1"]), decreasing = T),]

# Select top 10 predictors for Swiss DA prices (exluding Swiss DA prices as variable)
top_10_corr_predictors <- sorted_corr[2:11,]
print(top_10_corr_predictors)

top_10_predictos_names = colnames(df[,-1])[top_10_corr_predictors$index]

cor_names=c("day_ahead_price_ch", top_10_predictos_names)
scatterplotMatrix(df[,cor_names] )


'''Initial Findings:
- DA prices of different country strongly correlated
- Energy production methods in Germany / Italy seem to be more influential on Swiss DA price than Swiss production methods
- Auction price DE to CH relevant for swiss DA prices
'''

##Random forest Feature importance
install.packages("randomForest")
library(randomForest)

# Train a random forest model
rf_model <- randomForest(day_ahead_price_ch ~ ., data = df)

# Extract feature importance
feature_importance <- importance(rf_model, type = 1)

# Sort feature importance in descending order
sorted_feature_importance <- sort(feature_importance, decreasing = TRUE)

# Select top 10 predictors
top_10_rf_predictors <- names(sorted_feature_importance)[1:10]


##Recursive Feature Elimination with Cross Validation
install.packages("caret")
library(caret)

# Define control parameters for RFECV
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Run RFECV
rfecv_model <- rfe(df[, -1], df$day_ahead_price_ch, sizes = c(1:ncol(df)-1), rfeControl = control)

# Get the top 10 predictors
top_10_rfecv_predictors <- names(rfecv_model$optVariables)


## LASSO
library(glmnet)

# Fit Lasso regression model
lasso_model <- cv.glmnet(as.matrix(df[, !(colnames(df) %in% c("date", "day_ahead_price_ch"))]), df$day_ahead_price_ch, alpha = 1, nfolds =  )

# Get non-zero coefficient predictors
lasso_predictors <- coef(lasso_model, s = "lambda.min")

# Extract top 10 predictors
top_10_lasso_predictors <- names(lasso_predictors)[-1]




''' no PCA used, as PCA destroys explainability'''

#Forward and backward selection
library(leaps)

# Perform forward selection
forward_model <- regsubsets(day_ahead_price_ch ~ ., data = df, method = "forward")

# Get the best model from forward selection
best_forward_model <- which.min(summary(forward_model)$cp)

# Get selected predictors
selected_predictors_forward <- names(coef(forward_model, id = best_forward_model)[-1])
print(selected_predictors_forward)

#----------
# Perform backward selection
backward_model <- regsubsets(day_ahead_price_ch ~ ., data = df, method = "backward")

# Get the best model from backward selection
best_backward_model <- which.min(summary(backward_model)$cp)

# Get selected predictors
selected_predictors_backward <- names(coef(backward_model, id = best_backward_model)[-1])
print(selected_predictors_backward)


### EDA of Swiss Day ahead prices
density_plot <- ggplot(df, aes(x = day_ahead_price_ch)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of day_ahead_price_ch")
print(density_plot)



summary(df$day_ahead_price_ch)


density_plot_response <- ggplot(df, aes(x = day_ahead_price_ch)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of day_ahead_price_ch")
print(density_plot_response)


hist(df$day_ahead_price_ch, main = "Histogram of day_ahead_price_ch", xlab = "day_ahead_price_ch", col = "lightblue")

#Day ahead prices
df$date <- as.Date(df$date)
time_series_plot <- ggplot(df, aes(x = date, y = day_ahead_price_ch)) +
  geom_line() +
  labs(title = "Time Series Plot of day_ahead_price_ch", x = "Date", y = "day_ahead_price_ch")
print(time_series_plot)

#Log day ahead prices
df$date <- as.Date(df$date)
time_series_plot <- ggplot(df, aes(x = date, y = log(day_ahead_price_ch))) +
  geom_line() +
  labs(title = "Time Series Plot of day_ahead_price_ch", x = "Date", y = "LOG_day_ahead_price_ch")
print(time_series_plot)


