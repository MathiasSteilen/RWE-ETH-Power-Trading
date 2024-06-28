### Modelling of Swiss Day ahead prices

# Load necessary libraries
library(rugarch)
library(forecast)
library(dplyr)

#Functions
# Define a function to calculate R-squared
calculate_r_squared <- function(true_values, fitted_values) {
  # Calculate the mean of true values
  true_mean <- mean(true_values)
  
  # Calculate total sum of squares (TSS)
  TSS <- sum((true_values - true_mean)^2)
  
  # Calculate residual sum of squares (RSS)
  RSS <- sum((true_values - fitted_values)^2)
  
  # Calculate R-squared
  R_squared <- 1 - (RSS / TSS)
  
  return(R_squared)
}
# Load your dataset
df <- read.csv("~/Downloads/RWE-ETH-Power-Trading-main-5/00 Data Retrieval and Cleaning/0_df_final_imputed.csv")
df$date <- as.Date(df$date)

df= sort(df)

sorted_df <- arrange(df, date)


df_2023= sorted_df[35010:44510,]
fit= lm(day_ahead_price_ch~., data=df_2023)
summary(fit)


plot(df_2023$day_ahead_price_ch[0:1000], type="l")
lines(fit$fitted.values[0:1000], col="red")

(R2= calculate_r_squared(df_2023$day_ahead_price_ch, fit$fitted.values))
(mse= mean((df_2023$day_ahead_price_ch -fit$fitted.values)^2))
sqrt(mse)


predicted_values=fit$fitted.values
day_ahead_price_ch=df_2023$day_ahead_price_ch

# Calculate residuals
residuals <- day_ahead_price_ch - predicted_values

# Load the rugarch package
library(rugarch)

#making data stationary
q_value=c(0,1,2,3)
p_values=c(0,1,2,3)
results_mat= matrix(NA,length(q_value), length(p_values))
for (q in q_value){
  for (p in p_values){
    stationary_data <- log(residuals+500)
    
    
    garch_model <- ugarchspec(mean.model = list(armaOrder = c(q, p)), 
                              variance.model = list(model = "sGARCH"), 
                              distribution.model = "norm")
    fit_garch <- ugarchfit(spec = garch_model, data = stationary_data)
    fitted_values= fitted(fit_garch)
    length(fitted_values)
    
    mse=mean((stationary_data -fitted_values)^2)
    
    cat("with q = ", q, " and p = ", p, " --> MSE: ", mse, "\n")
    results_mat[q+1,p+1]= mse
    
    }
  }

min(results_mat)
min_index <- which(results_mat == min(results_mat), arr.ind = TRUE)


#best q= 1 and best p = 3

garch_model <- ugarchspec(mean.model = list(armaOrder = c(1, 3)), 
                          variance.model = list(model = "sGARCH"), 
                          distribution.model = "norm")
fit_garch <- ugarchfit(spec = garch_model, data = stationary_data)
stationary_predictions= fitted(fit_garch)

original_predictions=exp(stationary_predictions)-500

plot(original_predictions, type="l")


#Checking if GARCH improved model performance


fit= lm(day_ahead_price_ch~., data=df_2023)
GARCH_predicitons=fit$fitted.values+original_predictions
Predictions_raw= fit$fitted.values


(R2= calculate_r_squared(df_2023$day_ahead_price_ch, fit$fitted.values))
(R2= calculate_r_squared(df_2023$day_ahead_price_ch, GARCH_predicitons))

(mse= mean((df_2023$day_ahead_price_ch -fit$fitted.values)^2))
(mse= mean((df_2023$day_ahead_price_ch -GARCH_predicitons)^2))


#Significant increase in predictions accuracy using GARCH!




