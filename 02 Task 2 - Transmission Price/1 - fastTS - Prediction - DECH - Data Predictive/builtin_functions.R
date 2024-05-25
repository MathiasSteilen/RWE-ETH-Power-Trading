#-------------------------------------------------------------------------------
#                   Functions for interpolations

#Function for detecting variables to interpolate and the actual function for
#interpolations.


#                   Detect consecutive values different 0
# Function to detect consecutive values in a vector
detect_consecutive <- function(x, n_consecutive) {
  consecutive_count <- 1
  for (i in 2:length(x)) {
    if (x[i] != 0 && x[i] == x[i - 1]) {
      consecutive_count <- consecutive_count + 1
      if (consecutive_count >= n_consecutive) {
        return(TRUE)
      }
    } else {
      consecutive_count <- 1
    }
  }
  return(FALSE)
}

# Function to detect columns with more than three consecutive values
detect_columns_with_consecutive <- function(df, n_consecutive = 3) {
  df %>%
    summarise(across(where(is.numeric), ~ detect_consecutive(., n_consecutive))) %>%
    select(where(~ any(.)))
}


interpolation <- function(y0, y1, len){
  beta <- (y1-y0)/len
  return(y0 + beta*(0:len))
}


interpolation_vector <- function(vec){
  
  consecutive <- which(!diff(vec) == 0)
  
  for(i in 1:(length(consecutive)-1)){
    pos0 <- consecutive[i]
    pos1 <- consecutive[i+1]
    len <- pos1 - pos0 
    vec[pos0:pos1] <- interpolation(vec[pos0], vec[pos1], len = len)
  }
  return(vec)
}


#-------------------------------------------------------------------------------
#                   Helper function plot

# Function to extract lags and their values
extract_lags <- function(beta) {
  beta %>%
    enframe(name = "name", value = "value") %>%
    filter(value != 0) %>% 
    filter(str_detect(name, "lag")) %>%
    mutate(lag_position = str_extract(name, "\\d+") %>% as.integer())
}



extract_coefficient <- function(beta, response) {
  df <- enframe(beta, name = "name", value = "value") %>%
    filter(value != 0)
  
  # Separate positive and negative values
  df_positive <- df %>% filter(value > 0)
  df_positive <- df_positive %>% filter(value > quantile(value, .9))
  df_negative <- df %>% filter(value < 0)
  df_negative <- df_negative %>% filter(value < quantile(value, .1))
  
  # Plot for positive values
  plot_positive <- ggplot(df_positive, aes(x = reorder(name, -value), y = value)) +
    geom_bar(stat = "identity") +
    labs(x = "Name", y = "Value", title = "Positive Coefficients", subtitle = response) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot for negative values
  plot_negative <- ggplot(df_negative, aes(x = reorder(name, value), y = value)) +
    geom_bar(stat = "identity") +
    labs(x = "Name", y = "Value", title = "Negative Coefficients", subtitle = response) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot_positive)
  print(plot_negative)
}




#Plot lags coefficient
plot_lags <- function(lags, title, response) {
  ggplot(lags, aes(x = lag_position, y = value)) +
    geom_segment(aes(x = lag_position, xend = lag_position, y = 0, yend = value)) +
    geom_point(size = 0.9) +
    geom_vline(xintercept = 24*7*(1:4), linetype = "dashed", color = "blue")+
    labs(title = title, x = "Lag Position", y = "Coefficient Value", subtitle = response)
}


ihs10 <- function(x) asinh(x/2)/log(10)
inverse_ihs10 <- function(x) sinh(x*log(10))*2

invers_u.log <- function(x, c = 1){
  if(abs(x) <= c) return(x)
  sign(x)*abs(exp(x/c-1)/c)
}
invers_u.log <- Vectorize(invers_u.log)


inverse_log <- function(x) {
  exp(x) - 1 -142.88
}

#-------------------------------------------------------------------------------
#                   Functions for automatization fastTS


#Adapt the model
model_list <- function(data, p.train, response, n_lags_max) {
  # Selecting 'date' and 'response' columns and converting 'response' to numeric
  y <- xts(x = data[[response]], order.by = data$date)
  
  # Selecting predictor variables and converting to a matrix
  x <- data %>%
    select(-all_of(c('date', response))) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  
  model <- fastTS(y, x, ptrain = p.train, n_lags_max = n_lags_max, weight_type = "pacf")
  
  return(model)
}



#Identidy best models based on AICc and BIC
model_selected <- function(model_list, response) {

  n_lags_max <- model_list$n_lags_max
  train_idx <- model_list$train_idx
  y <- model_list$y
  X <- model_list$X
  Xfull <- get_model_matrix(y, X, n_lags_max)
  Xfull <- Xfull[train_idx, ]   #obtain the matrix 
  y <- y[train_idx]
  
  
  # AICc
  ics <- sapply(model_list$fits, AICc)
  best_idx <- which(ics == min(ics), arr.ind = TRUE)
  
  best_fit <- model_list$fits[[best_idx[2]]]
  best_gamma <- model_list$gamma[best_idx[2]]
  best_lambda <- best_fit$lambda[best_idx[1]]
  
  
  print(paste0('The best gamma based on AICc is: ', best_gamma))
  print(paste0('The best lambda based on AICc is: ', best_lambda))
  
  model <- best_fit
  beta_AICc <- model$beta[, best_idx[1]]
  lags_AICc <- extract_lags(beta_AICc)
  extract_coefficient(beta_AICc, response)
  
  print(plot_lags(lags_AICc, "Lag Coefficients (AICc)", response))

  models <- list(model_aic = model)
  
  
  penalty_factors <- model$penalty.factor
  pl <- data.frame(index = seq_along(penalty_factors), weights = penalty_factors) %>% 
    ggplot(aes(x = index, y = weights)) +
    geom_point() +
    geom_line() +
    labs(title = "Weights Lasso AICc", x = "Index", y = "Weights", subtitle = response)
  
  print(pl)

  
  fc_sra <- predict(model, X = Xfull, 
                    which = which.min(AICc(model)))
  
  fitted_values <- data.frame(
    fitted_aic = fc_sra,
    actual = y, 
    date = index(y)
  )
  
  fitted_values <- fitted_values %>% 
    mutate(residuals_aic = actual - fitted_aic)
  
  
  pl <- ggplot(fitted_values, aes(x = fitted_aic, y = residuals_aic)) +
    geom_point() +
    geom_smooth(method = "loess", color = "red") +
    geom_hline(yintercept = mean(fitted_values$residuals_aic), linetype = "dashed", col = 'blue') +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted Values", y = "Residuals", title = "Tukey-Anscombe Plot for AICc model", subtitle = response)
  
  print(pl)

  acf_aic <- na.omit(fitted_values) %>% 
    pull(residuals_aic) %>% 
    acf(., lag.max = n_lags_max, plot = F)
  
  ci_aic <- qnorm((1 + 0.95)/2)/sqrt(acf_aic$n.used)
  acf_aic <- data.frame(
    acf = acf_aic$acf,
    lag = acf_aic$lag
  )
  
  
  
  pacf_aic <- na.omit(fitted_values) %>% 
    pull(residuals_aic) %>% 
    acf(., lag.max = n_lags_max, plot = F)
  
  pacf_aic <- data.frame(
    pacf = pacf_aic$acf,
    lag = pacf_aic$lag
  )
  
  
  pl <- ggplot(acf_aic, aes(x = lag, y = acf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) +
    geom_point() +
    geom_vline(xintercept = 24*7*(1:4), linetype = "dashed", color = "blue")+
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = ci_aic, linetype = "dashed") +
    geom_hline(yintercept = -ci_aic, linetype = "dashed") +
    labs(x = "Lags", y = "ACF", title = "ACF for AIC model", subtitle = response)
  
  plot(pl)

  pl <- ggplot(pacf_aic, aes(x = lag, y = pacf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = pacf)) +
    geom_point() +
    geom_vline(xintercept = 24*7*(1:4), linetype = "dashed", color = "blue")+
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = ci_aic, linetype = "dashed") +
    geom_hline(yintercept = -ci_aic, linetype = "dashed") +
    labs(x = "Lags", y = "PACF", title = "PACF for AIC model", subtitle = response)
  
  plot(pl)

  
  # BIC
  ics <- sapply(model_list$fits, BIC)
  best_idx <- which(ics == min(ics), arr.ind = TRUE)
  
  best_fit <- model_list$fits[[best_idx[2]]]
  best_gamma <- model_list$gamma[best_idx[2]]
  best_lambda <- best_fit$lambda[best_idx[1]]
  
  
  print(paste0('The best gamma based on BIC is: ', best_gamma))
  print(paste0('The best lambda based on BIC is: ', best_lambda))
  
  model <- best_fit
  beta_BIC <- model$beta[, best_idx[1]]
  lags_BIC <- extract_lags(beta_BIC)
  extract_coefficient(beta_BIC, response)
  
  
  print(plot_lags(lags_BIC, "Lag Coefficients (BIC)", response))
  models$model_bic <-  model
  
  
  penalty_factors <- model$penalty.factor
  pl <- data.frame(index = seq_along(penalty_factors), weights = penalty_factors) %>% 
    ggplot(aes(x = index, y = weights)) +
    geom_point() +
    geom_line() +
    labs(title = "Weights Lasso BIC", x = "Index", y = "Weights", subtitle = response) 
  
  print(pl)
  
  fc_srb <- predict(model, X = Xfull, 
                    which = which.min(BIC(model)))
  
  fitted_values <- fitted_values %>% 
    mutate(fitted_bic = fc_srb) %>% 
    mutate(residuals_bic = actual - fitted_bic)
    
  
  pl <- ggplot(fitted_values, aes(x = fitted_bic, y = residuals_bic)) +
    geom_point() +
    geom_smooth(method = "loess", color = "red") +
    geom_hline(yintercept = mean(fitted_values$residuals_bic), linetype = "dashed", col = 'blue') +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted Values", y = "Residuals", title = "Tukey-Anscombe Plot for BIC model", subtitle = response)
  
  print(pl)
  
  
  fitted_values <- na.omit(fitted_values)

  acf_bic <- acf(fitted_values$residuals_bic, lag.max = n_lags_max, plot = F)
  ci_bic <- qnorm((1 + 0.95)/2)/sqrt(acf_bic$n.used)
  acf_bic <- data.frame(
    acf = acf_bic$acf,
    lag = acf_bic$lag
  )
  
  pacf_bic <- pacf(fitted_values$residuals_bic, lag.max = n_lags_max, plot = F)
  pacf_bic <- data.frame(
    pacf = pacf_bic$acf,
    lag = pacf_bic$lag
  )

  
  
  pl <- ggplot(acf_bic, aes(x = lag, y = acf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) +
    geom_point() +
    geom_vline(xintercept = 24*7*(1:4), linetype = "dashed", color = "blue")+
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = ci_bic, linetype = "dashed") +
    geom_hline(yintercept = -ci_bic, linetype = "dashed") +
    labs(x = "Lags", y = "ACF", title = "ACF for BIC model", subtitle = response)
  
  plot(pl)
  
  pl <- ggplot(pacf_bic, aes(x = lag, y = pacf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = pacf)) +
    geom_point() +
    geom_vline(xintercept = 24*7*(1:4), linetype = "dashed", color = "blue")+
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = ci_bic, linetype = "dashed") +
    geom_hline(yintercept = -ci_bic, linetype = "dashed") +
    labs(x = "Lags", y = "PACF", title = "PACF for BIC model", subtitle = response)
  
  plot(pl)
  
  
  models$fitted_values = fitted_values
  return(models)
}



#The input if this function is the output of fastTS.
#This function allows to asses the performance of the model outsample updating,
#the available information every 24 hours. To predict at time t+k, the values between
#t+1 and t+k-1 are replaced by their predictions (k =1, ..., 24)


library(yardstick)
#THE RESULT ARE DONE ON BACKTRANSFORMED VARIABLES
prediction_step <- function(model_list, auction, inverse_transformation, response){
  n_lags_max <- model_list$n_lags_max
  train_idx <- model_list$train_idx
  y <- model_list$y
  X <- model_list$X
  Xfull <- get_model_matrix(y, X, n_lags_max)
  Xfull <- Xfull[-train_idx, ]   #obtain the matrix 
  y <- y[-train_idx]
  
  d <- index(y)[1:24] %>% as.character() %>% substr(12, 20)
  pos <- which(d == "01:00:00")
  y <- y[pos:length(y)] 
  Xfull <- Xfull[pos:nrow(Xfull), ]
  len_y <- length(y)
  d <- index(y)[(len_y-24):len_y] %>% as.character() %>% substr(12, 20)
  pos <- which(d == "00:00:00") + (len_y-24)
  y <- y[1:pos]
  supp <- index(y)
  Xfull <- Xfull[1:pos, ]
  
  num_days <- floor(Xfull %>% nrow() / 24)
  pred <- rep(NA, length(y))
  best_fit_penalized_aic <- model_list$fits[[which.min(apply(sapply(model_list$fits, 
                                                                    AICc), 2, min))]]
  
  pred <- rep(NA, length(y))
  for (i in 1:num_days) {
    X_DAY <- Xfull[((i - 1) * 24 + 1):(i * 24), ]
    pre_temp <- rep(NA, 24)
    for (j in 1:24) {
      pre_temp[j] <- predict(best_fit_penalized_aic, X = X_DAY[j, ], 
                             which = which.min(AICc(best_fit_penalized_aic)))
      X_DAY[, j] <- pre_temp[j]
    }
    pred[((i - 1) * 24 + 1):(i * 24)] <- pre_temp
  }
  
  fc_sra <- pred
  
  pred <- rep(NA, length(y))
  best_fit_penalized_bic <- model_list$fits[[which.min(apply(sapply(model_list$fits, 
                                                                    BIC), 2, min))]]
  for (i in 1:num_days) {
    X_DAY <- Xfull[((i - 1) * 24 + 1):(i * 24), ]
    pre_temp <- rep(NA, 24)
    for (j in 1:24) {
      pre_temp[j] <- predict(best_fit_penalized_bic, X = X_DAY[j, ], 
                             which = which.min(BIC(best_fit_penalized_bic)))
      X_DAY[, j] <- pre_temp[j]
    }
    pred[((i - 1) * 24 + 1):(i * 24)] <- pre_temp
  }
  
  fc_srb <- pred
  
  predictions <- data.frame(
    fc_srb = fc_srb, 
    fc_sra = fc_sra,
    price = y,
    date = supp
  )
  
  predictions <- na.omit(predictions) %>% 
    mutate(fc_srb = inverse_transformation(fc_srb)) %>%  
    mutate(fc_sra = inverse_transformation(fc_sra)) %>% 
    mutate(price = inverse_transformation(price))
    
  
  if(auction) {
    predictions <- predictions %>%
      mutate(fc_srb = pmax(fc_srb, 0)) %>%
      mutate(fc_sra = pmax(fc_sra, 0))
  }
  
  predictions <- predictions %>% 
    mutate(residuals_b = price - fc_srb) %>%
    mutate(residuals_a = price - fc_sra)
  
  
  plot1 <- ggplot(predictions, aes(x = fc_srb, y = residuals_b)) +
    geom_point() +
    geom_smooth(method = "loess", color = "red") +
    geom_hline(yintercept = mean(predictions$residuals_b), linetype = "dashed", col = 'red') +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Prediction", y = "Prediction error", title = "Prediction error against Prediction for BIC model", subtitle = response)

  
  plot2 <- ggplot(predictions, aes(x = date, y = residuals_b)) +
    geom_line() +
    scale_x_datetime(date_labels = "%D", breaks = "4 day") +
    geom_smooth(method = "loess", color = "red") +
    geom_hline(yintercept = mean(predictions$residuals_b), linetype = "dashed", col = 'red') +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(data = data.frame(date = seq(min(predictions$date), max(predictions$date), by = "week")),
               aes(xintercept = as.numeric(date)), linetype = "dashed", color = "blue") +
    labs(x = "Hourly Data", y = "Prediction error", title = "Time series of Prediction errors for BIC model", subtitle = response)
  
  plot3 <- ggplot(predictions, aes(x = date)) +
    geom_line(aes(y = fc_srb, col = 'Prediction')) +
    geom_line(aes(y = price, col = 'Actual')) +
    scale_x_datetime(date_labels = "%D", breaks = "3 day") +
    labs(x = "Hourly data",
         y = "Price",
         color = "Day ahead prices", subtitle = response, title = "Prices and predictions fastTS for BIC model") +
    scale_color_manual(values = c("Actual" = "grey50", "Prediction" = "firebrick")) +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom") +
    geom_vline(data = data.frame(date = seq(min(predictions$date), max(predictions$date), by = "week")),
               aes(xintercept = as.numeric(date)), linetype = "dashed", color = "blue")
  
  print(plot1)
  print(plot2)
  print(plot3)
  
  
  plot1 <- ggplot(predictions, aes(x = fc_sra, y = residuals_a)) +
    geom_point() +
    geom_smooth(method = "loess", color = "red") +
    geom_hline(yintercept = mean(predictions$residuals_a), linetype = "dashed", col = 'red') +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Prediction", y = "Prediction error", title = "Prediction error against Prediction for AICc model", subtitle = response)
  
  
  plot2 <- ggplot(predictions, aes(x = date, y = residuals_a)) +
    geom_line() +
    scale_x_datetime(date_labels = "%D", breaks = "3 day") +
    geom_smooth(method = "loess", color = "red") +
    geom_hline(yintercept = mean(predictions$residuals_a), linetype = "dashed", col = 'red') +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Hourly Data", y = "Prediction error", title = "Time series of Prediction errors for AICc model", subtitle = response) +
    geom_vline(data = data.frame(date = seq(min(predictions$date), max(predictions$date), by = "week")),
               aes(xintercept = as.numeric(date)), linetype = "dashed", color = "blue")

  
  plot3 <- ggplot(predictions, aes(x = date)) +
    geom_line(aes(y = fc_sra, col = 'Prediction')) +
    geom_line(aes(y = price, col = 'Actual')) +
    scale_x_datetime(date_labels = "%D", breaks = "3 day") +
    labs(x = "Hourly data",
         y = "Price",
         color = "Day ahead prices", subtitle = response, title = "Prices and predictions fastTS for AICc model") +
    scale_color_manual(values = c("Actual" = "grey50", "Prediction" = "firebrick")) +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom") +
    geom_vline(data = data.frame(date = seq(min(predictions$date), max(predictions$date), by = "week")),
               aes(xintercept = as.numeric(date)), linetype = "dashed", color = "blue")
  
  print(plot1)
  print(plot2)
  print(plot3)
  
  
  oos_results_aic <- predictions %>% 
    mutate(rmse = rmse_vec(price, fc_sra)) %>% 
    mutate(rsq = rsq_trad_vec(price, fc_sra)) %>% 
    mutate(mae = mae_vec(price, fc_sra)) %>% 
    mutate(mape = mape_vec(price, fc_sra)) %>% 
    head(1) %>% 
    select(c(rmse, rsq, mae, mape))
  
  oos_results_bic <- predictions %>% 
    mutate(rmse = rmse_vec(price, fc_srb)) %>% 
    mutate(rsq = rsq_trad_vec(price, fc_srb)) %>% 
    mutate(mae = mae_vec(price, fc_srb)) %>% 
    mutate(mape = mape_vec(price, fc_srb)) %>% 
    head(1) %>% 
    select(c(rmse, rsq, mae, mape))
  
  
  oos_results <- rbind("AICc" = oos_results_aic, "BIC" = oos_results_bic)
  
  print(oos_results)
  return(list(result = predictions, oos_results = oos_results))
}




#-------------------------------------------------------------------------------
#                   Helper function copied from github FastTS

get_model_matrix <- function(y, X = NULL, n_lags_max) {
  ylags <- sapply(1:n_lags_max, function(i) lag(y, i))
  colnames(ylags) <- paste0('lag', 1:n_lags_max)
  cbind(ylags, X)
}

AICc <- function(fit, eps = 1) {
  ll <- logLik(fit)
  k <- attr(ll, "df")
  n <- attr(ll, "n")
  k_star <- pmin(k, n - eps - 1)
  AIC(fit) + (2 * k^2 + 2*k) / (n - k_star - 1)
}

get_oos_results <- function(fits, ytest, Xtest) {
  
  best_fit_penalized_bic <- fits[[which.min(apply(sapply(fits, BIC), 2, min))]]
  best_fit_penalized_aicc <- fits[[which.min(apply(sapply(fits, AICc), 2, min))]]
  
  predictions <- data.frame(
    y = ytest,
    fc_srb = predict(best_fit_penalized_bic, X = Xtest,
                     which = which.min(BIC(best_fit_penalized_bic))
    ),
    fc_sra = predict(best_fit_penalized_aicc, X = Xtest,
                     which = which.min(AICc(best_fit_penalized_aicc)))
  )
  
  oos_results_aic <- summarize(
    predictions,
    rmse = rmse_vec(.data$y, .data$fc_sra),
    rsq = rsq(.data$y, .data$fc_sra),
    mae = mae_vec(.data$y, .data$fc_sra),
  )
  
  oos_results_bic <- summarize(
    predictions,
    rmse = rmse_vec(.data$y, .data$fc_srb),
    rsq = rsq(.data$y, .data$fc_srb),
    mae = mae_vec(.data$y, .data$fc_srb)
  )
  
  oos_results <- rbind("AICc" = oos_results_aic, "BIC" = oos_results_bic)
}