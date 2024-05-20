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
#                   Functions for automatization fastTS


#Train the fastTS given the data and the response. 
#return list of the adapted model
model_list <- function(data, p.train = 0.8, response = 'day_ahead_price_de', nlag = 100) {
  # Selecting 'date' and 'response' columns and converting 'response' to numeric
  y <- xts(x = data[[response]], order.by = data$date)
  
  # Selecting predictor variables and converting to a matrix
  x <- data %>%
    select(-all_of(c('date', response))) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  # Creating the model using fastTS
  model <- fastTS(y, x, ptrain = p.train, n_lags_max = nlag)
  
  return(model)
}

#Summary statistics given the object returned from fastTS
#The model that is selected for the diagnostic is the best model based on the 
#best AICc.
model_selected <- function(model_list, p.train = 0.7, lag.max = 5040) {
  
  ics <- sapply(model_list$fits, AICc)
  best_idx <- which(ics == min(ics), arr.ind = TRUE)
  
  best_fit <- model_list$fits[[best_idx[2]]]
  best_gamma <- model_list$gamma[best_idx[2]]
  best_lambda <- best_fit$lambda[best_idx[1]]
  
  
  print(paste0('The best gamma based on AICc is: ', best_gamma))
  print(paste0('The best lambda based on AICc is: ',best_lambda))
  
  model <- best_fit
  
  
  pos <- best_idx[1]
  pred <- model$beta[, pos]
  
  X <- cbind(rep(1, nrow(model_list$Xfulltrain)), model_list$Xfulltrain)
  fitted <- X %*% pred %>% matrix(ncol = 1)
  date_index <- index(model_list$y_cc_train)
  price <- coredata(model_list$y_cc_train)
  sol <- data.frame(response = price, 
                    fitted_response = fitted,
                    date = date_index)
  
  sol$difference <- sol$response - sol$fitted_response
  
  plot1 <- ggplot(sol, aes(x = date)) +
    geom_line(aes(y = fitted_response, col = 'Fitted')) +
    geom_line(aes(y = response, col = 'Actual')) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(title = "Prices and Fitted fastTS",
         x = "Hourly data",
         y = "Price",
         color = "Day ahead prices") +
    scale_color_manual(values = c("Actual" = "grey", "Fitted" = "orange")) +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom")
  
  plot2 <- ggplot(sol, aes(x = date)) +
    geom_line(aes(y = difference), col = "blue") +
    scale_x_datetime(date_labels = "%Y-%m") +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom")
  
  print(plot1)
  print(plot2)
  
  acf(sol$difference, main = paste0('ACF Residuals max_lag', lag.max), lag.max = lag.max)
  pacf(sol$difference, main = paste0('PACF Residuals max_lag', lag.max), lag.max = lag.max)
  
  acf(sol$difference, main = 'ACF Residuals week', lag.max = 168)
  pacf(sol$difference, main = 'PACF Residuals week', lag.max = 168)
  
  
  acf(abs(sol$difference), main = paste0('ACF Absolute Residuals max_lag', lag.max), lag.max = lag.max)
  pacf(abs(sol$difference), main = paste0('PACF Absolute Residuals max_lag', lag.max), lag.max = lag.max)
  
  acf(abs(sol$difference), main = 'ACF Absolute Residuals week', lag.max = lag.max)
  pacf(abs(sol$difference), main = 'PACF Absolute Residuals week', lag.max = lag.max)
  

  qqnorm(sol$difference)
  qqline(sol$difference)
  
  return(list(sol = sol, pred = pred, model_matrix = X))
}


#-------------------------------------------------------------------------------
#               Functions for prediction model - no lags 1:24

#The function implement a version of fastTS without the lags between 1 and 24 since are 
#not available in the moment of prediction.  TRAIN
model_list_prediction <- function(data, response, n_lags_max, gamma = c(0, 2^(-2:4)), ptrain, 
                                  pf_eps = 0.01, w_endo, w_exo, weight_type = c("pacf", "parametric"), m = NULL,
                                  r = c(rep(0.1, length(m)), 0.01), plot = FALSE, 
                                  ncvreg_args = list(penalty = "lasso", returnX = FALSE, lambda.min = 0.001)) {
  
  # Selecting 'date' and 'response' columns and converting 'response' to numeric
  y <- xts(x = data[[response]], order.by = data$date)
  
  # Selecting predictor variables and converting to a matrix
  X <- data %>%
    select(-all_of(c('date', response))) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  names_to_remove <- paste0('lag', 1:24)
  
  
  n <- length(y)
  
  train_idx <- 1:floor(n * ptrain)
  if (any(is(y) == "ts")) 
    y <- as.numeric(y)
  ytest <- y[-train_idx]
  ytrain <- y[train_idx]
  if (!is.null(X)) {
    X <- as.matrix(X)
    if (is.null(colnames(X))) 
      colnames(X) <- paste0("X", 1:ncol(X))
    if (any(is_intercept <- apply(X, 2, function(x) all(x == 
                                                        1)))) {
      warning("Detected intercept; dropping")
      X <- X[, !is_intercept]
    }
    Xfull <- get_model_matrix(y, X, n_lags_max)
    Xfull <- Xfull[, !colnames(Xfull) %in% names_to_remove]
    Xtrain <- X[train_idx, , drop = FALSE]
    Xfulltrain <- na.omit(Xfull[train_idx, ])
    Xfulltest <- Xfull[-train_idx, ]
    if (missing(w_exo)) 
      w_exo <- abs(apply(Xtrain, 2, function(x) coef(lm((ytrain ~ 
                                                           x)))[2]))
    if (w_exo[1] == "unpenalized") 
      w_exo <- rep(Inf, ncol(Xtrain))
  }
  else {
    X <- NULL
    Xfull <- get_model_matrix(y, n_lags_max = n_lags_max)
    Xfulltrain <- na.omit(Xfull[train_idx, ])
    Xfulltest <- Xfull[-train_idx, ]
    w_exo <- NULL
  }
  y_cc_train <- ytrain[complete.cases(Xfull)[train_idx]]
  if (missing(w_endo)) {
    weight_type <- match.arg(weight_type)
    if (weight_type == "pacf") {
      pacfs <- pacf(ts(ytrain), lag.max = n_lags_max, 
                    plot = plot)
      w_endo <- abs(pacfs$acf[25:length(pacfs$acf)])
      if (length(m)) 
        warning("m is ignored when weight_type = 'pacf'")
    }
    else if (weight_type == "parametric") {
      w_endo <- 1/exp(penalty_scaler(1:n_lags_max, m, 
                                     r, plot = plot))
    }
  }
  w <- c(w_endo, w_exo)
  pfs <- sapply(1:length(gamma), function(g) w^-gamma[g])
  pfs[pfs < pf_eps] <- 0
  pfs[is.infinite(w), 1] <- 0
  ncvreg_args$X <- Xfulltrain
  ncvreg_args$y <- y_cc_train
  fits <- apply(pfs, 2, function(x) {
    ncvreg_args$penalty.factor <- x
    do.call(ncvreg::ncvreg, ncvreg_args)
  })
  best_fit_penalized_bic <- fits[[which.min(apply(sapply(fits, 
                                                         BIC), 2, min))]]
  best_fit_penalized_aicc <- fits[[which.min(apply(sapply(fits, 
                                                          AICc), 2, min))]]
  oos_results <- data.frame(rmse = NA, rsq = NA, mae = NA)
  if (ptrain < 1) 
    oos_results <- get_oos_results(fits, ytest = ytest, 
                                   Xtest = Xfulltest)
  results <- list(fits = fits, ncvreg_args = ncvreg_args, 
                  gamma = gamma, n_lags_max = n_lags_max, y = y, X = X, 
                  y_cc_train = y_cc_train, Xfulltrain = Xfulltrain, oos_results = oos_results, 
                  train_idx = train_idx, weight_type = weight_type, m = m, 
                  r = r, ptrain = ptrain)
  class(results) <- "fastTS"
  results
  
}



# THE MODEL MUST BE OUTPUT OF model_list_prediction!
#This function has to be used with the model outputted from the function
#model_list_prediction, which doesn't use the information of the lags between
# 1 to 24. 
library(yardstick)
prediction_model <- function(model_list) {
  n_lags_max <- model_list$n_lags_max
  train_idx <- model_list$train_idx
  y <- model_list$y
  X <- model_list$X
  Xfull <- get_model_matrix(y, X, n_lags_max)
  Xfull <- Xfull[-train_idx,-(1:24) ]   #obtain the matrix without the first 24 lags
  y <- y[-train_idx]
  supp <- index(y)

  best_fit_penalized_aic <- model_list$fits[[which.min(apply(sapply(model_list$fits, 
                                                                    AICc), 2, min))]]
  
  fc_sra <- predict(best_fit_penalized_aic, X = Xfull, 
                  which = which.min(AICc(best_fit_penalized_aic)))


  best_fit_penalized_bic <- model_list$fits[[which.min(apply(sapply(model_list$fits, 
                                                                    BIC), 2, min))]]
  fc_srb <- predict(best_fit_penalized_bic, X = Xfull, 
                    which = which.min(BIC(best_fit_penalized_bic)))
  
  predictions <- data.frame(
    fc_srb = fc_srb, 
    fc_sra = fc_sra,
    price = y,
    date = supp
  )
  
  plot1 <- ggplot(predictions, aes(x = date)) +
    geom_line(aes(y = fc_srb, col = 'Fitted BIC')) +
    geom_line(aes(y = price, col = 'Actual')) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(title = "Prices and Fitted fastTS BIC criterion",
         x = "Hourly data",
         y = "Price",
         color = "Day ahead prices") +
    scale_color_manual(values = c("Actual" = "grey", "Fitted BIC" = "orange")) +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom")
  
  plot2 <- ggplot(predictions, aes(x = date)) +
    geom_line(aes(y = fc_sra, col = 'Fitted AICc')) +
    geom_line(aes(y = price, col = 'Actual')) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(title = "Prices and Fitted fastTS BIC criterion",
         x = "Hourly data",
         y = "Price",
         color = "Day ahead prices") +
    scale_color_manual(values = c("Actual" = "grey", "Fitted AICc" = "orange")) +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom")
  
  
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
  
  
  
  print(plot1)
  print(plot2)
  return(list(predictions = predictions, oos_results = oos_results))
}



#The input if this function is the output of fastTS.
#This function allows to asses the performance of the model outsample updating,
#the available information every 24 hours. To predict at time t+k, the values between
#t+1 and t+k-1 are replaced by their predictions (k =1, ..., 24)
prediction_step <- function(model_list){
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
  
  num_days <- Xfull %>% nrow() /24
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
  
  plot1 <- ggplot(predictions, aes(x = date)) +
    geom_line(aes(y = fc_srb, col = 'Fitted BIC')) +
    geom_line(aes(y = price, col = 'Actual')) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(title = "Prices and Fitted fastTS BIC criterion",
         x = "Hourly data",
         y = "Price",
         color = "Day ahead prices") +
    scale_color_manual(values = c("Actual" = "grey", "Fitted BIC" = "orange")) +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom")
  
  plot2 <- ggplot(predictions, aes(x = date)) +
    geom_line(aes(y = fc_sra, col = 'Fitted AICc')) +
    geom_line(aes(y = price, col = 'Actual')) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(title = "Prices and Fitted fastTS BIC criterion",
         x = "Hourly data",
         y = "Price",
         color = "Day ahead prices") +
    scale_color_manual(values = c("Actual" = "grey", "Fitted AICc" = "orange")) +
    theme(plot.margin = margin(10, 10, 10, 10), legend.position = "bottom")
  
  
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
  
  
  
  print(plot1)
  print(plot2)
  return(list(predictions = predictions, oos_results = oos_results))
}

#Function for the JAO predictions:
jao_predictions <- function(pred){

  predictions <- pred$predictions
  predictions <- predictions %>% 
    mutate(fc_srb = pmax(fc_srb, 0)) %>% 
    mutate(fc_sra = pmax(fc_sra, 0))
  
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
  return(oos_results)
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
    rsq = rsq_trad_vec(.data$y, .data$fc_sra),
    mae = mae_vec(.data$y, .data$fc_sra),
  )
  
  oos_results_bic <- summarize(
    predictions,
    rmse = rmse_vec(.data$y, .data$fc_srb),
    rsq = rsq_trad_vec(.data$y, .data$fc_srb),
    mae = mae_vec(.data$y, .data$fc_srb)
  )
  
  oos_results <- rbind("AICc" = oos_results_aic, "BIC" = oos_results_bic)
}


ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}