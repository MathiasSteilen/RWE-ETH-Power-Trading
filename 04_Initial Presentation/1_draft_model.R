rm(list = ls())
library(tidyverse)
library(corrplot)
library(dplyr)
library(forecast)
library(ggsci)
library(scales)

# Default theme for charts
theme_set(
  theme_bw() +
    theme(  
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(
        face = "italic", size = 10, colour = "grey50"
      ),
      plot.title.position = "plot"
    )
)


#Import data
data <- read.csv('0_df_final_ch-de.csv')


#Drop columns w/ variables linked with it, at or fr --> No scope
#There is lu... we have to undersatnd how to use it
no_col_names <- c("it", "fr","IT", "FR","it_ch", "fr_ch","IT_CH", "FR_CH",
                  "at", "ch_at")
data <- data %>% select(-ends_with(no_col_names))
#Remove since all NA, or few values are not NA (+are missig in last years)
data <- data %>% select(-c(wind_onshore_actual_consumption_de,solar_actual_consumption_de,
                           other_renewable_actual_consumption_de, hydro_water_reservoir_actual_consumption_de,
                           fossil_oil_actual_consumption_de, nuclear_actual_consumption_de))
data %>% colnames
str(data)


#Formatting time
data <- data %>% 
  mutate(date= as.POSIXct(date, format = "%Y-%m-%d %H:%M"))
str(data)   #HConverting varaible into factor ecc.


#Sorting based on time
data <- data[order(data$date),] 

data <- data %>% mutate(dst = factor(dst))

#Remove NA data
date_cut <- '2024-02-01 01:00:00'
cond <- data$date < as.POSIXct(date_cut, format = "%Y-%m-%d %H:%M")
data <- data %>% filter(cond)



#Consider a window of time that is reasonably 'stationary'
date_cut <- '2023-01-01 01:00:00'
cond <- data$date > as.POSIXct(date_cut, format = "%Y-%m-%d %H:%M")
data_window <- subset(data, cond)



dependent <- data %>% select(c(day_ahead_price_de))
dependent <- dependent[1:(nrow(dependent)-1),]

covariates <- data %>% select(-c(date, auction_price, day_ahead_price_ch, day_ahead_price_de))
covariates <- covariates[2:nrow(covariates),]

#Reducing number of covariates - excludes factors, to many NA (then VA that I excluded
#cuz estimated coef close to zero --> NOT GOOD REASON)
covariates <- covariates %>% 
  mutate(allocatedCapacity = as.numeric(allocatedCapacity),
         ATC = as.numeric(ATC)) %>% 
  select(-c(dst, nuclear_actual_aggregated_de,
            fossil_gas_actual_consumption_de,
            fossil_coal.derived_gas_actual_aggregated_de)) %>% 
  select(-c(actual_load_ch, actual_load_de,
            solar_forecast_de, fossil_brown_coal_lignite_actual_aggregated_de,
            fossil_hard_coal_actual_aggregated_de, solar_actual_aggregated_de,
            wind_onshore_actual_aggregated_de, hydro_reservoir_storage_ch))

#model fit & summary
fit <- auto.arima(x = dependent, 
                   xreg = as.matrix(covariates))

summary(fit)


dates <- data$date[1:44510]
fitted <- fit$fitted
resid <- fit$residuals
forecast_values <- forecast(fit, h = 24, xreg = as.matrix(covariates)[44509:44486, ])



data_used <- cbind(dates, dependent, covariates, fitted, resid)
new_dates <- data_used[44463:44486, 'dates']
new_dates <- new_dates + as.difftime(2, units = "days")

forecast_values <- cbind(forecast_values, data.frame(new_dates))


date_cut <- '2024-01-28 01:00:00'
cond <- data_used$date > as.POSIXct(date_cut, format = "%Y-%m-%d %H:%M")








forecast_values$`Hi 95`
data_used %>% 
  filter(cond) %>% 
  ggplot(aes(x = dates))+
  geom_line(aes(y = dependent, color = "day_ahead_price_de")) +
  geom_line(aes(y = fitted, color = "fitted")) +
  geom_line(data = forecast_values, aes(x = new_dates, y = `Point Forecast` ))+
  geom_ribbon(data = forecast_values, 
              aes(x = new_dates, ymin = `Lo 95`, ymax = `Hi 95`), fill = "skyblue", alpha = 0.3)+
  geom_ribbon(data = forecast_values, 
              aes(x = new_dates, ymin = `Lo 80`, ymax = `Hi 80`), fill = "blue", alpha = 0.3)+
  labs(title = "Day-Ahead Price",
       subtitle = "Germany",
       y = "Price day ahead", x= "Hourly data") +
  ggsci::scale_colour_jama() +
    theme(plot.margin = margin(10, 10, 10, 10),
          legend.title = element_blank(), 
          legend.position = 'bottom')


ggsave("8_Predictions.png", width = 16, height = 10, dpi = 300,
       units = "cm")  



data_used %>%
  ggplot(aes(x = dates))+
  geom_line(aes(y = resid))+
  ggsci::scale_colour_jama() +
  labs(title = "Residual Model",
       subtitle = "Germany",
       y = "Price day ahead", x= "Hourly data") +
  theme(plot.margin = margin(10, 10, 10, 10),
        legend.title = element_blank())


ggsave("9_Residuals.png", width = 16, height = 10, dpi = 300,
       units = "cm") 

summary(fit)


