rm(list = ls())
library(tidyverse)
library(corrplot)

#Import data
setwd("path")
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

data %>% colnames()

#New colum
#day_ahead_price_de_ch : day_ahead_price_de - day_ahead_price_ch - auction_price
data$margin <- data$day_ahead_price_de - data$day_ahead_price_ch - data$auction_price



#######           PLOT PRICES de - ch - auc - margin full time window


#Plot day_ahead_price_ch wrt time 
ggplot(data = data, aes(x = date, y = day_ahead_price_ch)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Hourly data", y = "Electricity price ch") 


#Plot day_ahead_price_de wrt time 
ggplot(data = data, aes(x = date, y = day_ahead_price_de)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Hourly data", y = "Electricity price de") 

#Plot auction_price wrt time 
ggplot(data = data, aes(x = date, y = auction_price)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Hourly data", y = "JAO auction") 

#Plot margin wrt time 
ggplot(data = data, aes(x = date, y = margin)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Hourly data", y = "Margin")


#Plot prices
ggplot(data = data, aes(x = date)) +
  geom_line(aes(y = day_ahead_price_de, color = "day_ahead_price_de")) +
  geom_line(aes(y = day_ahead_price_ch, color = "day_ahead_price_ch")) +
  geom_line(aes(y = auction_price, color = "auction_price")) +
  labs(title = "Prices day ahead",
       x = "Hourly data",
       y = "Price day ahead",
       color = "Day ahead prices") +
  scale_color_manual(values = c("day_ahead_price_de" = "blue", "day_ahead_price_ch" = "red",
                                "auction_price" = "green"))+
  theme_minimal()



#######           PLOT PRICES de - ch - auction - margin PARTIAL time window

#Consider a window of time that is reasonably 'stationary'
date_cut <- '2023-01-01 01:00:00'
cond <- data$date > as.POSIXct(date_cut, format = "%Y-%m-%d %H:%M")
data_window <- subset(data, cond)

#SEEMS STATIONARY: understand why after drop beggining 2023 eyear verything seems
#quite cool!

#Should we give accountability of what happend in the past this date? The data
#before pandemic seems quite stationary but less volatile than current situaution.
#Idk if we can use this past value somehow, to porform some coparison or we should just 
#disregardem


#Plot prices in a 'reasonable' time window
ggplot(data = data_window, aes(x = date)) +
  geom_line(aes(y = day_ahead_price_de, color = "day_ahead_price_de")) +
  geom_line(aes(y = day_ahead_price_ch, color = "day_ahead_price_ch")) +
  geom_line(aes(y = auction_price, color = "auction_price")) +
  labs(title = "Prices day ahead",
       x = "Hourly data",
       y = "Price day ahead",
       color = "Day ahead prices") +
  scale_color_manual(values = c("day_ahead_price_de" = "blue", "day_ahead_price_ch" = "red",
                                "auction_price" = "green"))+
  theme_minimal()

#Plot margin in a 'reasonable' time window
ggplot(data = data_window, aes(x = date, y = margin)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Hourly data", y = "Margin")



#######           COVARIATES

#FULL TIME WINDOW
#Creating  a matrix with all covariates delayed of 1 wrt y (lose a row)
dependent <- data %>% select(c(auction_price, day_ahead_price_ch, day_ahead_price_de, margin))
dependent <- dependent[1:(nrow(dependent)-1),]

covariates <- data %>% select(-c(margin, date, auction_price, day_ahead_price_ch, day_ahead_price_de))
covariates <- covariates %>% select(where(~ !is.factor(.)))
covariates <- covariates[2:nrow(covariates),]

data_shifted <- bind_cols(dependent, covariates)


#Calculate correlation matrix omitting NA values & PLOT
correlation_matrix1 <- cor(data_shifted[,1:13], use = "pairwise.complete.obs")
corrplot(correlation_matrix1, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)

correlation_matrix2 <- cor(data_shifted[,c(1:4, 13:23)], use = "pairwise.complete.obs")
corrplot(correlation_matrix2, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)

correlation_matrix3 <- cor(data_shifted[,c(1:4, 23:33)], use = "pairwise.complete.obs")
corrplot(correlation_matrix3, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)

correlation_matrix3 <- cor(data_shifted[,c(1:4, 33:45)], use = "pairwise.complete.obs")
corrplot(correlation_matrix3, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)


#WINDOW 2023
dependent <- data_window %>% select(c(auction_price, day_ahead_price_ch, day_ahead_price_de, margin))
dependent <- dependent[1:(nrow(dependent)-1),]

covariates <- data_window %>% select(-c(margin, date, auction_price, day_ahead_price_ch, day_ahead_price_de))
covariates <- covariates %>% select(where(~ !is.factor(.)))
covariates <- covariates[2:nrow(covariates),]

data_shifted <- bind_cols(dependent, covariates)


#Calculate correlation matrix omitting NA values & PLOT
correlation_matrix1 <- cor(data_shifted[,1:13], use = "pairwise.complete.obs")
corrplot(correlation_matrix1, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)

correlation_matrix2 <- cor(data_shifted[,c(1:4, 13:23)], use = "pairwise.complete.obs")
corrplot(correlation_matrix2, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)

correlation_matrix3 <- cor(data_shifted[,c(1:4, 23:33)], use = "pairwise.complete.obs")
corrplot(correlation_matrix3, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)

correlation_matrix3 <- cor(data_shifted[,c(1:4, 33:45)], use = "pairwise.complete.obs")
corrplot(correlation_matrix3, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.5)

#Now selecting covariates that are resaonbly correlated with the ys
#plot of covaraites and y_s 

#Model with arimax:
#fit arima on residuals of lm(y ~ covariates) to define (p,q)
#arimax(y, x = covariates, setting(p, q))


#We should do a grouping of distinc covariates:
# temperature
# forecasting
# prices
# observed values ecc.


#Considerinbg the time_window I think this acf are nice
acf(data_window$day_ahead_price_ch)
acf(data_window$day_ahead_price_de)
acf(data_window$day_ahead_price_de_ch)