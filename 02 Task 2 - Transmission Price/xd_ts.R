setwd("C:/Users/Uni2/Desktop/statLAB_local/02_Task 2")
rm(list = ls())
library(tidyverse)
data <- read.csv('data_cleaned_UTC.csv')
library(fastTS)
library(xts)


data$date <- as.POSIXct(data$date)

y <- xts(data$day_ahead_price_de,
       order.by = data$date)

x <- data %>% 
  select(-c(date, day_ahead_price_de))

x <- x %>%
  mutate(across(everything(), as.numeric)) %>% 
  as.matrix()

fit_LH <- fastTS(y, x)

summary(fit_LH)

res <- fit_LH$fits %>% unlist()

rr <- fit_LH$ncvreg_args$y
plot(fit_LH)
plot(rr)
plot(y)

ff <- predict(fit_LH)

