library(tidymodels)
library(modeltime)
library(timetk)   
#library(zoo)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(highcharter)
library(ggfortify)
library(coefplot)
library(boot)
library(GGally)



path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

#loading data: GDP
#source of data: https://ec.europa.eu/eurostat/databrowser/view/namq_10_gdp/default/table?lang=en
GDP <- read.csv("gdp_quart_eu.csv", stringsAsFactors = F)

#quarter as a date
GDP$TIME_PERIOD <- yq(GDP$TIME_PERIOD)   

                           
#filtering data from Poland
GDP_PL <- GDP[GDP$geo == "PL", ]

#visualization plot time
GDP_PL %>%
  plot_time_series(TIME_PERIOD, OBS_VALUE, .interactive = T, 
                   .title = "GDP in Poland", .x_lab = "Date", .y_lab = "current prices (million euro)")

#splitting data into train and test sets

#splits <- initial_time_split(GDP_PL, prop = 0.9)
  
  
splits <- GDP_PL %>%time_series_split(assess = "2 years", cumulative = TRUE)


#train_GDP_PL <- GDP_PL[GDP_PL$TIME_PERIOD < "2021-01-01",]
#test_GDP_PL <- GDP_PL[GDP_PL$TIME_PERIOD > "2021-01-01",]

#visualization: train & test data
vis <- splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(TIME_PERIOD, OBS_VALUE, .interactive = T)
vis


#autoarima
model_spec <- arima_reg() %>%
  set_engine("auto_arima")
# Fit Spec
model_fit <- model_spec %>%
  fit(value ~ TIME_PERIOD, data = training(splits))
model_fit



mod_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(log(value) ~ splits$TIME_PERIOD, training(splits))
?arima_reg

