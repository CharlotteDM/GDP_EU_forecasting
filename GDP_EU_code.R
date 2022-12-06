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
library(xgboost)



path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

#loading data: GDP
#source of data: https://ec.europa.eu/eurostat/databrowser/view/namq_10_gdp/default/table?lang=en
GDP <- read.csv("gdp_quart_eu.csv", stringsAsFactors = F)

#quarter as a date
GDP$TIME_PERIOD <- yq(GDP$TIME_PERIOD)   

#filtering data from Poland
GDP_PL <- GDP[GDP$geo == "PL", ]

#choosing only two needed columns
GDP_PL_new <- GDP_PL %>% select(8,9)

#visualization plot time
plot_GDP_PL <- GDP_PL_new %>%
 plot_time_series(TIME_PERIOD, OBS_VALUE, .interactive = T, 
                  .title = "GDP in Poland", .x_lab = "Date", .y_lab = "current prices (million euro)")
plot_GDP_PL

#splitting data into train and test sets
splits <- GDP_PL_new %>% time_series_split(assess = "2 years", cumulative = TRUE)



#visualization: train & test data
vis <- splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(TIME_PERIOD, OBS_VALUE, .interactive = TRUE)
vis

#autoarima
mod_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(OBS_VALUE ~ TIME_PERIOD, training(splits))
mod_fit_arima

#prophet
mod_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(OBS_VALUE ~ TIME_PERIOD, training(splits))
mod_fit_prophet

#ets
mod_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(OBS_VALUE ~ TIME_PERIOD, data = training(splits))
mod_fit_ets

#modeltime table
models_tbl <- modeltime_table(
  mod_fit_arima,
  mod_fit_ets,
  mod_fit_prophet
)
models_tbl

#calibration
calib_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calib_tbl

#interactive forecast plot visualization for all models
calib_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = GDP_PL_new) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = T)
calib_tbl #model prophet looks the best

#accuracy metrics
calib_acc_tbl <- calib_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = T)
calib_acc_tbl #model prophet is performing the best, MAE=6068.66
