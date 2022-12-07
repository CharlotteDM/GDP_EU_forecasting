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
forecast_plot <- calib_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = GDP_PL_new) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = T)
forecast_plot #model prophet looks the best

#accuracy metrics
calib_acc_tbl <- calib_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = T)
calib_acc_tbl #model prophet is performing the best, MAE=6068.66


#refit the models to the full dataset 
refit_tbl <- calib_tbl %>%
  modeltime_refit(data = GDP_PL_new)
refit_tbl <- refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = GDP_PL_new) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = T)
refit_tbl

#preprocessing
recipe_spec <- recipe(OBS_VALUE ~ TIME_PERIOD, training(splits)) %>%
  step_timeseries_signature(TIME_PERIOD) %>%
  #step_rm(contains("am.pm"), contains("hour"), contains("minute"),
         # contains("second"), contains("xts")) %>%
  step_fourier(TIME_PERIOD, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()


#Elastic Net
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(TIME_PERIOD)) %>%
  fit(training(splits))
workflow_fit_glmnet


#filtering data from EU countries
GDP_EU <- GDP[GDP$geo == c("PL","AT","BE", "BG", "CH", "CY", "CZ", "DE", "EE", "FI", 
                           "EL", "FR", "NL", "ES", "IE", "LV", "LU", "LT", "MT", "DE",
                           "PT", "RO", "SI", "SK", "SE", "HU", "IT"),  ]

GDP_EU <- filter(GDP, geo == "PL" | geo == "AT" | geo == "BE" | geo == "BG" | geo == "CH" |
                   geo == "CY" |geo == "CZ" | geo == "DE" | geo == "EE" |geo == "FI" | 
                   geo == "EL" |  geo == "FR" | geo == "NL" |geo == "ES" | geo == "IE" | 
                   geo == "LV" |geo == "LU" | geo == "LT" | geo == "MT" |geo == "DE" |
                   geo == "PT" |  geo == "RO" |geo == "SI" | geo == "SK" | geo == "SE" |
                   geo == "HU" | geo == "IT" )

#choosing only three needed columns
GDP_EU_new <- GDP_EU %>% select(7, 8,9)
