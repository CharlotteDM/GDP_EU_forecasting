library(tidymodels)
install.packages("modeltime")
library(modeltime)
library(timetk)   
library(zoo)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
install.packages("highcharter")
library(highcharter)
install.packages("ggfortify")
library(ggfortify)
library(coefplot)
library(boot)
library(GGally)
library(xgboost)
library(useful)
library(plotly)
library(generics)
renv::init()

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


#Bibliography: 
#___https://cran.r-project.org/web/packages/modeltime/vignettes/getting-started-with-modeltime.html
#___Lander, J. P. (2014). R for everyone: Advanced analytics and graphics. Pearson Education.
#___https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20%26%20Anomaly%20Detection/Time%20Series%20Forecasting%20using%20ModelTime.R


#loading data: GDP (and main components - output, expenditure, income)
#source of data: https://ec.europa.eu/eurostat/databrowser/view/namq_10_gdp/default/table?lang=en
GDP <- read.csv("gdp_quart_eu.csv", stringsAsFactors = F)

#quarter as a date
GDP$TIME_PERIOD <- yq(GDP$TIME_PERIOD)   

#filtering data from Poland
GDP_PL <- GDP[GDP$geo == "PL", ]




#visualization plot time
plot_GDP_PL <- GDP_PL %>%
 plot_time_series(TIME_PERIOD, OBS_VALUE, .interactive = T, 
                  .title = "GDP in Poland", .x_lab = "Date", .y_lab = "current prices (million euro)")
plot_GDP_PL

#splitting data into train and test sets
splits <- GDP_PL %>% time_series_split(assess = "2 years", cumulative = TRUE)


#visualization: train & test data
vis <- splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(TIME_PERIOD, OBS_VALUE, .interactive = TRUE)
vis


#Auto ARIMA Model
mod_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  generics::fit(OBS_VALUE ~ TIME_PERIOD, training(splits))
mod_fit_arima


# Fit PROPHET Model
mod_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  generics::fit(OBS_VALUE ~ TIME_PERIOD, training(splits))
mod_fit_prophet


#Error Trend Season Model (ET) with function Expotential Smoothing State Space
mod_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  generics::fit(OBS_VALUE ~ TIME_PERIOD, data = training(splits))
mod_fit_ets


#Linear Regression Model
mod_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(OBS_VALUE ~ as.numeric(TIME_PERIOD), data = training(splits))


#Modeltime Table
models_tbl <- modeltime_table(
  mod_fit_arima,
  mod_fit_ets,
  mod_fit_prophet,
  mod_fit_lm
)
models_tbl


#Calibrating the model for testing data
calib_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calib_tbl



#Interactive Forecast Plot Visualization for All Models
forecast_plot <- calib_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = GDP_PL) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = T)
forecast_plot #model prophet looks the best


#Accuracy Metrics
calib_acc_tbl <- calib_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = T)
calib_acc_tbl #model prophet is performing the best, MAE=17258.46


#refitting the Models to the Full Dataset 
refit_tbl <- calib_tbl %>%
  modeltime_refit(data = GDP_PL)
refit_tbl <- refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = GDP_PL) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = T)
refit_tbl


#Preprocessing
recipe_spec <- recipe(OBS_VALUE ~ TIME_PERIOD, training(splits)) %>%
  step_timeseries_signature(TIME_PERIOD) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(TIME_PERIOD, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()






#filtering data from EU countries
GDP_EU <- filter(GDP, geo == "PL" | geo == "AT" | geo == "BE" | geo == "BG" | geo == "CH" |
                   geo == "CY" |geo == "CZ" | geo == "DE" | geo == "EE" |geo == "FI" | 
                   geo == "EL" |  geo == "FR" | geo == "NL" |geo == "ES" | geo == "IE" | 
                   geo == "LV" |geo == "LU" | geo == "LT" | geo == "MT" |geo == "DE" |
                   geo == "PT" |  geo == "RO" |geo == "SI" | geo == "SK" | geo == "SE" |
                   geo == "HU" | geo == "IT" )



#setting own palette
own_pal <- c("coral", "deeppink", "darkturquoise", "darkred", "darkmagenta", "blue3", "cyan3", "darkorchid1",
                "burlywood", "blueviolet", "aquamarine1", "darkolivegreen", "gold", "firebrick2", "dodgerblue2",
             "greenyellow", "lightpink", "mediumpurple1", "maroon1", "lightskyblue", "lightsalmon3", "hotpink4",
             "plum", "sienna", "violet", "thistle3", "tomato1")

#Ggplot: GDP in EU Countries
GDP_EU_plot <- ggplot(GDP_EU, aes(TIME_PERIOD, OBS_VALUE, color = geo)) +
  geom_line() +
  labs(
  title = "GDP in EU Countries",
  subtitle = "current prices, million euro",
  caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/NAMQ_10_GDP__custom_4050464/default/table?lang=en)",
  x = "Time",
  y = "GDP") +
  scale_colour_manual(values=own_pal) +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
    plot.subtitle = element_text(color="royalblue4", size=10, hjust = 0.5),
    plot.caption = element_text(color="darkblue", size=10, hjust = 0.5),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold")) 
GDP_EU_plot 
#plotly
ggplotly(GDP_EU_plot) 


#loading data: GDP - real GDP (euro, per capita)
#source of data: https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en
GDP_percapita <- read.csv("GDP_EU_Real_percapita.csv", stringsAsFactors = F)

GDP_percapita <- GDP_percapita[-c(846:1686), ]

#time period as a date
GDP_percapita$TIME_PERIOD <- as.Date(as.character(GDP_percapita$TIME_PERIOD), format = "%Y")
class(GDP_percapita$TIME_PERIOD)


#filtering data from EU countries - real GDP per capita
GDP_EU_pcpt <- filter(GDP_percapita, geo == "PL" | geo == "AT" | geo == "BE" | geo == "BG" | geo == "CH" |
                   geo == "CY" |geo == "CZ" | geo == "DE" | geo == "EE" |geo == "FI" | 
                   geo == "EL" |  geo == "FR" | geo == "NL" |geo == "ES" | geo == "IE" | 
                   geo == "LV" |geo == "LU" | geo == "LT" | geo == "MT" |geo == "DE" |
                   geo == "PT" |  geo == "RO" |geo == "SI" | geo == "SK" | geo == "SE" |
                   geo == "HU" | geo == "IT" )



#Ggplot: GDP real per capita in EU Countries 
GDP_EU_pcpt_plot <- ggplot(GDP_EU, aes(TIME_PERIOD, OBS_VALUE, color = geo)) +
  geom_line() +
  labs(
    title = "Real GDP in EU Countries",
    subtitle = "in euro, per capita",
    caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en)",
    x = "Time",
    y = "GDP") +
  scale_colour_manual(values=own_pal) +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
    plot.subtitle = element_text(color="royalblue4", size=10, hjust = 0.5),
    plot.caption = element_text(color="darkblue", size=10, hjust = 0.5),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold")) 
GDP_EU_pcpt_plot 
#plotly
ggplotly(GDP_EU_pcpt_plot) 


#Conclusions:
#The highest absolute GDP value is observed in Germany. The highest values GDP per capita is observed in Luxembourg

#PL & DE - comparison
GDP_PLDE_pcpt <- filter(GDP_EU, geo == "PL" | geo == "DE")

#Ggplot: GDP real per capita in Poland & Germany
GDP_PLDE_pcpt_plot <- ggplot(GDP_PLDE_pcpt, aes(TIME_PERIOD, OBS_VALUE, color = geo)) +
  geom_line() +
  labs(
    title = "Real GDP in Poland & Germany",
    subtitle = "in euro, per capita",
    caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en)",
    x = "Time",
    y = "GDP") +
  scale_colour_manual(values=own_pal) +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
    plot.subtitle = element_text(color="royalblue4", size=10, hjust = 0.5),
    plot.caption = element_text(color="darkblue", size=10, hjust = 0.5),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold")) 
GDP_PLDE_pcpt_plot 
#plotly
ggplotly(GDP_PLDE_pcpt_plot) 
