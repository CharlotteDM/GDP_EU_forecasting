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
                   .title = "GDP in Poland", .x_lab = "Date", .y_lab = "current prices (million euro")
?plot_time_series

