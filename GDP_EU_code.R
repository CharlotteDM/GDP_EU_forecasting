library(tidymodels)
library(modeltime)
library(timetk)   
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

#filtering data from Poland
GDP_PL <- GDP[GDP$geo == "PL", ]

