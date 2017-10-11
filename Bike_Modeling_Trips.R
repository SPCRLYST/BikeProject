library(ggplot2)
library(zoo)
library(dplyr)
library(xtable)
library(RCurl)
library(timeDate)

# reading data from previously created trip data; Bike_Modeling.R
start_trips <- readRDS("C:/Users/Tyler/Desktop/MSPA/MSPA 498/start_trips_agg.Rds")
end_trips <- readRDS("C:/Users/Tyler/Desktop/MSPA/MSPA 498/end_trips_agg.Rds")
