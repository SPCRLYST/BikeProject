library(ggplot2)
library(zoo)
library(dplyr)
library(xtable)
library(RCurl)
library(timeDate)

# reading data from previously created trip data; Bike_Modeling.R
start_trips <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/start_trips_agg.Rds?raw=true")))
end_trips <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/end_trips_agg.Rds?raw=true")))
