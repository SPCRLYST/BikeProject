library(ggplot2)
library(zoo)
library(dplyr)
library(xtable)

# reading data from previously obtained google bigquery, Bike_Data_Pull.R
bike_stations <- readRDS("whatever you have")
bike_trips <- readRDS("whatever you have")

# start date
bike_trips$start_date <- ""
bike_trips$start_date <- as.Date(as.POSIXct(bike_trips$starttime))

# end date
bike_trips$end_date <- ""
bike_trips$end_date <- as.Date(as.POSIXct(bike_trips$stoptime))

# creating age estimate
bike_trips$age_est <- 2017 - bike_trips$birth_year

# restructing variables to proper type
bike_trips$start_station_id <- as.factor(bike_trips$start_station_id)
bike_trips$end_station_id <- as.factor(bike_trips$end_station_id)
bike_trips$bikeid <- as.factor(bike_trips$bikeid)
bike_trips$birth_year <- as.factor(bike_trips$birth_year)
bike_trips$usertype <- as.factor(bike_trips$usertype)
bike_trips$gender <- as.factor(bike_trips$gender)

# checking nas in the data
nadf <- as.data.frame(sapply(bike_trips, function(x) sum(is.na(x))))
colnames(nadf) <- c("Missing Variables")
na_xtab <- xtable(nadf, align=c("@{}l","c"), digits=2, caption = "Bike Trips Missing Variables")
print(na_xtab, scalebox = 0.6, comment = FALSE)

# most trips by start station
start_trip_count <- bike_trips %>%
  group_by(start_station_id) %>%
  summarise(trip_count=sum(!is.na(bikeid)))
# renaming variable for merge
names(start_trip_count)[names(start_trip_count)=="start_station_id"] <- "station_id"
# merging with station data frame to add specific to station count
station_start_count <- merge(start_trip_count, bike_stations[,c(1:2,7)], by = "station_id", all.x = TRUE)

# setting up an aggregate data frame of how many trip occured from each station on a certain day
start_trips_agg <- bike_trips %>%
  group_by(start_station_id, start_date) %>%
  summarise(trip_count=sum(!is.na(bikeid)))

# most trips by end station
end_trip_count <- bike_trips %>%
  group_by(end_station_id) %>%
  summarise(trip_count=sum(!is.na(bikeid)))
# renaming variable for merge
names(end_trip_count)[names(end_trip_count)=="end_station_id"] <- "station_id"
# merging with station data frame to add specific to station count
station_end_count <- merge(end_trip_count, bike_stations[,c(1:2,7)], by = "station_id", all.x = TRUE)

# setting up an aggregate data frame of how many trip occured from each station on a certain day
end_trips_agg <- bike_trips %>%
  group_by(end_station_id, end_date) %>%
  summarise(trip_count=sum(!is.na(bikeid)))
  
