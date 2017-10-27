library(ggplot2)
library(zoo)
library(dplyr)
library(xtable)
library(RCurl)
library(timeDate)
library(devtools)
library(bigrquery)
library(ggmap)
library(stringr)

# reading data from previously obtained google bigquery, Bike_Data_Pull.R
bike_stations <- readRDS("C:/Users/Tyler/Desktop/MSPA/MSPA 498/bike_stations.Rds")
# https://www.dropbox.com/s/mtsm70lnda94kuz/full_trips.Rds?dl=0 , I'm trying to figure out if I can get dropbox to work
bike_trips <- readRDS("C:/Users/Tyler/Desktop/MSPA/MSPA 498/full_trips.Rds")
#weather data
weatherURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/BikeProject/master/NYC_Weather.csv")
nyc_weather <- read.csv(text = weatherURL)


###########################################################################################################################
# data manipulation and variable creation section
# renaming the weather date column
names(nyc_weather)[names(nyc_weather)=="DATE"] <- "start_date"
nyc_weather$start_date <- as.Date(as.character(nyc_weather$start_date), format="%m/%d/%Y")
# adding all weather data to the entire data frame is too large, this is done later in the code when needed

# trip data frame modifications
# start date
bike_trips$start_date <- ""
bike_trips$start_date <- as.Date(as.POSIXct(bike_trips$starttime))


bike_trips <- merge(bike_trips, nyc_weather, by = "start_date", all.x = TRUE)

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

# start trip data frame creation
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
# adding weather statistics to the start trip aggregate data frame
start_trips_agg <- merge(start_trips_agg, nyc_weather, by = "start_date", all.x = TRUE)

# building date structure as discussed for modeling
# long form day
start_trips_agg$day <- weekdays(as.Date(start_trips_agg$start_date,'%Y-%m-%d'))
# long form month
start_trips_agg$month <- strftime(start_trips_agg$start_date, "%B")
# getting all four seasons
start_trips_agg$season <- ""
start_trips_agg$season <- as.character(start_trips_agg$season)
start_trips_agg$season[start_trips_agg$month == "March" | 
                         start_trips_agg$month == "April" |  
                         start_trips_agg$month == "May"] <- "Spring"
start_trips_agg$season[start_trips_agg$month == "June" | 
                         start_trips_agg$month == "July" |  
                         start_trips_agg$month == "August"] <- "Summer"
start_trips_agg$season[start_trips_agg$month == "September" | 
                         start_trips_agg$month == "October" |  
                         start_trips_agg$month == "November"] <- "Fall"
start_trips_agg$season[start_trips_agg$month == "December" | 
                         start_trips_agg$month == "January" |  
                         start_trips_agg$month == "February"] <- "Winter"
# identifying holidays
holidays <- c(as.Date("2013-01-01"), 
              as.Date(Easter(2013)),
              as.Date(USIndependenceDay(2013)),
              as.Date(USLaborDay(2013)),
              as.Date(USThanksgivingDay(2013)),
              as.Date(USMemorialDay(2013)),
              as.Date("2013-12-25"),
              as.Date("2013-12-31"),
              as.Date("2014-01-01"), 
              as.Date(Easter(2014)),
              as.Date(USIndependenceDay(2014)),
              as.Date(USLaborDay(2014)),
              as.Date(USThanksgivingDay(2014)),
              as.Date(USMemorialDay(2014)),
              as.Date("2014-12-25"),
              as.Date("2014-12-31"),
              as.Date("2015-01-01"), 
              as.Date(Easter(2015)),
              as.Date(USIndependenceDay(2015)),
              as.Date(USLaborDay(2015)),
              as.Date(USThanksgivingDay(2015)),
              as.Date(USMemorialDay(2015)),
              as.Date("2015-12-25"),
              as.Date("2015-12-31"),
              as.Date("2016-01-01"), 
              as.Date(Easter(2016)),
              as.Date(USIndependenceDay(2016)),
              as.Date(USLaborDay(2016)),
              as.Date(USThanksgivingDay(2016)),
              as.Date(USMemorialDay(2016)),
              as.Date("2016-12-25"),
              as.Date("2016-12-31"))
start_trips_agg$holiday <- ifelse(start_trips_agg$start_date %in% holidays, 1, 0)

# end trip data frame creation
# most trips by end station
end_trip_count <- bike_trips %>%
  group_by(end_station_id) %>%
  summarise(trip_count=sum(!is.na(bikeid)))
# renaming variable for merge
names(end_trip_count)[names(end_trip_count)=="end_station_id"] <- "station_id"
# merging with station data frame to add specific to station count
station_end_count <- merge(end_trip_count, bike_stations[,c(1:2,7)], by = "station_id", all.x = TRUE)

# setting up an aggregate data frame of how many trips occured from each station on a certain day
end_trips_agg <- bike_trips %>%
  group_by(end_station_id, end_date) %>%
  summarise(trip_count=sum(!is.na(bikeid)))
# adding weather statistics to the start trip aggregate data frame
end_trips_agg <- merge(end_trips_agg, nyc_weather, by.x = "end_date", by.y = "start_date", all.x = TRUE)

# building date structure as discussed for modeling
# long form day
end_trips_agg$day <- weekdays(as.Date(end_trips_agg$end_date,'%Y-%m-%d'))
# long form month
end_trips_agg$month <- strftime(end_trips_agg$end_date, "%B")
# getting all four seasons
end_trips_agg$season <- ""
end_trips_agg$season <- as.character(end_trips_agg$season)
end_trips_agg$season[end_trips_agg$month == "March" | 
                         end_trips_agg$month == "April" |  
                         end_trips_agg$month == "May"] <- "Spring"
end_trips_agg$season[end_trips_agg$month == "June" | 
                         end_trips_agg$month == "July" |  
                         end_trips_agg$month == "August"] <- "Summer"
end_trips_agg$season[end_trips_agg$month == "September" | 
                         end_trips_agg$month == "October" |  
                         end_trips_agg$month == "November"] <- "Fall"
end_trips_agg$season[end_trips_agg$month == "December" | 
                         end_trips_agg$month == "January" |  
                         end_trips_agg$month == "February"] <- "Winter"
# identifying holidays
end_trips_agg$holiday <- ifelse(end_trips_agg$end_date %in% holidays, 1, 0)

# Project name for the group
project <- "animated-surfer-180415" # put your project ID here

# sql code to pull all stations
start_stations_sql <- "SELECT DISTINCT start_station_id
,start_station_name
,start_station_latitude
,start_station_longitude
FROM `bigquery-public-data.new_york.citibike_trips`"
# Execute the query and store the result for citi stations
start_bike_stations <- query_exec(start_stations_sql, project = project, use_legacy_sql = FALSE, max_pages = Inf)

# sql code to pull all stations
end_stations_sql <- "SELECT DISTINCT end_station_id
,end_station_name
,end_station_latitude
,end_station_longitude
FROM `bigquery-public-data.new_york.citibike_trips`"
# Execute the query and store the result for citi stations
end_bike_stations <- query_exec(end_stations_sql, project = project, use_legacy_sql = FALSE, max_pages = Inf)

# adding zipcode to the data frame
# adding zipcode to better group stations for starting stations
result <- do.call(rbind,
                  lapply(1:nrow(start_bike_stations),
                         function(i)revgeocode(as.numeric(start_bike_stations[i,4:3]))))
start_bike_stations <- cbind(start_bike_stations,result)
start_bike_stations$zipcode <- substr(str_extract(start_bike_stations$address," [0-9]{5}, .+"),2,6)
start_bike_stations$address <- NULL

###########################################################################################################################
# writing data to RDS
saveRDS(start_trips_agg, "C:/Users/Tyler/Desktop/MSPA/MSPA 498/start_trips_agg.Rds")
saveRDS(end_trips_agg, "C:/Users/Tyler/Desktop/MSPA/MSPA 498/end_trips_agg.Rds")
saveRDS(start_bike_stations, "C:/Users/Tyler/Desktop/MSPA/MSPA 498/start_bike_stations.Rds")
saveRDS(end_bike_stations, "C:/Users/Tyler/Desktop/MSPA/MSPA 498/end_bike_stations.Rds")
###########################################################################################################################
# data exploration section
# function for multiplotting
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# distribution trips by age and gender
ggplot(subset(bike_trips, gender != "unknown"), aes(x = age_est, fill = gender)) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 50) +
  facet_grid(gender ~ .)
