# install.packages('devtools') devtools::install_github("rstats-db/bigrquery")
library(devtools)
library(bigrquery)
library(ggmap)
library(geosphere)

# Project name for the group
project <- "animated-surfer-180415" # put your project ID here

# sql code to pull all stations
sql_stations <- "SELECT * FROM `bigquery-public-data.new_york.citibike_stations`"
# Execute the query and store the result for citi stations in NY
bike_stations <- query_exec(sql_stations, project = project, use_legacy_sql = FALSE)
bike_stations$station_id <- as.factor(bike_stations$station_id)

# code to pull all of the trip from female, male, and unknown
# female sql code to pull all trips
f_sql_trips <- "SELECT tripduration
,starttime
,stoptime
,start_station_id
,start_station_latitude
,start_station_longitude
,end_station_id
,end_station_latitude
,end_station_longitude
,bikeid
,usertype
,birth_year
,gender
FROM `bigquery-public-data.new_york.citibike_trips`
WHERE gender = 'female'"
# Execute the query and store the result for trips in NY
f_bike_trips <- query_exec(f_sql_trips, project = project, use_legacy_sql = FALSE, max_pages = Inf)

# males trips need to be broken up in order to pull all of them 
# male sql code to pull all trips less than 595
m1_sql_trips <- "SELECT tripduration
,starttime
,stoptime
,start_station_id
,start_station_latitude
,start_station_longitude
,end_station_id
,end_station_latitude
,end_station_longitude
,bikeid
,usertype
,birth_year
,gender
FROM `bigquery-public-data.new_york.citibike_trips`
WHERE gender = 'male'
AND tripduration <= 500"
# Execute the query and store the result for trips in NY
m1_bike_trips <- query_exec(m1_sql_trips, project = project, use_legacy_sql = FALSE, max_pages = Inf)

# male sql code to pull all trips greater than or equal to 595 and less than 1145
m2_sql_trips <- "SELECT tripduration
,starttime
,stoptime
,start_station_id
,start_station_latitude
,start_station_longitude
,end_station_id
,end_station_latitude
,end_station_longitude
,bikeid
,usertype
,birth_year
,gender
FROM `bigquery-public-data.new_york.citibike_trips`
WHERE gender = 'male'
AND tripduration BETWEEN 501 AND 900"
# Execute the query and store the result for trips in NY
m2_bike_trips <- query_exec(m2_sql_trips, project = project, use_legacy_sql = FALSE, max_pages = Inf)

# male sql code to pull all trips greater than or equal to 595 and less than 1145
m3_sql_trips <- "SELECT tripduration
,starttime
,stoptime
,start_station_id
,start_station_latitude
,start_station_longitude
,end_station_id
,end_station_latitude
,end_station_longitude
,bikeid
,usertype
,birth_year
,gender
FROM `bigquery-public-data.new_york.citibike_trips`
WHERE gender = 'male'
AND tripduration > 900"
# Execute the query and store the result for trips in NY
m3_bike_trips <- query_exec(m3_sql_trips, project = project, use_legacy_sql = FALSE, max_pages = Inf)
# creating complete male bike trips data frame
m_bike_trips <- rbind(m1_bike_trips, m2_bike_trips, m3_bike_trips)

# unknown sql code to pull all trips
u_sql_trips <- "SELECT tripduration
,starttime
,stoptime
,start_station_id
,start_station_latitude
,start_station_longitude
,end_station_id
,end_station_latitude
,end_station_longitude
,bikeid
,usertype
,birth_year
,gender
FROM `bigquery-public-data.new_york.citibike_trips`
WHERE gender = 'unknown'"
# Execute the query and store the result for trips in NY
u_bike_trips <- query_exec(u_sql_trips, project = project, use_legacy_sql = FALSE, max_pages = Inf)

# combining all data frames
full_trips <- rbind(f_bike_trips, u_bike_trips, m_bike_trips)

# method 1
# needs to be changed to include everything, ggmap seems to have a limit
quick_bframe <- full_trips[1:25,]
mdist <- apply(quick_bframe, 1, function(x){
  f = as.numeric(c(x[['start_station_longitude']], x[['start_station_latitude']]))
  t = as.numeric(c(x[['end_station_longitude']], x[['end_station_latitude']]))
  mapdist(f, t)
})
mapDist <- do.call(rbind, mdist)
# adding miles to the whole shortened dataframe
quick_bframe$miles <- mapDist$miles

# method 2
# geosphere test to see how many it can process
full_trips$meters <- distHaversine(p1 = matrix(c(full_trips$start_station_longitude, full_trips$start_station_latitude), ncol = 2),
                                   p2 = matrix(c(full_trips$end_station_longitude, full_trips$end_station_latitude), ncol = 2))
# it seems like geosphere works fine, the ggmap might be working better since it seems to be actual road distance

# writing full data frame to a tab txt
saveRDS(bike_stations, "C:/Users/Tyler/Desktop/MSPA/MSPA 498/bike_stations.Rds")
saveRDS(full_trips, "C:/Users/Tyler/Desktop/MSPA/MSPA 498/full_trips.Rds")
