# install.packages('devtools') devtools::install_github("rstats-db/bigrquery")
library(devtools)
library(bigrquery)


# Project name for the group
project <- "animated-surfer-180415" # put your project ID here

# sql code to pull all stations
sql_stations <- "SELECT * FROM `bigquery-public-data.new_york.citibike_stations`"
# Execute the query and store the result for citi stations in NY
bike_stations <- query_exec(sql_stations, project = project, use_legacy_sql = FALSE)

# sql code to pull 1000 trips
sql_trips <- "SELECT * FROM `bigquery-public-data.new_york.citibike_trips` LIMIT 1000"
# Execute the query and store the result for trips in NY
bike_trips <- query_exec(sql_trips, project = project, use_legacy_sql = FALSE)

