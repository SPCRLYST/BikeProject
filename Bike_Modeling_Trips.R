library(ggplot2)
library(zoo)
library(dplyr)
library(xtable)
library(RCurl)
library(timeDate)
library(rpart)
library(rpart.plot)
library(zipcode)
library(ggmap)
library(stringr)
library(randomForest)

# reading data from previously created trip data; Bike_Modeling.R
start_trips <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/start_trips_agg.Rds?raw=true")))
end_trips <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/end_trips_agg.Rds?raw=true")))
start_bike_stations <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/start_bike_stations.Rds?raw=true")))
end_bike_stations <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/end_bike_stations.Rds?raw=true")))

# merging data frames to include zipcode
start_trips <- merge(start_trips, start_bike_stations[,c(1,5)], by = "start_station_id", all = TRUE)

# modeling data frames
mod_df_start <- start_trips[,c(3:14)]

# getting variables in the right format
cols <- c(8:12)
mod_df_start[cols] <- lapply(mod_df_start[cols], factor)
mod_df_start$trip_count <- as.numeric(mod_df_start$trip_count)

# checking for missing variables
nadf <- as.data.frame(sapply(mod_df_start, function(x) sum(is.na(x))))
colnames(nadf) <- c("Missing Variables")
na_xtab <- xtable(nadf, align=c("@{}l","l"), digits=2, caption = "Citi Start Trips Missing Variables")
print(na_xtab, scalebox = 0.80, comment = FALSE)

# remove observations with missing zipcodes and AWND
mod_df_start <- mod_df_start[complete.cases(mod_df_start), ]

# rechecking for missing variables
nadf2 <- as.data.frame(sapply(mod_df_start, function(x) sum(is.na(x))))
colnames(nadf2) <- c("Missing Variables")
na2_xtab <- xtable(nadf2, align=c("@{}l","l"), digits=2, caption = "Citi Start Trips Missing Variables")
print(na2_xtab, scalebox = 0.80, comment = FALSE)

# forumla for modeling variables
n <- names(mod_df_start[,c(2:12)])
f <- as.formula(paste("trip_count ~",
                       paste(n[!n %in% "trip_count"],
                             collapse = " + ")))

# dividing test and training data sets
set.seed(seed)
ratio_sep <- 0.75
index_s <- sample(1:nrow(mod_df_start), round(ratio_sep*nrow(mod_df_start)))

# test and training start and end trip data frames
train.start <- mod_df_start[index_s,]
test.start <- mod_df_start[-index_s,]

# modeling
# backward variable selection
library(leaps)
set.seed(seed)
regfit.bwd.start <- regsubsets(f, data = train.start, nvmax = 12, method = "backward")  
bkd.start.lmsum <- summary(regfit.bwd.start)
par(mfrow=c(1,3))
plot(bkd.start.lmsum$adjr2 ,xlab = "Number of Variables", ylab = "Adjusted RSq")
plot(bkd.start.lmsum$bic ,xlab = "Number of Variables", ylab = "BIC")
plot(bkd.start.lmsum$cp ,xlab = "Number of Variables", ylab = "CP")
bkd.lm.start <- as.data.frame(coef(regfit.bwd.start, 12))

# forward variable selection
set.seed(seed)
regfit.fwd.start <- regsubsets(f, data = train.start, nvmax = 12, method = "forward")  
fwd.start.lmsum <- summary(regfit.fwd.start)
par(mfrow=c(1,3))
plot(fwd.start.lmsum$adjr2 ,xlab = "Number of Variables", ylab = "Adjusted RSq")
plot(fwd.start.lmsum$bic ,xlab = "Number of Variables", ylab = "BIC")
plot(fwd.start.lmsum$cp ,xlab = "Number of Variables", ylab = "CP")
fwd.lm.start <- as.data.frame(coef(regfit.fwd.start, 12))

# stepwise variable selection
set.seed(seed)
regfit.step.start = regsubsets(f, data = train.start, nvmax = 12, method = "seqrep")  
step.start.lmsum <- summary(regfit.step.start)
par(mfrow=c(1,3))
plot(step.start.lmsum$adjr2 ,xlab = "Number of Variables", ylab = "Adjusted RSq")
plot(step.start.lmsum$bic ,xlab = "Number of Variables", ylab = "BIC")
plot(step.start.lmsum$cp ,xlab = "Number of Variables", ylab = "CP")
steps.lm.start <- as.data.frame(coef(regfit.step.start, 12))

# making a data frame of coefficients for all selection methods
leaps_dfs <- cbind(bkd.lm.start, fwd.lm.start, steps.lm.start)
colnames(leaps_dfs) <- c("Backward Variable Selection","Forward Variable Selection","Stepwise Variable Selection")
# basically our selection models told us that all variables were important

# regression using all variables
# Least squares regression
set.seed(seed)
model.ls1 <- lm(f, train.start)
pred.valid.ls1 <- predict(model.ls1, newdata = test.start) # validation predictions
hist(pred.valid.ls1)
start_mean <- as.data.frame((test.start$trip_count - pred.valid.ls1)^2)
start_mean[is.na(start_mean)] <- 0
start_mean_error <- mean(start_mean$`(test.start$trip_count - pred.valid.ls1)^2`) # mean prediction error
start_mean_error
# 2815.817
start_std_error <- sd(start_mean$`(test.start$trip_count - pred.valid.ls1)^2`)/sqrt(length(test.start$trip_count)) # std error
start_std_error
# 34.33616
par(mfrow=c(1,1))
plot(test.start$trip_count,pred.valid.ls1,xlab="Known Trips",ylab="Regression Estimate",col='red',
     main='Real vs Predicted Trips Regression',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Regression',pch=18,col='red', bty='n')

# playing with the data set to get matrix format for use with nueral networks and random forests
# creating a matrix form of the data to use with more interesting model types
mod_matrix_start <- cbind(mod_df_start[,1:7], 
                          model.matrix(~ . + 0, 
                                       data= mod_df_start[,8:12], 
                                       contrasts.arg = lapply(mod_df_start[,8:12], 
                                                              contrasts, 
                                                              contrasts=FALSE)))

# forumla for modeling variables
n1 <- names(mod_matrix_start[,c(2:90)])
f1 <- as.formula(paste("trip_count ~",
                      paste(n1[!n1 %in% "trip_count"],
                            collapse = " + ")))

# dividing test and training data sets
set.seed(seed)
ratio_sep <- 0.75
index_s1 <- sample(1:nrow(mod_matrix_start), round(ratio_sep*nrow(mod_matrix_start)))

# test and training start but using matrix form
train.start.mat <- mod_matrix_start[index_s1,]
test.start.mat <- mod_matrix_start[-index_s1,]

# Random Forest used to predict amount
set.seed(seed)
rf.model = randomForest(f1, data =  train.start.mat)
post.valid.rf = predict(rf.model, newdata = test.start.mat)
hist(post.valid.rf2)
mean((test.start.mat$trip_count - post.valid.rf2)^2) # mean prediction error
# model didn't finish running 
sd((test.start.mat$trip_count - post.valid.rf2)^2)/sqrt(test.start.mat) # std error
# model didn't finish running 






# used to get zip codes added, also updated the other 
# adding zipcode to better group stations
# result <- do.call(rbind,
#                   lapply(1:nrow(start_bike_stations),
#                          function(i)revgeocode(as.numeric(start_bike_stations[i,4:3]))))
# start_bike_stations <- cbind(start_bike_stations,result)
# start_bike_stations$zipcode <- substr(str_extract(start_bike_stations$address," [0-9]{5}, .+"),2,6)
# start_bike_stations$address <- NULL
# saveRDS(start_bike_stations, "C:/Users/Tyler/Desktop/MSPA/MSPA 498/start_bike_stations.Rds")
