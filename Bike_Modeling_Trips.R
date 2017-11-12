library(ggplot2)
library(zoo)
library(dplyr)
library(xtable)
library(RCurl)
library(timeDate)
library(leaps)
library(rpart)
library(rpart.plot)
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
seed = 555
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
fwd.lm.start <- as.data.frame(coef(regfit.fwd.start, 13))

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

# exploring the data to see if there is a way to create a variable to high traffice areas, basically reducing the amount of variables
zip_count <- start_trips %>%
  group_by(zipcode) %>%
  summarise(zip_agg=sum(!is.na(day)))
zip_count

# creating a new variable to show zipcode groupings of 5 major categorys based on size
zips <- c('11201',
          '10002',
          '10003',
          '11205',
          '10011',
          '10013',
          '10019',
          '10009',
          '10016',
          '10001',
          '10014',
          '10017',
          '10022',
          '11249',
          '10012',
          '10036',
          '11238',
          '11217',
          '10038',
          '10018',
          '10010',
          '11211',
          '11222',
          '10007',
          '11206',
          '11216',
          '10004',
          '10005',
          '',
          '10023',
          '11101',
          '10282',
          '10168',
          '10024',
          '10028',
          '10065',
          '10021',
          '11221',
          '10020',
          '10075',
          '10155',
          '10280',
          '07306',
          '10045',
          '10006',
          '11231',
          '07732',
          '10025',
          '11233',
          '11215',
          '10029',
          '10128',
          '11213',
          '10069',
          '11109',
          '11220',
          '11236',
          '10026',
          '11237')

traffic_cat <- c('traf1',
                 'traf1',
                 'traf1',
                 'traf1',
                 'traf1',
                 'traf1',
                 'traf1',
                 'traf2',
                 'traf2',
                 'traf2',
                 'traf2',
                 'traf2',
                 'traf2',
                 'traf2',
                 'traf3',
                 'traf3',
                 'traf3',
                 'traf3',
                 'traf3',
                 'traf3',
                 'traf3',
                 'traf4',
                 'traf4',
                 'traf4',
                 'traf4',
                 'traf4',
                 'traf4',
                 'traf4',
                 'traf5',
                 'traf5',
                 'traf5',
                 'traf5',
                 'traf5',
                 'traf5',
                 'traf5',
                 'traf6',
                 'traf6',
                 'traf6',
                 'traf6',
                 'traf6',
                 'traf6',
                 'traf6',
                 'traf7',
                 'traf7',
                 'traf7',
                 'traf7',
                 'traf7',
                 'traf7',
                 'traf7',
                 'traf8',
                 'traf8',
                 'traf8',
                 'traf8',
                 'traf8',
                 'traf8',
                 'traf8',
                 'traf8',
                 'traf8',
                 'traf8')

simp_traf <- data.frame(zips, traffic_cat)

# merge to get more categorical zip codes
mod_df_start <- merge(mod_df_start, simp_traf, by.x = "zipcode", by.y = "zips", all = TRUE)

# reordering to include zipcode for modeling with more complex models
mod_df_start_nz <- mod_df_start[,c(2,3:13,1)]
# remove observations with missing values
mod_df_start_nz <- mod_df_start_nz[complete.cases(mod_df_start_nz), ]

# forumla for modeling variables
n1 <- names(mod_df_start_nz [,c(2:12)])
f1 <- as.formula(paste("trip_count ~",
                       paste(n1[!n1 %in% "trip_count"],
                             collapse = " + ")))

# dividing test and training data sets
ratio_sep <- 0.75
index_s1 <- sample(1:nrow(mod_df_start_nz), round(ratio_sep*nrow(mod_df_start_nz)))

# test and training start and end trip data frames
train.start1 <- mod_df_start_nz[index_s1,]
test.start1 <- mod_df_start_nz[-index_s1,]

# Random Forest used to predict amount
set.seed(seed)
rf.model = randomForest(f1, data =  train.start1)
post.valid.rf = predict(rf.model, newdata = test.start1)
# plot distribution of predicted values
par(mfrow=c(1,1))
hist(post.valid.rf)
mean((test.start1$trip_count - post.valid.rf)^2) # mean prediction error
# model didn't finish running 
sd((test.start1$trip_count - post.valid.rf)^2)/sqrt(test.start1) # std error
# plotting results
par(mfrow=c(1,1))
plot(test.start1$trip_count,post.valid.rf,xlab="Known Trips",ylab="Regression Estimate",col='blue',
     main='Real vs Predicted Trips Random Forest',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Random Forest',pch=18,col='blue', bty='n')


# model matrix stuff
mod_df_start_mat <- mod_df_start[,c(2,3:13,1)]
# Random Forest using model matrix
mod_matrix_start <- cbind(mod_df_start_mat[,1:11], 
                          model.matrix(~ . + 0, 
                                       data= mod_df_start_mat[,12:13], 
                                       contrasts.arg = lapply(mod_df_start_mat[,12:13], 
                                                              contrasts, 
                                                              contrasts=FALSE)))
mod_matrix_start$zipcode <- NULL

# forumla for modeling variables
n_mat <- names(mod_matrix_start[,c(2:77)])
f_mat <- as.formula(paste("trip_count ~",
                          paste(n_mat[!n_mat %in% "trip_count"],
                                collapse = " + ")))

# setting up the model matrix index
set.seed(seed)
ratio_sep <- 0.75
index_s_mat <- sample(1:nrow(mod_matrix_start), round(ratio_sep*nrow(mod_matrix_start)))

# remove observations with missing zipcodes and AWND
mod_matrix_start<- mod_matrix_start[complete.cases(mod_matrix_start), ]

# test and training start and end trip data frames using model matrix
train.start.mat <- mod_matrix_start[index_s_mat,]
test.start.mat <- mod_matrix_start[-index_s_mat,]

# Random Forest used to predict amount with model matrix
set.seed(seed)
rf.model.mat = randomForest(f_mat, data =  train.start.mat)
post.valid.rf.mat = predict(rf.model.mat, newdata = test.start.mat)
# plot distribution of predicted values
par(mfrow=c(1,1))
hist(post.valid.rf.mat)
mean((test.start.mat$trip_count - post.valid.rf.mat)^2) # mean prediction error
# model didn't finish running 
sd((test.start.mat$trip_count - post.valid.rf.mat)^2)/sqrt(length(test.start.mat$trip_count)) # std error
# plotting results
par(mfrow=c(1,1))
plot(test.start.mat$trip_count,post.valid.rf.mat,xlab="Known Trips",ylab="Random Forest Estimate",col='green',
     main='Real vs Predicted Trips Matirx Random Forest',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Random Forest',pch=18,col='green', bty='n')

# creation of difference variable
diff_df <- full_join(start_trips[,1:3], 
                     end_trips[,1:3], 
                     by = c("start_date" = "end_date", "start_station_id" = "end_station_id"))
# changing NA's to zero
diff_df[is.na(diff_df)] <- 0
# renaming columns
colnames(diff_df) <- c("Date","Station_ID","Start_Trips","End_Trips")
# diff variable
diff_df$diff <- ""
diff_df$diff <- (diff_df$Start_Trips - diff_df$End_Trips)

# Write CSV in R
write.csv(diff_df, file = "C:/Users/Tyler/Desktop/MSPA/MSPA 498/Bike_Diff.csv")
