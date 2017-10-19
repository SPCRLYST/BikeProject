library(ggplot2)
library(zoo)
library(dplyr)
library(xtable)
library(RCurl)
library(timeDate)
library(rpart)
library(rpart.plot)

# reading data from previously created trip data; Bike_Modeling.R
start_trips <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/start_trips_agg.Rds?raw=true")))
end_trips <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/end_trips_agg.Rds?raw=true")))
start_bike_stations <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/start_bike_stations.Rds?raw=true")))
end_bike_stations <- readRDS(gzcon(url("https://github.com/SPCRLYST/BikeProject/blob/master/end_bike_stations.Rds?raw=true")))

# modeling data frames
mod_df_start <- start_trips[,c(3,2,4:10,12:13)]
mod_df_end <- end_trips[,c(3,2,4:10,12:13)]

# getting variables in the right format
cols <- c(2,9:11)
mod_df_start[cols] <- lapply(mod_df_start[cols], factor)
mod_df_end[cols] <- lapply(mod_df_end[cols], factor)

# eda

# forumla for modeling variables
n <- names(mod_df_start[,c(2:11)])
f <- as.formula(paste("trip_count ~",
                       paste(n[!n %in% "trip_count"],
                             collapse = " + ")))

# dividing test and training data sets
seed = 555
set.seed(seed)
ratio_sep <- 0.75
index_s <- sample(1:nrow(mod_df_start), round(ratio_sep*nrow(mod_df_start)))
index_e <- sample(1:nrow(mod_df_end), round(ratio_sep*nrow(mod_df_end)))

# test and training start and end trip data frames
train.start <- mod_df_start[index_s,]
test.start <- mod_df_start[-index_s,]
train.end <- mod_df_end[index_e,]
test.end <- mod_df_end[-index_e,]

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
# all subset variable selection
set.seed(seed)
regfit.alls.start <- regsubsets(f, data = train.start, nvmax = 12, method = "exhaustive")  
alls.start.lmsum <- summary(regfit.alls.start)
par(mfrow=c(1,3))
plot(alls.start.lmsum$adjr2 ,xlab = "Number of Variables", ylab = "Adjusted RSq")
plot(alls.start.lmsum$bic ,xlab = "Number of Variables", ylab = "BIC")
plot(alls.start.lmsum$cp ,xlab = "Number of Variables", ylab = "CP")
alls.lm.start <- as.data.frame(coef(regfit.alls.start, 12))
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
leaps_dfs <- cbind(bkd.lm.start, fwd.lm.start, alls.lm.start, steps.lm.start)
colnames(leaps_dfs) <- c("Backward Variable Selection","Forward Variable Selection",
                         "All Subset Variable Selection","Stepwise Variable Selection")

# LASSO Variable Selection Method
library(glmnet)
set.seed(seed)
xf = model.matrix(f, data = train.start)[,-1] 
yf = data.frame(train.start)
yf = yf$trip_count
grid = 10^seq(10,-2, length =100) 
lasso.mod = glmnet(xf,yf,alpha = 1, lambda = grid) 
plot(lasso.mod)
set.seed(seed) 
cv.out=cv.glmnet(xf,yf,alpha=1) 
#plot(cv.out) 
bestlam =cv.out$lambda.min  
#bestlam
xt = model.matrix(f, train.start)[,-1]
lasso.pred=predict(lasso.mod, s=bestlam, newx=xt)
lasso.coef=predict(lasso.mod, type="coefficients", s= bestlam)[1:11,]  
#lasso.coef

# M5P Regression Tree Model
library(RWeka)
set.seed(seed)
M5P.mod <- M5P(f, data=train.start)
plot(M5P.mod) 
M5P.mod 
