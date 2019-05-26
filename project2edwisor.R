#clearing all the contents of the environment
rm(list=ls())

#get the working directory
getwd()

#set the working directory
setwd("C:/Users/Gaurav's BEAST/Desktop/project2edwisor")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#read the data
data=read.csv("train_cab.csv",header=T)

#first 5 rows of the data
head(data)
#last 5 rows of the data
tail(data)
#class of the data
class(data)
#class of the fare_amount
class(data$fare_amount)
#summary of the data            
summary(data)
#structure of the data
str(data)
#
missing_val=data.frame(apply(data,2,function(X){sum(is.na(x))}))
#
sum(is.na(data))
#
na.omit(data)
#
summary(data)
#
data$fare_amount <-  as.integer(data$fare_amount)
#
boxplot(data$fare_amount)
#
str(data)
#
sapply(data, class)
#
data$pickup_datetime=as.Date.POSIXct(data$pickup_datetime)
#
# Cab rides should not have negative numbers, along with that, taxi standarad fares begin at $2.50
data = data$fare_amount > 2.5
#
# our latitude and longitude should not be equal to 0 becuase the dataset is based in NY
data = data['pickup_latitude'] != 0
data = data['pickup_longitude']!= 0
data = data['dropoff_latitude']!= 0
data = data['dropoff_longitude']!= 0

# latitude and longitude are bounded by 90 and -90. We shouldnt have any coordiantes out of that range
data = data[(data['pickup_latitude']<=90) & (data['pickup_latitude']>=-90)]
data = data[(data['pickup_longitude']<=90) & (data['pickup_longitude']>=-90)]
data = data[(data['dropoff_latitude']<=90) & (data['dropoff_latitude']>=-90)]
data = data[(data['dropoff_longitude']<=90) & (data['dropoff_longitude']>=-90)]

# I dont want to include destinations that have not moved from there pickup coordinates to there dropoff coordinates
data = data[(data['pickup_latitude'] != data['dropoff_latitude']) & (data['pickup_longitude'] != data['dropoff_longitude'])]

data = (data['passenger_count'] < 7) & (data['passenger_count'] > 0)

#defining the haversine formula
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) 
  # Distance in km
  
  gcd <- function(long1, lat1, long2, lat2) {
    
    # Convert degrees to radians
    long1 <- deg2rad(long1)
    lat1 <- deg2rad(lat1)
    long2 <- deg2rad(long2)
    lat2 <- deg2rad(lat2)
    
    return(list(haversine = gcd.hf(long1, lat1, long2, lat2),
                sphere = gcd.slc(long1, lat1, long2, lat2),
                vincenty = gcd.vif(long1, lat1, long2, lat2)) )
  }
  data=as.numeric(data['fare_amount'])
library(datacombine)
  library(rpart)
  library(MASS)
  rmexcept('data')
#
  set.seed(1234)
  train.index=createdatapartition(data$fare_amount,p=0.7,list=FALSE)
write(capture.output(summary(C50_model)),'c50Rules.txt')
train=data[train.index,] 
test=data[-train.index,]
#
fit=rpart(fare_amount ~.,data = train,method = 'anova')
predictions_dt=predict(fit,test[-10])

library(randomForest)
# random forest
rf_model=randomForest(fare_amount~.train,importance=TRUE,ntree=100)

# extract rules from random forest
# transform rf object to an intrees format 
treelist=RF2List(rf_model)
exec=extractrules(treelist,train[-17])
exec[1:2,]
readablerules=presentrules(exec,colnames(train))
readablerules[1:2,]

#
library(usdm)
lm_model=lm(data~.,data = train)
#summary of the model
summary(lm_model)
#predict
predictions_lr=predict(lm_model,test[,1:9])
#calculate mape
MAPE(test[,10],predictions_lr)

#
write.csv("submission.csv",row.names=F)