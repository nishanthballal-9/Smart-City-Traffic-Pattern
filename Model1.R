#Importing Data

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Splitting Date and Time
train$Date <- as.Date(train$DateTime)
train$Time <- format(as.POSIXct(train$DateTime) ,format = "%H:%M:%S")
test$Date <- as.Date(test$DateTime)
test$Time <- format(as.POSIXct(test$DateTime) , format = "%H:%M:%S")

#Data Exploration

#Checking for null values
table(train$Time)
table(is.na(train$Date))
table(is.na(train$Time))
table(is.na(train$Vehicles))
table(is.na(train$Junction))
table(is.na(train$ID))

#No Null values

#Set Time as Factors
train$Time <- as.factor(train$Time)
test$Time <- as.factor(test$Time)

levels(train$Time)

#Time vs Vehicle plot
library("ggplot2", lib.loc="~/R/win-library/3.2")
p1 <- ggplot(train, aes(Time, Vehicles))
p1 + geom_point()

#Time vs Vehicle mean
time_hrs <- levels(train$Time)
vehicle_mean_time <- c()
for(i in 1:length(time_hrs))
{
  vehicle_mean_time[i] <- mean(train$Vehicles[train$Time == time_hrs[i]])
}
vehicle_mt <- data.frame(time_hrs,vehicle_mean_time)
colnames(vehicle_mt) <- c('Time hrs', 'Vehicle Mean')

#Junction vs Vehicle
p2 <- ggplot(train, aes(Junction, Vehicles))
p2 + geom_point()
junction <- unique(train$Junction)
vehicle_mean_junction <- c()
for(i in 1:length(junction))
{
  vehicle_mean_junction[i] <- mean(train$Vehicles[train$Junction == junction[i]])
}
vehicle_mj <- data.frame(junction, vehicle_mean_junction)
colnames(vehicle_mj) <- c('Junction','Vehicles Mean')

#Junction 3
max(train$Vehicles[train$Junction == 3])
min(train$Vehicles[train$Junction == 3])
ggplot() + geom_histogram(aes(train$Vehicles[train$Junction == 3]))

#Junction 1
max(train$Vehicles[train$Junction == 1])
min(train$Vehicles[train$Junction == 1])
ggplot() + geom_histogram(aes(train$Vehicles[train$Junction == 1]))


#Day variable
library("lubridate", lib.loc="~/R/win-library/3.2")
train$Day <- weekdays(as.Date(train$Date))
test$Day <- weekdays(as.Date(test$Date))

day <- unique(train$Day)
vehicle_mean_day <- c()
for(i in 1:length(day))
{
  vehicle_mean_day[i] <- mean(train$Vehicles[train$Day == day[i]])
}
vehicle_md <- data.frame(day, vehicle_mean_day)
colnames(vehicle_md) <- c('Day', 'Vehicles Mean')

train$Day <- as.factor(train$Day)
test$Day <- as.factor(test$Day)
train$Junction <- as.factor(train$Junction)
test$Junction <- as.factor(test$Junction)

#Random Forest Regression
library("randomForest", lib.loc="~/R/win-library/3.2")
set.seed(1234)
regressor <- randomForest(x = train[-c(1,3:5)],
                          y = train$Vehicles,
                          ntree = 100)

vehicle_pred <- predict(regressor, test[-c(1,3:4)])
submission <- data.frame(test$ID,vehicle_pred)
colnames(submission) <- c('ID','Vehicles')
submission$Vehicles <- as.integer(submission$Vehicles)
write.csv(submission,'Submission1.csv',row.names = FALSE)