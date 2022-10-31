# install.packages("rpart.plot")
# install.packages("caret", dependencies = TRUE)
# library(rpart)
# library(rpart.plot)
# library(caret)

unfiltered.delays.df <- read.csv("FlightDelay.csv", stringsAsFactors = TRUE)
delays.df <- unfiltered.delays.df[, c(1, 2, 4:9)]

load("TrainIndex2.rda")  # stores the variable train.index 
train.df <- delays.df[train.index, ] 
valid.df <- delays.df[-train.index, ]

names(delays.df)
default.ct <- rpart(Flight.Status ~ ., data = train.df, method = "class")

rpart.plot(default.ct)
rpart.plot(default.ct, extra = 1)

default.ct.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix(default.ct.pred, factor(valid.df$Flight.Status))

# Given Flight Example
new.flight.df <- data.frame(CRS_DEP_TIME = "17_18", CARRIER = "CO", DEST = "EWR", DISTANCE = 199, ORIGIN = "DCA", Weather = "No", DAY_WEEK = "Tue")
pred <- predict(default.ct, new.flight.df, type = "class")
pred


# 6 

train.df.exclude.weather <- train.df[, c(1:5, 7:8)]
valid.df.exclude.weather <- valid.df[, c(1:5, 7:8)]

new.ct <- rpart(Flight.Status ~ ., data = train.df.exclude.weather, method = "class")
rpart.plot(new.ct)
