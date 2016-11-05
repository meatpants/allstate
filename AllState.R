library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gbm)

claims <- read.csv("/home/sam/Dropbox/Sam Dropbox/Analytics/Kaggle/AllState/train.csv")
submit.set <- read.csv("/home/sam/Dropbox/Sam Dropbox/Analytics/Kaggle/AllState/test.csv")

nrow(submit.set)

#dummies <- dummyVars(loss ~ ., data = train)
#train.dummied <- predict(dummies, newdata = train)
#write.csv(train.dummied, "C:/Users/spatten/Dropbox/Sam Dropbox/Analytics/Kaggle/AllState/train_dummied.csv")


trainIndex <- createDataPartition(claims$loss, p = .8, 
                                  list = FALSE, 
                                  times = 1)
claims.train <- claims[trainIndex,]
claims.test <- claims[-trainIndex,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


claims.train.numeric <- sapply(claims.train , function(x) as.numeric(x))
nrow(claims.train.numeric)
gbmFit1 <- train(loss ~ ., data = claims.train.debug, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

claims.test.numeric <- sapply(claims.test, function(x) as.numeric(x))

submit.set.numeric <- sapply(submit.set, function(x) as.numeric(x))

nrow(submit.set)


summary(submit.set.numeric)

nrow(submit.set.numeric)

summary(claims.test)

claims.predicted <- predict(gbmFit1, newdata = claims.test.numeric)

summary(claims.predicted)

length(claims.predicted)



nrow(submit.set)

submit.predicted <- predict(gbmFit1, newdata = submit.set.numeric)
submission1 <- data.frame(id = submit.set$id, loss = round(submit.predicted))

round(submission1$loss)

           
write.csv(submission1
          , "/home/sam/Dropbox/Sam Dropbox/Analytics/Kaggle/AllState/submssion.csv"
          , quote = FALSE
          , row.names = FALSE)

head(submit.set$id)

head(submit.predicted)

length(claims.predicted)

summary(bind_cols(claims.test, data.frame(predicted.loss = claims.predicted)))




summary(claims.train.debug)
ncol(train)