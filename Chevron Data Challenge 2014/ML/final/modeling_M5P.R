# write.table(train, "best_train.csv",sep=",",row.names=FALSE, quote = FALSE)
# write.table(test, "best_test.csv",sep=",",row.names=FALSE, quote = FALSE)

# TODO after finding best model, train it on whole training data!


# sub = sample(nrow(baseTrain),floor(nrow(baseTrain)*0.70))
# train=baseTrain[sub,]
# test=baseTrain[-sub,]

# train = read.csv("best_train_log.csv", header = TRUE)
# test = read.csv("best_test.csv", header = TRUE)
# str(train)
# train = train[,c(-2,-8,-15,-17)]
# test = test[,c(-2,-8,-15,-17)]
# str(train)

# the model - LM
# fit <- lm(EUR_o..Mstb. ~. , data = train, na.action = na.exclude)

# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set the work directory
setwd("~/Chevron/DSChallenge/final")

# load the training data
data = read.csv("data_multicollinear.csv", header = TRUE)

# exclude some unwanted columns
# baseTrain = data[,c(-1,-5,-(13:14),-(17:20))]
baseTrain = data
str(baseTrain)

# The test set for calculating actual predictions
#baseTest = read.csv("base_test.csv", header = TRUE)

require(RWeka)
require(caret)

iterations <- 100
best.rmse <- 10000
best.train <- baseTrain
best.test <- baseTrain
best.fit <- M5P(EUR_o..Mstb. ~. , data = baseTrain, na.action = na.pass)
best.cor <- -1
# repeat iterations to get best model
for(i in 1:iterations) {
  # set seed
  seed.val <- sample(1:200, 1)
  set.seed(seed.val)
  # do stratified random sampling to generate training & test sets
  trainIndex <- sample(nrow(baseTrain),floor(nrow(baseTrain)*0.70))
  # createDataPartition(baseTrain$Operator.Numeric, p = .7,list = FALSE,times = 1)
  train=baseTrain[trainIndex,]
  test=baseTrain[-trainIndex,]
  
  # the model - M5P from RWeka package
  fit <- M5P(EUR_o..Mstb. ~. , data = train, na.action = na.pass)
  predicted <- predict(fit, newdata = test)
  
  #Convert predicted response vector as dataframe
  actual= test$EUR_o..Mstb.
  predicted <- as.data.frame(predicted)
  colnames(predicted) <- "predicted.EUR"
  #Replacing NA with average EUR from training set (NA not allowed for submission)
  # predicted[is.na(predicted)] <- mean(train$EUR_o..Mstb.)
  
  # The RMSE
  RMSE<-sqrt((sum((actual-predicted)^2))/nrow(test))
  COR <- cor(actual, predicted)
  
  if (RMSE < best.rmse) {
    #     if (COR > best.cor) {
    print(paste("====>", COR, RMSE, seed.val, sep = " "))
    best.rmse <- RMSE
    best.train <- train
    best.test <- test
    best.fit <- fit
    best.cor <- COR
    
    #     final = cbind(actual,predicted)
    #     write.table(final, "output.csv",sep=",",row.names=FALSE, quote = FALSE)
  }
}

summary(best.fit)
print(best.rmse)
print(best.cor)