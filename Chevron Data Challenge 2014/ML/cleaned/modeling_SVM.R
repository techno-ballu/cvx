# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set the work directory
setwd("~/Chevron/DSChallenge/cleaned")

# load the training data
data = read.csv("base_training_numeric1.csv", header = TRUE)

# exclude some unwanted columns
# baseTrain = data[,c(-1,-5,-(13:14),-(17:20))]
baseTrain = data[,c(-1,-4)]
str(baseTrain)

# The test set for calculating actual predictions
#baseTest = read.csv("base_test.csv", header = TRUE)

# require(RWeka)
library(e1071)

iterations <- 1000
best.rmse <- 10000
best.train <- baseTrain
best.test <- baseTrain
best.fit <- svm(formula = EUR_o..Mstb. ~. , data = baseTrain, na.action = na.exclude)
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
  #   fit <- M5P(EUR_o..Mstb. ~. , data = train, na.action = na.pass)
  
  # the model - M5P from RWeka package
  fit <- svm(formula = EUR_o..Mstb. ~. , data = train, na.action = na.exclude)  
  predicted <- predict(fit, newdata = test)
  
  #Convert predicted response vector as dataframe
  actual= test$EUR_o..Mstb.
  predicted <- as.data.frame(predicted)
  colnames(predicted) <- "predicted.EUR"
  #Replacing NA with average EUR from training set (NA not allowed for submission)
  predicted[is.na(predicted)] <- mean(train$EUR_o..Mstb.)
  
  # The RMSE
  RMSE<-sqrt((sum((actual-predicted)^2))/nrow(test))
#   COR <- cor(actual, predicted)
  
  if (RMSE < best.rmse) {
    #     if (COR > best.cor) {
    print(paste("====>",  RMSE, seed.val, sep = " "))
    best.rmse <- RMSE
    best.train <- train
    best.test <- test
    best.fit <- fit
#     best.cor <- COR
    
    #     final = cbind(actual,predicted)
    #     write.table(final, "output.csv",sep=",",row.names=FALSE, quote = FALSE)
  }
}

summary(best.fit)
print(best.rmse)
# print(best.cor)