# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set the work directory
setwd("~/Chevron/DSChallenge/final")

# load the training data
data = read.csv("data_multicollinear.csv", header = TRUE)

# exclude some unwanted columns
baseTrain = data
str(baseTrain)

# The test set for calculating actual predictions
#baseTest = read.csv("base_test.csv", header = TRUE)

require(RWeka)
library(foreach)

# set seed
# seed.val <- sample(1:10000, 1)
# set.seed(seed.val)
set.seed(65)

# do random sampling to generate training & test sets
trainIndex = sample(nrow(baseTrain),floor(nrow(baseTrain)*0.75))
train=baseTrain[trainIndex,]
test=baseTrain[-trainIndex,]

actual= test$EUR_o..Mstb.
length_divisor<-4
iterations<-100
predicted<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(train), size=floor((nrow(train)/length_divisor)))
  train_pos<-1:nrow(train) %in% training_positions
  fit <- M5P(EUR_o..Mstb. ~. , data=train[training_positions,], na.action = na.pass)
  predict(fit,newdata=test)
}
predicted<-rowMeans(predicted)

# The RMSE
RMSE<-sqrt((sum((actual-predicted)^2))/nrow(test))
COR <- cor(actual, predicted)

summary(fit)
print(RMSE)
print(COR)


