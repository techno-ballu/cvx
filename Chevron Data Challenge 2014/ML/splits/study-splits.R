# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(randomForest)
require(RWeka)
library(foreach)
require(caret)
require(gdata)

# set the work directory
setwd("~/Chevron/DSChallenge/splits")

# data sets
train <- read.csv("good_train.csv", header = TRUE)
test <- read.csv("good_test.csv", header = TRUE)
data <- rbind(train, test)

# attach the prediction errors
errors <- read.csv("predictionerror1.csv", header = TRUE)
data <- merge(data, errors, by="WellID")

write.table(data, "data.csv",sep=",",row.names=FALSE, quote = FALSE)

table(data$WB.Spacing.Proxy)

# train <- read.csv("bad_train.csv", header = TRUE)
# test <- read.csv("bad_test.csv", header = TRUE)

# check histogram of distribution!
hist(train$EUR_o..Mstb.,breaks = 10)
hist(test$EUR_o..Mstb.,breaks = 10)

# replace "undefined", empty strings and #VALUE! as NAs for train
# for(i in names(train)) {
#   if (length(which(isUnknown(x=train[[i]], unknown = c("undefined","","#VALUE!")))) > 0) {
#     train[[i]] <- unknownToNA(x=train[[i]], unknown=c("undefined","","#VALUE!"))
#     print(paste(i,"replaced UNDEFINEDs=", any(isUnknown(x=train[[i]], unknown = c("undefined","","#VALUE!"))), sep = " "))
#   }
# }
str(train)

# replace "undefined", empty strings and #VALUE! as NAs for test
# for(i in names(test)) {
#   if (length(which(isUnknown(x=test[[i]], unknown = c("undefined","","#VALUE!")))) > 0) {
#     test[[i]] <- unknownToNA(x=test[[i]], unknown=c("undefined","","#VALUE!"))
#     print(paste(i,"replaced UNDEFINEDs=", any(isUnknown(x=test[[i]], unknown = c("undefined","","#VALUE!"))), sep = " "))
#   }
# }
str(test)

# the model - M5P from RWeka package
fit <- M5P(EUR_o..Mstb. ~ ., data = train, na.action = na.pass)
# fit <- randomForest(EUR_o..Mstb. ~. , data = train, ntree = 100, na.action = na.pass)

# fit_control <- trainControl(method = "LGOCV",number  = 10,p = 0.70)
# set.seed(825)
# fit <- train(EUR_o..Mstb. ~ .,data = train, method = "M5",trControl = fit_control, na.action = na.pass)
# fit

predicted <- predict(fit, newdata = test)

#Convert predicted response vector as dataframe
actual= test$EUR_o..Mstb.
predicted <- as.data.frame(predicted)
colnames(predicted) <- "predicted.EUR"
#Replacing NA with average EUR from training set (NA not allowed for submission)
predicted[is.na(predicted)] <- mean(train$EUR_o..Mstb.)

# The RMSE
RMSE<-sqrt((sum((actual-predicted)^2))/nrow(test))
COR <- cor(actual, predicted)

# print results of the best iteration
summary(fit)
print(RMSE)
print(COR)

# bagging
# length_divisor<-4
# iterations<-70
# predicted<-foreach(m=1:iterations,.combine=cbind) %do% {
#   # set the best seed for bagging
#   #   seed.val <- sample(1:1000, 1)
#   #   set.seed(m) - may intro bias!
#   training_positions <- sample(nrow(train), size=floor((nrow(train)/length_divisor)))
#   train_pos<-1:nrow(train) %in% training_positions
#   fit <- M5P(EUR_o..Mstb. ~. , data=train[training_positions,], na.action = na.pass)
#   predict(fit,newdata=test)
# }
# predicted<-rowMeans(predicted)
# 
# # The RMSE
# RMSE<-sqrt((sum((actual-predicted)^2))/nrow(test))
# COR <- cor(actual, predicted)
# 
# # summary(fit)
# print(RMSE)
# print(COR)
# 
# final = cbind(actual,predicted)
# write.table(final, "bagged_output.csv",sep=",",row.names=FALSE, quote = FALSE)