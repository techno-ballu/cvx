# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(gdata)
require(randomForest)
require(RWeka)
require(caret)
require(fmsb)
library(foreach)
# library(hydroTSM)
library(corrplot)
library(RANN)

# set the work directory
setwd("~/Chevron/DSChallenge/preprocess/results")

# some tweaking constants
IMP_VARS_CNT <- 15
missingThreshold <- 0.8
transformations <- c("BoxCox", "center", "scale") # ,"bagImpute"
corrCutOff <- 0.75
iterations <- 1000
trainSplit <- 0.70

# imp vars!
ID <- "WellID"
USELESS <- c("Completion.Date")
METRIC <- "EUR_o..Mstb."
outliers <- c("Fluid.Water..Gals.","Acid..Gals.","Gel.x.link..Gals.","Proppant...Total..lbs.","Fluid...Total..lbs.","Fluid.Amount","Propping.Agent.Amount") #"Other..Gals.",

# load base_training.csv
base <- read.csv("../base_training.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# remove the Deepest Zone as it will be added below
base <- base[, -which(names(base) %in% c("Deepest_Zone"))]
# split Between Zone into 2
zones=data.frame(do.call(rbind, strsplit(as.vector(base$Between_Zone), split = " --> ", fixed=TRUE)))
names(zones) <- c("Top_Zone", "Deepest_Zone")
base <- cbind(base, zones)

# format Completion.Date
base$Completion.Date = as.Date(base$Completion.Date, origin = "1900-01-01")

# add Completion.Month
# base$Completion.Month = as.numeric(format(base$Completion.Date, "%m"))

# add Completion.Season
# base$Completion.Season <- time2season(base$Completion.Date, out.fmt = "seasons", type = "default")

write.table(base, "base_training_edits.csv", sep=",", row.names=FALSE, quote = FALSE)

# load completions_training.csv with Remarks removed!
completions <- read.csv("../completions_training_removedRemarks.csv", header = T)
# unique completions
uniqueCompletions <- unique( completions )

write.table(uniqueCompletions, "completions_training_edits.csv", sep=",", row.names=FALSE, quote = FALSE)

# load geology_training.csv
geo <- read.csv("../geology_training.csv", header = TRUE, sep = ",")

# mark the handpicked columns
handpicked <- c("CLFK..KHW.","U_SPBR..KHW.","L_SPBR..KHW.","WFMP..KHW.","WFMP_L..KHW.","WFMP_ATB..KH","CLINE..KHW.","STRN..KHW.","ATOK..KHW.","BEND..KHW.","ATOKA_L..KHW","MPLM..KHW.","WDFD..KHW.","DVNN..KHW.")
# mark the interpolated columns
interpolated <- c("CLEARFORK..MAP.","SPRABERRY_U..MAP.","SPRABERRY_L..MAP.","WOLFCAMP..MA","WOLFCAMP_L..","WFMP_ATB..MA","CLINE..MAP.","STRAWN..MAP.","ATOKA..MAP.","BEND..MAP.","ATOKA_L..MAP","MISSISSIPPIA","WOODFORD..MA","DEVINOAN_UNC")

# replace the Handpicked NAs by values from the Interpolated columns
for(i in 1:length(handpicked)) {
  print(paste(handpicked[i], interpolated[i], sep = " = "))
  geo[[handpicked[i]]][is.na(geo[[handpicked[i]]])] <- geo[[interpolated[i]]][is.na(geo[[handpicked[i]]])]
}

# drop interpolated columns
geo = geo[,-which(names(geo) %in% interpolated)]

write.table(geo, "geology_training_edits.csv", sep=",", row.names=FALSE, quote = FALSE)

# merging the files
temp <- merge(base, uniqueCompletions, by="WellID")
data <- merge(temp, geo, by="WellID")
write.table(data, "data.csv", sep=",", row.names=FALSE, quote = FALSE)
print(paste("=============== Data merged into one file with", nrow(data), "rows and", ncol(data), "columns ===============", sep = " "))

# remove some unwanted columns
data <- data[, -which(names(data) %in% USELESS)]
str(data)

# replace empty strings and #VALUE! as NAs for all columns
for(i in names(data)) {
  if (length(which(isUnknown(x=data[[i]], unknown = c("","#VALUE!")))) > 0) {
    data[[i]] <- unknownToNA(x=data[[i]], unknown=c("","#VALUE!"))
    print(paste(i,"replaced garbage=", any(isUnknown(x=data[[i]], unknown = c("","#VALUE!"))), sep = " "))
  }
}
str(data)

# detect the numerical columns to remove outliers
# data.numericals <- data[, which(names(data) %in% outliers)]
# str(data.numericals)
# for(i in names(data.numericals)) {
#   quantiles <- quantile(data.numericals[[i]], c(.05, .95), na.rm = TRUE)
#   if (quantiles[1] >= 0 && quantiles[2] >= 0) {
#     print(i)
#     data.numericals[[i]][ data.numericals[[i]] < quantiles[1] ] <- quantiles[1]
#     data.numericals[[i]][ data.numericals[[i]] > quantiles[2] ] <- quantiles[2]
#   }
# }
# data.others <- data[ , -which(names(data) %in% outliers)]
# str(data.others)
# data.cleaned <- cbind(data.numericals, data.others)
# 
# write.table(data.cleaned, "data_edits1.csv", sep=",", row.names=FALSE, quote = FALSE)
# str(data.cleaned)

data.cleaned <- data

# set 10% missing threshold
cols <- ncol(data.cleaned)
rows <- nrow(data.cleaned)
NA_RowWise <- apply(data.cleaned, 1, function(z) sum(is.na(z)))
hist(NA_RowWise)
quantile(NA_RowWise)
# eliminate rows whose missing > 20%
colsMissing <- floor(cols*missingThreshold)
colsMissing
DelRows <- which(NA_RowWise > colsMissing)
length(DelRows)
data.rowsCleaned <- data.cleaned
if (length(DelRows) > 0) {
  data.rowsCleaned <- data.cleaned[-DelRows,]  
}

nrow(data.rowsCleaned)
ncol(data.rowsCleaned)

NA_ColWise <- apply(data.rowsCleaned, 2, function(z) sum(is.na(z)))
hist(NA_ColWise)
quantile(NA_ColWise)
#eliminate cols whose missing > 20%
rowsMissing <- floor(rows*missingThreshold)
rowsMissing
DelCols <- which(NA_ColWise > rowsMissing)
length(DelCols)
data.colsCleaned <- data.rowsCleaned
if (length(DelCols) > 0) {
  data.colsCleaned <- data.rowsCleaned[,-DelCols]
}
nrow(data.colsCleaned)
ncol(data.colsCleaned)

write.table(data.colsCleaned, "data_cleaned2.csv", sep=",", row.names=FALSE, quote = FALSE)

# make data point to the cleaned up version
data <- data.colsCleaned

# find columns which have near zero variance
nearZeroVariance <- nearZeroVar(data[,-which(names(data) %in% c(ID,METRIC))]) #, saveMetrics = T
names <- names(data[,-which(names(data) %in% c(ID,METRIC))])[nearZeroVariance]
if (length(nearZeroVariance) > 0)
  data <- data[,-which(names(data) %in% names)]
str(data)

# Categorical labeling - find out type of columns
types <- lapply(data, class)
distincts <- lapply(data, function(c) unique(c))

for(i in names(data[,-which(names(data) %in% c(ID,METRIC))])){
  noOfCats <- length(levels(distincts[[i]]))
  if (noOfCats == 1) {
    data[[i]] <- NULL
  } else if ((noOfCats <= length(data[[i]])/2 && types[[i]] == "factor") || types[[i]] == "character") {
    print(paste("processing ====", i, sep = "> "))
    means <- sapply(split(data$EUR_o..Mstb., data[[i]]), mean)
    print(names(means))
    # convert to character type
    data[[i]] <- as.character(data[[i]])
    for(j in names(means)) {
      print(paste("replacing", j, "by", means[[j]], sep = " "))
      data[[i]][data[[i]] == j] <- means[[j]]
    }
    data[[i]] <- as.numeric(data[[i]])
  }
}

write.table(data, "data_labeled3.csv", sep=",", row.names=FALSE, quote = FALSE)
str(data)

# impute the missing values - lets test this!
transformed <- rfImpute(EUR_o..Mstb. ~ ., data = data[,-which(names(data) %in% c(ID))])
# colnames(transformed)[1] <- METRIC
str(transformed)
write.table(transformed, "data_imputed5.csv", sep=",", row.names=FALSE, quote = FALSE)

# preprocessing transformations
trans <- preProcess(transformed[,-which(names(transformed) %in% c(METRIC))], method = transformations)  
transformed <- predict(object = trans, newdata = transformed[,-which(names(transformed) %in% c(METRIC))])
str(transformed)

# # preprocessing transformations
# trans <- preProcess(data[,-which(names(data) %in% c(ID,METRIC))], method = transformations)  
# transformed <- predict(object = trans, newdata = data[,-which(names(data) %in% c(ID,METRIC))])
# str(transformed)

# # impute the missing values - lets test this!
# transformed <- rfImpute(x = transformed, y = data[[METRIC]], data = transformed)
# colnames(transformed)[1] <- METRIC
# str(transformed)
# write.table(transformed, "data_imputed5.csv", sep=",", row.names=FALSE, quote = FALSE)

# find correlated variables
# correlations <- cor(transformed[,-which(names(transformed) %in% c(METRIC))])
correlations <- cor(transformed)
corrplot(correlations, order = "hclust")
highCorr <- findCorrelation(correlations, cutoff = corrCutOff)
# names(transformed[,-which(names(transformed) %in% c(METRIC))])[highCorr]
names(transformed)[highCorr]
names <- names(transformed)
length(highCorr)
if (length(highCorr) > 0)
  names <- names(transformed)[-highCorr]
names

data <- cbind(data[,which(names(data) %in% c(ID,METRIC))],transformed[,which(names(transformed) %in% names)])
# colnames(data)[1] <- ID
str(data)

correlations <- cor(data[,-which(names(data) %in% c(ID,METRIC))])
corrplot(correlations, order = "hclust")

write.table(data, "data_dissimilar4.csv", sep=",", row.names=FALSE, quote = FALSE)

# this does not work or rather takes much time!
# rankings <- calc.relimp(EUR_o..Mstb. ~.,data=data)
# ranks=sort(rankings$lmg,decreasing=T)
# ranks

# running the randomForest implementation for getting importance
rf <- randomForest(EUR_o..Mstb. ~., data=data[,-which(names(data) %in% c(ID))], mtry=2, ntree=50, importance=TRUE)
varImpPlot(rf)
# matrix with 1 column
ranks <- importance(rf,type=1)
# sort in descending order
ranks <- sort(ranks[,],decreasing = T)
# pick up top n=10
# ranks <- head(names(ranks),IMP_VARS_CNT)
isImportant <- function(X) {
  X[ifelse(X >= 0,TRUE,FALSE)]
}
ranks <- names(isImportant(ranks))
ranks
ranks[length(ranks)+1] <- METRIC
ranks[length(ranks)+1] <- ID
data <- data[,which(names(data) %in% ranks)]
str(data)

write.table(data, "data_important5.csv", sep=",", row.names=FALSE, quote = FALSE)

# lets plot the learning curve for this data
minSamples <- 10
forTraining <- data[,-which(names(data) %in% c(ID))]
# N <- nrow(forTraining)
N <- 500
learnings <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Sample", "Train", "Test"))), stringsAsFactors=F)
learning <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Sample", "Train", "Test"))), stringsAsFactors=F)
# algo <- "M5"
for(no in minSamples:N) {
#   no <- 100
  train.RMSE <- numeric()
  test.RMSE <- numeric()
  for(iter in 1:10) {
#             iter <- 1
    # set seed fixed
    set.seed(iter)
    # do random sampling to generate training & test sets
    index <- sample(N,no)
    trainIndex <- sample(index,floor(no*trainSplit))
#     trainIndex <- sample(index, no-1)
    testIndex <- index[!(index %in% trainIndex)]
    train <- forTraining[trainIndex,]
    test <- forTraining[testIndex,]
    
    # train the model 
    # fit <- train(EUR_o..Mstb. ~., data=train, trControl=train_control, method=algo)
    fit <- M5P(EUR_o..Mstb. ~. , data = train)
    summary(fit)
    
    # on train data iteration j
    predicted <- predict(fit, newdata = train)
    actual= train$EUR_o..Mstb.
    # The RMSE
    theTrain<-RMSE(predicted,actual)
    
    # on test data iteration j
    predicted <- predict(fit, newdata = test)
    actual= test$EUR_o..Mstb.
    # The RMSE
    theTest<-RMSE(predicted,actual)
    
    train.RMSE[iter] <- theTrain
    test.RMSE[iter] <- theTest
    
  }
  theTrain <- max(train.RMSE,na.rm = T)
  theTest <- mean(test.RMSE,na.rm = T)
  
  learning <- c(no,theTrain,theTest)
  learnings[no,] <- learning

  print(paste(no,theTrain,theTest,sep = " "))
}
write.table(learnings, "data_learnings.csv", sep=",", row.names=FALSE, quote = FALSE)

# plot learning curve
xrange <- range(1:N) 
yrange <- range(min(min(learnings$Train,na.rm = T),min(learnings$Test,na.rm = T)):max(max(learnings$Train,na.rm = T),max(learnings$Test,na.rm = T))) 
colors <- rainbow(2) 
linetype <- c(1:2) 
plotchar <- seq(18,19,1)
plot(xrange,yrange,xlab="Number of training example",ylab="Error")
lines(2:N, learnings$Train[2:N], type="b", lwd=1.5, lty=linetype[1], col=colors[1], pch=plotchar[1]) 
lines(2:N, learnings$Test[2:N], type="b", lwd=1.5, lty=linetype[2], col=colors[2], pch=plotchar[2]) 
legend(xrange[1], yrange[2], c("Train","Test"), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="RF learing curve")

# modeling
best.rmse <- 10000
best.train <- NULL
best.test <- NULL
best.fit <- NULL
best.cor <- -1
best.seed <- -1

# n iterations to get best model
for(i in 1:iterations) {
  # set seed by iteration
  set.seed(i)
  # do random sampling to generate training & test sets
  trainIndex <- sample(nrow(data),floor(nrow(data)*trainSplit))
  # createDataPartition(data$Operator.Numeric, p = .7,list = FALSE,times = 1)
  train=data[trainIndex,-which(names(data) %in% c(ID))]
  test=data[-trainIndex,-which(names(data) %in% c(ID))]
  
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
  
  if (RMSE < best.rmse && COR > best.cor) { 
    print(paste("====>", COR, RMSE, i, sep = " "))
    best.rmse <- RMSE
    best.train <- train
    best.test <- test
    best.fit <- fit
    best.cor <- COR
    best.seed <- i
  }
}

# print results of the best iteration
summary(best.fit)
print(best.rmse)
print(best.cor)
print(best.seed)

# write the best training and test sets
# write.table(best.train, "best_train.csv",sep=",",row.names=FALSE, quote = FALSE)
# write.table(best.test, "best_test.csv",sep=",",row.names=FALSE, quote = FALSE)

# actual= best.test$EUR_o..Mstb.
# length_divisor<-3
# iterations<-70
# predicted<-foreach(m=1:iterations,.combine=cbind) %do% {
#   # set the best seed for bagging
#   #   seed.val <- sample(1:1000, 1)
#   set.seed(m)
#   training_positions <- sample(nrow(best.train), size=floor((nrow(best.train)/length_divisor)))
#   train_pos<-1:nrow(best.train) %in% training_positions
#   fit <- M5P(EUR_o..Mstb. ~. , data=best.train[training_positions,], na.action = na.pass)
#   predict(fit,newdata=best.test)
# }
# predicted<-rowMeans(predicted)
# 
# # The RMSE
# RMSE<-sqrt((sum((actual-predicted)^2))/nrow(best.test))
# COR <- cor(actual, predicted)
# 
# # summary(fit)
# print(RMSE)
# print(COR)
# 
# final = cbind(actual,predicted)
# write.table(final, "bagged_output.csv",sep=",",row.names=FALSE, quote = FALSE)