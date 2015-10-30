# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(gdata)
require(randomForest)
require(RWeka)
require(caret)
require(fmsb)
# library(foreach)
library(Metrics)
library(e1071)
library(corrplot)
library(RANN)
library(ipred)
library(plyr)

# set the work directory
setwd("/home/bolaka/EOG-data/Completions")

# load well wise Completions data
data <- read.csv("completions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(data), "rows and", ncol(data), "columns ===============", sep = " "))

# some tweaking constants
IMP_VARS_CNT <- 15
trainSplit <- 0.70
corrCutOff <- 0.85
# imp vars!
ID <- "Wellname"
USELESS <- c("First.prod.dt","DaysOfProd","CumProdTot")
METRIC <- "CumProd"
outliers <- c("Total.Clean.Fluid","Pump.Down.fluid")
nonactionables <- c("Frac.ball.psi","Pr.Drop.Acid","Initial.FG","Final.FG","Production.Year")
# collinear <- c("Max.Rate.bpm","Avg.Rate.bpm","Max.PSI")

# replace "undefined", empty strings and #VALUE! as NAs for all columns
for(i in names(data)) {
  if (length(which(isUnknown(x=data[[i]], unknown = c("undefined","","#VALUE!")))) > 0) {
    data[[i]] <- unknownToNA(x=data[[i]], unknown=c("undefined","","#VALUE!"))
    print(paste(i,"replaced UNDEFINEDs=", any(isUnknown(x=data[[i]], unknown = c("undefined","","#VALUE!"))), sep = " "))
  }
}
str(data)

# detect the numerical columns to remove outliers
# data.numericals <- data[, which(names(data) %in% outliers)]
# # str(data.numericals)
# for(i in names(data.numericals)) {
#   quantiles <- quantile(data.numericals[[i]], c(.05, .95), na.rm = TRUE)
#   if (quantiles[1] >= 0 && quantiles[2] >= 0) {
#     print(i)
#     data.numericals[[i]][ data.numericals[[i]] < quantiles[1] ] <- quantiles[1]
#     data.numericals[[i]][ data.numericals[[i]] > quantiles[2] ] <- quantiles[2]
#   }
# }
# data.others <- data[ , -which(names(data) %in% outliers)]
# # str(data.others)
# data.cleaned <- cbind(data.numericals, data.others)
# data.cleaned <- data

# write.table(data.cleaned, "data_edits1.csv", sep=",", row.names=FALSE, quote = FALSE)
# str(data.cleaned)

# make data point to the cleaned up version
# data <- data.cleaned
# str(data)

# remove some unwanted columns
data <- data[, -which(names(data) %in% USELESS)]
str(data)

# find columns which have near zero variance
nearZeroVariance <- nearZeroVar(data[, -which(names(data) %in% c(ID,METRIC))])
if (length(nearZeroVariance) > 0) {
  print("Removing near zero variance!")
  data <- data[, -nearZeroVariance]
}

# impute the missing values
temp <- data[,-which(names(data) %in% c(ID))]
temp <- rfImpute(CumProd ~., temp)
data <- cbind(data[[ID]], temp)
colnames(data)[1] <- ID
write.table(data, "data_imputed.csv", sep=",", row.names=FALSE, quote = FALSE)

# preprocess
trans <- preProcess(data[, -which(names(data) %in% c(ID,METRIC))], method = c("BoxCox", "center", "scale")) #,"pca", "spatialSign"
transformed <- predict(object = trans, newdata = data[, -which(names(data) %in% c(ID,METRIC))])
transformed <- as.data.frame(transformed)
str(transformed)

# find correlated variables
# transformed <- data[, -which(names(data) %in% c(ID,METRIC))]
correlations <- cor(transformed)
corrplot(correlations, order = "hclust")
highCorr <- findCorrelation(correlations, cutoff = corrCutOff)
names <- names(transformed)
names[highCorr]
length(highCorr)
if (length(highCorr) > 0)
  names <- names(transformed)[-highCorr]

data <- cbind(transformed[,which(names(transformed) %in% names)],data[,which(names(data) %in% c(ID,METRIC))])
str(data)
write.table(data, "data_not_collinear.csv", sep=",", row.names=FALSE, quote = FALSE)

# running the randomForest implementation for getting importance
# rf <- randomForest(CumProd ~., data=data[,-which(names(data) %in% c(ID))], mtry=2, ntree=50, importance=TRUE)
# define training control
train_control <- trainControl(method="LGOCV",returnResamp = "all",number = 100,p = 0.7,predictionBounds = c(TRUE,TRUE))
rf <- train(CumProd ~., data=data[,-which(names(data) %in% c(ID))], method = "rf", importance=TRUE)
# varImpPlot(rf)
# matrix with 1 column
ranks <- importance(rf$finalModel,type=1)
# sort in descending order
ranks <- sort(ranks[,],decreasing = T)
# pick up top n=10
# ranks <- head(names(ranks),IMP_VARS_CNT)
# pick non-negative
isImportant <- function(X) {
  X[ifelse(X >= 0,TRUE,FALSE)]
}
ranks <- names(isImportant(ranks))
ranks
# ranks <- names(ranks)
ranks[length(ranks)+1] <- METRIC
ranks[length(ranks)+1] <- ID
data <- data[,which(names(data) %in% ranks)]
str(data)

write.table(data, "data_training.csv", sep=",", row.names=FALSE, quote = FALSE)

# lets plot the learning curve for this data
minSamples <- 3
forTraining <- data[,-which(names(data) %in% c(ID))]
N <- nrow(forTraining)
learnings <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Sample", "Train", "Test"))), stringsAsFactors=F)
learning <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Sample", "Train", "Test"))), stringsAsFactors=F)
algo <- "M5"
for(no in minSamples:N) {
  #     no <- 19
  
  #   best.fit <- NULL
  train.RMSE <- numeric()
  test.RMSE <- numeric()
  for(iter in 1:30) {
    #         iter <- 1
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
    fit <- M5P(CumProd ~. , data = train)
    summary(fit)
    
    # on train data iteration j
    predicted <- predict(fit, newdata = train)
    actual= train$CumProd
    # The RMSE
    theTrain<-RMSE(predicted,actual)
    
    # on test data iteration j
    predicted <- predict(fit, newdata = test)
    actual= test$CumProd
    # The RMSE
    theTest<-RMSE(predicted,actual)
    
    train.RMSE[iter] <- theTrain
    test.RMSE[iter] <- theTest
    
  }
  theTrain <- mean(train.RMSE,na.rm = T)
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
iterations <- 500
best.rmse <- 100000
best.train <- NULL
best.test <- NULL
best.fit <- NULL
best.cor <- -1
# define training control
# train_control <- trainControl(method="LGOCV",returnResamp = "all",number = 100,p = 0.7,predictionBounds = c(TRUE,TRUE))

# n iterations to get best model
for(i in 1:iterations) {
  #   i<-1
  # set seed by iteration
  set.seed(i)
  # do random sampling to generate training & test sets
  trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
  # createDataPartition(data$Operator.Numeric, p = .7,list = FALSE,times = 1)
  train=forTraining[trainIndex,]
  test=forTraining[-trainIndex,]
  
  # the model - lm
  #   fit <- lm(CumProd ~. , data = train, na.action = na.pass)
    fit <- M5P(CumProd ~. , data = train, na.action = na.pass)
#   fit <- train(CumProd ~., data=train, method = "rf",trControl = train_control)
  predicted <- predict(fit, newdata = test)
  
  #Convert predicted response vector as dataframe
  actual= test$CumProd
  # predicted <- as.data.frame(predicted)
  # colnames(predicted) <- "CUM"
  #Replacing NA with average CUM from training set (NA not allowed for submission)
  # predicted[is.na(predicted)] <- mean(train$CumProd)
  
  # The RMSE
  RMSE<-sqrt((sum((actual-predicted)^2))/nrow(test))
  RMSLE <- rmsle(actual, predicted)
  COR <- cor(actual, predicted)
  # score the test data and plot pred vs. obs 
  # plot(data.frame('Predicted'=predicted, 'Observed'=actual))
  
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

# save the model to disk
save(best.fit, file='completions.model')

# load the model back from disk (prior variable name is restored)
# load('completions.model')

actual <- best.test$CumProd
predicted <- predict(best.fit,best.test)
# 
final = cbind(actual,predicted)
write.table(final, "output.csv",sep=",",row.names=FALSE, quote = FALSE)

# optimization of wells!
M <- ncol(data)
names <- names(data)
names[length(names)+1] <- "PctInc"
optimums <- data.frame(matrix(vector(), 0, M+1, dimnames=list(c(), names)), stringsAsFactors=T)
actionables <- names(data)[!(names(data) %in% nonactionables)]
actionables <- actionables[!(actionables %in% c(ID))]
data$Wellname <- as.character(data$Wellname)

for (well in 1:N) {
  #     well <- 2
  stub <- data[well, ]
  #   stub
  print(stub$Wellname)
  
  # create optimization wrapper for CUM
  opt_gm <- function(opt.x, known.x) {
    x <- ifelse(is.na(known.x), opt.x, known.x)
    z <- stub[,-which(names(stub) %in% c(ID))]
    
    # copy the data over top of the stub record
    for (i in names(x)) {
      if (i %in% actionables)
        z[[i]] <- x[[i]]
    }
    # score the data and return the negative
    y <- (-1 * predict(best.fit,z))
    y
  }
  
  # start with given values and replace by mean for actionables
  opt_start <- stub[,-which(names(stub) %in% c(ID))]
  for (i in names(opt_start)) {
    if (i %in% actionables)
      opt_start[[i]] <- mean(data[[i]])
  }
  opt_start
  
  # lower
  opt_min <- opt_start
  for (i in names(opt_min)) {
    #     if (i %in% actionables)
    opt_min[[i]] <- min(data[[i]],na.rm = T)
  }
  opt_min
  
  # upper
  opt_max <- opt_start
  for (i in names(opt_max)) {
    #     if (i %in% actionables)
    opt_max[[i]] <- max(data[[i]],na.rm = T)
  }
  opt_max
  
  opt_nons <- opt_start
  for (i in names(opt_nons)) {
    if (i %in% actionables)
      opt_nons[[i]] <- NA
  }
  opt_nons
  
  # optimize
  opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", lower=opt_min, upper=opt_max, known.x=opt_nons)
  
  # view the optimized inputs & predicted output (CUM)
  opt_results
  
  # test the optima
  temp <- stub
  for(i in names(opt_results$par)) {
    temp[[i]] <-  opt_results$par[[i]]
  }
  temp$CumProd <- abs(opt_results$value)
  temp$PctInc <- (abs(opt_results$value) - stub$CumProd)/stub$CumProd
  temp$PctInc <- temp$PctInc * 100
  #   temp$CumProd <- predict(best.fit,temp)
  optimums[well, ] <- temp
  #     predict(best.fit,temp)
}
write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)

