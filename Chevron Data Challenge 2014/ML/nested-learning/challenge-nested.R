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
library(Metrics)
library(e1071)
library(corrplot)
library(RANN)
library(ipred)
library(plyr)
library(rJava)
library(dfoptim)
library(proxy)
library(googleVis)
library(DMwR)
library(rpart)

# set the work directory
setwd("~/Chevron/DSChallenge/nested-learning/results")

if (!exists('challengeModel', mode = "function"))
  source("../challenge-model.R")

# load base_training.csv
base <- read.csv("../base_training.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# remove the Deepest Zone as it will be added below
base <- base[, -which(names(base) %in% c("Deepest_Zone"))]
# split Between Zone into 2
zones=data.frame(do.call(rbind, strsplit(as.vector(base$Between_Zone), split = " --> ", fixed=TRUE)))
names(zones) <- c("Top_Zone", "Deepest_Zone")
base <- cbind(base, zones)

# format Completion.Date
# base$Completion.Date = as.Date(base$Completion.Date, origin = "1900-01-01")

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
DATA <- merge(temp, geo, by="WellID")
write.table(DATA, "data.csv", sep=",", row.names=FALSE, quote = FALSE)
print(paste("=============== Data merged into one file with", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))

# load well wise Completions data
# DATA <- read.csv("../eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# print(paste("=============== Data has", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))
str(DATA)

# some tweaking constants
missingThreshold <- 0.8
# transformations <- c("BoxCox", "center", "scale") # ,"bagImpute"
iterations <- 100
VIF_THRESHOLD <- 10
trainSplit <- 0.70
corrCutOff <- 0.70

# imp vars!
ID <- "WellID"
USELESS <- c("Completion.Date")
METRIC <- "EUR_o..Mstb."
outliers <- c("Fluid.Water..Gals.","Acid..Gals.","Gel.x.link..Gals.","Proppant...Total..lbs.","Fluid...Total..lbs.","Fluid.Amount","Propping.Agent.Amount") #"Other..Gals.",

# names(DATA)[names(DATA) == "CUM_180PD_OIL_PROD"] <- METRIC

# replace empty strings and #VALUE! as NAs for all columns
for(i in names(DATA)) {
  if (length(which(isUnknown(x=DATA[[i]], unknown = c("","#VALUE!")))) > 0) {
    DATA[[i]] <- unknownToNA(x=DATA[[i]], unknown=c("","#VALUE!"))
    print(paste(i,"replaced garbage=", any(isUnknown(x=DATA[[i]], unknown = c("","#VALUE!"))), sep = " "))
  }
}
# str(DATA)

# remove some unwanted columns
# DATA <- DATA[, -which(names(DATA) %in% USELESS)]
# str(DATA)

# detect the numerical columns to remove outliers
data.numericals <- DATA[, which(names(DATA) %in% outliers)]
str(data.numericals)
for(i in names(data.numericals)) {
  quantiles <- quantile(data.numericals[[i]], c(.05, .95), na.rm = TRUE)
  if (quantiles[1] >= 0 && quantiles[2] >= 0) {
    print(i)
    data.numericals[[i]][ data.numericals[[i]] < quantiles[1] ] <- quantiles[1]
    data.numericals[[i]][ data.numericals[[i]] > quantiles[2] ] <- quantiles[2]
  }
}
data.others <- DATA[ , -which(names(DATA) %in% outliers)]
str(data.others)
data.cleaned <- cbind(data.numericals, data.others)

write.table(data.cleaned, "data_edits1.csv", sep=",", row.names=FALSE, quote = FALSE)
str(data.cleaned)

# data.cleaned <- DATA

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
DATA <- data.colsCleaned

# hist(x = DATA$EUR_o..Mstb., plot = T)

# divide into training and test sets
testing <- subset(DATA, is.na(DATA$EUR_o..Mstb.))
data <- subset(DATA, !is.na(DATA$EUR_o..Mstb.))

# category labeling
types <- lapply(data, class)
distincts <- lapply(data, function(c) unique(c))

for(i in names(data)){
  noOfCats <- length(levels(distincts[[i]]))
  if (noOfCats == 1) {
    data[[i]] <- NULL
  } else if ((noOfCats <= length(data[[i]])/2 && types[[i]] == "factor") || types[[i]] == "character") {
    print(paste("processing ====", i, sep = "> "))
    means <- sapply(split(data$EUR_o..Mstb., data[[i]]), function(x) mean(x, na.rm=TRUE))
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

# the normal fit that we do!
# theModel <- challengeModel(data)
# summary(theModel)

# TODO change the cut-offs
LOW <- 0.55
HIGH <- 0.96

# LOW <- 0.65
# HIGH <- 0.98

l1 <- 0.15
l2 <- 0.35

m1 <- 0.65
m2 <- 0.85

h1 <- 0.98
h2 <- 1

marks1 <- as.numeric(quantile(data$EUR_o..Mstb., c(0, LOW, HIGH, 1)))
marks1
marks2 <- as.numeric(quantile(data$EUR_o..Mstb., c(l1, l2, m1, m2, h1, h2)))
marks2
# h <- hist(x = data$EUR_o..Mstb., plot = T, breaks = marks)
# h$counts

# integer code the EURs
copy <- data
# copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = marks2, include.lowest = T, labels = c(1,2,3,4,5)) # labels = FALSE, 
copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = marks1, include.lowest = T, labels = F) 
# copy$Class <- factor(copy$Class)
# str(copy)
table(copy$Class)

# trainIndex <- createDataPartition(copy$Class, times = 1, p = 0.010, groups = 3, list = F)
# t=data[trainIndex,]
# hist(x = t$EUR_o..Mstb., plot = T, breaks = 3)
# write.table(t, "stratified.csv",sep=",",row.names=FALSE, quote = FALSE)
# data <- t

temp <- copy[,-which(names(copy) %in% c(METRIC))] # ID,
temp$Class <- as.numeric(temp$Class)
table(temp$Class)
temp <- subset(temp, !Class %in% c(2,4))
temp$Class <- factor(temp$Class)
temp$Class <- revalue(temp$Class, c("3"="2", "5"="3"))
table(temp$Class)
str(temp)

copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = marks1, include.lowest = T, labels = FALSE) # labels = FALSE, 
table(copy$Class)

# fitOri <- rpart(Class ~ ., data = temp, method = "class")
# summary(residuals(fitOri))
# pred <- predict(fitOri, type="class") #newdata = temp[,-which(names(temp) %in% c("Class"))]
# confusionMatrix(pred, temp$Class)
fitOri <- challengeModel(temp,F,T)
pred <- predict(fitOri, type="class",newdata = copy)
cm <- confusionMatrix(pred, copy$Class)
cm
sensitivity <- mean(cm$byClass[1:3,1])
sensitivity
accuracy <- mean(cm$byClass[1:3,8])
accuracy

# optimized SMOTE to get best fit!
fSmote <- function(x) {
  seed <- floor(x[1])
  set.seed(seed)
  t <- SMOTE(Class ~ ., data = temp, k = 5) #, perc.over = 300, perc.under = 300, k = 5, learner = "rpartXse", se = 0.5
  table(t$Class)
  fit <- challengeModel(t,F,T)
  pred <- predict(fit, type="class",newdata = copy)
  cm <- confusionMatrix(pred, copy$Class)
  sensitivity <- mean(cm$byClass[1:3,1])
  sensitivity
}

# fSmote(100)
xmin <- optimize(fSmote, c(1, 5000), maximum = T) # , tol = 0.0001
xmin

# set.seed(xmin$maximum)
set.seed(384)
t <- SMOTE(Class ~ ., data = temp, k = 5) #, perc.over = 300, perc.under = 300, k = 5, learner = "rpartXse", se = 0.5
table(t$Class)
fitSMOTE <- challengeModel(t,F,T)
pred <- predict(fitSMOTE, type="class",newdata = copy)
cm <- confusionMatrix(pred, copy$Class)
cm
sensitivity <- mean(cm$byClass[1:3,1])
sensitivity
accuracy <- mean(cm$byClass[1:3,8])
accuracy

# optimized Down-sampling to get best fit!
fDown <- function(x) {
  seed <- floor(x[1])
  set.seed(seed)
  t <- downSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
  table(t$Class)
  fit <- challengeModel(t,F,T)
  pred <- predict(fit, type="class",newdata = copy)
  cm <- confusionMatrix(pred, copy$Class)
  sensitivity <- mean(cm$byClass[1:3,1])
  sensitivity
}

# fDown(1000)
xmin <- optimize(fDown, c(1, 5000), maximum = T) # , tol = 0.0001
xmin

# set.seed(xmin$maximum)
set.seed(384)
t <- downSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
table(t$Class)
fitDownSample <- challengeModel(t,F,T)
pred <- predict(fitDownSample, type="class",newdata = copy)
cm <- confusionMatrix(pred, copy$Class)
sensitivity <- mean(cm$byClass[1:3,1])
sensitivity
accuracy <- mean(cm$byClass[1:3,8])
accuracy

# # create a predictors sample having maximum dissimilarity! (is taking much too time!)
# set.seed(300)
# # A random sample of 1 data points
# startSet <- sample(1:dim(copy)[1], 1)
# samplePool <- copy[-startSet, ]
# start <- copy[startSet, ]
# trainIndex <- maxDissim(start, samplePool, n = 30)
# t=copy[trainIndex,]

# optimized Down-sampling to get best fit!
fUp <- function(x) {
  seed <- floor(x[1])
  set.seed(seed)
  t <- upSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
  table(t$Class)
  fit <- challengeModel(t,F,T)
  pred <- predict(fit, type="class",newdata = copy)
  cm <- confusionMatrix(pred, copy$Class)
  sensitivity <- mean(cm$byClass[1:3,1])
  sensitivity
}

# fUp(3467)
xmin <- optimize(fUp, c(1, 5000), maximum = T) # , tol = 0.0001
xmin

# set.seed(xmin$maximum)
set.seed(1910)
t <- upSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
table(t$Class)
fitUpSample <- challengeModel(t,F,T)
pred <- predict(fitUpSample, type="class",newdata = copy)
cm <- confusionMatrix(pred, copy$Class)
cm
sensitivity <- mean(cm$byClass[1:3,1])
sensitivity
accuracy <- mean(cm$byClass[1:3,8])
accuracy

# t <- t[rep(row.names(t), 3),]

# hist(x = t$EUR_o..Mstb., plot = T, breaks = 3)
# s <- t[order(t$EUR_o..Mstb.),]
# line <- gvisLineChart(xvar = "Wellname", yvar = c("EUR_o..Mstb."), data = s)
# plot(line)
# leftover=data[-trainIndex,]
# leftover <- data[-which(row.names(data) %in% row.names(t)),]
# sortingHat <- challengeModel(t[,-which(names(t) %in% c("Class"))])
# write.table(t, "stratified.csv",sep=",",row.names=FALSE, quote = FALSE)
# summary(sortingHat)

# create different brackets of CUM values!
# LOWs - either fit on t$Class 1 i.e 68 samples or the whole data within the brackets
# lows <- quantile(data$EUR_o..Mstb., c(0, 0.33))
# lowCUMs <- data[data$EUR_o..Mstb.>lows[[1]] & data$EUR_o..Mstb.<lows[[2]], ]
lowCUMs <- data[data$EUR_o..Mstb.>=marks1[[1]] & data$EUR_o..Mstb.<=marks1[[2]], ]
# str(lowCUMs)
hist(x = lowCUMs$EUR_o..Mstb., plot = T)
lowCUMsModel <- challengeModel(lowCUMs)
summary(lowCUMsModel)

# MEDIUMs
# meds <- quantile(data$EUR_o..Mstb., c(0.33, 0.66))
# medCUMs <- data[data$EUR_o..Mstb.>meds[[1]] & data$EUR_o..Mstb.<meds[[2]], ]
medCUMs <- data[data$EUR_o..Mstb.>marks1[[2]] & data$EUR_o..Mstb.<=marks1[[3]], ]
# str(medCUMs)
hist(x = medCUMs$EUR_o..Mstb., plot = T)
medCUMsModel <- challengeModel(medCUMs)
summary(medCUMsModel)

# HIGHs
# highs <- quantile(data$EUR_o..Mstb., c(0.66, 1.0))
# highCUMs <- data[data$EUR_o..Mstb.>highs[[1]] & data$EUR_o..Mstb.<highs[[2]], ]
highCUMs <- data[data$EUR_o..Mstb.>marks1[[3]] & data$EUR_o..Mstb.<=marks1[[4]], ]
# str(highCUMs)
hist(x = highCUMs$EUR_o..Mstb., plot = T)
highCUMsModel <- challengeModel(highCUMs)
summary(highCUMsModel)

# predict each instance of data using model
d <- copy[,-which(names(copy) %in% c(ID))]
model <- fitOri
predicted<-foreach(i=1:nrow(d),.combine=rbind) %do% {
#   i<-100
  print(i)
  inst <- d[i,]
  predCUM <- NA
  actual <- inst$Class
  #   flagCUM <- predict(model,newdata = inst)
  class <- predict(model,newdata = inst,type = "class")
  sorter <- class
#   sorter <- as.numeric(class)
  if (sorter == 1)
    predCUM <- predict(lowCUMsModel,newdata = inst)
  else if (sorter == 2)
    predCUM <- predict(medCUMsModel,newdata = inst)
  else
    predCUM <- predict(highCUMsModel,newdata = inst)
  predCUM
}
d$predicted <- as.vector(predicted)
str(d)
write.table(d, "results.csv",sep=",",row.names=FALSE, quote = FALSE)

RMSE <- RMSE(d$predicted, d$EUR_o..Mstb.)
RMSE

RMSLE <- rmsle(actual = d$EUR_o..Mstb.,predicted = d$predicted)
RMSLE

COR <- cor(d$EUR_o..Mstb., d$predicted)
COR



# d <- data
# model <- sortingHat
# predicted<-foreach(i=1:nrow(d),.combine=rbind) %do% {
#   print(i)
#   inst <- d[i,]
#   predCUM <- NA
#   actual <- inst$EUR_o..Mstb.
#   flagCUM <- predict(model,newdata = inst)
#   sorter <- flagCUM
#   if (sorter <= lows[[2]])
#     predCUM <- predict(lowCUMsModel,newdata = inst)
#   else if (sorter <= meds[[2]])
#     predCUM <- predict(medCUMsModel,newdata = inst)
#   else
#     predCUM <- predict(highCUMsModel,newdata = inst)
#   predCUM
# }
# d$predicted <- as.vector(predicted)
# str(d)
# write.table(d, "results.csv",sep=",",row.names=FALSE, quote = FALSE)
# 
# RMSE <- RMSE(d$predicted, d$EUR_o..Mstb.)
# RMSE
# 
# RMSLE <- rmsle(actual = d$EUR_o..Mstb.,predicted = d$predicted)
# RMSLE
# 
# COR <- cor(d$EUR_o..Mstb., d$predicted)
# COR
# # train on whole data
# fit <- M5P(EUR_o..Mstb. ~. , data = forTraining)
# predicted <- predict(fit, newdata = forTraining)
# actual <- forTraining$EUR_o..Mstb.
# RMSE <- RMSE(pred = predicted, obs = actual)
# COR <- cor(actual, predicted)
# summary(fit)
# print(RMSE)
# print(COR)
# 
# # save the model to disk
# rJava::.jcache(best.fit$classifier)
# save(best.fit, file = "completions_final.rda")
# 
# # optimization of wells!
# M <- ncol(data)
# names <- names(data)
# actionables <- names(data)[!(names(data) %in% nonactionables)]
# actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
# optimums <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
# actionables <- actionables[!(actionables %in% c(ID))]
# data$Wellname <- as.character(data$Wellname)
# 
# for (well in 1:N) {
#   well <- 11
#   stub <- data[well, ]
#   print(stub$Wellname)
#   name=unlist(strsplit(gsub("-", " ", stub$Wellname),split=" ",fixed=TRUE))[1]
#   wellsData <- data[grepl(name,data$Wellname),]
#   
#   # create optimization wrapper for CUM
#   opt_gm <- function(x, known.x) { #
#     z <- stub[,-which(names(stub) %in% c(ID,METRIC))]
#     
#     #If these conditions are violated, the function returns a large positive number
#     # which the search procedure will avoid
#     # if(x[3] < 0 | x[3] > 1) return(10^38)
#     
#     # copy the data over top of the stub record
#     for (i in names(x)) {
#       if (i %in% actionables) {
#         z[[i]] <- x[[i]]
#       } else {
#         z[[i]] <- known.x[[i]]
#       }
#     }
#     # score the data and return the negative
#     y <- predict(best.fit,newdata = z)
#     -y
#   }
#   
#   # start with given values and replace by mean for actionables
#   start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
#   
#   # lower
#   opt_min <- start
#   for (i in names(opt_min)) {
#     opt_min[[i]] <- min(wellsData[[i]],na.rm = T)
#   }
#   opt_min
#   
#   # upper
#   opt_max <- start
#   for (i in names(opt_max)) {
#     opt_max[[i]] <- max(wellsData[[i]],na.rm = T)
#   }
#   opt_max
#   
#   opt_start <- start
#   for (i in names(opt_start)) {
#     if (i %in% actionables) {
#       opt_start[[i]] <- mean(wellsData[[i]],na.rm = T)
#     }
#   }
#   opt_start
#   
#   opt_nons <- start
#   for (i in names(opt_nons)) {
#     if (i %in% actionables)
#       opt_nons[[i]] <- NA
#   }
#   opt_nons
#   
#   # optimize
#   opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max,known.x=opt_nons) # ,
#   #   opt_results <- nmkb(opt_start, opt_gm, lower=opt_min, upper=opt_max,known.x=opt_nons) # result ~ 
#   
#   # view the optimized inputs & predicted output (CUM)
#   opt_results
#   
#   # test the optima
#   temp <- stub
#   action <- stub
#   for(i in names(opt_results$par)) {
#     opti <- opt_results$par[[i]]
#     ori <- temp[[i]]
#     action[[i]] <- "NA"
#     if (ori != 0)
#       action[[i]] <- ((opti - ori)/ori)*100
#     temp[[i]] <-  opti
#   }
#   
#   temp$EUR_o..Mstb. <- abs(opt_results$value)
#   action$EUR_o..Mstb. <- ((temp$EUR_o..Mstb. - stub$EUR_o..Mstb.)/stub$EUR_o..Mstb.) * 100
#   
#   optimums[well, ] <- temp
#   actions[well, ] <- action
# }
# write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
# write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
