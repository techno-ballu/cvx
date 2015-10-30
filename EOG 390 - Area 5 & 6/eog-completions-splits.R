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

# set the work directory
setwd("/home/bolaka/EOG-data/AllWells")

if (!exists('buildModel', mode = "function"))
  source("buildModel.R")

# load well wise Completions data
DATA <- read.csv("eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))
# str(DATA)

# some tweaking constants
VIF_THRESHOLD <- 10
trainSplit <- 0.70
corrCutOff <- 0.85
# imp vars!
ID <- "Wellname"
USELESS <- c("CUM_60PD_OIL_PROD","CUM_30PD_OIL_PROD","CUM_90PD_OIL_PROD") #"Avg.ShutIn.Days","ShutinsPeriods","ShutinDays"
METRIC <- "CumProd"
nonactionables <- c("INITIAL_FRAC_GRADIENT","FINAL_FRAC_GRADIENT","SURFACE_LOC_LAT","SURFACE_LOC_LONG","AVG_AZIMUTH")

names(DATA)[names(DATA) == "CUM_180PD_OIL_PROD"] <- METRIC

# replace "undefined", empty strings and #VALUE! as NAs for all columns
for(i in names(DATA)) {
  if (length(which(isUnknown(x=DATA[[i]], unknown = c("undefined","","#VALUE!")))) > 0) {
    DATA[[i]] <- unknownToNA(x=DATA[[i]], unknown=c("undefined","","#VALUE!"))
    print(paste(i,"replaced UNDEFINEDs=", any(isUnknown(x=DATA[[i]], unknown = c("undefined","","#VALUE!"))), sep = " "))
  }
}
# str(DATA)

# remove some unwanted columns
DATA <- DATA[, -which(names(DATA) %in% USELESS)]
# str(DATA)

# hist(x = DATA$CumProd, plot = T)

# divide into training and test sets
testing <- subset(DATA, is.na(DATA$CumProd))
data <- subset(DATA, !is.na(DATA$CumProd))

# category labeling
types <- lapply(data, class)
distincts <- lapply(data, function(c) unique(c))

for(i in names(data)){
  noOfCats <- length(levels(distincts[[i]]))
  if (noOfCats == 1) {
    data[[i]] <- NULL
  } else if ((noOfCats <= length(data[[i]])/2 && types[[i]] == "factor") || types[[i]] == "character") {
    print(paste("processing ====", i, sep = "> "))
    means <- sapply(split(data$CumProd, data[[i]]), function(x) mean(x, na.rm=TRUE))
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

# integer code the CUMs
data$CumClass <- cut(x = data$CumProd, breaks = 4, labels = FALSE)
data$CumClass <- factor(data$CumClass)
str(data)
table(data$CumClass)

# trainIndex <- createDataPartition(data$CumProd, times = 1, p = 0.15, groups = 3, list = F)
# t=data[trainIndex,]
# hist(x = t$CumProd, plot = T)
# write.table(t, "stratified.csv",sep=",",row.names=FALSE, quote = FALSE)
# data <- t

theModel <- buildModel(data)
summary(theModel)

set.seed(1000)
t <- SMOTE(CumClass ~ ., data = data) #  perc.over = 200, perc.under = 200, k = 5, learner = "rpartXse", se = 0.5
dim(t)
table(t$CumClass)
write.table(t, "smoted.csv",sep=",",row.names=FALSE, quote = FALSE)

# # create a predictors sample having maximum dissimilarity!
# set.seed(300)
# # A random sample of 1 data points
# startSet <- sample(1:dim(data)[1], 1)
# samplePool <- data[-startSet, ]
# start <- data[startSet, ]
# trainIndex <- maxDissim(start, samplePool, n = 70)
# t=data[trainIndex,]
# t <- t[rep(row.names(t), 3),]
t <- t[,-which(names(t) %in% c("CumClass"))]
str(t)
hist(x = t$CumProd, plot = T)
# s <- t[order(t$CumProd),]
# line <- gvisLineChart(xvar = "Wellname", yvar = c("CumProd"), data = s)
# plot(line)
# leftover=data[-trainIndex,]
leftover <- data[-which(row.names(data) %in% row.names(t)),]
sortingHat <- buildModel(t)
# write.table(t, "stratified.csv",sep=",",row.names=FALSE, quote = FALSE)
summary(sortingHat)

# create different brackets of CUM values!
data <- data[,-which(names(data) %in% c("CumClass"))]
# LOWs
lows <- quantile(data$CumProd, c(0, 0.33))
lowCUMs <- data[data$CumProd>lows[[1]] & data$CumProd<lows[[2]], ]
str(lowCUMs)
hist(x = lowCUMs$CumProd, plot = T)
lowCUMsModel <- buildModel(lowCUMs)
summary(lowCUMsModel)

# MEDIUMs
meds <- quantile(data$CumProd, c(0.33, 0.66))
medCUMs <- data[data$CumProd>meds[[1]] & data$CumProd<meds[[2]], ]
str(medCUMs)
hist(x = medCUMs$CumProd, plot = T)
medCUMsModel <- buildModel(medCUMs)
summary(medCUMsModel)

# HIGHs
highs <- quantile(data$CumProd, c(0.66, 1.0))
highCUMs <- data[data$CumProd>highs[[1]] & data$CumProd<highs[[2]], ]
str(highCUMs)
hist(x = highCUMs$CumProd, plot = T)
highCUMsModel <- buildModel(highCUMs)
summary(highCUMsModel)

# predict each instance of data using sortingHat
d <- leftover
predicted<-foreach(i=1:nrow(d),.combine=rbind) %do% {
  print(i)
  inst <- d[i,]
  predCUM <- NA
  actual <- inst$CumProd
  flagCUM <- predict(sortingHat,newdata = inst)
  if (flagCUM <= lows[[2]])
    predCUM <- predict(lowCUMsModel,newdata = inst)
  else if (flagCUM <= meds[[2]])
    predCUM <- predict(medCUMsModel,newdata = inst)
  else
    predCUM <- predict(highCUMsModel,newdata = inst)
  predCUM
}
d$predicted <- as.vector(predicted)
str(d)
write.table(d, "results.csv",sep=",",row.names=FALSE, quote = FALSE)

RMSE <- RMSE(d$predicted, d$CumProd)
RMSE

RMSLE <- rmsle(actual = d$CumProd,predicted = d$predicted)
RMSLE

COR <- cor(d$CumProd, d$predicted)
COR
# # train on whole data
# fit <- M5P(CumProd ~. , data = forTraining)
# predicted <- predict(fit, newdata = forTraining)
# actual <- forTraining$CumProd
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
#   temp$CumProd <- abs(opt_results$value)
#   action$CumProd <- ((temp$CumProd - stub$CumProd)/stub$CumProd) * 100
#   
#   optimums[well, ] <- temp
#   actions[well, ] <- action
# }
# write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
# write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
