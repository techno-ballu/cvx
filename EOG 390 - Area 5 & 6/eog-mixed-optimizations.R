# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(gdata)
require(randomForest)
require(RWeka)
require(caret)
require(fmsb)
require(foreach)
require(Metrics)
require(e1071)
# require(corrplot)
# require(RANN)
# require(ipred)
require(plyr)
require(rJava)
require(dfoptim)
# require(proxy)
# require(googleVis)
require(DMwR)
require(rpart)

library(kernlab)

# set the work directory
setwd("/home/bolaka/EOG-data/AllWells/UsingOptimums")

if (!exists('buildRegression', mode = "function"))
  source("buildRegression.R")

if (!exists('buildOptimizations', mode = "function"))
  source("buildOptimizations.R")

# load well wise Completions data
DATA <- read.csv("eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))
str(DATA)

# some tweaking constants
VIF_THRESHOLD <- 100
IMP_VARS_CNT <- 18
trainSplit <- 0.70
corrCutOff <- 0.90
# imp vars!
ID <- "Wellname"
USELESS <- c("CUM_60PD_OIL_PROD","CUM_30PD_OIL_PROD","CUM_90PD_OIL_PROD") #"Avg.ShutIn.Days","ShutinsPeriods","ShutinDays"
METRIC <- "CumProd"
nonactionables <- c("INITIAL_FRAC_GRADIENT","FINAL_FRAC_GRADIENT","SURFACE_LOC_LAT","SURFACE_LOC_LONG","AVG_AZIMUTH","TEAM","TOTAL_VERTICAL_DEPTH")

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
original <- DATA
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

# test the correlation in the features between the highest producing wells and the lowest!
sorted <- data[order(data$CumProd),]
lowHighCor <- cor(as.numeric(sorted[1,]),as.numeric(sorted[nrow(data),]))
lowHighCor

resultWhole <- buildRegression(data)
data.cleaned <- resultWhole$data
data.model <- resultWhole$model
summary(data.model)

# create different brackets of CUM values!
# LOWs
lows <- quantile(data.cleaned$CumProd, c(0, 0.2))
lows
lowCUMs <- data.cleaned[data.cleaned$CumProd>lows[[1]] & data.cleaned$CumProd<lows[[2]], ]
str(lowCUMs)
# test the correlation in the features between the highest producing wells and the lowest!
sortedL <- lowCUMs[order(lowCUMs$CumProd),]
corL <- cor(as.numeric(sortedL[1,]),as.numeric(sortedL[nrow(lowCUMs),]))
corL
hist(x = lowCUMs$CumProd, plot = T)
lowsResult <- buildRegression(lowCUMs)
lowCUMsModel <- lowsResult$model
lowCUMs <- lowsResult$data
summary(lowCUMsModel)
# write.table(lowCUMs, "the_lows.csv", sep=",", row.names=FALSE, quote = FALSE)

# MEDIUMs
meds <- quantile(data.cleaned$CumProd, c(0.3, 0.5))
meds
medCUMs <- data.cleaned[data.cleaned$CumProd>meds[[1]] & data.cleaned$CumProd<meds[[2]], ]
str(medCUMs)
hist(x = medCUMs$CumProd, plot = T)
medsResult <- buildRegression(medCUMs)
medCUMsModel <- medsResult$model
medCUMs <- medsResult$data
summary(medCUMsModel)
write.table(medCUMs, "the_meds.csv", sep=",", row.names=FALSE, quote = FALSE)
# medCUMs <- merge(medCUMs, original[,c(1,18:20)], by=ID)

# HIGHs
highs <- quantile(data.cleaned$CumProd, c(0.6, 0.8))
highs
highCUMs <- data.cleaned[data.cleaned$CumProd>highs[[1]] & data.cleaned$CumProd<highs[[2]], ]
str(highCUMs)
hist(x = highCUMs$CumProd, plot = T)
highsResult <- buildRegression(highCUMs)
highCUMsModel <- highsResult$model
highCUMs <- highsResult$data
summary(highCUMsModel)
write.table(highCUMs, "the_highs.csv", sep=",", row.names=FALSE, quote = FALSE)

# VERY HIGHs
veryhighs <- quantile(data.cleaned$CumProd, c(0.9, 1.0))
veryhighCUMs <- data.cleaned[data.cleaned$CumProd>veryhighs[[1]] & data.cleaned$CumProd<veryhighs[[2]], ]
# veryhighCUMs <- read.csv("the_veryhighs.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
str(veryhighCUMs)
hist(x = veryhighCUMs$CumProd, plot = T)
veryhighsResult <- buildRegression(veryhighCUMs)
veryhighCUMsModel <- veryhighsResult$model
veryhighCUMs <- veryhighsResult$data
summary(veryhighCUMsModel)
write.table(veryhighCUMs, "the_veryhighs.csv", sep=",", row.names=FALSE, quote = FALSE)

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
# save(medCUMsModel, file = "medium_completions_gaussian.rda")

# optimization of all wells!
optimumsAll <- buildOptimizations(data.cleaned,data.model)
hist(x = optimumsAll$CumProd, plot = T)
str(optimumsAll)
da <- rbind(data.cleaned,optimumsAll)
write.table(da, "all.csv",sep=",",row.names=FALSE, quote = FALSE)
resultAll <- buildRegression(da)
summary(resultAll$model)

# optimization of very high wells!
optimumsVH <- buildOptimizations(veryhighCUMs,veryhighCUMsModel)
str(optimumsVH)
d <- rbind(data,optimumsVH)
write.table(d, "merged.csv",sep=",",row.names=FALSE, quote = FALSE)

resultMerged <- buildRegression(d)
summary(resultMerged$model)

# optimization of high wells!
optimumsH <- buildOptimizations(highCUMs,highCUMsModel)
str(optimumsH)
d1 <- rbind(d,optimumsH)
write.table(d1, "merged1.csv",sep=",",row.names=FALSE, quote = FALSE)

resultMerged1 <- buildRegression(d1)
summary(resultMerged1$model)

# optimization of med wells!
optimumsM <- buildOptimizations(medCUMs,medCUMsModel)
str(optimumsM)
d2 <- rbind(d1,optimumsM)
write.table(d1, "merged2.csv",sep=",",row.names=FALSE, quote = FALSE)

resultMerged2 <- buildRegression(d2)
summary(resultMerged1$model)

