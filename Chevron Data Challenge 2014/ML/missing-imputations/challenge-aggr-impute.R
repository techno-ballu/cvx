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
# library(corrplot)
# library(RANN)
# library(ipred)
library(plyr)
# library(rJava)
library(dfoptim)
# library(proxy)
library(googleVis)
library(DMwR)
library(rpart)
# library(earth)
library(kernlab)
library(MASS)
# library(elasticnet)
library(robustbase)
library(FSelector)
library(robCompositions)

# set the work directory
setwd("~/Chevron/DSChallenge/missing-imputations/results")

if (!exists('challengeRegression', mode = "function"))
  source("../../using-optimizations/challenge-regression.R")

# if (!exists('challengeOptimizations', mode = "function"))
#   source("../challenge-optimizations.R")

# load base_training.csv
base <- read.csv("../base_training.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# remove the Deepest Zone as it will be added below
base <- base[, -which(names(base) %in% c("Deepest_Zone"))]
# split Between Zone into 2
zones=data.frame(do.call(rbind, strsplit(as.vector(base$Between_Zone), split = " --> ", fixed=TRUE)))
names(zones) <- c("Top_Zone", "Deepest_Zone")
base <- cbind(base, zones)

write.table(base, "base_training_edits.csv", sep=",", row.names=FALSE, quote = FALSE)

# load completions_training.csv with Remarks removed!
completions <- read.csv("../completions_training_removedRemarks.csv", header = T)
# unique completions
uniqueCompletions <- unique( completions )

uniqueCompletions$Treated.Stage.Height <- uniqueCompletions$Depth.Base - uniqueCompletions$Depth.Top
uniqueCompletions$Fluid.Amount.Per.Foot <- uniqueCompletions$Fluid.Amount / uniqueCompletions$Treated.Stage.Height
uniqueCompletions$Propping.Agent.Amount.Per.Foot <- uniqueCompletions$Propping.Agent.Amount / uniqueCompletions$Treated.Stage.Height

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
# colnames(geo) <- c("WellID","Clearfork","Upper_Spraberry","Lower_Spraberry","Wolfcamp","Lower_Wolfcamp",
#                    "Amacker_Tippet_Base","Cline","Strawn","Atoka","Bend","Lower_Atoka","Mississippian",
#                    "Woodford","Devonian")
colnames(geo) <- c("WellID",1,2,3,4,5,6,7,8,9,10,11,12,13,14)
geo_names <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
# Clearfork, Upper Spraberry, Lower Spraberry, Wolfcamp,  Lower Wolfcamp, Amacker Tippet Base, Cline, Strawn, Atoka, Bend, Lower Atoka, Mississippian, Woodford, Devonian
map <- c("Clearfork" = 1,"Upper_Spraberry" = 2,"Lower_Spraberry" = 3,"Wolfcamp" = 4,"Lower_Wolfcamp" = 5,
         "Amacker_Tippet_Base" = 6,"Cline" = 7,"Strawn" = 8,"Atoka" = 9,"Bend" = 10,"Lower_Atoka" = 11,"Mississippian" = 12,
         "Woodford" = 13,"Devonian" = 14)
write.table(geo, "geology_training_edits.csv", sep=",", row.names=FALSE, quote = FALSE)

# load scores derived from Remarks!
# scores <- read.csv("../score_sum.csv", header = T)

# merging the files
temp <- merge(base, uniqueCompletions, by="WellID")
DATA <- merge(temp, geo, by="WellID")
# DATA <- merge(base,geo)
# DATA <- merge(DATA, scores, by="WellID")

# replace geo names in top & between zones
DATA$Top_Zone <- as.character(DATA$Top_Zone)
DATA$Top_Zone[DATA$Top_Zone %in% c("CLFK")] <- 1
DATA$Top_Zone[DATA$Top_Zone %in% c("SPBR_L")] <- 3
DATA$Top_Zone[DATA$Top_Zone %in% c("SPBR_U")] <- 2
DATA$Top_Zone[DATA$Top_Zone %in% c("STRAWN")] <- 8
DATA$Top_Zone[DATA$Top_Zone %in% c("WFMP")] <- 4

DATA$Deepest_Zone <- as.character(DATA$Deepest_Zone)
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("ATOKA")] <- 9
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("MISS")] <- 12
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("SPBR_L")] <- 3
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("STRAWN")] <- 8
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("WFMP")] <- 4
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("WOOD")] <- 13
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("SIL_DEV")] <- 15
DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("FUSS")] <- 16

# DATA$Top_Zone[DATA$Top_Zone %in% c("CLFK")] <- "Clearfork"
# DATA$Top_Zone[DATA$Top_Zone %in% c("SPBR_L")] <- "Lower_Spraberry"
# DATA$Top_Zone[DATA$Top_Zone %in% c("SPBR_U")] <- "Upper_Spraberry"
# DATA$Top_Zone[DATA$Top_Zone %in% c("STRAWN")] <- "Strawn"
# DATA$Top_Zone[DATA$Top_Zone %in% c("WFMP")] <- "Wolfcamp"
# 
# DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("ATOKA")] <- "Atoka"
# DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("MISS")] <- "Mississippian"
# DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("SPBR_L")] <- "Lower_Spraberry"
# DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("STRAWN")] <- "Strawn"
# DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("WFMP")] <- "Wolfcamp"
# DATA$Deepest_Zone[DATA$Deepest_Zone %in% c("WOOD")] <- "Woodford"

DATA[["Total.Height"]] <- 0
for(i in 1:nrow(DATA)) {
  if (!is.null(DATA[[DATA$Deepest_Zone[i]]]) & !is.null(DATA[[DATA$Top_Zone[i]]])) #
    DATA[["Total.Height"]][i] <- DATA[[DATA$Deepest_Zone[i]]][i] - DATA[[DATA$Top_Zone[i]]][i]
}
DATA$Total.Height<- unknownToNA(x=DATA$Total.Height, unknown=c(0))

DATA[["Geology.Score"]] <- 0
for(i in 1:nrow(DATA)) {
  #   i <- 1
  if (!is.null(DATA[[DATA$Deepest_Zone[i]]]) & !is.null(DATA[[DATA$Top_Zone[i]]])) {
    start <- DATA$Top_Zone[i]
    end <- DATA$Deepest_Zone[i]
    score <- 0
    for(j in as.numeric(start):as.numeric(end)) {
      #         j <- 2
      if (j != as.numeric(end))
        score <- score + (DATA[[as.character(j+1)]][i] - DATA[[as.character(j)]][i])*j
      else {
        score <- score + (DATA[[as.character(j+1)]][i] - DATA[[as.character(j)]][i])*j
#         actualEnd <- ifelse(DATA[[as.character(j+1)]][i] > DATA$Depth.Total.Driller..ft.[i], DATA$Depth.Total.Driller..ft.[i], DATA[[as.character(j+1)]][i])
#         score <- score + (actualEnd - DATA[[as.character(j)]][i])*j
      }
    }
    DATA[["Geology.Score"]][i] <- score
  }
}
DATA$Geology.Score<- unknownToNA(x=DATA$Geology.Score, unknown=c(0))
DATA$Between_Zone <- (as.numeric(DATA$Deepest_Zone) - as.numeric(DATA$Top_Zone)) + 1

DATA$Top_Zone <- as.numeric(DATA$Top_Zone)
DATA$Deepest_Zone <- as.numeric(DATA$Deepest_Zone)

# Drop the geology columns
DATA <- DATA[,which(!names(DATA) %in% geo_names)]

write.table(DATA, "data.csv", sep=",", row.names=FALSE, quote = FALSE)
print(paste("=============== Data merged into one file with", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))
str(DATA)

# some tweaking constants
missingThreshold <- 0.8
# transformations <- c("BoxCox", "center", "scale") # ,"bagImpute"
iterations <- 100
VIF_THRESHOLD <- 10
IMP_VARS_CNT <- 10
trainSplit <- 0.70
corrCutOff <- 0.90
naCutOff <- 0.70

# imp vars!
ID <- "WellID"
USELESS <- c("Depth.Top","Propping.Agent.Units","Fluid.Units") #,,,"Propping.Agent.Amount","Fluid.Amount","Between_Zone"
METRIC <- "EUR_o..Mstb."
outliers <- c("Fluid.Water..Gals.","Acid..Gals.","Gel.x.link..Gals.","Proppant...Total..lbs.","Fluid...Total..lbs.","Fluid.Amount","Propping.Agent.Amount","Propping.Agent.Amount.Per.Foot","Fluid.Amount.Per.Foot") #"Other..Gals.",
nonactionables <- c("Subarea","Operator","County","Completion.Date","Completion.Year","Surface.Latitude","Surface.Longitude","Depth.Total.Driller..ft.","Between_Zone",
                    "Top_Zone","Deepest_Zone","Depth.Top","Depth.Base","Fluid.Units","Propping.Agent.Units","CLFK..KHW.","U_SPBR..KHW.",
                    "L_SPBR..KHW.","WFMP..KHW.","WFMP_L..KHW.","WFMP_ATB..KH","CLINE..KHW.","STRN..KHW.","ATOK..KHW.","BEND..KHW.",
                    "ATOKA_L..KHW","MPLM..KHW.","WDFD..KHW.","DVNN..KHW.")

DATA <- DATA[, -which(names(DATA) %in% USELESS)]

# replace empty strings and #VALUE! as NAs for all columns
for(i in names(DATA)) {
  if (length(which(isUnknown(x=DATA[[i]], unknown = c("","#VALUE!")))) > 0) {
    DATA[[i]] <- unknownToNA(x=DATA[[i]], unknown=c("","#VALUE!"))
    print(paste(i,"replaced garbage=", any(isUnknown(x=DATA[[i]], unknown = c("","#VALUE!"))), sep = " "))
  }
}
# str(DATA)

# detect the numerical columns to remove outliers
data.numericals <- DATA[, which(names(DATA) %in% outliers)]
# str(data.numericals)
for(i in names(data.numericals)) {
  quantiles <- quantile(data.numericals[[i]], c(.01, .99), na.rm = TRUE)
  if (quantiles[1] >= 0 && quantiles[2] >= 0) {
    print(i)
    data.numericals[[i]][ data.numericals[[i]] < quantiles[1] ] <- quantiles[1]
    data.numericals[[i]][ data.numericals[[i]] > quantiles[2] ] <- quantiles[2]
  }
}
data.others <- DATA[ , -which(names(DATA) %in% outliers)]
# str(data.others)
data.cleaned <- cbind(data.numericals, data.others)

write.table(data.cleaned, "data_edits1.csv", sep=",", row.names=FALSE, quote = FALSE)
# str(data.cleaned)

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
data <- data.colsCleaned

# divide into training and test sets
# testing <- subset(DATA, is.na(DATA$EUR_o..Mstb.))
# data <- subset(DATA, !is.na(DATA$EUR_o..Mstb.))

# deal with discrepancies in $Mesh.Size
data$Mesh.Size <- as.character(data$Mesh.Size)
data$Mesh.Size[data$Mesh.Size %in% c("20/04","20 /40","20/20")] <- "20/40"
data$Mesh.Size[data$Mesh.Size %in% c("20/40 & 16/3")] <- "MIXED"

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

# s <- data[,-which(names(data) %in% c(ID,"Depth.Total.Driller..ft."))]
# t <- aggregate(x = s, by = list("Depth.Total.Driller..ft."=data$Depth.Total.Driller..ft.), FUN = "mean")
# write.table(t, "aggregated.csv",sep=",",row.names=FALSE, quote = FALSE)

# sort the data based on CUM values!
# data <- data[order(-data$EUR_o..Mstb.),] #-which(names(data) %in% ID)
# for(well in 1:nrow(data)) {
# #   well <- 2
#   ref <- data[well,]
#   if (any(is.na(ref))) {
#     others <- data[-well,]
#     corr_rowWise <- apply(others, 1, function(z) cor(as.numeric(ref),as.numeric(z),use="complete"))
#     t <- cbind(others,corr_rowWise)
#     t <- t[order(-t$corr_rowWise),] # -t$EUR_o..Mstb.,
#     highest <- t[1,]
#     data[well,][which(is.na(ref))] <- h <- highest[which(is.na(ref))]
#     print(paste(sum(!is.na(h)), "replaced for", ref$EUR_o..Mstb., sep = " "))
#   }
# }

# # test the correlations in the features between the highest producing wells and the lowest!
# correlated <- list() # numeric vector
# highest <- -1
# friend <- NULL
# 
# for(well in 1:nrow(sorted)) {
#   well <- 2
#   if (any(is.na(sorted[well,]))) {
#     
#     
#     for(other in 1:nrow(others)) {
#       #     other <- 1
#       COR <- cor(as.numeric(ref),as.numeric(others[other,]), use="complete")
#       if (COR > highest) {
#         highest <- COR
#         friend <- others[other,]
#       }
#     }
#   }
# }
# correlations
# sorted <- cbind(sorted,correlations)
# sorted$id <- seq_len(nrow(sorted))
# line <- gvisLineChart(xvar = "id", yvar = c("correlations"), data = sorted)
# plot(line)

marks <- as.numeric(quantile(data$EUR_o..Mstb., seq(0, 1, length=501)))
marks

# integer code the EURs
copy <- data
copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = 500, include.lowest = T, labels = F) 
table(copy$Class)

s <- copy[,-which(names(copy) %in% c("Class","Surface.Latitude","Surface.Longitude"))]
t <- aggregate(x = s, by = list("Class"=copy$Class), FUN = "mean", na.rm=TRUE)
write.table(t, "aggregated.csv",sep=",",row.names=FALSE, quote = FALSE)
imputeAggr <- impKNNa(x = t, method = "knn", k = 2, metric = "Euclidean")
t <- as.data.frame(imputeAggr$xImp)

# t$Class <- factor(t$Class)
# # smote <- SMOTE(Class ~ ., data = t) #, k = 5, learner = "rpartXse", se = 0.5, , perc.over = 1000, perc.under = 500
# t <- t[rep(1:nrow(t), times=2),]
# table(t$Class)

t <- t[,-which(names(t) %in% c("Class"))]
result <- challengeRegression(t)
data.cleaned <- result$data
# data <- data.cleaned
data.model <- result$model
rmses <- result$rmses
hist(x = rmses, plot = T)
min(result$rmses)
max(result$rmses)

temp <- data[,-which(names(data) %in% c(ID,METRIC))] # 

print("Impute the missing values from knnImpute!")
i <- impKNNa(x = temp, method = "knn", k = 3, metric = "Euclidean")
temp <- as.data.frame(i$xImp)
# trans <- preProcess(temp, method =  c("knnImpute"))  
# temp <- predict(object = trans, newdata = temp)
temp <- cbind(data[[METRIC]], temp)
colnames(temp)[1] <- METRIC
data <- cbind(data[[ID]], temp)
colnames(data)[1] <- ID
str(data)

# let's set aside some "unseen" stratified data from the model!
# set.seed(100)
# indx <- createDataPartition(data$EUR_o..Mstb.,p = trainSplit)
# index <- indx$Resample1
# testing=data[-index,]
# data=data[index,]
# write.table(data, "train.csv",sep=",",row.names=FALSE, quote = FALSE)
# write.table(testing, "test.csv",sep=",",row.names=FALSE, quote = FALSE)




# # the normal fit that we do!
# result <- challengeRegression(data)
# data.cleaned <- result$data
# # data <- data.cleaned
# data.model <- result$model

# test on the held out test set
testing <- data
t <- testing[,which(names(testing) %in% names(data.cleaned))]
predicted <- predict(data.model, newdata = t[,which(!names(t) %in% METRIC)])
actuals <- testing$EUR_o..Mstb.
RMSE <- RMSE(predicted,actuals,na.rm = T)
RMSE
COR <- cor(predicted,actuals,use="complete")
COR
RMSLE <- rmsle(actuals,predicted)
RMSLE
data.fit <- cbind(actuals,predicted)
data.fit <- as.data.frame(data.fit)
hist(x = data.fit$actuals, plot = T, breaks = 3)
ordered <- data.fit[order(data.fit$actuals),]
ordered$id <- seq_len(nrow(ordered))
ordered$error <- ((ordered$predicted - ordered$actuals)/ordered$actuals)*100
str(ordered)
# line <- gvisLineChart(data = ordered, xvar = "id", yvar = c("actuals","error"), options=list(series="[{targetAxisIndex: 0},
#                                  {targetAxisIndex:1}]",vAxes="[{title:'actuals'}, {title:'error'}]"))
line <- gvisLineChart(data = ordered, xvar = "id", yvar = c("actuals","predicted"))
plot(line)

# TODO change the cut-offs
LOW <- 0.85
# marks <- as.numeric(quantile(data$EUR_o..Mstb., c(0, LOW, 1)))
marks <- as.numeric(quantile(data$EUR_o..Mstb., seq(0, 1, length=501)))
marks

# lowCUMs <- data[data$EUR_o..Mstb.>=marks[[1]] & data$EUR_o..Mstb.<=marks[[2]], ]
# hist(x = lowCUMs$EUR_o..Mstb., plot = T)

# medCUMs <- data[data$EUR_o..Mstb.>marks[[2]] & data$EUR_o..Mstb.<=marks[[3]], ]
# hist(x = medCUMs$EUR_o..Mstb., plot = T)

# highCUMs <- data[data$EUR_o..Mstb.>marks[[3]] & data$EUR_o..Mstb.<=marks[[4]], ]
# hist(x = highCUMs$EUR_o..Mstb., plot = T)

# integer code the EURs
copy <- data
copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = marks, include.lowest = T, labels = F) 
table(copy$Class)

s <- copy[,-which(names(copy) %in% c("Class","Surface.Latitude","Surface.Longitude"))]
t <- aggregate(x = s, by = list("Class"=copy$Class), FUN = "mean", na.rm=TRUE)
write.table(t, "aggregated.csv",sep=",",row.names=FALSE, quote = FALSE)

result <- challengeRegression(t)
t.cleaned <- result$data
# data <- data.cleaned
t.model <- result$model

temp <- copy # ID,[,-which(names(copy) %in% c(METRIC))]
table(temp$Class)
temp$Class <- factor(temp$Class)

# fitOri <- challengeRegression(temp,F,T)
# pred <- predict(fitOri$model, type="class",newdata = temp)
# cm <- confusionMatrix(pred, temp$Class)
# cm
# sensitivity <- mean(cm$byClass[1:3,1])
# sensitivity
# accuracy <- mean(cm$byClass[1:3,8])
# accuracy

# optimized SMOTE to get best fit!
# fSmote <- function(x) {
#   seed <- floor(x[1])
#   set.seed(seed)
#   t <- SMOTE(Class ~ ., data = temp, k = 5) #, perc.over = 300, perc.under = 300, k = 5, learner = "rpartXse", se = 0.5
#   table(t$Class)
#   fit <- challengeRegression(t,F,T)
#   pred <- predict(fit, type="class",newdata = copy)
#   cm <- confusionMatrix(pred, copy$Class)
#   sensitivity <- mean(cm$byClass[1:3,1])
#   sensitivity
# }

# xmin <- optimize(fSmote, c(1, 5000), maximum = T) # , tol = 0.0001
# xmin

# set.seed(xmin$maximum)
set.seed(1231)
smote <- SMOTE(Class ~ ., data = temp, perc.over = 1000, perc.under = 500) #, k = 5, learner = "rpartXse", se = 0.5, 
table(smote$Class)
t <- smote[,-which(names(smote) %in% c("Class"))]
str(t)
write.table(t, "SMOTE.csv",sep=",",row.names=FALSE, quote = FALSE)
# the normal fit that we do!
resultS <- challengeRegression(t)
smote.cleaned <- resultS$data
smote.model <- resultS$model

# test on the held out test set
tt <- testing[,which(names(testing) %in% names(smote.cleaned))]
predicted <- predict(smote.model, newdata = tt[,which(!names(tt) %in% METRIC)])
actuals <- tt$EUR_o..Mstb.
RMSE <- RMSE(predicted,actuals,na.rm = T)
RMSE
COR <- cor(predicted,actuals,use="complete")
COR
RMSLE <- rmsle(actuals,predicted)
RMSLE
data.fit <- cbind(actuals,predicted)
data.fit <- as.data.frame(data.fit)
hist(x = data.fit$actuals, plot = T, breaks = 3)
ordered <- data.fit[order(data.fit$actuals),]
ordered$id <- seq_len(nrow(ordered))
ordered$error <- ordered$actuals - ordered$predicted
str(ordered)
line <- gvisLineChart(xvar = "id", yvar = c("actuals","predicted","error"), data = ordered)
plot(line)

# smp <- smote[,-which(names(smote) %in% c(METRIC))]
# fitSMOTE <- challengeRegression(smp,F,T)
# pred <- predict(fitSMOTE$model, type="class",newdata = smp)
# cm <- confusionMatrix(pred, smp$Class)
# cm
# sensitivity <- mean(cm$byClass[1:3,1])
# sensitivity
# accuracy <- mean(cm$byClass[1:3,8])
# accuracy

# optimized Down-sampling to get best fit!
# fDown <- function(x) {
#   seed <- floor(x[1])
#   set.seed(seed)
#   t <- downSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
#   table(t$Class)
#   fit <- challengeRegression(t,F,T)
#   pred <- predict(fit, type="class",newdata = copy)
#   cm <- confusionMatrix(pred, copy$Class)
#   sensitivity <- mean(cm$byClass[1:3,1])
#   sensitivity
# }

# fDown(1000)
# xmin <- optimize(fDown, c(1, 5000), maximum = T) # , tol = 0.0001
# xmin

# set.seed(xmin$maximum)
# set.seed(384)
# t <- downSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
# table(t$Class)
# fitDownSample <- challengeRegression(t,F,T)
# pred <- predict(fitDownSample, type="class",newdata = copy)
# cm <- confusionMatrix(pred, copy$Class)
# sensitivity <- mean(cm$byClass[1:3,1])
# sensitivity
# accuracy <- mean(cm$byClass[1:3,8])
# accuracy

# optimized Down-sampling to get best fit!
# fUp <- function(x) {
#   seed <- floor(x[1])
#   set.seed(seed)
#   t <- upSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
#   table(t$Class)
#   fit <- challengeRegression(t,F,T)
#   pred <- predict(fit, type="class",newdata = copy)
#   cm <- confusionMatrix(pred, copy$Class)
#   sensitivity <- mean(cm$byClass[1:3,1])
#   sensitivity
# }

# fUp(3467)
# xmin <- optimize(fUp, c(1, 5000), maximum = T) # , tol = 0.0001
# xmin

# set.seed(xmin$maximum)
# set.seed(1910)
# t <- upSample(temp[,-which(names(temp) %in% c("Class"))],temp$Class, list = F, yname = "Class")
# table(t$Class)
# fitUpSample <- challengeRegression(t,F,T)
# pred <- predict(fitUpSample, type="class",newdata = copy)
# cm <- confusionMatrix(pred, copy$Class)
# cm
# sensitivity <- mean(cm$byClass[1:3,1])
# sensitivity
# accuracy <- mean(cm$byClass[1:3,8])
# accuracy

# create different brackets of CUM values!
# LOWs - either fit on t$Class 1 i.e 68 samples or the whole data within the brackets
# lows <- quantile(data$EUR_o..Mstb., c(0, 0.33))
# lowCUMs <- data[data$EUR_o..Mstb.>lows[[1]] & data$EUR_o..Mstb.<lows[[2]], ]
# str(lowCUMs)
# resultL <- challengeRegression(lowCUMs)
# data.cleaned.low <- resultL$data
# data.model.low <- resultL$model
# data.predicted.low <- resultL$predicted
# data.actuals.low <- resultL$actual
# data.fit.low <- cbind(data.actuals.low,data.predicted.low)
# data.fit.low <- as.data.frame(data.fit.low)
# hist(x = data.fit.low$data.actuals.low, plot = T, breaks = 3)
# ordered.low <- data.fit.low[order(data.fit.low$data.actuals.low),]
# ordered.low$id <- seq_len(nrow(ordered.low))
# ordered.low$error <- ordered.low$data.actuals.low - ordered.low$data.predicted.low
# str(ordered.low)
# line.low <- gvisLineChart(xvar = "id", yvar = c("data.actuals.low","error"), data = ordered.low)
# plot(line.low)

# MEDIUMs
# meds <- quantile(data$EUR_o..Mstb., c(0.33, 0.66))
# medCUMs <- data[data$EUR_o..Mstb.>meds[[1]] & data$EUR_o..Mstb.<meds[[2]], ]
# str(medCUMs)
# resultM <- challengeRegression(medCUMs)
# data.cleaned.med <- resultM$data
# data.model.med <- resultM$model
# data.predicted.med <- resultM$predicted
# data.actuals.med <- resultM$actual
# data.fit.med <- cbind(data.actuals.med,data.predicted.med)
# data.fit.med <- as.data.frame(data.fit.med)
# hist(x = data.fit.med$data.actuals.med, plot = T, breaks = 3)
# ordered.med <- data.fit.med[order(data.fit.med$data.actuals.med),]
# ordered.med$id <- seq_len(nrow(ordered.med))
# ordered.med$error <- ordered.med$data.actuals.med - ordered.med$data.predicted.med
# str(ordered.med)
# line.med <- gvisLineChart(xvar = "id", yvar = c("data.actuals.med","data.predicted.med","error"), data = ordered.med)
# plot(line.med)

# HIGHs
# highs <- quantile(data$EUR_o..Mstb., c(0.66, 1.0))
# highCUMs <- data[data$EUR_o..Mstb.>highs[[1]] & data$EUR_o..Mstb.<highs[[2]], ]
# str(highCUMs)
# resultH <- challengeRegression(highCUMs)
# data.cleaned.high <- resultH$data
# data.model.high <- resultH$model
# data.predicted.high <- resultH$predicted
# data.actuals.high <- resultH$actual
# data.fit.high <- cbind(data.actuals.high,data.predicted.high)
# data.fit.high <- as.data.frame(data.fit.high)
# hist(x = data.fit.high$data.actuals.high, plot = T, breaks = 3)
# ordered.high <- data.fit.high[order(data.fit.high$data.actuals.high),]
# ordered.high$id <- seq_len(nrow(ordered.high))
# ordered.high$error <- ordered.high$data.actuals.high - ordered.high$data.predicted.high
# str(ordered.high)
# line.high <- gvisLineChart(xvar = "id", yvar = c("data.actuals.high","error"), data = ordered.high)
# plot(line.high)

# predict high wells using the model on high wells
# predict.high.using.high <- predict(data.model.high, newdata = data.cleaned.high)
# highs= data.cleaned.high$EUR_o..Mstb.
# RMSE<-RMSE(pred = predict.high.using.high,obs = highs)
# RMSE
# RMSLE <- rmsle(highs, predict.high.using.high)
# RMSLE
# COR <- cor(highs, predict.high.using.high,use="complete")
# COR

# predict high wells using the model on medium wells
# predict.high.using.med <- predict(data.model.med, newdata = data.cleaned.high)
# highs= data.cleaned.high$EUR_o..Mstb.
# RMSE<-RMSE(pred = predict.high.using.med,obs = highs)
# RMSE
# RMSLE <- rmsle(highs, predict.high.using.med)
# RMSLE
# COR <- cor(highs, predict.high.using.med,use="complete")
# COR

# # save the model to disk
# rJava::.jcache(best.fit$classifier)
# save(medCUMsModel, file = "medium_completions_gaussian.rda")

# optimization of all wells!
wells.to.optimize <- highCUMs
optimumsAll <- challengeOptimizations(wells.to.optimize,data.model)
str(optimumsAll)
# da <- rbind(data.cleaned,optimumsAll)
# write.table(da, "all.csv",sep=",",row.names=FALSE, quote = FALSE)
# resultAll <- challengeRegression(da)
# summary(resultAll$model)

# create a predictors sample having maximum dissimilarity from ALL wells!
set.seed(1134)
# A random sample of 1 data points
startSet <- sample(1:dim(data)[1], 1)
samplePool <- data[-startSet, ]
start <- data[startSet, ]
trainIndex <- maxDissim(start, samplePool, n = 50)
wells.to.optimize=data[trainIndex,]
# nrow(startWells)
# t <- t[order(t$EUR_o..Mstb.),]
write.table(wells.to.optimize, "the_ones_to_optimize.csv", sep=",", row.names=FALSE, quote = FALSE)

optimumsM <- challengeOptimizations(data.cleaned.med,data.model.med)
str(optimumsM)

# # optimization of very high wells!
# # write.table(data.cleaned.med, "the_meds.csv", sep=",", row.names=FALSE, quote = FALSE)
# optimumsM <- challengeOptimizations(wells.to.optimize,data.model.med)
# str(optimumsM)
# hist(x = optimumsM$EUR_o..Mstb., plot = T, breaks = 3)
# d <- rbind.fill(data,optimumsM)
# write.table(d, "mediums.csv",sep=",",row.names=FALSE, quote = FALSE)
# 
# resultMerged <- challengeRegression(d)
# summary(resultMerged$model)
# 
# # optimization of high wells!
# optimumsH <- challengeOptimizations(data.cleaned.high,data.model.high)
# str(optimumsH)
# max(optimumsH$EUR_o..Mstb.)
d1 <- rbind.fill(data,optimumsH)
hist(x = d1$EUR_o..Mstb., plot = T, breaks = 3)
write.table(d1, "highs.csv",sep=",",row.names=FALSE, quote = FALSE)

resultMerged1 <- challengeRegression(d1)
summary(resultMerged1$model)
# 
# # optimization of med wells!
# optimumsM <- challengeOptimizations(medCUMs,medCUMsModel)
# str(optimumsM)
# d2 <- rbind(d1,optimumsM)
# write.table(d1, "merged2.csv",sep=",",row.names=FALSE, quote = FALSE)
# 
# resultMerged2 <- challengeRegression(d2)
# summary(resultMerged1$model)

