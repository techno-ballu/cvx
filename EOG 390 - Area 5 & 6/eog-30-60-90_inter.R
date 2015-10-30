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

library(kernlab)
library(earth)

library(FSelector)
library(robCompositions)

library(missForest)
library(Hmisc)

# set the work directory
setwd("/home/bolaka/EOG-data/AllWells/30-60-90")

if (!exists('buildRegression', mode = "function"))
  source("../buildRegression.R")
# result <- buildRegression(data)

if (!exists('buildOptimizationsWithYConstraints', mode = "function"))
  source("../buildOptimizations.R")

# load well wise Completions data
DATA <- read.csv("../eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))

# add some special columns
# 1. Stages
DATA$Stages <- DATA$TREATED_LENGTH/DATA$TREATED_LATERAL_PER_STAGE
# 2. Clusters
DATA$Clusters <- DATA$TREATED_LATERAL_PER_STAGE/DATA$AVG_CLUSTER_SPACING
# 3. Total Propant Used
DATA[["Total.Propant.Used"]] <- DATA$TREATED_LENGTH*DATA$PROPANT_PER_FOOT
# 4. Amount of 100 mesh sand used
DATA[["Amount.100.mesh"]] <- DATA$Total.Propant.Used*DATA$Fraction.100.MESH
# 5. Amount of other sands used
DATA[["Amount.Others"]] <- DATA$Total.Propant.Used - DATA$Amount.100.mesh
str(DATA)

# TODO divide into training and test sets
testing <- subset(DATA, is.na(DATA$CUM_90PD_OIL_PROD))
DATA <- subset(DATA, !is.na(DATA$CUM_90PD_OIL_PROD))
str(DATA)

# just lets look at "AREA 5" & "AREA 6" separately!
DATA5 <- subset(DATA, TEAM == "AREA 5")
DATA6 <- subset(DATA, TEAM == "AREA 6")

DATA <- DATA6

# 6. Well Proximity
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

REF_CUM_COL<- "CUM_90PD_OIL_PROD"

j<-1
wp <- c()
for(i in rownames(DATA)){   
  print(paste("well id",i,sep=" "))    
  distance <- c()
  for(k in rownames(DATA)){  
    dist <- earth.dist(DATA[i,c("SURFACE_LOC_LONG")],DATA[i,c("SURFACE_LOC_LAT")],DATA[k,c("SURFACE_LOC_LONG")],DATA[k,c("SURFACE_LOC_LAT")])
    distance[k] <- round(dist, digits = 3)   
  }  
  temp <- cbind(DATA,distance)
  
  if(nrow(subset(temp,temp$distance > 0 & temp$distance < 0.5 )) > 0 ){   
    wp[j] <- round(mean((subset(temp,temp$distance > 0.0 &temp$distance < 0.5))[,REF_CUM_COL]),digits=3)
    print(paste("0.0-0.5",wp[j],sep=" "))
  }else if(nrow(subset(temp,temp$distance >= 0.5 & temp$distance < 1.0)) > 0 ){   
    wp[j] <- round(mean((subset(temp,temp$distance >= 0.5 & temp$distance < 1.0))[,REF_CUM_COL]),digits=3)    
    print(paste("0.5-1.0",wp[j],sep=" "))
  }else if(nrow(subset(temp,temp$distance >= 1.0 & temp$distance < 2.0)) > 0 ){   
    tmp <- subset(temp,temp$distance >= 1.0 & temp$distance < 2.0)    
    val <- tmp[,c(REF_CUM_COL)]*tmp[,c("distance")]/sum(tmp$distance)
    wp[j] <- round(sum(val),digits=3)
    
    print(paste("1.0-2.0",wp[j],sep=" "))
  }else if(nrow(subset(temp,temp$distance >= 2.0 & temp$distance < 5.0)) > 0 ){      
    tmp <- subset(temp,temp$distance >= 2.0 & temp$distance < 5.0)    
    val <- tmp[,c(REF_CUM_COL)]*tmp[,c("distance")]/sum(tmp$distance)
    print(paste("2.0-5.0",sum(val),sep=" "))
    wp[j] <- round(sum(val),digits=3)
    
  }else{    
    tmp <- subset(temp,temp$distance >= 5.0) 
    tmp <- tmp[order(tmp$distance,decreasing=F),]
    wp[j] <- round(tmp[1,REF_CUM_COL],digits=3)   
    print(paste("5.0>",wp[j],sep=" "))
  }
  j<-j+1  
}
DATA <- cbind(DATA,wp)

# # 377!
# DATA30 <- subset(DATA, !is.na(DATA$CUM_30PD_OIL_PROD))
# str(DATA30)
# 
# # let's study the correlations between the 30-60-90-180 days productions
# cor(DATA30$CUM_60PD_OIL_PROD, DATA30$CUM_30PD_OIL_PROD,use="complete")
# cor(DATA30$CUM_30PD_OIL_PROD, DATA30$CUM_90PD_OIL_PROD,use="complete")
# cor(DATA30$CUM_30PD_OIL_PROD, DATA30$CUM_180PD_OIL_PROD,use="complete")
# 
# fit <- approxfun(DATA30$CUM_30PD_OIL_PROD, DATA30$CUM_60PD_OIL_PROD)
# predicted <- fit(DATA30$CUM_30PD_OIL_PROD)
# actual= DATA30$CUM_60PD_OIL_PROD
# error <- predicted - actual
# temp <- cbind(predicted,actual)
# write.table(cbind(temp,error), "interpolate60from30.csv", sep=",", row.names=FALSE, quote = FALSE)
# RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
# RMSE
# actual[is.na(actual)] <- predicted[is.na(actual)]
# print(paste(cor(DATA30$CUM_30PD_OIL_PROD, predicted),cor(DATA30$CUM_60PD_OIL_PROD, DATA30$CUM_30PD_OIL_PROD,use="complete"), sep = " vs "))
# DATA30$CUM_60PD_OIL_PROD <- actual # successfully interpolated 60 day productions
# 
# # 377 - on interpolateds!
# DATA60 <- subset(DATA30, !is.na(DATA30$CUM_60PD_OIL_PROD))
# str(DATA60)
# 
# # let's study the correlations between the 60-90-180 days productions
# cor(DATA60$CUM_60PD_OIL_PROD, DATA60$CUM_90PD_OIL_PROD,use="complete")
# cor(DATA60$CUM_60PD_OIL_PROD, DATA60$CUM_180PD_OIL_PROD,use="complete")
# 
# fit <- approxfun(DATA60$CUM_60PD_OIL_PROD, DATA60$CUM_90PD_OIL_PROD)
# predicted <- fit(DATA60$CUM_60PD_OIL_PROD)
# actual= DATA60$CUM_90PD_OIL_PROD
# error <- predicted - actual
# temp <- cbind(predicted,actual)
# write.table(cbind(temp,error), "interpolate90from60.csv", sep=",", row.names=FALSE, quote = FALSE)
# RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
# RMSE
# actual[is.na(actual)] <- predicted[is.na(actual)]
# print(paste(cor(DATA60$CUM_60PD_OIL_PROD, predicted),cor(DATA60$CUM_60PD_OIL_PROD, actual,use="complete"), sep = " vs "))
# DATA60$CUM_90PD_OIL_PROD <- actual # successfully interpolated 90 day productions

# 345 - on interpolateds!
DATA90 <- subset(DATA, !is.na(DATA$CUM_90PD_OIL_PROD))
str(DATA90)

# let's study the correlations between the 60-90-180 days productions
cor(DATA90$CUM_90PD_OIL_PROD, DATA90$CUM_180PD_OIL_PROD,use="complete")

fit <- approxfun(DATA90$CUM_90PD_OIL_PROD, DATA90$CUM_180PD_OIL_PROD,rule = 2)
predicted <- fit(DATA90$CUM_90PD_OIL_PROD)
actual= DATA90$CUM_180PD_OIL_PROD
RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
RMSE
actual[is.na(actual)] <- predicted[is.na(actual)]
print(paste(cor(DATA90$CUM_90PD_OIL_PROD, predicted,use="complete"),cor(DATA90$CUM_90PD_OIL_PROD, actual,use="complete"), sep = " vs "))
DATA90$CUM_180PD_OIL_PROD <- actual # successfully interpolated 90 day productions

# fit 180 vs 30 + 60 + 90
# controlObject <- trainControl(method = "repeatedcv", repeats = 5, number = 10)
# fit <- train(CUM_180PD_OIL_PROD ~ CUM_30PD_OIL_PROD + CUM_60PD_OIL_PROD + CUM_90PD_OIL_PROD, data = DATA90, method = "gcvEarth", trControl = controlObject)
# predicted <- predict(fit, newdata = DATA90)
# actual= DATA90$CUM_180PD_OIL_PROD
# RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
# RMSE

# DATA$CUM_30PD_OIL_PROD <- DATA90$CUM_30PD_OIL_PROD
# DATA$CUM_60PD_OIL_PROD <- DATA90$CUM_60PD_OIL_PROD
# DATA$CUM_90PD_OIL_PROD <- DATA90$CUM_90PD_OIL_PROD
DATA$CUM_180PD_OIL_PROD <- DATA90$CUM_180PD_OIL_PROD

# some tweaking constants
VIF_THRESHOLD <- 10
trainSplit <- 0.70
corrCutOff <- 0.9
# imp vars!
ID <- "Wellname"
USELESS <- c("CUM_90PD_OIL_PROD","CUM_30PD_OIL_PROD","CUM_60PD_OIL_PROD","TEAM") #"Avg.ShutIn.Days","ShutinsPeriods","ShutinDays",
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

# hist(x = DATA$CumProd, plot = T)

data <- DATA#[,-which(names(DATA) %in% "TEAM")]
str(data)

# category labeling
types <- lapply(data, class)
distincts <- lapply(data, function(c) unique(c))
categoryMappings <- list()

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
    #     categoryMappings <- c(categoryMappings, i=means)
    categoryMappings[[i]] <- means
  }
}
categoryMappings

#"CUM_90PD_OIL_PROD","CUM_30PD_OIL_PROD","CUM_60PD_OIL_PROD"
# # fit and predict from CUM_30
# data30 <- data[, -which(names(data) %in% c("CUM_90PD_OIL_PROD","CUM_60PD_OIL_PROD"))]
# str(data30)
# result30 <- buildRegression(data30)
# data.cleaned.30 <- result30$data
# data.model.30 <- result30$model
# data.predicted.30 <- predict(data.model.30, newdata = data.cleaned.30)
# data.actuals.30 <- data.cleaned.30$CumProd
# RMSE.30<-RMSE(pred = data.predicted.30,obs = data.actuals.30)
# RMSE.30
# RMSLE.30 <- rmsle(data.actuals.30,data.predicted.30)
# RMSLE.30
# COR.30 <- cor(data.actuals.30, data.predicted.30)
# COR.30
# 
# # fit and predict from CUM_60
# data60 <- data[, -which(names(data) %in% c("CUM_90PD_OIL_PROD","CUM_30PD_OIL_PROD"))]
# str(data60)
# result60 <- buildRegression(data60)
# data.cleaned.60 <- result60$data
# data.model.60 <- result60$model
# data.predicted.60 <- predict(data.model.60, newdata = data.cleaned.60)
# data.actuals.60 <- data.cleaned.60$CumProd
# RMSE.60<-RMSE(pred = data.predicted.60,obs = data.actuals.60)
# RMSE.60
# RMSLE.60 <- rmsle(data.actuals.60,data.predicted.60)
# RMSLE.60
# COR.60 <- cor(data.actuals.60, data.predicted.60)
# COR.60

# fit and predict from CUM_90
# data90 <- data[, -which(names(data) %in% c("CUM_60PD_OIL_PROD","CUM_30PD_OIL_PROD"))]
# data90 <- data
# str(data90)
# result90 <- buildRegression(data90)
# data.cleaned.90 <- result90$data
# data.model.90 <- result90$model
# data.predicted.90 <- predict(data.model.90, newdata = data.cleaned.90)
# data.actuals.90 <- data.cleaned.90$CumProd
# RMSE.90<-RMSE(pred = data.predicted.90,obs = data.actuals.90)
# RMSE.90
# RMSLE.90 <- rmsle(data.actuals.90,data.predicted.90)
# RMSLE.90
# COR.90 <- cor(data.actuals.90, data.predicted.90)
# COR.90

# results <- as.data.frame(cbind(data.actuals.30, data.predicted.30))
# str(results)
# results <- cbind(results, data.predicted.60)
# str(results)
# results <- cbind(results, data.predicted.90)
# str(results)
# write.table(results, "results.csv", sep=",", row.names=FALSE, quote = FALSE)

# # now use the best bit model to fill next set of wells - i.e pick up wells from test set having CUM_90
# str(testing)
# 
# # 67
# testing90 <- subset(testing, !is.na(testing$CUM_90PD_OIL_PROD))
# # replace categoris with pre-defined labels
# testing90$TEAM[testing90$TEAM == "AREA 5"] <- categoryMappings$TEAM[["AREA 5"]]
# testing90$TEAM[testing90$TEAM == "AREA 6"] <- categoryMappings$TEAM[["AREA 6"]]
# testing90$TEAM <- as.numeric(testing90$TEAM)
# str(testing90)
# temp <- testing90[which(names(testing90) %in% names(data.cleaned.90))]
# testing90.predicted <- predict(data.model.90$finalModel, newdata = temp[which(!names(temp) %in% c(ID,METRIC))])
# testing90$CumProd <- testing90.predicted

# data.imputed.90 <- rbind(data,testing90)
# data.imputed.90 <- data
# data.imputed.90$CumProd <- data.predicted.90
  
# # remove some unwanted columns
# data.imputed.90 <- data.imputed.90[, -which(names(data.imputed.90) %in% USELESS)]
# result.imputed.90 <- buildRegression(data.imputed.90)
# data.imputed.cleaned.90 <- result.imputed.90$data
# data.imputed.model.90 <- result.imputed.90$model
# str(data.imputed.cleaned.90)

data <- data[, -which(names(data) %in% USELESS)]

# sorted <- data[order(data$CumProd),-which(names(data) %in% ID)]
# # test the correlations in the features between the highest producing wells and the lowest!
# correlations <- numeric() # numeric vector
# first <- as.numeric(sorted[1,])
# 
# for(well in 1:nrow(sorted)) {
#   corr <- cor(first,as.numeric(sorted[well,]),use="complete")
#   correlations[length(correlations)+1] <- corr
# }
# correlations
# sorted <- cbind(sorted,correlations)
# sorted$id <- seq_len(nrow(sorted))
# line <- gvisLineChart(xvar = "id", yvar = c("correlations"), data = sorted)
# plot(line)

temp <- data #[,-which(names(data) %in% c(ID,METRIC))] # 
temp.imp <- missForest(xmis = temp, variablewise = T, verbose = T)
temp <- temp.imp$ximp
describe(data)
# print("Impute the missing values from knnImpute!")
# i <- impKNNa(x = temp, method = "knn", k = 3, metric = "Euclidean")
# temp <- as.data.frame(i$xImp)
# trans <- preProcess(temp, method =  c("knnImpute"))  
# temp <- predict(object = trans, newdata = temp)
# temp <- cbind(data[[METRIC]], temp)
# colnames(temp)[1] <- METRIC
# data <- cbind(data[[ID]], temp)
# colnames(data)[1] <- ID
data <- temp
describe(data)
# str(data)

# let's set aside some "unseen" stratified data from the model!
set.seed(786)
index <- createDataPartition(data$CumProd,p = trainSplit)[[1]]
testing=data[-index,]
data=data[index,]
write.table(data, "train.csv",sep=",",row.names=FALSE, quote = FALSE)
write.table(testing, "test.csv",sep=",",row.names=FALSE, quote = FALSE)

result <- buildRegression(data)
data.cleaned <- result$data
data.model <- result$model
rmses <- result$rmses
hist(x = rmses, plot = T)
min(rmses)
max(rmses)
rmsles <- result$rmsles
hist(x = rmsles, plot = T)

# test on the held out test set
t <- testing[,which(names(testing) %in% names(data.cleaned))]
predicted <- predict(data.model, newdata = t[,which(!names(t) %in% METRIC)])
actuals <- testing$CumProd
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
line <- gvisLineChart(data = ordered, xvar = "id", yvar = c("actuals","predicted","error"))
# line <- gvisLineChart(data = ordered, xvar = "id", yvar = c("actuals","error"), options=list(series="[{targetAxisIndex: 0},
#                                  {targetAxisIndex:1}]",vAxes="[{title:'actuals'}, {title:'error'}]"))
plot(line)

# TODO change the cut-offs
# LOW <- 0.40
# HIGH <- 0.90
# marks <- as.numeric(quantile(data$CumProd, c(0, HIGH, 1)))
# marks
# 
# copy <- data
# copy$Class <- cut(x = copy$CumProd, breaks = marks, include.lowest = T, labels = F) 
# table(copy$Class)
# 
# temp <- copy # ID,[,-which(names(copy) %in% c(METRIC))]
# table(temp$Class)
# temp$Class <- factor(temp$Class)
# 
# set.seed(1231)
# smote <- SMOTE(Class ~ ., data = temp, perc.over = 2000, perc.under = 500) #, k = 5, learner = "rpartXse", se = 0.5, 
# table(smote$Class)
# t <- smote[,-which(names(smote) %in% c("Class"))]
# str(t)
# write.table(t, "SMOTE.csv",sep=",",row.names=FALSE, quote = FALSE)
# # the normal fit that we do!
# resultS <- buildRegression(t)
# smote.cleaned <- resultS$data
# smote.model <- resultS$model
# rmses <- resultS$rmses
# hist(x = rmses, plot = T)
# 
# # test on the held out test set
# tt <- testing[,which(names(testing) %in% names(smote.cleaned))]
# predicted <- predict(smote.model, newdata = tt[,which(!names(tt) %in% METRIC)])
# actuals <- tt$CumProd
# RMSE <- RMSE(predicted,actuals,na.rm = T)
# RMSE
# COR <- cor(predicted,actuals,use="complete")
# COR
# RMSLE <- rmsle(actuals,predicted)
# RMSLE
# data.fit <- cbind(actuals,predicted)
# data.fit <- as.data.frame(data.fit)
# hist(x = data.fit$actuals, plot = T, breaks = 3)
# ordered <- data.fit[order(data.fit$actuals),]
# ordered$id <- seq_len(nrow(ordered))
# ordered$error <- ((ordered$predicted - ordered$actuals)/ordered$actuals)*100
# str(ordered)
# line <- gvisLineChart(data = ordered, xvar = "id", yvar = c("actuals","error"), options=list(series="[{targetAxisIndex: 0},
#                                  {targetAxisIndex:1}]",vAxes="[{title:'actuals'}, {title:'error'}]"))
# plot(line)

# highCUMs <- data.cleaned[data.cleaned$CumProd>marks[[3]] & data.cleaned$CumProd<=marks[[4]], ]
# hist(x = highCUMs$CumProd, plot = T)
# str(highCUMs)
# 
# wells.to.optimize <- data[which(names(data) %in% names(smote.cleaned))]
# source("../buildOptimizations.R")

source("../buildOptimizations.R")
wells.to.optimize <- data.cleaned
optimums <- buildOptimizationsWithCost(wells.to.optimize,data.model)
# 
# # discard the optimums greater than the max of the historicals
# optimums <- optimums[optimums$CumProd <= max(data.cleaned$CumProd),]
# str(optimums)
# 
# # merge the optimizations with the wells
# d <- rbind(data.cleaned,optimums[,which(!names(optimums) %in% c("Actual"))])
# write.table(d, "merged.csv",sep=",",row.names=FALSE, quote = FALSE)
# str(d)
# 
# resultMerged <- buildRegression(d)
# data.merged <- resultMerged$data
# model.merged <- resultMerged$model
# data.merged.predicted <- predict(model.merged, newdata = data.merged)
# data.actuals <- data.merged$CumProd
# RMSE<-RMSE(pred = data.merged.predicted,obs = data.actuals)
# RMSE
# RMSLE <- rmsle(data.actuals,data.merged.predicted)
# RMSLE
# COR <- cor(data.actuals, data.merged.predicted)
# COR

# create different brackets of CUM values!
# LOWs
# lows <- quantile(data$CumProd, c(0, 0.2))
# lowCUMs <- data[data$CumProd>lows[[1]] & data$CumProd<lows[[2]], ]
# str(lowCUMs)
# hist(x = lowCUMs$CumProd, plot = T)
# lowsResult <- buildRegression(lowCUMs)
# lowCUMsModel <- lowsResult$model
# lowCUMs <- lowsResult$data
# summary(lowCUMsModel)

# MEDIUMs
# meds <- quantile(data$CumProd, c(0.3, 0.5))
# medCUMs <- data[data$CumProd>meds[[1]] & data$CumProd<meds[[2]], ]
# str(medCUMs)
# hist(x = medCUMs$CumProd, plot = T)
# medsResult <- buildRegression(medCUMs)
# medCUMsModel <- medsResult$model
# medCUMs <- medsResult$data
# summary(medCUMsModel)
# medCUMs <- merge(medCUMs, original[,c(1,18:20)], by=ID)

# HIGHs
# highs <- quantile(data$CumProd, c(0.6, 0.8))
# highCUMs <- data[data$CumProd>highs[[1]] & data$CumProd<highs[[2]], ]
# str(highCUMs)
# hist(x = highCUMs$CumProd, plot = T)
# highsResult <- buildRegression(highCUMs)
# highCUMsModel <- highsResult$model
# highCUMs <- highsResult$data
# summary(highCUMsModel)

# VERY HIGHs
# veryhighs <- quantile(data$CumProd, c(0.9, 1.0))
# veryhighCUMs <- data[data$CumProd>veryhighs[[1]] & data$CumProd<veryhighs[[2]], ]
# str(veryhighCUMs)
# hist(x = veryhighCUMs$CumProd, plot = T)
# veryhighsResult <- buildRegression(veryhighCUMs)
# veryhighCUMsModel <- veryhighsResult$model
# veryhighCUMs <- veryhighsResult$data
# summary(veryhighCUMsModel)

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
save(model.merged$finalModel, file = "eog_final_model.rda")

# # optimization of wells!
# write.table(medCUMs, "the_meds.csv", sep=",", row.names=FALSE, quote = FALSE)
# data <- medCUMs
# model <- medCUMsModel
# M <- ncol(data)
# N <- nrow(data)
# names <- names(data)
# actionables <- names(data)[!(names(data) %in% nonactionables)]
# actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
# optimums <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
# actionables <- actionables[!(actionables %in% c(ID))]
# data$Wellname <- as.character(data$Wellname)
# 
# for (well in 1:N) {
# #   well <- 7
#   stub <- data[well, ]
#   print(stub$Wellname)
#   
#   # this is to get min-max of Steen Scruggs & Kerner Carson wells
#   # name=unlist(strsplit(gsub("-", " ", stub$Wellname),split=" ",fixed=TRUE))[1]
#   # wellsData <- data[grepl(name,data$Wellname),]
#   
#   # create optimization wrapper for CUM
#   opt_gm <- function(x, known.x) { #
#     val <- stub[[METRIC]]
#     z <- stub[,-which(names(stub) %in% c(ID,METRIC))]
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
#     y <- predict(model,newdata = z)
#     
#     #If these conditions are violated, the function returns a large positive number
#     # which the search procedure will avoid
# #     if(y > val*1.1) return(10^38)
#     -y
#   }
#   
#   # start with given values and replace by mean for actionables
#   start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
#   
#   # lower
#   opt_min <- start
#   for (i in names(opt_min)) {
#     opt_min[[i]] <- min(data[[i]],na.rm = T)
#   }
#   opt_min
#   
#   # upper
#   opt_max <- start
#   for (i in names(opt_max)) {
#     opt_max[[i]] <- max(data[[i]],na.rm = T)
#   }
#   opt_max
#   
#   opt_start <- start
#   for (i in names(opt_start)) {
#     if (i %in% actionables) {
#       opt_start[[i]] <- mean(data[[i]],na.rm = T)
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
#   opt_gm(opt_start,opt_nons)
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
