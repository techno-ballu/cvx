# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(gdata)
# require(randomForest)
# require(RWeka)
# require(caret)
require(fmsb)
library(foreach)
library(Metrics)
library(e1071)
# library(corrplot)
# library(RANN)
# library(ipred)
library(plyr)
library(rJava)
library(rJava)
library(dfoptim)
# library(proxy)
library(googleVis)
library(DMwR)
library(caret)

# library(kernlab)
# library(earth)

library(FSelector)
library(robCompositions)

library(missForest)
library(Hmisc)
library(MASS)

# set the work directory
setwd("D:\\ubunto\\Completions\\Deliverables")

if (!exists('buildRegression', mode = "function"))
  source("buildRegression.R")

if (!exists('buildOptimizationsWithYConstraints', mode = "function"))
  source("buildOptimizations.R")

# load well wise Completions data
DATA <- read.csv("eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
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
# testing <- subset(DATA, is.na(DATA$CUM_90PD_OIL_PROD))
# DATA <- subset(DATA, !is.na(DATA$CUM_90PD_OIL_PROD))
# str(DATA)

# 345 - on interpolateds!
DATA <- DATA90 <- subset(DATA, !is.na(DATA$CUM_90PD_OIL_PROD))
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

DATA$CUM_180PD_OIL_PROD <- DATA90$CUM_180PD_OIL_PROD

# this can be further improved by 90 <- 180, 60 <- 90, 30 <- 60!
# TODO lets create some interpolation functions for CUM_30, CUM_60 & CUM_90 from CUM_180
cor(DATA$CUM_180PD_OIL_PROD, DATA$CUM_90PD_OIL_PROD,use="complete")
interpolate90 <- approxfun(DATA$CUM_180PD_OIL_PROD,DATA$CUM_90PD_OIL_PROD,rule = 2)

cor(DATA$CUM_180PD_OIL_PROD, DATA$CUM_60PD_OIL_PROD,use="complete")
interpolate60 <- approxfun(DATA$CUM_180PD_OIL_PROD,DATA$CUM_60PD_OIL_PROD,rule = 2)

cor(DATA$CUM_180PD_OIL_PROD, DATA$CUM_30PD_OIL_PROD,use="complete")
interpolate30 <- approxfun(DATA$CUM_180PD_OIL_PROD,DATA$CUM_30PD_OIL_PROD,rule = 2)

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
colnames(DATA)[27] <- "Geology.Proxy"

# some tweaking constants
VIF_THRESHOLD <- 10
trainSplit <- 0.70
corrCutOff <- 0.9
# imp vars!
ID <- "Wellname"
USELESS <- c("CUM_90PD_OIL_PROD","CUM_30PD_OIL_PROD","CUM_60PD_OIL_PROD") #,"Total.Propant.Used"
METRIC <- "CumProd"
nonactionables <- c("Geology.Proxy","INITIAL_FRAC_GRADIENT","FINAL_FRAC_GRADIENT","SURFACE_LOC_LAT","SURFACE_LOC_LONG","AVG_AZIMUTH","TEAM","TOTAL_VERTICAL_DEPTH")
important <- c("Fraction.100.MESH")

names(DATA)[names(DATA) == "CUM_180PD_OIL_PROD"] <- METRIC

# replace "undefined", empty strings and #VALUE! as NAs for all columns
for(i in names(DATA)) {
  if (length(which(isUnknown(x=DATA[[i]], unknown = c("undefined","","#VALUE!")))) > 0) {
    DATA[[i]] <- unknownToNA(x=DATA[[i]], unknown=c("undefined","","#VALUE!"))
    print(paste(i,"replaced UNDEFINEDs=", any(isUnknown(x=DATA[[i]], unknown = c("undefined","","#VALUE!"))), sep = " "))
  }
}

data <- DATA

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

temp <- data #[,-which(names(data) %in% c(ID,METRIC))] # 
temp.imp <- missForest(xmis = temp, variablewise = T, verbose = T)
temp <- temp.imp$ximp
# describe(data)
# print("Impute the missing values from knnImpute!")
# i <- impKNNa(x = temp, method = "knn", k = 3, metric = "Euclidean")
# temp <- as.data.frame(i$xImp)
# temp <- cbind(data[[METRIC]], temp)
# colnames(temp)[1] <- METRIC
# data <- cbind(data[[ID]], temp)
# colnames(data)[1] <- ID
data <- temp
# describe(data)

# let's set aside some "unseen" stratified data from the model!
set.seed(786)
index <- createDataPartition(data$CumProd,p = trainSplit)[[1]]
wholetraining <- data
testing=data[-index,]
data=data[index,]
write.table(data, "train.csv",sep=",",row.names=FALSE, quote = FALSE)
write.table(testing, "test.csv",sep=",",row.names=FALSE, quote = FALSE)

trainingNtesting <- function(training, testing) {
  training <- training[, -which(names(training) %in% USELESS)]
  str(training)
  result <- buildRegression(training)
  data.cleaned <- result$data
  data.model <- result$model
  rmses <- result$rmses
  hist(x = rmses, plot = T)
  min(rmses)
  max(rmses)
  rmsles <- result$rmsles
  hist(x = rmsles, plot = T)
  print(paste("Min rmse", min(rmses), "Min rmsle", min(rmsles,na.rm = T), sep=" | "))
  print(paste("Max rmse", max(rmses), "Max rmsle", max(rmsles,na.rm = T), sep=" | "))
  
  # test on the held out test set
  t <- testing[,which(names(testing) %in% names(data.cleaned))]
  predicted <- predict(data.model, newdata = t[,which(!names(t) %in% METRIC)])
  actuals <- testing$CumProd
  RMSE <- RMSE(predicted,actuals,na.rm = T)
  COR <- cor(predicted,actuals,use="complete")
  RMSLE <- rmsle(actuals,predicted)
  print(paste("RMSE", RMSE, "RMSLE", RMSLE, "COR", COR, sep=" | "))
  data.fit <- cbind(actuals,predicted)
  data.fit <- as.data.frame(data.fit)
  hist(x = data.fit$actuals, plot = T, breaks = 3)
  ordered <- data.fit[order(data.fit$actuals),]
  ordered$id <- seq_len(nrow(ordered))
  ordered$error <- ((ordered$predicted - ordered$actuals)/ordered$actuals)*100
#   str(ordered)
  line <- gvisLineChart(data = ordered, xvar = "id", yvar = c("actuals","predicted","error"))
  # line <- gvisLineChart(data = ordered, xvar = "id", yvar = c("actuals","error"), options=list(series="[{targetAxisIndex: 0},
  #                                  {targetAxisIndex:1}]",vAxes="[{title:'actuals'}, {title:'error'}]"))
  plot(line)
  testing$CumProd <- predicted
  result <- list(testing=testing,data=data.cleaned,model=data.model)
  return(result)
}
newtesting <- trainingNtesting(data,testing)
newwhole <- trainingNtesting(wholetraining,wholetraining)

# wells.to.optimize <- data[which(names(data) %in% names(smote.cleaned))]
# original <- wholetraining # data
source("eog_financial.R")
# merge the CUMs from wholetraining into this
original <- cbind(newwhole$data,wholetraining[,which(names(wholetraining) %in% USELESS)])
write.table(original, "originals-pre-financials.csv",sep=",",row.names=FALSE, quote = FALSE)
finances_original <- calculateFinancial(original,"original")

# prepare the predicteds file - interpolate the CUMs 30,60,90 from 180
predictions <- newwhole$testing[,which(names(newwhole$testing) %in% names(newwhole$data))] 
predictions$CUM_90PD_OIL_PROD <- interpolate90(predictions$CumProd)
predictions$CUM_60PD_OIL_PROD <- interpolate60(predictions$CumProd)
predictions$CUM_30PD_OIL_PROD <- interpolate30(predictions$CumProd)
write.table(predictions, "predicteds-pre-financials.csv",sep=",",row.names=FALSE, quote = FALSE)
finances_predicted <- calculateFinancial(predictions,"predicted") # TODO use interpolated CUMs 30,60 & 90

source("buildOptimizations.R")
optim.model <- newwhole$model
finances <- finances_predicted
# merge the data and financial values into one
# wells.to.optimize <- head(merge(newwhole$data, finances[, c(ID, setdiff(colnames(finances),colnames(newwhole$data)))], by=ID),10)
wells.to.optimize <- merge(newwhole$data, finances[, c(ID, setdiff(colnames(finances),colnames(newwhole$data)))], by=ID)
financial_names <- setdiff(colnames(finances),colnames(newwhole$data))
optimums <- buildBestOptimizationsWithCost(wells.to.optimize,optim.model,financial_names,"ROR") #

# # save the model to disk
# rJava::.jcache(best.fit$classifier)
save(optim.model, file = "eog_final_model6.rda")

# TODO change the cut-offs
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