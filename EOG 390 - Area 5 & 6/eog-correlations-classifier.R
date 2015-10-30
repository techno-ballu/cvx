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

# set the work directory
setwd("/home/bolaka/EOG-data/AllWells/30-60-90")

if (!exists('buildRegression', mode = "function"))
  source("../buildRegression.R")

if (!exists('buildOptimizationsWithYConstraints', mode = "function"))
  source("../buildOptimizations.R")

# load well wise Completions data
DATA <- read.csv("../eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))
str(DATA)

# some tweaking constants
VIF_THRESHOLD <- 10
trainSplit <- 0.70
corrCutOff <- 0.85
# imp vars!
ID <- "Wellname"
USELESS <- c("CUM_90PD_OIL_PROD","CUM_30PD_OIL_PROD","CUM_60PD_OIL_PROD") #"Avg.ShutIn.Days","ShutinsPeriods","ShutinDays",
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
DATA <- DATA[, -which(names(DATA) %in% USELESS)]
str(DATA)

# divide into training and test sets
testing <- subset(DATA, is.na(DATA$CumProd))
data <- subset(DATA, !is.na(DATA$CumProd))
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

# class the wells into 2 groups
copy <- data
# copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = marks2, include.lowest = T, labels = c(1,2,3,4,5)) # labels = FALSE, 
copy$Class <- cut(x = copy$CumProd, breaks = 2, include.lowest = T, labels = F) 
table(copy$Class)

# divide the wells into random split of 70-30%
trainIndex <- sample(nrow(copy),floor(nrow(copy)*trainSplit))
train=copy[trainIndex,]
test=copy[-trainIndex,]

# sort the data based on CUM values!
temp <- copy[,which(!names(copy) %in% c("Class"))]
sorted <- temp[order(temp$CumProd),]
reference <- as.numeric(sorted[1,which(!names(sorted) %in% c(METRIC,ID))])
# test the correlation in the features between the highest producing wells and the lowest!
correlations <- numeric()
for(well in 1:nrow(sorted)) {
  inst <- sorted[well,which(!names(sorted) %in% c(METRIC,ID))] #
  corr <- cor(reference,as.numeric(inst),use="complete")
  correlations[length(correlations)+1] <- corr
}
correlations
sorted <- cbind(sorted,correlations)
View(sorted)
line <- gvisLineChart(xvar = "CumProd", yvar = c("correlations"), data = sorted)
plot(line)  

# Now to classify the test wells!
str(test)

# iterate well by well
for(testWell in 1:nrow(test)) {
  testWell <- 1
  inst <- test[testWell,which(!names(train) %in% c("Class",METRIC))]
  # correlation with reference well
  corr <- cor(reference,as.numeric(inst),use="complete")
  corr
}


# TODO change the cut-offs
LOW <- 0.5
HIGH <- 0.8

marks <- as.numeric(quantile(data$EUR_o..Mstb., c(0, LOW, HIGH, 1)))
marks

# integer code the EURs
copy <- data
# copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = marks2, include.lowest = T, labels = c(1,2,3,4,5)) # labels = FALSE, 
copy$Class <- cut(x = copy$EUR_o..Mstb., breaks = marks, include.lowest = T, labels = F) 
table(copy$Class)

lowCUMs <- data[data$EUR_o..Mstb.>=marks[[1]] & data$EUR_o..Mstb.<=marks[[2]], ]
hist(x = lowCUMs$EUR_o..Mstb., plot = T)

medCUMs <- data[data$EUR_o..Mstb.>marks[[2]] & data$EUR_o..Mstb.<=marks[[3]], ]
hist(x = medCUMs$EUR_o..Mstb., plot = T)

highCUMs <- data[data$EUR_o..Mstb.>marks[[3]] & data$EUR_o..Mstb.<=marks[[4]], ]
hist(x = highCUMs$EUR_o..Mstb., plot = T)


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
# save(medCUMsModel, file = "medium_completions_gaussian.rda")

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
