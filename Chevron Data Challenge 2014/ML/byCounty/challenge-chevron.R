# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(gdata)
require(randomForest)
require(caret)
library(foreach)
library(Metrics)
require(fmsb)
library(e1071)
library(googleVis)
library(MASS)
library(robustbase)
library(FSelector)
library(missForest)

# set the work directory
results <- "final"
mainDir <- "~/Chevron/DSChallenge/byCounty"
dir.create(file.path(mainDir, results), showWarnings = TRUE)
setwd(file.path(mainDir, results))

sink("notes.log", type=c("output", "message"))
# closeAllConnections()

if (!exists('loadData', mode = "function"))
  source("../loadCVXdata.R")

if (!exists('challengeRegression', mode = "function"))
  source("../challenge-regression.R")

# some tweaking constants
missingThreshold <- 0.8
iterations <- 100
VIF_THRESHOLD <- 10
IMP_VARS_CNT <- 10
trainSplit <- 0.70
corrCutOff <- 0.90
naCutOff <- 0.70

# imp vars!
ID <- "WellID"
USELESS <- c("Depth.Top","Propping.Agent.Units","Fluid.Units","Top_Zone") #,,,"Propping.Agent.Amount","Fluid.Amount","Between_Zone"
METRIC <- "EUR_o..Mstb."
outliers <- c("Fluid.Water..Gals.","Acid..Gals.","Gel.x.link..Gals.","Proppant...Total..lbs.","Fluid...Total..lbs.","Fluid.Amount","Propping.Agent.Amount","Propping.Agent.Amount.Per.Foot","Fluid.Amount.Per.Foot") #"Other..Gals.",
nonactionables <- c("Subarea","Operator","County","Completion.Date","Completion.Year","Surface.Latitude","Surface.Longitude","Depth.Total.Driller..ft.","Between_Zone",
                    "Top_Zone","Deepest_Zone","Depth.Top","Depth.Base","Fluid.Units","Propping.Agent.Units","CLFK..KHW.","U_SPBR..KHW.",
                    "L_SPBR..KHW.","WFMP..KHW.","WFMP_L..KHW.","WFMP_ATB..KH","CLINE..KHW.","STRN..KHW.","ATOK..KHW.","BEND..KHW.",
                    "ATOKA_L..KHW","MPLM..KHW.","WDFD..KHW.","DVNN..KHW.")

data <- loadData()
DATA <- data$DATA

geo_names <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
mapLayers <- c("1. Clearfork" = 1,"2. Upper_Spraberry" = 2,"3. Lower_Spraberry" = 3,"4. Wolfcamp" = 4,"5. Lower_Wolfcamp" = 5,
         "6. Amacker_Tippet_Base" = 6,"7. Cline" = 7,"8. Strawn" = 8,"9. Atoka" = 9,"10. Bend" = 10,"11. Lower_Atoka" = 11,"12. Mississippian" = 12,
         "13. Woodford" = 13,"14. Devonian" = 14)
# closeAllConnections()
cat("\n[geology] Ordered the layers as:\n")
cat(names(mapLayers),sep="\n")

# replace geo names in top & between zones
cat("\n[geology] Numbered the Top_Zone & Deepest_Zone")
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

cat("\n[geology] Adding Total.Height = (height at which Deepest_Zone starts – height at which Top_Zone starts)")
DATA[["Total.Height"]] <- 0
for(i in 1:nrow(DATA)) {
  if (!is.null(DATA[[DATA$Deepest_Zone[i]]]) & !is.null(DATA[[DATA$Top_Zone[i]]])) #
    DATA[["Total.Height"]][i] <- DATA[[DATA$Deepest_Zone[i]]][i] - DATA[[DATA$Top_Zone[i]]][i]
}
DATA$Total.Height<- unknownToNA(x=DATA$Total.Height, unknown=c(0))

cat("\n[geology] Adding Geology.Score (consult docs!)")
DATA[["Geology.Score"]] <- 0
for(i in 1:nrow(DATA)) {
  #   i <- 1
  if (!is.null(DATA[[DATA$Deepest_Zone[i]]]) & !is.null(DATA[[DATA$Top_Zone[i]]])) {
    start <- DATA$Top_Zone[i]
    end <- DATA$Deepest_Zone[i]
    score <- 0
    for(j in as.numeric(start):as.numeric(end)) {
        score <- score + (DATA[[as.character(j+1)]][i] - DATA[[as.character(j)]][i])*j
    }
    DATA[["Geology.Score"]][i] <- score
  }
}
DATA$Geology.Score<- unknownToNA(x=DATA$Geology.Score, unknown=c(0))

cat("\n[geology] Updating Between_Zone = (Top_Zone - Deepest_Zone)")
DATA$Between_Zone <- (as.numeric(DATA$Deepest_Zone) - as.numeric(DATA$Top_Zone)) + 1

DATA$Top_Zone <- as.numeric(DATA$Top_Zone)
DATA$Deepest_Zone <- as.numeric(DATA$Deepest_Zone)

# Drop the geology columns
cat("\n[geology] Dropping the geology layers info\n")
DATA <- DATA[,which(!names(DATA) %in% geo_names)]

write.table(DATA, "data.csv", sep=",", row.names=FALSE, quote = FALSE)
cat("\nData has", nrow(DATA), "rows and", ncol(DATA), "columns", sep = " ")
# str(DATA)

cat("\nDropping some more redundant columns -",USELESS,"\n",sep = " ")
DATA <- DATA[, -which(names(DATA) %in% USELESS)]

# options to split by Subarea, Operator, County!
closeAllConnections()
theSplitters <- c("Subarea","County") #"Operator",
dir <- getwd()
for(splitter in theSplitters) {
#   splitter <- "Operator"
  
  # create a sub dir for this splitter & point the working dir to it!
  dir.create(file.path(dir, splitter), showWarnings = TRUE)
  setwd(file.path(dir, splitter))
  subdir <- getwd()
  
  DATA[[splitter]] <- as.factor(DATA[[splitter]])
  splits <- split(DATA,DATA[[splitter]],drop = T)
  for(splitNo in 1:length(splits)) {
#     splitNo <- 1
    splitted <- splits[[splitNo]]
    category <- as.character(head(splitted[[splitter]],1))
    
    # create a sub dir for this category & point the working dir to it!
    dir.create(file.path(subdir, category), showWarnings = TRUE)
    setwd(file.path(subdir, category))
    
    # sink the results to a new file for this category
    sink(paste(category,".log",sep = ""), type=c("output", "message"))
    
#     splitted <- splitted[, -which(names(splitted) %in% splitter)]
    
    cat("\nReplace empty strings('') and #VALUE! as NA for:")
    # replace empty strings and #VALUE! as NAs for all columns
    for(i in names(splitted)) {
      if (length(which(isUnknown(x=splitted[[i]], unknown = c("","#VALUE!")))) > 0) {
        cat("\n",i)
        splitted[[i]] <- unknownToNA(x=splitted[[i]], unknown=c("","#VALUE!"))
      }
    }
    # str(splitted)
    
    # detect the numerical columns to remove outliers
    cat("\nKnow of some outliers in -",outliers,sep = " ")
    data.numericals <- splitted[, which(names(splitted) %in% outliers)]
    for(i in names(data.numericals)) {
      quantiles <- quantile(data.numericals[[i]], c(.0001, .9999), na.rm = TRUE)
      if (quantiles[1] >= 0 && quantiles[2] >= 0) {
        cat("\n",i,"has outliers!")
        data.numericals[[i]][ data.numericals[[i]] < quantiles[1] ] <- quantiles[1]
        data.numericals[[i]][ data.numericals[[i]] > quantiles[2] ] <- quantiles[2]
      }
    }
    cat("\nRemoved outliers by adjusting to the 99.99 | 0.01 percentile value if more than 99.99 | 0.01 percentile.")
    data.others <- splitted[ , -which(names(splitted) %in% outliers)]
    data.cleaned <- cbind(data.numericals, data.others)
#     write.table(data.cleaned, "data-cleaned.csv", sep=",", row.names=FALSE, quote = FALSE)
    
    # set 10% missing threshold
    cols <- ncol(data.cleaned)
    rows <- nrow(data.cleaned)
    NA_RowWise <- apply(data.cleaned, 1, function(z) sum(is.na(z)))
    colsMissing <- floor(cols*missingThreshold)
    cat("\nCheck for rows that have missing values >",missingThreshold*100,"% i.e.",colsMissing,"columns missing",sep = " ")
    DelRows <- which(NA_RowWise > colsMissing)
    cat("\n",length(DelRows),"such rows found!",sep = " ")
    data.rowsCleaned <- data.cleaned
    if (length(DelRows) > 0) {
      cat("\nDeleting rows",DelRows,sep = " ")
      data.rowsCleaned <- data.cleaned[-DelRows,]  
    }
    
    NA_ColWise <- apply(data.rowsCleaned, 2, function(z) sum(is.na(z)))
    rowsMissing <- floor(rows*missingThreshold)
    cat("\nCheck for columns that have missing values >",missingThreshold*100,"% i.e.",rowsMissing,"rows missing",sep = " ")
    DelCols <- which(NA_ColWise > rowsMissing)
    cat("\n",length(DelCols),"such columns found!",sep = " ")
    data.colsCleaned <- data.rowsCleaned
    if (length(DelCols) > 0) {
      cat("\nDeleting columns",names(DelCols),sep = " ")
      data.colsCleaned <- data.rowsCleaned[,-DelCols]
    }
    
    cat("\nData now has", nrow(data.colsCleaned), "rows and", ncol(data.colsCleaned), "columns", sep = " ")
#     write.table(data.colsCleaned, "data-without-missing.csv", sep=",", row.names=FALSE, quote = FALSE)
    
    # make data point to the cleaned up version
    splitted <- data.colsCleaned
    
    # deal with discrepancies in $Mesh.Size
    cat("\nCleaning the Mesh.Size as:")
    splitted$Mesh.Size <- as.character(splitted$Mesh.Size)
    cat("\n1. Replaced '20/04', '20 /40', '20/20', '24/40', '20/80' by '20/40'")
    splitted$Mesh.Size[splitted$Mesh.Size %in% c("20/04","20 /40","20/20","24/40","20/80")] <- "20/40"
    cat("\n2. Replaced '20/40 & 16/3' by 'MIXED'")
    splitted$Mesh.Size[splitted$Mesh.Size %in% c("20/40 & 16/3")] <- "MIXED"
    
    # in-discrepancies in test data
    cat("\nDealing with some new categories in test data as:")
    splitted$Propping.Agent.Type <- as.character(splitted$Propping.Agent.Type)
    cat("\n1. Propping.Agent.Type replaced 'SANDCRRCSD' by 'SANDRCSD'")
    splitted$Propping.Agent.Type[splitted$Propping.Agent.Type %in% c("SANDCRRCSD")] <- "SANDRCSD"
    splitted$Type <- as.character(splitted$Type)
    cat("\n2. Type replaced 'REACID' by 'ACID'")
    splitted$Type[splitted$Type %in% c("REACID")] <- "ACID"
    cat("\n3. Mesh.Size replaced '16/50' & '18/50' by '18/40'")
    splitted$Mesh.Size[splitted$Mesh.Size %in% c("16/50")] <- "18/40"
    splitted$Mesh.Size[splitted$Mesh.Size %in% c("18/50")] <- "18/40"
    
    # category labeling
    cat("\n\nConversion of categorical values to numeric values:","For a categorical feature F, replace categorical value x with mean target value for the samples for which feature F has value x.",sep = "\n")
    types <- lapply(splitted, class)
    distincts <- lapply(splitted, function(c) unique(c))
    
    for(i in names(splitted)){
      noOfCats <- length(levels(distincts[[i]]))
      if (noOfCats == 1) {
        splitted[[i]] <- NULL
      } else if ((noOfCats <= length(splitted[[i]])/2 && types[[i]] == "factor") || types[[i]] == "character") {
        means <- sapply(split(splitted$EUR_o..Mstb., splitted[[i]]), function(x) mean(x, na.rm=TRUE))
        cat("\n=>",i,"has",length(means),"categories", sep = " ")
        # convert to character type
        splitted[[i]] <- as.character(splitted[[i]])
        for(j in names(means)) {
          if (j == "")
            next
          cat("\nreplacing", j, "by", means[[j]], sep = " ")
          splitted[[i]][splitted[[i]] == j] <- means[[j]]
        }
        splitted[[i]] <- as.numeric(splitted[[i]])
      }
    }
    cat("\n\nNonparametric imputation of Missing Values using Random Forest\n")
    temp <- splitted[,-which(names(splitted) %in% c(ID,METRIC))] # 
    temp.imp <- missForest(xmis = temp)
    temp <- temp.imp$ximp
    temp <- cbind(splitted[[METRIC]], temp)
    colnames(temp)[1] <- METRIC
    splitted <- cbind(splitted[[ID]], temp)
    colnames(splitted)[1] <- ID
    
#     temp <- splitted #[,-which(names(data) %in% c(ID,METRIC))] # 
#     temp.imp <- missForest(xmis = temp, variablewise = T, verbose = T)
#     temp <- temp.imp$ximp
#     splitted <- temp
    
    # divide into training and test sets
    TESTING <- subset(splitted, is.na(splitted[[METRIC]])) # with unknown EURs
    data <- subset(splitted, !is.na(splitted[[METRIC]]))

    cat("\n\n# of wells on which to train = ",dim(data)[1])
    cat("\n# of wells on which to predict = ",dim(TESTING)[1])
    
    # let's set aside some "unseen" stratified data from the model!
    trainSplit <- 0.8
    set.seed(100)
    indx <- createDataPartition(data$EUR_o..Mstb.,p = trainSplit)
    index <- indx$Resample1
    wholetraining <- data
    testing=data[-index,] # with known EURs but part of unseen data by model
    data=data[index,]
    cat("\n\nKeep aside ",(1-trainSplit)*100,"% unseen wells from the training model = ",dim(testing)[1]," wells \n")
    
    trainingNtesting <- function(training, testing) {
      # the normal fit that we do!
      result <- challengeRegression(training)
      data.cleaned <- result$data
      data.model <- result$model
      rmses <- result$rmses
      hist(x = rmses, plot = T)
      write.table(rmses, "rmses.csv",sep=",",row.names=FALSE, quote = FALSE)
      
      # test on the held out test set
      t <- testing[,which(names(testing) %in% names(data.cleaned))]
      predicted <- predict(data.model, newdata = t[,which(!names(t) %in% METRIC)])
      actuals <- testing$EUR_o..Mstb.
      RMSE <- RMSE(predicted,actuals,na.rm = T)
      COR <- cor(predicted,actuals,use="complete")
      RMSLE <- rmsle(actuals,predicted)
      cat("\nRMSE", RMSE, "RMSLE", RMSLE, "COR", COR, sep=" | ")
      data.fit <- cbind(actuals,predicted)
      data.fit <- as.data.frame(data.fit)
      hist(x = data.fit$actuals, plot = T, breaks = 3)
      ordered <- data.fit[order(data.fit$actuals),]
      ordered$id <- seq_len(nrow(ordered))
      ordered$error <- ordered$actuals - ordered$predicted
      line <- gvisLineChart(xvar = "id", yvar = c("actuals","predicted","error"), data = ordered)
      plot(line)
      
      testing$EUR_o..Mstb. <- predicted
      result <- list(testing=testing,data=data.cleaned,model=data.model)
      return(result)
    }
    newtesting <- trainingNtesting(data,testing)
#     TESTING <- TESTING[,which(names(TESTING) %in% names(data.cleaned))]
#     TESTING <- TESTING[,which(!names(TESTING) %in% METRIC)]
#     predicted <- predict(data.model, newdata = TESTING)
#     TESTING[[METRIC]] <- predicted
#     write.table(TESTING[,which(names(TESTING) %in% c(ID,METRIC))], "predicted.csv",sep=",",row.names=FALSE, quote = FALSE)
#     newwhole <- trainingNtesting(wholetraining,wholetraining)
  }
}
