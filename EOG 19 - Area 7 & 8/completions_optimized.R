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
library(optimx)
library(crs)
library(rJava)
# Load psych package
library(psych) #Calls: pairs.panels
library(car)
library(plyr)

# set the work directory
setwd("/home/bolaka/EOG-data/Completions")

# load well wise Completions data
data <- read.csv("WellWiseCompletionsn100mod.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(data), "rows and", ncol(data), "columns ===============", sep = " "))
str(data)

# some tweaking constants
VIF_THRESHOLD <- 50
IMP_VARS_CNT <- 17
trainSplit <- 0.70
corrCutOff <- 0.85
# imp vars!
ID <- "Wellname"
USELESS <- c("First.prod.dt","DaysOfProd") #"Avg.ShutIn.Days","ShutinsPeriods","ShutinDays"
METRIC <- "CumProd"
outliers <- c("Total.Fluid.Used","Pump.Down.fluid")
nonactionables <- c("Frac.ball.increase","Pr.Drop.Acid","Initial.FG","Final.FG","Production.Year") #"Avg.ShutIn.Days","DaysOfProd"
# collinear <- c("Max.Rate.bpm","Avg.Rate.bpm","Max.PSI")

names(data)[names(data) == "CumProd.100days."] <- METRIC

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
  data <- data[, -nearZeroVariance]
  print("Removing near zero variant columns!")
}

temp <- data[,-which(names(data) %in% c(ID,METRIC))]
# find correlated variables
for(i in names(temp)) {
  #   i <- "Acid"
  if (is.null(temp[[i]]))
    next
  print(paste("processing =========",i,sep = " "))
  
  highest.cor <- -1
  alias <- NULL
  t <- temp[,-which(names(temp) %in% c(i))]
  for(j in names(t)) {
    #         j <- "Production.Year"
    if (is.null(t[[j]]))
      next
    print(j)
    cor <- cor.test(temp[[i]],t[[j]],na.action = "na.exclude")
    if (!is.na(cor$estimate) && cor$estimate > highest.cor) {
      highest.cor <- cor$estimate
      alias <- j
    }
  }
  if (highest.cor > corrCutOff) {
    print(paste(i,alias,sep = " ***** and ***** "))
    iNAs <- length(is.na(temp[[i]]))
    jNAs <- length(is.na(t[[alias]]))
    if (iNAs <= jNAs) {
      print(paste("-------------------------------- dropping",alias,highest.cor,sep = " "))
      temp <- temp[,-which(names(temp) %in% c(alias))]
    } else if (iNAs > jNAs) {
      print(paste("-------------------------------- dropping",i,highest.cor,sep = " "))
      temp <- temp[,-which(names(temp) %in% c(i))]
    }
  }
}
temp <- cbind(data[[METRIC]], temp)
colnames(temp)[1] <- METRIC
data <- cbind(data[[ID]], temp)
colnames(data)[1] <- ID
str(data)

# impute the missing values
optimizer <- function(x) {
  seed <- x[1]
  iter <- x[2]
  ntree <- x[3]
  
  temp <- data[,-which(names(data) %in% c(ID))]
  set.seed(seed)
  temp <- rfImpute(CumProd ~., temp,iter = iter,ntree = ntree)
  fit <- M5P(CumProd ~. , data = temp)
  s<-summary(fit)
  rmse <- s$details[["rootMeanSquaredError"]]
  rmse
}

x_init <- c(100,100,100)
optimizer(x_init)
optim <- optim(x_init,optimizer)
optim
# optim <- nlm(optimizer,x_init,iterlim = 1000)
# optim <- optimx(par=x_init, fn=optimizer, method = c("Nelder-Mead"), control=list(follow.on = TRUE), itnmax = 10)
# opt_results <- optimx(par=opt_start, fn=opt_gm, method = "L-BFGS-B", lower = opt_start, upper = opt_start*1.2, itnmax = 10, control=list(maximize=TRUE,trace=6))
temp <- data[,-which(names(data) %in% c(ID))]
set.seed(floor(optim$par[1]))
temp <- rfImpute(CumProd ~., temp,iter = floor(optim$par[2]),ntree = floor(optim$par[3]))

#,"pca", "BoxCox", "knnImpute", "scale", "bagImpute", "spatialSign"
# preprocess
# temp <- data[,-which(names(data) %in% c(ID))]
# trans <- preProcess(temp, method = c("center"))
# transformed <- predict(object = trans, newdata = temp)
# transformed <- as.data.frame(transformed)
# str(transformed)
# names <- names(transformed)

# Fit a linear model to the data
# mod=lm(CumProd~.,data=temp)
# NAs <- is.na(coef(mod))
# names <- names(NAs)[NAs]
# names
# temp <- temp[,-which(names(temp) %in% names)]

# correlations <- cor(temp)
# corrplot(correlations, order = "hclust")
# highCorr <- findCorrelation(correlations, cutoff = corrCutOff)
# names(temp)[highCorr]
# length(highCorr)
# if (length(highCorr) > 0)
#   names <- names(temp)[-highCorr]
# 
# temp <- temp[,which(names(temp) %in% names)]
# str(temp)

#stepwise VIF function used below
vif_func<-function(in_frame,thresh,trace){
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  for(val in names(in_frame)){
    form_in<-formula(paste(val,' ~ .'))
    vif_init<-rbind(vif_init,c(val,VIF(lm(form_in,data=in_frame))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  print(paste("default", vif_max, sep = ","))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(names(in_frame))
  } else {
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      
      for(val in names(in_dat)){
        form_in<-formula(paste(val,' ~ .'))
        vif_add<-VIF(lm(form_in,data=in_dat))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    return(names(in_dat))
  }
}

sample <- temp[,-which(names(temp) %in% c(METRIC))]
# mod=lm(CumProd~.,data=temp)
# vif(mod)
names <- vif_func(in_frame=sample,thresh=VIF_THRESHOLD,trace=T)
names[length(names)+1] <- METRIC
names

# data <- cbind(data[[ID]], temp)
# colnames(data)[1] <- ID
data <- cbind(data[[ID]], temp[,which(names(temp) %in% names)])
colnames(data)[1] <- ID
str(data)

# write.table(data, "data_labeled3.csv", sep=",", row.names=FALSE, quote = FALSE)
# str(data)

# data <- data[,-which(names(data) %in% collinear)] # manual cleaning

# define training control
# train_control <- trainControl(method="LGOCV",returnResamp = "all",number = 100,p = 0.7,predictionBounds = c(FALSE,FALSE))
# rf <- train(CumProd ~., data=data[,-which(names(data) %in% c(ID))], method = "rf", importance=TRUE)

temp <- data[,-which(names(data) %in% c(ID))]
M <- ncol(temp)
# optimizer to get ranks
optimizer <- function(x) {
  seed <- x[1]
  mtry <- abs(x[2])
  ntree <- x[3]
  
  set.seed(seed)
  # running the randomForest implementation for getting importance
  rf <- randomForest(CumProd ~., data=temp, mtry=mtry, ntree=ntree, importance=TRUE)
  rmse <- rmse(rf$y,rf$predicted)
  rmse
}

x_init <- c(250,5,50)
x_min <- c(1,1,1)
x_max <- c(500,M-1,100)
# optimizer(x_init)
# optim <- optim(x_init,optimizer,control = list(trace=0,maxit=1000)) # result ~ 13000
# optim <- optim(x_init, optimizer, method="L-BFGS-B", control = list(trace=0,maxit=1000),lower=x_min, upper=x_max) # ABNORMAL_TERMINATION_IN_LNSRCH
library(dfoptim)
optim <- nmkb(x_init, optimizer, lower=x_min, upper=x_max) # result ~ 11782.86
# library(Rvmmin)
# optim <- Rvmmin(x_init, optimizer, lower=x_min, upper=x_max) # result ~ 13108.34
# library(DEoptim)
# optim <- DEoptim(fn = optimizer, lower = x_min, upper = x_max) # NaN value of objective function! 
optim

set.seed(floor(optim$par[1]))
# running the randomForest implementation for getting importance
rf <- randomForest(CumProd ~., data=temp, mtry=floor(optim$par[2]), ntree=floor(optim$par[3]), importance=TRUE)

# matrix with 1 column
ranks <- importance(rf,type=1)
# sort in descending order
ranks <- sort(ranks[,],decreasing = T)
ranks
# pick up top n=10
# ranks <- head(names(ranks),IMP_VARS_CNT)
# pick non-negative
isImportant <- function(X) {
  X[ifelse(X >= 0,TRUE,FALSE)]
}
ranks <- names(isImportant(ranks))
# ranks <- names(ranks)
ranks[length(ranks)+1] <- METRIC
ranks[length(ranks)+1] <- ID
data <- data[,which(names(data) %in% ranks)]
str(data)

write.table(data, "data_training.csv", sep=",", row.names=FALSE, quote = FALSE)

forTraining <- data[,-which(names(data) %in% c(ID))]
N <- nrow(forTraining)
# lets plot the learning curve for this data
# minSamples <- 3
# learnings <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Sample", "Train", "Test"))), stringsAsFactors=F)
# learning <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Sample", "Train", "Test"))), stringsAsFactors=F)
# # algo <- "M5"
# for(no in minSamples:N) {
#   #     no <- 19
#   # define training control
#   #   train_control <- trainControl(method="LOOCV",returnResamp = "all",verboseIter = FALSE)
#   
#   #   best.fit <- NULL
#   train.RMSE <- numeric()
#   test.RMSE <- numeric()
#   for(iter in 1:30) {
#     #         iter <- 1
#     # set seed fixed
#     set.seed(iter)
#     # do random sampling to generate training & test sets
#     index <- sample(N,no)
#     trainIndex <- sample(index,floor(no*trainSplit))
#     #     trainIndex <- sample(index, no-1)
#     testIndex <- index[!(index %in% trainIndex)]
#     train <- forTraining[trainIndex,]
#     test <- forTraining[testIndex,]
#     
#     # train the model 
#     #     fit <- train(CumProd ~ ., data=train, trControl=train_control, method=algo)
#     fit <- M5P(CumProd ~. , data = train)
#     summary(fit)
#     
#     # on train data iteration j
#     predicted <- predict(fit, newdata = train)
#     actual= train$CumProd
#     # The RMSE
#     theTrain<-RMSE(predicted,actual)
#     
#     # on test data iteration j
#     predicted <- predict(fit, newdata = test)
#     actual= test$CumProd
#     # The RMSE
#     theTest<-RMSE(predicted,actual)
#     
#     train.RMSE[iter] <- theTrain
#     test.RMSE[iter] <- theTest
#     
#   }
#   theTrain <- mean(train.RMSE,na.rm = T)
#   theTest <- mean(test.RMSE,na.rm = T)
#   
#   learning <- c(no,theTrain,theTest)
#   learnings[no,] <- learning
#   
#   print(paste(no,theTrain,theTest,sep = " "))
# }
# write.table(learnings, "data_learnings.csv", sep=",", row.names=FALSE, quote = FALSE)
# 
# # plot learning curve
# xrange <- range(1:N) 
# yrange <- range(min(min(learnings$Train,na.rm = T),min(learnings$Test,na.rm = T)):max(max(learnings$Train,na.rm = T),max(learnings$Test,na.rm = T))) 
# colors <- rainbow(2) 
# linetype <- c(1:2) 
# plotchar <- seq(18,19,1)
# plot(xrange,yrange,xlab="Number of training example",ylab="Error")
# lines(2:N, learnings$Train[2:N], type="b", lwd=1.5, lty=linetype[1], col=colors[1], pch=plotchar[1]) 
# lines(2:N, learnings$Test[2:N], type="b", lwd=1.5, lty=linetype[2], col=colors[2], pch=plotchar[2]) 
# legend(xrange[1], yrange[2], c("Train","Test"), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="RF learing curve")

# modeling
iterations <- 500
best.rmse <- 100000
best.train <- NULL
best.test <- NULL
best.fit <- NULL
best.cor <- -1

# n iterations to get best model
for(i in 1:iterations) {
  #       i<- 50
  print(i)
  # set seed by iteration
  set.seed(i)
  # do random sampling to generate training & test sets
  trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
  # createDataPartition(data$Operator.Numeric, p = .7,list = FALSE,times = 1)
  train=forTraining[trainIndex,]
  test=forTraining[-trainIndex,]
  
  # the model - lm
  #   fit <- lm(CumProd ~. , data = train, na.action = na.pass)
  fit <- M5P(CumProd ~. , data = train)
  #   train_control <- trainControl(method="LGOCV",returnResamp = "all",number = 10,p = 0.7) #,predictionBounds = c(TRUE,FALSE)
  #   fit <- train(CumProd ~., data=train, method = "rf", trControl = train_control)
  predicted <- predict(fit, newdata = test)
  
  #Convert predicted response vector as dataframe
  actual= test$CumProd
  # predicted <- as.data.frame(predicted)
  # colnames(predicted) <- "CUM"
  #Replacing NA with average CUM from training set (NA not allowed for submission)
  # predicted[is.na(predicted)] <- mean(train$CumProd)
  
  # The RMSE
  RMSE<-RMSE(pred = predicted,obs = actual)
  #   RMSLE <- rmsle(actual, predicted)
  COR <- cor(actual, predicted)
  # score the test data and plot pred vs. obs 
  # plot(data.frame('Predicted'=predicted, 'Observed'=actual))
  
  if (RMSE < best.rmse && !is.na(COR) && COR > best.cor) {
    print(paste("====>", RMSE, i, sep = " "))
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
rJava::.jcache(best.fit$classifier)
save(best.fit, file = "completions_final.rda")

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
actionables <- names(data)[!(names(data) %in% nonactionables)]
actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
optimums <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
actionables <- actionables[!(actionables %in% c(ID))]
# names[length(names)+1] <- "ActualCum"
# names[length(names)+1] <- "PredCum"
# names[length(names)+1] <- "Error"
# names[length(names)+1] <- "PresActualInc"
# names[length(names)+1] <- "PresPredInc"

data$Wellname <- as.character(data$Wellname)

for (well in 1:N) {
  #   well <- 3
  stub <- data[well, ]
  print(stub$Wellname)
  name=unlist(strsplit(gsub("-", " ", stub$Wellname),split=" ",fixed=TRUE))[1]
  wellsData <- data[grepl(name,data$Wellname),]
  
  # create optimization wrapper for CUM
  opt_gm <- function(x, known.x) { #
    #     x <- opt_start*1.1
    #     known.x <- opt_nons
    #     x <- ifelse(is.na(known.x), opt.x, known.x)
    z <- stub[,-which(names(stub) %in% c(ID,METRIC))]
    
    # copy the data over top of the stub record
    for (i in names(x)) {
      #       i <- "X..Clusters"
      if (i %in% actionables) {
        #         print(paste("actionable",i,x[[i]],sep = " = "))
        z[[i]] <- x[[i]]
      } else {
        #         print(paste("non-actionable",i,known.x[[i]],sep = " = "))
        z[[i]] <- known.x[[i]]
      }
    }
    # score the data and return the negative
    y <- predict(best.fit,z)
    -y
  }
  
  # start with given values and replace by mean for actionables
  start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
  
  # lower
  opt_min <- start
  for (i in names(opt_min)) {
    #     if (i %in% actionables)
    opt_min[[i]] <- min(wellsData[[i]],na.rm = T)
  }
  opt_min
  
  # upper
  opt_max <- start
  for (i in names(opt_max)) {
    #     if (i %in% actionables)
    opt_max[[i]] <- max(wellsData[[i]],na.rm = T)
  }
  opt_max
  
  opt_start <- start
  for (i in names(opt_start)) {
    if (i %in% actionables) {
      #       opt_start[[i]] <- opt_start[[i]]*1.1
      opt_start[[i]] <- mean(wellsData[[i]],na.rm = T)
    }
  }
  opt_start
  
  opt_nons <- start
  for (i in names(opt_nons)) {
    if (i %in% actionables)
      opt_nons[[i]] <- NA
  }
  opt_nons
  
  # optimize
  opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max,known.x=opt_nons) # ,
  #   opt_results <- nlminb(opt_start*1.5, opt_gm, scale = 1, control = list(iter.max=1000,trace=1), lower = opt_start, upper = opt_start*2,known.x=opt_nons)
  #   opt_results <- optimx(par=opt_start, fn=opt_gm, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
  #   opt_results <- optimx(par=opt_start, fn=opt_gm, method = "L-BFGS-B", lower = opt_start, upper = opt_start*1.2, itnmax = 10, control=list(maximize=TRUE,trace=6))
  #   opts <-list("MAX_BB_EVAL"=500,
  #               "MIN_MESH_SIZE"=0.001,
  #               "INITIAL_MESH_SIZE"=0.1,
  #               "MIN_POLL_SIZE"=0.0001)
  #   bbout <- c(2,1,0)
  #   opt_results <- snomadr(eval.f=opt_gm, n=as.numeric(x = length(start)), x0 = as.numeric(as.vector(opt_start)), bbin=bbin, bbout=bbout, lb=opt_min, ub=opt_max, known.x=opt_nons)
  
  # view the optimized inputs & predicted output (CUM)
  opt_results
  
  # test the optima
  temp <- stub
  action <- stub
  #   temp$ActualCum <- stub$CumProd
  #   temp$PredCum <- predict(best.fit,start)
  for(i in names(opt_results$par)) {
    opti <- opt_results$par[[i]]
    ori <- temp[[i]]
    action[[i]] <- "NA"
    if (ori != 0)
      action[[i]] <- ((opti - ori)/ori)*100
    temp[[i]] <-  opti
  }
  
  # remove non actionables
  #   temp <- temp[!(names(temp) %in% nonactionables)]
  #   action <- action[!(names(action) %in% nonactionables)]
  
  #   names(temp)[names(temp) == "CumProd"] <- "PresCum"
  temp$CumProd <- abs(opt_results$value)
  #   temp$Error <- (temp$PredCum - temp$ActualCum)/temp$ActualCum
  #   temp$Error <- temp$Error * 100
  #   temp$PresPredInc <- (temp$PresCum - temp$PredCum)/temp$PredCum
  #   temp$PresPredInc <- temp$PresPredInc * 100
  #   temp$PresActualInc <- (temp$PresCum - temp$ActualCum)/temp$ActualCum
  #   temp$PresActualInc <- temp$PresActualInc * 100
  action$CumProd <- ((temp$CumProd - stub$CumProd)/stub$CumProd) * 100
  #   temp$CumProd <- abs(opt_results$value)
  #   temp$PctInc <- (abs(opt_results$value) - temp$PredCum)/temp$PredCum
  #     temp$CumProd <- abs(opt_results$objective)
  #     temp$PctInc <- (abs(opt_results$objective) - temp$PredCum)/temp$PredCum
  
  optimums[well, ] <- temp
  actions[well, ] <- action
}
write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)


