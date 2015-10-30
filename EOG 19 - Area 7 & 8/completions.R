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
library(rJava)
library(dfoptim)

# set the work directory
setwd("/home/bolaka/EOG-data/Completions")

# load well wise Completions data
data <- read.csv("completions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(data), "rows and", ncol(data), "columns ===============", sep = " "))
str(data)

# some tweaking constants
VIF_THRESHOLD <- 10
IMP_VARS_CNT <- 17
trainSplit <- 0.70
corrCutOff <- 0.85
# imp vars!
ID <- "Wellname"
USELESS <- c("First.prod.dt","DaysOfProd","Price") #"Avg.ShutIn.Days","ShutinsPeriods","ShutinDays"
METRIC <- "CumProd"
nonactionables <- c("Frac.ball.psi.increase","Pr.Drop.Due.to.Acid","Initial.FG","Final.FG","Production.Year") #"Avg.ShutIn.Days","DaysOfProd"

names(data)[names(data) == "CumProd.100days."] <- METRIC

# replace "undefined", empty strings and #VALUE! as NAs for all columns
for(i in names(data)) {
  if (length(which(isUnknown(x=data[[i]], unknown = c("undefined","","#VALUE!")))) > 0) {
    data[[i]] <- unknownToNA(x=data[[i]], unknown=c("undefined","","#VALUE!"))
    print(paste(i,"replaced UNDEFINEDs=", any(isUnknown(x=data[[i]], unknown = c("undefined","","#VALUE!"))), sep = " "))
  }
}
str(data)

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
  if (is.null(temp[[i]]))
    next
  print(paste("processing =========",i,sep = " "))
  
  highest.cor <- -1
  alias <- NULL
  t <- temp[,-which(names(temp) %in% c(i))]
  for(j in names(t)) {
    if (is.null(t[[j]]))
      next
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
optim <- optim(x_init,optimizer)
optim
# x_init <- c(100,100,50)
# x_min <- c(1,100,50)
# x_max <- c(500,500,100)
# optim <- nmkb(x_init, optimizer, lower=x_min, upper=x_max) # result ~ 
# optim
temp <- data[,-which(names(data) %in% c(ID))]
set.seed(floor(optim$par[1]))
temp <- rfImpute(CumProd ~., temp,iter = floor(optim$par[2]),ntree = floor(optim$par[3]))

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
names <- vif_func(in_frame=sample,thresh=VIF_THRESHOLD,trace=T)
names[length(names)+1] <- METRIC
names

data <- cbind(data[[ID]], temp[,which(names(temp) %in% names)])
colnames(data)[1] <- ID
str(data)

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
optim <- nmkb(x_init, optimizer, lower=x_min, upper=x_max) # result ~ 11782.86
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

# modeling
iterations <- 500
best.rmse <- 100000
best.train <- NULL
best.test <- NULL
best.fit <- NULL
best.cor <- -1

# n iterations to get best model
for(i in 1:iterations) {
  print(i)
  # set seed by iteration
  set.seed(i)
  # do random sampling to generate training & test sets
  trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
  # createDataPartition(data$Operator.Numeric, p = .7,list = FALSE,times = 1)
  train=forTraining[trainIndex,]
  test=forTraining[-trainIndex,]
  
  fit <- M5P(CumProd ~. , data = train)
  predicted <- predict(fit, newdata = test)
  
  #Convert predicted response vector as dataframe
  actual= test$CumProd
  
  # The RMSE
  RMSE<-RMSE(pred = predicted,obs = actual)
  COR <- cor(actual, predicted)
  RMSLE <- rmsle(actual,predicted)
  
  if (!is.na(RMSE) && RMSE < best.rmse && !is.na(COR) && COR > best.cor) {
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

# train on whole data
fit <- M5P(CumProd ~. , data = forTraining)
predicted <- predict(fit, newdata = forTraining)
actual <- forTraining$CumProd
RMSE <- RMSE(pred = predicted, obs = actual)
COR <- cor(actual, predicted)
summary(fit)
print(RMSE)
print(COR)

# save the model to disk
rJava::.jcache(best.fit$classifier)
save(best.fit, file = "completions_final.rda")

# optimization of wells!
M <- ncol(data)
names <- names(data)
actionables <- names(data)[!(names(data) %in% nonactionables)]
actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
optimums <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
actionables <- actionables[!(actionables %in% c(ID))]
data$Wellname <- as.character(data$Wellname)

for (well in 1:N) {
#     well <- 11
  stub <- data[well, ]
  print(stub$Wellname)
  name=unlist(strsplit(gsub("-", " ", stub$Wellname),split=" ",fixed=TRUE))[1]
  wellsData <- data[grepl(name,data$Wellname),]
  
  # create optimization wrapper for CUM
  opt_gm <- function(x, known.x) { #
    z <- stub[,-which(names(stub) %in% c(ID,METRIC))]
    
    # copy the data over top of the stub record
    for (i in names(x)) {
      if (i %in% actionables) {
        z[[i]] <- x[[i]]
      } else {
        z[[i]] <- known.x[[i]]
      }
    }
    # score the data and return the negative
    y <- predict(best.fit,newdata = z)
    -y
  }
  
  # start with given values and replace by mean for actionables
  start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
  
  # lower
  opt_min <- start
  for (i in names(opt_min)) {
    opt_min[[i]] <- min(wellsData[[i]],na.rm = T)
  }
  opt_min
  
  # upper
  opt_max <- start
  for (i in names(opt_max)) {
    opt_max[[i]] <- max(wellsData[[i]],na.rm = T)
  }
  opt_max
  
  opt_start <- start
  for (i in names(opt_start)) {
    if (i %in% actionables) {
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
#   opt_results <- nmkb(opt_start, opt_gm, lower=opt_min, upper=opt_max,known.x=opt_nons) # result ~ 
  
  # view the optimized inputs & predicted output (CUM)
  opt_results
  
  # test the optima
  temp <- stub
  action <- stub
  for(i in names(opt_results$par)) {
    opti <- opt_results$par[[i]]
    ori <- temp[[i]]
    action[[i]] <- "NA"
    if (ori != 0)
      action[[i]] <- ((opti - ori)/ori)*100
    temp[[i]] <-  opti
  }
  
  temp$CumProd <- abs(opt_results$value)
  action$CumProd <- ((temp$CumProd - stub$CumProd)/stub$CumProd) * 100
  
  optimums[well, ] <- temp
  actions[well, ] <- action
}
write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)

