print("=============== loading script to build best.fit model on given data ===============")

challengeModel <- function(data, returnRMSE = F, classify = F) {
  
  if (classify == T)
    METRIC <- "Class"
  
  if (any(is.na(data))) {
    print("Clean rows & columns missing > 80%")
    # set 10% missing threshold
    cols <- ncol(data)
    rows <- nrow(data)
    NA_RowWise <- apply(data, 1, function(z) sum(is.na(z)))
    quantile(NA_RowWise)
    # eliminate rows whose missing > 20%
    colsMissing <- floor(cols*missingThreshold)
    colsMissing
    DelRows <- which(NA_RowWise > colsMissing)
    length(DelRows)
    data.rowsCleaned <- data
    if (length(DelRows) > 0) {
      data.rowsCleaned <- data[-DelRows,]  
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
    
    # make data point to the cleaned up version
    data <- data.colsCleaned
  }
  
  temp <- data[,-which(names(data) %in% c(ID,METRIC))]

  # find columns which have near zero variance
  nearZeroVariance <- nearZeroVar(temp)
  if (length(nearZeroVariance) > 0) {
    print(paste("Removing near zero variant columns!", names(temp)[nearZeroVariance],sep = " "))
    temp <- temp[, -nearZeroVariance]
  }
  
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
#   str(data)
  
  temp <- data[,-which(names(data) %in% c(ID))]
  
  if (classify == F && any(is.na(data))) {
    print("Impute the missing values!")
    # impute the missing values
    optimizerMissing <- function(x) {
      seed <- x[1]
      #   iter <- x[2]
      ntree <- x[2]
      
      temp <- data[,-which(names(data) %in% c(ID))]
      set.seed(seed)
      temp <- rfImpute(EUR_o..Mstb. ~., temp,iter = 5,ntree = ntree)
      fit <- M5P(EUR_o..Mstb. ~. , data = temp)
      s<-summary(fit)
      rmse <- s$details[["rootMeanSquaredError"]]
      rmse
    }
    
    # x_init <- c(106,52)
    # optimizerMissing(x_init)
    set.seed(106)
    temp <- rfImpute(EUR_o..Mstb. ~., temp,iter = 5,ntree = 52)
    
    # optim <- optim(x_init,optimizerMissing)
    # optim
    # x_init <- c(100,100,50)
    # x_min <- c(1,100,50)
    # x_max <- c(500,500,100)
    # optim <- nmkb(x_init, optimizerMissing, lower=x_min, upper=x_max) # result ~ 
    # optim
    
    # set.seed(floor(optim$par[1]))
    # temp <- rfImpute(EUR_o..Mstb. ~., temp,iter = ceiling(optim$par[2]),ntree = ceiling(optim$par[3]))
  }
  write.table(temp, "debug.csv", sep=",", row.names=FALSE, quote = FALSE)
  
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
#   str(data)
  
  temp <- data[,-which(names(data) %in% c(ID))]
  M <- ncol(temp)
  
  if (classify == F) {
    # optimizer to get ranks
    optimizerRanker <- function(x) {
      seed <- x[1]
      mtry <- abs(x[2])
      ntree <- x[3]
      
      set.seed(seed)
      # running the randomForest implementation for getting importance
      rf <- randomForest(EUR_o..Mstb. ~., data=temp, mtry=mtry, ntree=ntree, importance=TRUE)
      rmse <- rmse(rf$y,rf$predicted)
      rmse
    }
    
    x_init <- c(105,35,55)
    #   x_min <- c(1,1,1)
    #   x_max <- c(500,M-1,100)
    #   optim <- nmkb(x_init, optimizerRanker, lower=x_min, upper=x_max) # result ~ 14395.25
    optim <- optim(x_init,optimizerRanker,control = list(trace=0,maxit=100))
    optim
    
    set.seed(floor(optim$par[1]))
    # running the randomForest implementation for getting importance
    rf <- randomForest(EUR_o..Mstb. ~., data=temp, mtry=floor(optim$par[2]), ntree=floor(optim$par[3]), importance=TRUE)
    
    # matrix with 1 column
    ranks <- importance(rf,type=1)
    
    # sort in descending order
    ranks <- sort(ranks[,],decreasing = T)
  } else {
    rpart <- rpart(Class ~ ., data = temp, method = "class")
    ranks <- rpart$variable.importance
  }

#   print(ranks)
  # pick up top n=10
  # ranks <- head(names(ranks),IMP_VARS_CNT)
  # pick non-negative
  isImportant <- function(X) {
    X[ifelse(X >= 0,TRUE,FALSE)]
  }
  ranks <- names(isImportant(ranks))
#   cat(ranks)
  # ranks <- names(ranks)
  ranks[length(ranks)+1] <- METRIC
  ranks[length(ranks)+1] <- ID
  data <- data[,which(names(data) %in% ranks)]
#   str(data)
  
  write.table(data, "data_training.csv", sep=",", row.names=FALSE, quote = FALSE)
  
  forTraining <- data[,-which(names(data) %in% c(ID))]
  N <- nrow(forTraining)
  
  if (classify == F) {
    # modeling
    best.rmse <- numeric()
    best.rmsle <- numeric()
    best.train <- NULL
    best.test <- NULL
    best.fit <- NULL
    best.cor <- numeric()
    
    # n iterations to get best model
    for(i in 1:iterations) {
      # print(i)
      # set seed by iteration
      set.seed(i)
      # do random sampling to generate training & test sets
      trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
      # createDataPartition(data$Operator.Numeric, p = .7,list = FALSE,times = 1)
      train=forTraining[trainIndex,]
      test=forTraining[-trainIndex,]
      
      fit <- M5P(EUR_o..Mstb. ~. , data = train)
      predicted <- predict(fit, newdata = test)
      
      #Convert predicted response vector as dataframe
      actual= test$EUR_o..Mstb.
      
      # The RMSE
      RMSE<-RMSE(pred = predicted,obs = actual)
      RMSLE <- rmsle(predicted = predicted, actual = actual)
      COR <- cor(actual, predicted)
      
      best.rmse[i] <- RMSE
      best.rmsle[i] <- RMSLE
      best.cor[i] <- COR
      
      if (RMSE <= min(best.rmse, na.rm = T)) { # && !is.na(COR) && COR >= max(best.cor,na.rm = T)
        print(paste("====>", RMSE, i, sep = " "))
        #       best.rmse <- RMSE
        #       best.train <- train
        #       best.test <- test
        best.fit <- fit
        #       best.cor <- COR
        #       best.rmsle <- RMSLE
        #       best.seed <- i
      }
    }
    
    # print results of the best iteration
    # summary(best.fit)
    RMSE <- mean(best.rmse,na.rm = T)
    COR <- mean(best.cor,na.rm = T)
    RMSLE <- mean(best.rmsle,na.rm = T)
    print(paste("mean RMSE =",RMSE,sep = " "))
    print(paste("min RMSE =",min(best.rmse,na.rm = T),sep = " "))
    print(paste("mean COR =",COR,sep = " "))
    print(paste("mean RMSLE =",RMSLE,sep = " "))
    print(paste("min RMSLE =",min(best.rmsle,na.rm = T),sep = " "))
  } else {
    best.fit <- rpart(Class ~ ., data = forTraining, method = "class")
  }
  if (returnRMSE == T)
    return(RMSE)
  return(best.fit)
}