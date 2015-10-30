print("=============== loading script to build best.fit model on given data ===============")

challengeRegression <- function(data, returnRMSE = F, classify = F) {
#   
#   data <- smp
#   classify = T
  
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
  
  # print("Impute the missing values from correlations and drop highly correlated!")
  for(i in names(temp)) {
    #   i <- "Clearfork"
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
    if (highest.cor > naCutOff) {
      iNAs <- length(which(is.na(temp[[i]])))
      jNAs <- length(which(is.na(temp[[alias]])))
      print(paste(i,iNAs,alias,jNAs,sep = " "))
      if (iNAs <= jNAs) {
        if (F && any(is.na(temp[[i]]))) {
          print(paste("-------------------------------- interpolating",i,highest.cor,sep = " "))
          interpolate <- approxfun(temp[[alias]], temp[[i]],rule = 2)
          predicted <- interpolate(temp[[alias]])
          actual= temp[[i]]
          RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
          RMSE
          actual[is.na(actual)] <- predicted[is.na(actual)]
          temp[[i]] <- actual
        }
        if (highest.cor > corrCutOff) {
          print(paste("-------------------------------- dropping",alias,highest.cor,sep = " "))
          temp <- temp[,-which(names(temp) %in% c(alias))]
        }
      } else if (iNAs > jNAs) {
        if (F && any(is.na(temp[[alias]]))) {
          print(paste("-------------------------------- interpolating",alias,highest.cor,sep = " "))
          interpolate <- approxfun(temp[[i]], temp[[alias]],rule = 2)
          predicted <- interpolate(temp[[i]])
          actual= temp[[alias]]
          RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
          RMSE
          actual[is.na(actual)] <- predicted[is.na(actual)]
          temp[[alias]] <- actual
        }
        if (highest.cor > corrCutOff) {
          print(paste("-------------------------------- dropping",i,highest.cor,sep = " "))
          temp <- temp[,-which(names(temp) %in% c(i))]
        }
      }
    }
  }
  
  #   temp <- cbind(data[[METRIC]], temp)
  #   colnames(temp)[1] <- METRIC
  #   data <- cbind(data[[ID]], temp)
  #   colnames(data)[1] <- ID
  #   str(data)
  
  #   temp <- data[,-which(names(data) %in% c(ID))]
  #   sample <- temp[,-which(names(temp) %in% c(METRIC))]
  
  #   if (classify == F && any(is.na(temp))) {
  #     print("Impute the missing values from knnImpute!")
  #     trans <- preProcess(temp, method =  c("knnImpute"))  
  #     temp <- predict(object = trans, newdata = temp)
  #     str(temp)
  #   }
  #   write.table(temp, "debug.csv", sep=",", row.names=FALSE, quote = FALSE)
  
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
  
  names <- vif_func(in_frame=temp,thresh=VIF_THRESHOLD,trace=T)
  names
  
  x <- cbind(data[[ID]], temp[,which(names(temp) %in% names)])
  colnames(x)[1] <- ID
  x <- cbind(data[[METRIC]], x)
  colnames(x)[1] <- METRIC
  data <- x
  str(data)
  
  temp <- data[,-which(names(data) %in% c(ID))]
  M <- ncol(temp)
  
#   if (classify == F) {
#     ranks <- information.gain(EUR_o..Mstb. ~ ., temp)
#   } else {
#     ranks <- information.gain(Class ~ ., temp)
#     #     rpart <- rpart(Class ~ ., data = temp, method = "class")
#     #     ranks <- rpart$variable.importance
#   }
#   ranks$names <- rownames(ranks)
#   # sort in descending order
#   ranks <- ranks[order(-ranks$attr_importance),]
#   ranks <- ranks[ranks$attr_importance > 0,2]
# #   ranks <- ranks[,2]
#   ranks[length(ranks)+1] <- METRIC
#   ranks[length(ranks)+1] <- ID
#   print(ranks)
#   data <- data[,which(names(data) %in% ranks)]
#   str(data)
  
  write.table(data, "data_training.csv", sep=",", row.names=FALSE, quote = FALSE)
  
  forTraining <- data[,-which(names(data) %in% c(ID))]
  N <- nrow(forTraining)
  controlObject <- trainControl(method = "repeatedcv", repeats = 5, number = 10)
  
  if (classify == F) {
    # modeling
    best.rmse <- numeric()
    best.rmsle <- numeric()
    best.train <- NULL
    best.test <- NULL
    best.fit <- NULL
    best.cor <- numeric()
    seeds <- sample(1:1000, 100, replace=F)
    
    # n iterations 
    for(i in 1:iterations) {
      #       print(i)
      # set seed by iteration
      set.seed(seeds[i])
      # do random sampling to generate training & test sets
      trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
      #     trainIndex <- createDataPartition(forTraining$EUR_o..Mstb.,p = trainSplit)[[1]]
      # createDataPartition(data$Operator.Numeric, p = .7,list = FALSE,times = 1)
      train=forTraining[trainIndex,]
      test=forTraining[-trainIndex,]
      #       fit <- train(EUR_o..Mstb. ~ ., data = train, method = "earth")      
      #       fit <- M5P(EUR_o..Mstb. ~. , data = train)
      #       fit = rlm(EUR_o..Mstb. ~ ., data = train, na.action = "na.exclude") # best
      fit <- lmrob(EUR_o..Mstb. ~ ., data = train, na.action = "na.exclude")
      predicted <- predict(fit, newdata = test)
      actual= test$EUR_o..Mstb.
      
      # The RMSE
      RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
      RMSLE <- rmsle(actual, predicted)
      COR <- cor(actual, predicted,use="complete")
      
      print(paste("*", RMSE, RMSLE, COR, i, sep = " * "))
      
      best.rmse[i] <- RMSE
      best.rmsle[i] <- RMSLE
      best.cor[i] <- COR
      
      #       if (RMSE <= min(best.rmse, na.rm = T)) { # && !is.na(COR) && COR >= max(best.cor,na.rm = T)
      #         print(paste("====>", RMSE, i, sep = " "))
      #         #       best.rmse <- RMSE
      #         #       best.train <- train
      #         #       best.test <- test
      #         best.fit <- fit
      #         #       best.cor <- COR
      #         #       best.rmsle <- RMSLE
      #         #       best.seed <- i
      #       }
    }
    
    # fit model on the whole data
    whole_fit <- lmrob(EUR_o..Mstb. ~ ., data = forTraining, na.action = "na.exclude")
    
    # print results of the best iteration
    # summary(best.fit)
    RMSE <- mean(best.rmse,na.rm = T)
    COR <- mean(best.cor,na.rm = T)
    RMSLE <- mean(best.rmsle,na.rm = T)
    print(paste("mean RMSE =",RMSE,sep = " "))
    print(paste("mean COR =",COR,sep = " "))
    print(paste("mean RMSLE =",RMSLE,sep = " "))
    result <- list(model = whole_fit, data = data, rmses = best.rmse, rmsles = best.rmsle)
  } else {
    fit <- rpart(Class ~ ., data = forTraining, method = "class")
    result <- list(model = fit, data = data)
  }
  if (returnRMSE == T)
    return(RMSE)
  
  return(result)
}