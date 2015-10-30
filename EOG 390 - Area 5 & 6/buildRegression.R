print("=============== loading script to build best.fit model on given data ===============")

buildRegression <- function(data) {
  # The Predictors
  temp <- data[,-which(names(data) %in% c(ID,METRIC))]
  
  # find columns which have near zero variance
  nearZeroVariance <- nearZeroVar(temp)
  if (length(nearZeroVariance) > 0) {
    temp <- temp[, -nearZeroVariance]
    print("Removing near zero variant columns!")
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
  str(data)
  
  temp <- data[,-which(names(data) %in% c(ID))]
  
  #   if (F && any(is.na(data))) {
  #     # impute the missing values
  #     optimizerMissing <- function(x) {
  #       seed <- x[1]
  #       iter <- x[2]
  #       ntree <- x[3]
  #       
  #       temp <- data[,-which(names(data) %in% c(ID))]
  #       set.seed(seed)
  #       temp <- rfImpute(CumProd ~., temp,iter = iter,ntree = ntree)
  #       fit <- M5P(CumProd ~. , data = temp)
  #       s<-summary(fit)
  #       rmse <- s$details[["rootMeanSquaredError"]]
  #       rmse
  #     }
  #     
  #     x_init <- c(117,100,94)
  #     # optimizerMissing(x_init)
  #     set.seed(100)
  #     temp <- rfImpute(CumProd ~., temp,iter = 100,ntree = 100)
  #     
  #     # optim <- optim(x_init,optimizerMissing)
  #     # optim
  #     # x_init <- c(100,100,50)
  #     # x_min <- c(1,100,50)
  #     # x_max <- c(500,500,100)
  #     # optim <- nmkb(x_init, optimizerMissing, lower=x_min, upper=x_max) # result ~ 
  #     # optim
  #     
  #     # set.seed(floor(optim$par[1]))
  #     # temp <- rfImpute(CumProd ~., temp,iter = ceiling(optim$par[2]),ntree = ceiling(optim$par[3]))
  #     
  #   }
  
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
  names <- c(names,important,METRIC)
#   names[length(names)+1] <- METRIC
  names
  
  data <- cbind(data[[ID]], temp[,which(names(temp) %in% names)])
  colnames(data)[1] <- ID
  str(data)
  
  temp <- data[,-which(names(data) %in% c(ID))]
  M <- ncol(temp)
  # optimizer to get ranks
  ranks <- information.gain(CumProd ~ ., temp)
  
  ranks$names <- rownames(ranks)
  # sort in descending order
  ranks <- ranks[order(-ranks$attr_importance),]
  ranks <- ranks[ranks$attr_importance > 0,2]
  # pick up top n=10
  # ranks <- head(names(ranks),IMP_VARS_CNT)
  # pick non-negative
  #   isImportant <- function(X) {
  #     X[ifelse(X >= 0,TRUE,FALSE)]
  #   }
  #   ranks <- names(isImportant(ranks))
  ranks <- c(ranks,important,METRIC,ID)
  print(ranks)
#   ranks <- names(ranks)
#   ranks[length(ranks)+1] <- METRIC
#   ranks[length(ranks)+1] <- ID
  data <- data[,which(names(data) %in% ranks)]
  str(data)
  
  write.table(data, "data_training.csv", sep=",", row.names=FALSE, quote = FALSE)
  
  forTraining <- data[,-which(names(data) %in% c(ID))]
  N <- nrow(forTraining)
  
  controlObject <- trainControl(method = "repeatedcv", repeats = 5, number = 10)
  
  # modeling
  iterations <- 100
  best.rmse <- numeric()
  best.rmsle <- numeric()
  best.fit <- NULL
  best.cor <- numeric()
  seeds <- sample(1:1000, 100, replace=F)
  
  # n iterations
  for(i in 1:iterations) {
    # set seed by iteration
    set.seed(seeds[i])
    # do random sampling to generate training & test sets
    trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
    # createDataPartition(data$Operator.Numeric, p = .7,list = FALSE,times = 1)
    train=forTraining[trainIndex,]
    test=forTraining[-trainIndex,]
    
    #   set.seed(975)
    #   trainIndex <- createDataPartition(forTraining$CumProd,p = 3/4)[[1]]
    #   trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
    #   train <- forTraining[trainIndex,]
    #   test <- forTraining[-trainIndex,]
    #   fit <- train(CumProd ~ ., data = train, method = "gaussprLinear", trControl = controlObject)
    #     fit <- train(CumProd ~ ., data = train, method = "gcvEarth", trControl = controlObject)
    
    #       fit <- M5P(CumProd ~. , data = train)
    #     fit <- rpart(CumProd ~. , data = train)
    #     fit = earth(CumProd ~ ., data = train)
    fit = rlm(CumProd ~ ., data = train, na.action = "na.exclude")
    #         fit <- lmrob(CumProd ~ ., data = train, na.action = "na.exclude")
    #     fit <- lm(CumProd ~ ., data = train, na.action = "na.exclude")
    predicted <- predict(fit, newdata = test)
    actual= test$CumProd
    
    # The RMSE
    RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
    RMSLE <- rmsle(predicted = predicted, actual = actual)
    COR <- cor(actual, predicted,use="complete")
    
    # print(i)
    print(paste("*", RMSE, RMSLE, COR, i, sep = " * "))
    
    best.rmse[i] <- RMSE
    best.rmsle[i] <- RMSLE
    best.cor[i] <- COR
    
    #   if (RMSE <= min(best.rmse, na.rm = T)) { # && !is.na(COR) && COR >= max(best.cor,na.rm = T)
    #       best.rmse <- RMSE
    #       best.train <- train
    #       best.test <- test
    #     best.fit <- fit
    #       best.cor <- COR
    #       best.rmsle <- RMSLE
    #       best.seed <- i
    #   }
  }

  # fit model on the whole data
  whole_fit <- rlm(CumProd ~ ., data = forTraining, na.action = "na.exclude")
  
  # print results of the best iteration
  # summary(best.fit)
  RMSE <- mean(best.rmse,na.rm = T)
  COR <- mean(best.cor,na.rm = T)
  RMSLE <- mean(best.rmsle,na.rm = T)
  print(paste("mean RMSE =",RMSE,sep = " "))
  print(paste("mean COR =",COR,sep = " "))
  print(paste("mean RMSLE =",RMSLE,sep = " "))
  
  result <- list(model = whole_fit, data = data, rmses = best.rmse, rmsles = best.rmsle, cors = best.cor, ranks = ranks)
  return(result)
}