challengeRegression <- function(data, returnRMSE = F, classify = F) {
  
  if (classify == T)
    METRIC <- "Class"
  
  temp <- data[,-which(names(data) %in% c(ID,METRIC))]
  
  # find columns which have near zero variance
  cat("\n\nCheck for near zero variance columns:")
  nearZeroVariance <- nearZeroVar(temp)
  if (length(nearZeroVariance) > 0) {
    cat("\nRemoving near zero variant columns -", names(temp)[nearZeroVariance],sep = " ")
    temp <- temp[, -nearZeroVariance]
  }
  
  cat("\n\nCheck for pair-wise collinear columns:")
  for(i in names(temp)) {
    #   i <- "Clearfork"
    if (is.null(temp[[i]]))
      next
    #     print(paste("processing =========",i,sep = " "))
    
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
    if (highest.cor > corrCutOff) { #naCutOff
      iNAs <- length(which(is.na(temp[[i]])))
      jNAs <- length(which(is.na(temp[[alias]])))
      cat("\n",i,"and",alias,"are collinear having",highest.cor,sep = " ")
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
          cat(" ==> dropping",alias,sep = " ")
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
          cat(" ==> dropping",i,sep = " ")
          temp <- temp[,-which(names(temp) %in% c(i))]
        }
      }
    }
  }
  
  cat("\n\nCheck and remove for multicollinear columns:")
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
#     print(paste("default", vif_max, sep = ","))
    
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
  
  names <- vif_func(in_frame=temp,thresh=VIF_THRESHOLD,trace=F)
  cat("\n",setdiff(colnames(temp),names)," are multicollinear!")
  
  x <- cbind(data[[ID]], temp[,which(names(temp) %in% names)])
  colnames(x)[1] <- ID
  x <- cbind(data[[METRIC]], x)
  colnames(x)[1] <- METRIC
  data <- x
  
  temp <- data[,-which(names(data) %in% c(ID))]
  M <- ncol(temp)
  
  if (classify == F) {
    ranks <- information.gain(EUR_o..Mstb. ~ ., temp)
#     ranks <- chi.squared(EUR_o..Mstb. ~ ., temp)
#     ranks <- gain.ratio(EUR_o..Mstb. ~ ., temp)
    if (sum(ranks > 0) == 0)
      ranks <- random.forest.importance(EUR_o..Mstb. ~ ., temp)
  } else {
    ranks <- information.gain(Class ~ ., temp)
  }
  ranks$names <- rownames(ranks)
  # sort in descending order
  ranks <- ranks[order(-ranks$attr_importance),]
  ranks <- ranks[ranks$attr_importance > 0,2]
  cat("\n\nThe ranked order of importance of features:\n")
  cat(ranks,sep="\n")
  ranks[length(ranks)+1] <- METRIC
  ranks[length(ranks)+1] <- ID
  data <- data[,which(names(data) %in% ranks)]
  
  write.table(data, "data-training.csv", sep=",", row.names=FALSE, quote = FALSE)
  
  cat("\nDo random 80%-20% splits, build model on training set and predict on test set 100 times")
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
    seeds <- sample(1:1000, 100, replace=F)
    
    # n iterations 
    for(i in 1:iterations) {
      # set seed by iteration
      set.seed(seeds[i])
      # do random sampling to generate training & test sets
      trainIndex <- sample(nrow(forTraining),floor(nrow(forTraining)*trainSplit))
      train=forTraining[trainIndex,]
      test=forTraining[-trainIndex,]
      #       fit <- train(EUR_o..Mstb. ~ ., data = train, method = "earth")      
      #       fit <- M5P(EUR_o..Mstb. ~. , data = train)
      #       fit <- lm(EUR_o..Mstb. ~ ., data = train, na.action = "na.exclude")
      fit = rlm(EUR_o..Mstb. ~ ., data = train, na.action = "na.exclude") # best
      #       fit <- lmrob(EUR_o..Mstb. ~ ., data = train, na.action = "na.exclude")
      predicted <- predict(fit, newdata = test)
      actual= test$EUR_o..Mstb.
      
      # The RMSE
      RMSE<-RMSE(pred = predicted,obs = actual,na.rm = T)
      RMSLE <- rmsle(actual, predicted)
      COR <- cor(actual, predicted,use="complete")
      
      cat("\n*", RMSE, RMSLE, COR, i, sep = " * ")
      
      best.rmse[i] <- RMSE
      best.rmsle[i] <- RMSLE
      best.cor[i] <- COR
    }
    
    # fit model on the whole data
    #     whole_fit <- lmrob(EUR_o..Mstb. ~ ., data = forTraining, na.action = "na.exclude")
    whole_fit <- rlm(EUR_o..Mstb. ~ ., data = forTraining, na.action = "na.exclude")
    
    # print results of the best iteration
    # summary(best.fit)
    cat("\n\nMin rmse", min(best.rmse,na.rm = T), "Avg rmse", mean(best.rmse,na.rm = T),"Max rmse", max(best.rmse,na.rm = T), sep=" | ")
    cat("\nMin rmsle", min(best.rmsle,na.rm = T), "Avg rmsle", mean(best.rmsle,na.rm = T),"Max rmsle", max(best.rmsle,na.rm = T), sep=" | ")
    cat("\nMin cor", min(best.cor,na.rm = T), "Avg cor", mean(best.cor,na.rm = T),"Max cor", max(best.cor,na.rm = T), sep=" | ")
    result <- list(model = whole_fit, data = data, rmses = best.rmse, rmsles = best.rmsle, ranks = ranks)
  } else {
    fit <- rpart(Class ~ ., data = forTraining, method = "class")
    result <- list(model = fit, data = data)
  }
  if (returnRMSE == T)
    return(RMSE)
  
  return(result)
}