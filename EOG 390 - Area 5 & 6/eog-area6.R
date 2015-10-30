# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(gdata)
require(fmsb)
library(foreach)
library(Metrics)
library(e1071)
library(plyr)
library(rJava)
library(rJava)
library(dfoptim)
library(googleVis)
library(DMwR)
library(caret)
library(FSelector)
library(robCompositions)

library(missForest)
library(Hmisc)
library(MASS)

# set the work directory
setwd("~/EOG-data/AllWells/Final-EOG/Deliverables")

# the AREA for which to predict & prescribe
area <- "AREA 6"

# this is the main optimizer!
buildBestOptimizationsWithCost <- function(toOptimize,model,financial_names,optimize = "NPV") {
#       toOptimize <- wells.to.optimize
#       model <- optim.model
#       financial_names <- financial_names
#       optimize = "NPV"
  
  M <- ncol(toOptimize)
  N <- nrow(toOptimize)
  names <- names(toOptimize)
  actionables <- names(toOptimize)[!(names(toOptimize) %in% c(nonactionables,financial_names))]
  #   names[length(names)+1] <- "Actual"
  #   optimums <- data.frame(matrix(vector(), 0, M+1, dimnames=list(c(), names)), stringsAsFactors=T)
  # actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
  #   merged <- merge(toOptimize,  finances[,-which(names(finances) %in% fin_vars)], by = ID)
  #   merged <- merge(toOptimize, finances[, c(ID, setdiff(colnames(finances),colnames(toOptimize)))], by=ID) 
  optimums <- toOptimize[which(is.na(toOptimize$Wellname)), ]
  actions <- toOptimize[which(is.na(toOptimize$Wellname)), ]
  actionables <- actionables[!(actionables %in% c(ID))]
  toOptimize$Wellname <- as.character(toOptimize$Wellname)
  sum_bbls <- 0  
  
  for (well in 1:N) {
    #               well <- 21
    stub <- toOptimize[well, ]
    #     financial <- finances[well, ]
    print(paste(stub$Wellname, sep = " == "))
    
    # start with given values and replace by mean for actionables
    start <- stub[,-which(names(stub) %in% c(ID,METRIC,financial_names))]
    
    target <- stub$ROR * 0.5
    if (optimize == "NPV")
      target <- stub$NPV_MM * 1.5
    
    opt_nons <- start
    for (i in names(opt_nons)) {
      if (i %in% actionables)
        opt_nons[[i]] <- NA
    }
    opt_nons
    
    # lower
    opt_min <- start
    for (i in names(opt_min)) {
      opt_min[[i]] <- min(toOptimize[[i]],na.rm = T)
    }
    opt_min
    
    # upper
    opt_max <- start
    for (i in names(opt_max)) {
      opt_max[[i]] <- max(toOptimize[[i]],na.rm = T)
    }
    opt_max
    
    # create optimization wrapper for CUM
    opt_gm <- function(x) { #
      #       x <- opt_start
      #       opt_start <- as.numeric(opt_max) + 1
      
      deltaMin <- x - as.numeric(opt_min)
      deltaMax <- as.numeric(opt_max) - x
      if (sum(deltaMin < 0) > 0 | sum(deltaMax < 0) > 0) {
        #         print("x exceeds set limits!")
        if (sum(deltaMin < 0) > 0)
          x[deltaMin < 0] <- opt_min[deltaMin < 0]
        if (sum(deltaMax < 0) > 0)
          x[deltaMax < 0] <- opt_max[deltaMax < 0]
        x <- unlist(x)
      }
      
      z <- stub[,-which(names(stub) %in% c(ID,METRIC,financial_names))]
      z[is.na(as.numeric(opt_nons))] <- abs(x[is.na(as.numeric(opt_nons))])
      
      # calculate CUM_180
      y_180 <- predict(model,newdata = z)
      z$CumProd <- y_180
      #       z$Stages <- round(z$TREATED_LENGTH/z$TREATED_LATERAL_PER_STAGE)
      #       z[["Total.Propant.Used"]] <- z$TREATED_LENGTH*z$PROPANT_PER_FOOT
      #       z[["Amount.100.mesh"]] <- z$Total.Propant.Used*z$Fraction.100.MESH
      #       z[["Amount.Others"]] <- z$Total.Propant.Used - z$Amount.100.mesh
      #       z$Clusters <- round(z$Clusters)
      #       optimum <- calculateNPV(z,y_180)
      #       optimum <- calculateFinancial(z,y_180)
      optimum <- calculateFinancial(z,NULL,optimize,duringOptimization=T)
      optimum
    }
    
    opt_start <- as.numeric(start)
    opt_start
    
    if (optimize == "NPV") {
      # optimize
      ncol <- length(opt_start)+1
      optims <- matrix(0,nrow = 3,ncol = ncol,byrow = T)
      # this chooses the best optimization in 3 iterations!
      for(optimIter in 1:2) {
        opt_results <- hjkb(opt_start, opt_gm, control = list(maximize=TRUE,info=F,tol=0.01), lower = as.numeric(opt_min), upper = as.numeric(opt_max)) # 
        optims[optimIter,2:ncol] <- opt_results$par
        optims[optimIter,1] <- opt_results$value
      }
      max <- which(optims==max(optims[,1]), arr.ind=TRUE)
      row <- max[1]
      opt_results$par <- abs(optims[row,2:ncol])
    } else {
      opt_results <- hjkb(opt_start, opt_gm, control = list(maximize=TRUE,info=F,tol=0.01), lower = as.numeric(opt_min), upper = as.numeric(opt_max)) # 
      opt_results$par <- abs(opt_results$par)
    }
    opt_results$par[!is.na(as.numeric(opt_nons))] <- opt_nons[!is.na(as.numeric(opt_nons))]
    opt_results$par <- unlist(opt_results$par)
    opt_results
    
    # test the optima
    temp <- stub
    recipe <- opt_results$par
    
    # check if the recipe exceeds the min-max
    deltaMin <- recipe - opt_min
    deltaMax <- opt_max - recipe
    if (sum(deltaMin < 0) > 0 | sum(deltaMax < 0) > 0) {
      #       print("This recipe exceeds set limits!")
      if (sum(deltaMin < 0) > 0)
        recipe[deltaMin < 0] <- opt_min[deltaMin < 0]
      if (sum(deltaMax < 0) > 0)
        recipe[deltaMax < 0] <- opt_max[deltaMax < 0]
    }
    
    temp[which(!names(temp) %in% c(ID,METRIC,financial_names))] <- recipe
    recipe <- temp[which(!names(temp) %in% c(ID,METRIC,financial_names))]
    y_180 <- predict(model,newdata = recipe)
    y_180
    addnBBLs <- y_180 - stub$CumProd
    sum_bbls <- sum_bbls + addnBBLs
    
    #     temp <- temp[which(!names(temp) %in% c(financial_names))]
    #     temp <- calculateNPV(temp,y_180,duringOptimization = F)
    temp$CumProd <- y_180
    #     temp <- calculateFinancial(temp,NULL,duringOptimization = F)
    #     temp
    
    #     stub <- merge(stub,financial[, c(ID, setdiff(colnames(financial),colnames(stub)))],by = ID)
    #     action <- stub
    
    #     t <- temp$CumProd
    #     temp$CumProd <- y_180
    #     temp$Actual <- t
    
    #     temp <- merge(temp,fin[, c(ID, setdiff(colnames(fin),colnames(temp)))],by = ID)
    #     action <- unlist(((as.numeric(temp) - as.numeric(stub))/as.numeric(stub))*100)
    #     action[1] <- stub$Wellname
    # setdiff(colnames(finances),colnames(newwhole$data))
    
    optimums[well, ] <- temp
    #     actions[well, ] <- action
  }
  
  optimums <- calculateFinancial(optimums[which(!names(optimums) %in% c(financial_names))],"prescribed",NULL,duringOptimization=F,extra = T)
  print(paste("Additional BBLs of oil produced", sum_bbls, sep = " = "))
  write.table(optimums, paste("prescriptions-",area,"-",optimize,".csv",sep = ""),sep=",",row.names=FALSE, quote = FALSE)
  
  diff <- merge(wells.to.optimize,optimums,by=ID,suffixes = c(".original",".prescribed"))
  actions <- ((diff[,grepl("*\\.prescribed$",names(diff))] - diff[,grepl("*\\.original$",names(diff))])/diff[,grepl("*\\.original$",names(diff))])*100
  actions <- cbind(diff[,1,drop=FALSE],actions)
  colnames(actions) <- colnames(wells.to.optimize)
#   actions <- ((as.numeric(optimums) - as.numeric(wells.to.optimize))/as.numeric(wells.to.optimize))*100
  write.table(actions, paste("actions-",area,"-",optimize,".csv",sep = ""),sep=",",row.names=FALSE, quote = FALSE)
  
  return(optimums)
}

# builds the prediction model
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
  ranks <- c(ranks,important,METRIC,ID)
  print(ranks)
  data <- data[,which(names(data) %in% ranks)]
  str(data)
  
  if (!is.null(area))
    write.table(data, paste("data-training-",area,".csv",sep = ""), sep=",", row.names=FALSE, quote = FALSE)
  
  forTraining <- data[,-which(names(data) %in% c(ID))]
  N <- nrow(forTraining)
  
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
    train=forTraining[trainIndex,]
    test=forTraining[-trainIndex,]
    
    fit = rlm(CumProd ~ ., data = train, na.action = "na.exclude")
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

# calculates the completed well costs
calculateWellCompletionCost <- function(amount.100.mesh,amount.others,stages,clusters) {
  # Completion well costs
  cost.100.mesh <- amount.100.mesh * DOLLARS.100.MESH.PER.POUND 
  cost.30by70.mesh <- amount.others * (0/3) * DOLLARS.30by70.MESH.PER.POUND 
  cost.20by40.mesh <- amount.others * (1/3) * DOLLARS.20by40.MESH.PER.POUND 
  cost.40by70.mesh <- amount.others * (2/3) * DOLLARS.40by70.MESH.PER.POUND 
  total.propant <- cost.100.mesh + cost.20by40.mesh + cost.30by70.mesh + cost.40by70.mesh 
  equipment <- (stages / STAGES.COMPLETED.PER.DAY) * DOLLARS.EQUIPMENT.PER.DAY 
  chemicals <- stages * DOLLARS.CHEMICALS.PER.STAGE 
  fluids <- stages * GALLONS.FLUIDS.PER.STAGE * DOLLARS.FLUID.PER.GALLON 
  fuel <- DOLLARS.FUEL 
  
  simulation <- total.propant + equipment + chemicals + fluids + fuel 
  completions <- simulation * 0.75 
  perforations <- clusters * stages * DOLLARS.PERFORATIONS.PER.CLUSTER 
  facilities <- DOLLARS.FACILITIES 
  return(simulation + completions + perforations + facilities)
}

# calculates the financial variables for a given data
calculateFinancial <- function(DATA,file,optimize="NPV",duringOptimization=T,extra=F) {
  
  #   DATA <- original
  
  # copy a backup for financial
  financial <- DATA #[,which(names(DATA) %in% fin_vars)]
  financial$CumProd <- financial$CumProd
  
  # 1. Stages
  financial$Stages <- round(financial$TREATED_LENGTH/financial$TREATED_LATERAL_PER_STAGE)
  # 2. Total Propant Used
  financial[["Total.Propant.Used"]] <- financial$TREATED_LENGTH*financial$PROPANT_PER_FOOT
  # 3. Amount of 100 mesh sand used
  financial[["Amount.100.mesh"]] <- financial$Total.Propant.Used*financial$Fraction.100.MESH
  # 4. Amount of other sands used
  financial[["Amount.Others"]] <- financial$Total.Propant.Used - financial$Amount.100.mesh
  financial$Clusters <- round(financial$Clusters)
  
  y_30 <- financial$CUM_30PD_OIL_PROD <- interpolate30(financial$CumProd)
  y_60 <- financial$CUM_60PD_OIL_PROD <- interpolate60(financial$CumProd)
  y_90 <- financial$CUM_90PD_OIL_PROD <- interpolate90(financial$CumProd)
  
  if (!is.null(file) && extra == F) {
    write.table(financial, paste("pre-financials-",file,".csv",sep = ""),sep=",",row.names=FALSE, quote = FALSE)
  } else if (extra == T) {
    write.table(financial, paste("pre-financials-",file,"-",area,"-",optimize,".csv",sep = ""),sep=",",row.names=FALSE, quote = FALSE)
  }
  
  completed_well_cost <- financial[["Completed Well Cost"]] <- calculateWellCompletionCost(financial$Amount.100.mesh,financial$Amount.Others,financial$Stages,financial$Clusters)
  drilling_cost <-  financial[["Drilling Cost"]] <- financial$TREATED_LENGTH * DOLLARS.DRILLING.PER.FOOT
  
  numberOfMonths <- c(1,2,3,6)
  numberOfDays <- c(30,60,90,180)
  sum_operating <- 0
  for(month in numberOfMonths) {
    #       month <- 1
    if (month == 1) {
      # first month production
      monthly <- financial$CUM_30PD_OIL_PROD
    } else if (month == 2) {
      # second month production
      monthly <- financial$CUM_60PD_OIL_PROD - financial$CUM_30PD_OIL_PROD
    } else if (month == 3) {
      # third month production
      monthly <- financial$CUM_90PD_OIL_PROD - financial$CUM_60PD_OIL_PROD
    } else
      # fourth+fifth+sixth months production
      monthly <- financial$CumProd - financial$CUM_90PD_OIL_PROD
    
    namer <- function(name) {
      return(paste(name,month,sep = ""))
    }
    
    # Monthly Production
    financial[[namer("Monthly")]] <- monthly
    
    # Revenues
    gross <- financial[[namer("Gross Revenue")]] <- monthly * DOLLARS.OIL.PER.BBL
    net <- financial[[namer("Net Revenue")]] <- gross * NET.REVENUE.INTEREST.PERCENT
    
    # Operating well costs
    lease <- financial[[namer("Lease Operating Expenses")]] <- monthly * LEASE.OPERATING.COST.PER.BBL
    production_taxes <- financial[[namer("Production Taxes")]] <- monthly * PRODUCTION.TAX.PER.BBL
    ga <- financial[[namer("G&A")]] <- monthly * GA.PER.BBL
    gathering_trans_n_others <- financial[[namer("Gathering & Transport")]] <- monthly * GATHERING.TRANS.OTHER.PER.BBL
    total_operating <- financial[[namer("Total Operating Costs")]] <- lease + production_taxes + ga + gathering_trans_n_others
    sum_operating <- sum_operating + total_operating
    
    profit <- financial[[namer("Monthly Operating Profit")]] <- net - total_operating
    
    if (month == 1) {
      # first month
      cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- profit - completed_well_cost
    } else if (month == 2) {
      # second month
      cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- financial[["Cumulative Cash Flow1"]] + profit
    } else if (month == 3) {
      # third month
      cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- financial[["Cumulative Cash Flow2"]] + profit
    } else
      # fourth + fifth + sixth months
      cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- financial[["Cumulative Cash Flow3"]] + profit
    
    monthly_cash_flow <- financial[[namer("Monthly Cash Flows")]] <- profit/(1 + (DISCOUNT.RATE/12)) ^ month
  }
  #   str(financial)
  
  net_present_value <- financial[["Net Present Value"]] <- financial[["Monthly Cash Flows1"]] + financial[["Monthly Cash Flows2"]] + financial[["Monthly Cash Flows3"]]  + financial[["Monthly Cash Flows6"]] + (financial[["Monthly Operating Profit6"]]/DISCOUNT.RATE) - completed_well_cost
  npv <- financial[["NPV_MM"]] <- net_present_value / 1000000
  eur <- financial[["EUR"]] <- financial$CumProd/0.47
  ror <- financial[["ROR"]] <- (((financial$CumProd * DOLLARS.OIL.PER.BBL) - ((completed_well_cost + drilling_cost) - sum_operating))/((completed_well_cost + drilling_cost) - sum_operating))*100
  
  if (duringOptimization == TRUE) {
    if (optimize == "NPV")
      return(npv)
    return(ror)
  } else if (extra == F) {
    write.table(financial, paste("post-financials-",file,".csv",sep = ""), sep=",", row.names=FALSE, quote = FALSE)
  }
  
  return(financial)
}

# Some financial constants,rates etc..
DOLLARS.OIL.PER.BBL <- 90
NET.REVENUE.INTEREST.PERCENT <- 0.8
LEASE.OPERATING.COST.PER.BBL <- 9
PRODUCTION.TAX.PER.BBL <- 7
GA.PER.BBL <- 8
GATHERING.TRANS.OTHER.PER.BBL <- 1
DOLLARS.100.MESH.PER.POUND <- 0.06
DOLLARS.30by70.MESH.PER.POUND <- 0.07
DOLLARS.20by40.MESH.PER.POUND <- 0.42
DOLLARS.40by70.MESH.PER.POUND <- 0.08
DOLLARS.EQUIPMENT.PER.DAY <- 112000
STAGES.COMPLETED.PER.DAY <- 3
DOLLARS.CHEMICALS.PER.STAGE <- 3000
GALLONS.FLUIDS.PER.STAGE <- 275000
DOLLARS.FLUID.PER.GALLON <- 0.083
DOLLARS.FUEL <- 218750
DOLLARS.PERFORATIONS.PER.CLUSTER <- 1329
DOLLARS.FACILITIES <- 330000
DISCOUNT.RATE <- 0.1
DOLLARS.DRILLING.PER.FOOT <- 513

# load well wise Completions data
DATA <- read.csv("../eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))

# add some special columns
# 1. Stages
DATA$Stages <- round(DATA$TREATED_LENGTH/DATA$TREATED_LATERAL_PER_STAGE)
# DATA$Stages <- DATA$TREATED_LENGTH/DATA$TREATED_LATERAL_PER_STAGE
# 2. Clusters
# DATA$Clusters <- DATA$TREATED_LATERAL_PER_STAGE/DATA$AVG_CLUSTER_SPACING
DATA$Clusters <- round(DATA$TREATED_LATERAL_PER_STAGE/DATA$AVG_CLUSTER_SPACING)
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
# DATA5 <- subset(DATA, TEAM == "AREA 5")
# DATA6 <- subset(DATA, TEAM == "AREA 6")

DATA <- subset(DATA, TEAM == area)

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
if (area == "AREA 5")
  corrCutOff <- 0.91
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
data <- temp

# let's set aside some "unseen" stratified data from the model!
set.seed(786)
index <- createDataPartition(data$CumProd,p = trainSplit)[[1]]
wholetraining <- data
testing=data[-index,]
data=data[index,]

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

original <- cbind(newwhole$data,wholetraining[,which(names(wholetraining) %in% USELESS)])
finances_original <- calculateFinancial(original,"original",NULL,duringOptimization=F)

predictions <- newwhole$testing[,which(names(newwhole$testing) %in% names(newwhole$data))] 
finances_predicted <- calculateFinancial(predictions,"predicted",NULL,duringOptimization=F) 

# source("buildOptimizations.R")
optim.model <- newwhole$model
finances <- finances_predicted
wells.to.optimize <- merge(newwhole$data, finances[, c(ID, setdiff(colnames(finances),colnames(newwhole$data)))], by=ID)
financial_names <- setdiff(colnames(finances),colnames(newwhole$data))
optimumsNPV <- buildBestOptimizationsWithCost(wells.to.optimize,optim.model,financial_names,"NPV") #
optimumsROR <- buildBestOptimizationsWithCost(wells.to.optimize,optim.model,financial_names,"ROR") #

# # save the model to disk
# rJava::.jcache(best.fit$classifier)
save(optim.model, file = paste("eog_final_model_",area,".rda",sep = ""))

# compare finances_original, finances_predicted, optimumsNPV & optimumsROR to get interpretations
interpretation_columns <- c(ID,"CumProd","NPV_MM","EUR","ROR")
interpretations1 <- merge(finances_original[,interpretation_columns],finances_predicted[,interpretation_columns],by=ID,suffixes = c(".original",".predicted"))
# interprations <- cbind(finances_original[,1,drop=FALSE],interprations)
interpretations2 <- merge(optimumsNPV[,interpretation_columns],optimumsROR[,interpretation_columns],by=ID,suffixes = c(".prescribed.npv",".prescribed.ror"))
interpretations <- merge(interpretations1, interpretations2, by = ID)
write.table(interpretations, paste("interpretations-",area,".csv",sep = ""),sep=",",row.names=FALSE, quote = FALSE)

# calculateNPV <- function(financial,y_180,duringOptimization=T) {
#   
#   #       financial <- financial
#   #       y_180 <- stub$CumProd
#   
#   # interpolate CUM_30, CUM_60, CUM_90 from CUM_180
#   #       y_180 <- financial[[METRIC]]
#   
#   # 1. Stages
#   financial$Stages <- round(financial$TREATED_LENGTH/financial$TREATED_LATERAL_PER_STAGE)
#   # 2. Total Propant Used
#   financial[["Total.Propant.Used"]] <- financial$TREATED_LENGTH*financial$PROPANT_PER_FOOT
#   # 3. Amount of 100 mesh sand used
#   financial[["Amount.100.mesh"]] <- financial$Total.Propant.Used*financial$Fraction.100.MESH
#   # 4. Amount of other sands used
#   financial[["Amount.Others"]] <- financial$Total.Propant.Used - financial$Amount.100.mesh
#   financial$Clusters <- round(financial$Clusters)
#   
#   financial[[METRIC]] <- y_180
#   y_30 <- financial$CUM_30PD_OIL_PROD <- interpolate30(y_180)
#   y_60 <- financial$CUM_60PD_OIL_PROD <- interpolate60(y_180)
#   y_90 <- financial$CUM_90PD_OIL_PROD <- interpolate90(y_180)
#   
#   completed_well_cost <- financial[["Completed Well Cost"]] <- calculateWellCompletionCost(financial$Amount.100.mesh,financial$Amount.Others,financial$Stages,financial$Clusters)
#   drilling_cost <-  financial[["Drilling Cost"]] <- financial$TREATED_LENGTH * DOLLARS.DRILLING.PER.FOOT
#   
#   numberOfMonths <- c(1,2,3,6)
#   numberOfDays <- c(30,60,90,180)
#   sum_monthly_cash_flow <- 0
#   sum_operating <- 0
#   for(month in numberOfMonths) {
#     #               month <- 2
#     if (month == 1) {
#       # first month production
#       monthly <- y_30
#     } else if (month == 2) {
#       # second month production
#       monthly <- y_60 - y_30
#     } else if (month == 3) {
#       # third month production
#       monthly <- y_90 - y_60
#     } else
#       # fourth + fifth + sixth months production
#       monthly <- y_180 - y_90
#     
#     namer <- function(name) {
#       return(paste(name,month,sep = ""))
#     }
#     
#     # Monthly Production
#     financial[[namer("Monthly")]] <- monthly
#     
#     # Revenues
#     gross <- financial[[namer("Gross Revenue")]] <- monthly * DOLLARS.OIL.PER.BBL
#     net <- financial[[namer("Net Revenue")]] <- gross * NET.REVENUE.INTEREST.PERCENT
#     
#     # Operating well costs
#     lease <- financial[[namer("Lease Operating Expenses")]] <- monthly * LEASE.OPERATING.COST.PER.BBL
#     production_taxes <- financial[[namer("Production Taxes")]] <- monthly * PRODUCTION.TAX.PER.BBL
#     ga <- financial[[namer("G&A")]] <- monthly * GA.PER.BBL
#     gathering_trans_n_others <- financial[[namer("Gathering & Transport")]] <- monthly * GATHERING.TRANS.OTHER.PER.BBL
#     total_operating <- financial[[namer("Total Operating Costs")]] <- lease + production_taxes + ga + gathering_trans_n_others
#     sum_operating <- sum_operating + total_operating
#     
#     profit <- financial[[namer("Monthly Operating Profit")]] <- net - total_operating
#     
#     if (month == 1) {
#       # first month
#       cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- profit - completed_well_cost
#     } else if (month == 2) {
#       # second month
#       cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- financial[["Cumulative Cash Flow1"]] + profit
#     } else if (month == 3) {
#       # third month
#       cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- financial[["Cumulative Cash Flow2"]] + profit
#     } else
#       # fourth + fifth + sixth months
#       cumulative_cash_flow <- financial[[namer("Cumulative Cash Flow")]] <- financial[["Cumulative Cash Flow3"]] + profit
#     
#     monthly_cash_flow <- financial[[namer("Monthly Cash Flows")]] <- profit/(1 + (DISCOUNT.RATE/12)) ^ month
#     sum_monthly_cash_flow <- monthly_cash_flow + sum_monthly_cash_flow
#   }
#   
#   net_present_value <- financial[["Net Present Value"]] <- sum_monthly_cash_flow + (profit/DISCOUNT.RATE) - completed_well_cost
#   #       print(paste(net_present_value,financial[["Net Present Value"]], sep = " == "))
#   npv <- financial[["NPV_MM"]] <- net_present_value / 1000000
#   
#   eur <- financial[["EUR"]] <- y_180/0.47
#   ror <- financial[["ROR"]] <- (((y_180 * DOLLARS.OIL.PER.BBL) - ((completed_well_cost + drilling_cost) - sum_operating))/((completed_well_cost + drilling_cost) - sum_operating))*100
#   
#   if (duringOptimization == TRUE) {
#     if (optimize == "NPV")
#       return(npv)
#     return(ror)
#   }
#   return(financial)
# }
