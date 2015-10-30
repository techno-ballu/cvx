print("=============== loading script to build the optimizations on given data using given model ===============")

buildOptimizationsWithXConstraints <- function(toOptimize,model) {
#   toOptimize <- wells.to.optimize
#   model <- smote.model
  
  M <- ncol(toOptimize)
  N <- nrow(toOptimize)
  names <- names(toOptimize)
  actionables <- names(toOptimize)[!(names(toOptimize) %in% nonactionables)]
  actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
  names[length(names)+1] <- "Actual"
  optimums <- data.frame(matrix(vector(), 0, M+1, dimnames=list(c(), names)), stringsAsFactors=T)
  actionables <- actionables[!(actionables %in% c(ID))]
  toOptimize$Wellname <- as.character(toOptimize$Wellname)
  
  for (well in 1:N) {
#       well <- 7
    stub <- toOptimize[well, ]
    print(stub$Wellname)

    # start with given values and replace by mean for actionables
    start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
    
    opt_nons <- start
    for (i in names(opt_nons)) {
      if (i %in% actionables)
        opt_nons[[i]] <- NA
    }
    opt_nons
    
    # create optimization wrapper for CUM
    opt_gm <- function(x, known.x) { #
#       val <- stub[[METRIC]]
      z <- stub[,-which(names(stub) %in% c(ID,METRIC))]

      z[is.na(as.numeric(opt_nons))] <- abs(x[is.na(as.numeric(opt_nons))])
      
#       # copy the data over top of the stub record
#       for (i in names(x)) {
#         if (i %in% actionables) {
#           z[[i]] <- x[[i]]
#         } else {
#           z[[i]] <- known.x[[i]]
#         }
#       }
      # score the data and return the negative
      y <- predict(model,newdata = z)
      
      #If these conditions are violated, the function returns a large positive number
      # which the search procedure will avoid
      #     if(y > val*1.1) return(10^38)
      -y
    }
    
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
    
    opt_start <- start
#     for (i in names(opt_start)) {
#       if (i %in% actionables) {
#         opt_start[[i]] <- mean(toOptimize[[i]],na.rm = T)
#       }
#     }
    opt_start
    
    opt_gm(opt_start,opt_nons)
    
    # optimize
    opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max,known.x=opt_nons) # ,
    #   opt_results <- nmkb(opt_start, opt_gm, lower=opt_min, upper=opt_max,known.x=opt_nons) # result ~ 
    
    # view the optimized inputs & predicted output (CUM)
    opt_results
    
    # test the optima
temp <- stub
action <- stub
temp[which(!names(temp) %in% c(ID,METRIC))] <- opt_results$par
#     for(i in names(opt_results$par)) {
#       opti <- opt_results$par[[i]]
#       ori <- temp[[i]]
#       action[[i]] <- "NA"
#       if (ori != 0)
#         action[[i]] <- ((opti - ori)/ori)*100
#       temp[[i]] <-  opti
#     }
    
t <- temp$CumProd
temp$CumProd <- abs(opt_results$value)
#     action$CumProd <- ((temp$CumProd - stub$CumProd)/stub$CumProd) * 100
action <- ((as.numeric(temp) - as.numeric(stub))/as.numeric(stub))*100
temp$Actual <- t
    
    optimums[well, ] <- temp
    actions[well, ] <- action
  }
  write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
  write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
  
  return(optimums)
}

buildOptimizationsWithYConstraints <- function(toOptimize,model) {
#   toOptimize <- wells.to.optimize
#   model <- data.imputed.model.90$finalModel
  
  M <- ncol(toOptimize)
  N <- nrow(toOptimize)
  names <- names(toOptimize)
  actionables <- names(toOptimize)[!(names(toOptimize) %in% nonactionables)]
  actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
  names[length(names)+1] <- "Actual"
  optimums <- data.frame(matrix(vector(), 0, M+1, dimnames=list(c(), names)), stringsAsFactors=T)
  actionables <- actionables[!(actionables %in% c(ID))]
  toOptimize$Wellname <- as.character(toOptimize$Wellname)
  
  for (well in 1:N) {
#       well <- 5
    stub <- toOptimize[well, ]
    print(stub$Wellname)
    
    # start with given values and replace by mean for actionables
    start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
    target <- stub[[METRIC]]*1.1
    
    opt_nons <- start
    for (i in names(opt_nons)) {
      if (i %in% actionables)
        opt_nons[[i]] <- NA
    }
    opt_nons
    
    # create optimization wrapper for CUM
    opt_gm <- function(x) { #
      z <- stub[,-which(names(stub) %in% c(ID,METRIC))]
      
      z[is.na(as.numeric(opt_nons))] <- abs(x[is.na(as.numeric(opt_nons))])
      
      #       # copy the data over top of the stub record
      #       for (i in names(x)) {
      #         if (i %in% actionables) {
      #           z[[i]] <- x[[i]]
      #         } else {
      #           z[[i]] <- known.x[[i]]
      #         }
      #       }
      # score the data and return the negative
      y <- predict(model,newdata = z)
      
      #If these conditions are violated, the function returns a large positive number
      # which the search procedure will avoid
      #     if(y > val*1.1) return(10^38)
      y
    }
    
    opt_start <- as.numeric(start)
    #     for (i in names(opt_start)) {
    #       if (i %in% actionables) {
    #         opt_start[[i]] <- mean(toOptimize[[i]],na.rm = T)
    #       }
    #     }
    opt_start
    
    # optimize
    #     opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max,known.x=opt_nons) # ,
    #   opt_results <- nmkb(opt_start, opt_gm, lower=opt_min, upper=opt_max,known.x=opt_nons) # result ~ 
    opt_results <- hjk(opt_start, opt_gm,control = list(maximize=TRUE,target=target)) # result ~ 
    #     opt_results <- hjkb(opt_start, opt_gm, control = list(maximize=TRUE,target=target), lower = as.numeric(opt_min), upper = as.numeric(opt_max)) # result ~ 
    
    opt_results$par <- abs(opt_results$par)
    opt_results$par[!is.na(as.numeric(opt_nons))] <- opt_nons[!is.na(as.numeric(opt_nons))]
    opt_results$par <- unlist(opt_results$par)
    opt_results
    
    # test the optima
    temp <- stub
    action <- stub
    temp[which(!names(temp) %in% c(ID,METRIC))] <- opt_results$par
    
    #     for(i in names(opt_results$par)) {
    #       opti <- opt_results$par[[i]]
    #       ori <- temp[[i]]
    #       action[[i]] <- "NA"
    #       if (ori != 0)
    #         action[[i]] <- ((opti - ori)/ori)*100
    #       temp[[i]] <-  opti
    #     }
    t <- temp$CumProd
    temp$CumProd <- abs(opt_results$value)
    #     action$CumProd <- ((temp$CumProd - stub$CumProd)/stub$CumProd) * 100
    action <- ((as.numeric(temp) - as.numeric(stub))/as.numeric(stub))*100
    temp$Actual <- t
    
    optimums[well, ] <- temp
    actions[well, ] <- action
  }
  write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
  write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
  
  return(optimums)
}

buildOptimizationsWithCost <- function(toOptimize,model,financial_names,optimize = "NPV",targetConstrained=F) {
#   toOptimize <- wells.to.optimize
#   model <- optim.model
#   financial_names <- financial_names
#   optimize = "NPV"
#   targetConstrained = T
  
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
#           well <- 21
    stub <- toOptimize[well, ]
#     financial <- finances[well, ]
    print(paste(stub$Wellname, sep = " == "))
    
    # start with given values and replace by mean for actionables
    start <- stub[,-which(names(stub) %in% c(ID,METRIC,financial_names))]
    
    target <- stub$ROR * 0.9
    if (optimize == "NPV")
      target <- stub$NPV_MM * 1.10
    
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
    
    calculateNPV <- function(financial,y_180,duringOptimization=T) {
      
#       financial <- financial
#       y_180 <- stub$CumProd
      
      # interpolate CUM_30, CUM_60, CUM_90 from CUM_180
#       y_180 <- financial[[METRIC]]
      
      # 1. Stages
      financial$Stages <- financial$TREATED_LENGTH/financial$TREATED_LATERAL_PER_STAGE
      # 2. Total Propant Used
      financial[["Total.Propant.Used"]] <- financial$TREATED_LENGTH*financial$PROPANT_PER_FOOT
      # 3. Amount of 100 mesh sand used
      financial[["Amount.100.mesh"]] <- financial$Total.Propant.Used*financial$Fraction.100.MESH
      # 4. Amount of other sands used
      financial[["Amount.Others"]] <- financial$Total.Propant.Used - financial$Amount.100.mesh
      
      financial[[METRIC]] <- y_180
      y_30 <- financial$CUM_30PD_OIL_PROD <- interpolate30(y_180)
      y_60 <- financial$CUM_60PD_OIL_PROD <- interpolate60(y_180)
      y_90 <- financial$CUM_90PD_OIL_PROD <- interpolate90(y_180)
      
      completed_well_cost <- financial[["Completed Well Cost"]] <- calculateWellCompletionCost(financial$Amount.100.mesh,financial$Amount.Others,financial$Stages,financial$Clusters)
      drilling_cost <-  financial[["Drilling Cost"]] <- financial$TREATED_LENGTH * DOLLARS.DRILLING.PER.FOOT
      
      numberOfMonths <- c(1,2,3,6)
      numberOfDays <- c(30,60,90,180)
      sum_monthly_cash_flow <- 0
      sum_operating <- 0
      for(month in numberOfMonths) {
#               month <- 2
        if (month == 1) {
          # first month production
          monthly <- y_30
        } else if (month == 2) {
          # second month production
          monthly <- y_60 - y_30
        } else if (month == 3) {
          # third month production
          monthly <- y_90 - y_60
        } else
          # fourth + fifth + sixth months production
          monthly <- y_180 - y_90
        
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
        sum_monthly_cash_flow <- monthly_cash_flow + sum_monthly_cash_flow
      }
      
      net_present_value <- financial[["Net Present Value"]] <- sum_monthly_cash_flow + (profit/DISCOUNT.RATE) - completed_well_cost
#       print(paste(net_present_value,financial[["Net Present Value"]], sep = " == "))
      npv <- financial[["NPV_MM"]] <- net_present_value / 1000000

      eur <- financial[["EUR"]] <- y_180/0.47
      ror <- financial[["ROR"]] <- (((y_180 * DOLLARS.OIL.PER.BBL) - ((completed_well_cost + drilling_cost) - sum_operating))/((completed_well_cost + drilling_cost) - sum_operating))*100
      # irr <- financial[[""]]
      # payback
#       print(paste(npv,financial$NPV_MM, sep = " == "))
      
      if (duringOptimization == TRUE) {
        if (optimize == "NPV")
          return(npv)
        return(ror)
      }
      return(financial)
    }
    
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
      
      z$Stages <- z$TREATED_LENGTH/z$TREATED_LATERAL_PER_STAGE
      z[["Total.Propant.Used"]] <- z$TREATED_LENGTH*z$PROPANT_PER_FOOT
      z[["Amount.100.mesh"]] <- z$Total.Propant.Used*z$Fraction.100.MESH
      z[["Amount.Others"]] <- z$Total.Propant.Used - z$Amount.100.mesh
      optimum <- calculateNPV(z,y_180)
      optimum
    }
    
    opt_start <- as.numeric(start)
    opt_start
    
    # optimize
    #     opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max,known.x=opt_nons) # ,
    #   opt_results <- nmkb(opt_start, opt_gm, lower=opt_min, upper=opt_max,known.x=opt_nons) # result ~ 
#     opt_results <- hjk(opt_start, opt_gm,control = list(maximize=TRUE,target=target)) # result ~ 
    
    if (targetConstrained == F) {
      opt_results <- hjkb(opt_start, opt_gm, control = list(maximize=TRUE), lower = as.numeric(opt_min), upper = as.numeric(opt_max)) # ,target=target
    } else {
      opt_results <- hjkb(opt_start, opt_gm, control = list(maximize=TRUE,target=target), lower = as.numeric(opt_min), upper = as.numeric(opt_max)) # 
    }
    
    opt_results$par <- abs(opt_results$par)
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
    temp <- calculateNPV(temp,y_180,duringOptimization = F)
    temp

#     stub <- merge(stub,financial[, c(ID, setdiff(colnames(financial),colnames(stub)))],by = ID)
    action <- stub
    
#     t <- temp$CumProd
#     temp$CumProd <- y_180
#     temp$Actual <- t

#     temp <- merge(temp,fin[, c(ID, setdiff(colnames(fin),colnames(temp)))],by = ID)
    action <- unlist(((as.numeric(temp) - as.numeric(stub))/as.numeric(stub))*100)
    action[1] <- stub$Wellname
    # setdiff(colnames(finances),colnames(newwhole$data))

    optimums[well, ] <- temp
    actions[well, ] <- action
  }
  print(paste("Additional BBLs of oil produced", sum_bbls, sep = " = "))
  write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
  write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
  
  return(optimums)
}

buildBestOptimizationsWithCost <- function(toOptimize,model,financial_names,optimize = "NPV") {
#     toOptimize <- wells.to.optimize
#     model <- optim.model
#     financial_names <- financial_names
#     optimize = "NPV"
  
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
    
    target <- stub$ROR * 0.9
    if (optimize == "NPV")
      target <- stub$NPV_MM * 1.10
    
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
    
    calculateNPV <- function(financial,y_180,duringOptimization=T) {
      
      #       financial <- financial
      #       y_180 <- stub$CumProd
      
      # interpolate CUM_30, CUM_60, CUM_90 from CUM_180
      #       y_180 <- financial[[METRIC]]
      
      # 1. Stages
      financial$Stages <- financial$TREATED_LENGTH/financial$TREATED_LATERAL_PER_STAGE
      # 2. Total Propant Used
      financial[["Total.Propant.Used"]] <- financial$TREATED_LENGTH*financial$PROPANT_PER_FOOT
      # 3. Amount of 100 mesh sand used
      financial[["Amount.100.mesh"]] <- financial$Total.Propant.Used*financial$Fraction.100.MESH
      # 4. Amount of other sands used
      financial[["Amount.Others"]] <- financial$Total.Propant.Used - financial$Amount.100.mesh
      
      financial[[METRIC]] <- y_180
      y_30 <- financial$CUM_30PD_OIL_PROD <- interpolate30(y_180)
      y_60 <- financial$CUM_60PD_OIL_PROD <- interpolate60(y_180)
      y_90 <- financial$CUM_90PD_OIL_PROD <- interpolate90(y_180)
      
      completed_well_cost <- financial[["Completed Well Cost"]] <- calculateWellCompletionCost(financial$Amount.100.mesh,financial$Amount.Others,financial$Stages,financial$Clusters)
      drilling_cost <-  financial[["Drilling Cost"]] <- financial$TREATED_LENGTH * DOLLARS.DRILLING.PER.FOOT
      
      numberOfMonths <- c(1,2,3,6)
      numberOfDays <- c(30,60,90,180)
      sum_monthly_cash_flow <- 0
      sum_operating <- 0
      for(month in numberOfMonths) {
        #               month <- 2
        if (month == 1) {
          # first month production
          monthly <- y_30
        } else if (month == 2) {
          # second month production
          monthly <- y_60 - y_30
        } else if (month == 3) {
          # third month production
          monthly <- y_90 - y_60
        } else
          # fourth + fifth + sixth months production
          monthly <- y_180 - y_90
        
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
        sum_monthly_cash_flow <- monthly_cash_flow + sum_monthly_cash_flow
      }
      
      net_present_value <- financial[["Net Present Value"]] <- sum_monthly_cash_flow + (profit/DISCOUNT.RATE) - completed_well_cost
      #       print(paste(net_present_value,financial[["Net Present Value"]], sep = " == "))
      npv <- financial[["NPV_MM"]] <- net_present_value / 1000000
      
      eur <- financial[["EUR"]] <- y_180/0.47
      ror <- financial[["ROR"]] <- (((y_180 * DOLLARS.OIL.PER.BBL) - ((completed_well_cost + drilling_cost) - sum_operating))/((completed_well_cost + drilling_cost) - sum_operating))*100
      # irr <- financial[[""]]
      # payback
      #       print(paste(npv,financial$NPV_MM, sep = " == "))
      
      if (duringOptimization == TRUE) {
        if (optimize == "NPV")
          return(npv)
        return(ror)
      }
      return(financial)
    }
    
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
      
      z$Stages <- z$TREATED_LENGTH/z$TREATED_LATERAL_PER_STAGE
      z[["Total.Propant.Used"]] <- z$TREATED_LENGTH*z$PROPANT_PER_FOOT
      z[["Amount.100.mesh"]] <- z$Total.Propant.Used*z$Fraction.100.MESH
      z[["Amount.Others"]] <- z$Total.Propant.Used - z$Amount.100.mesh
      optimum <- calculateNPV(z,y_180)
      optimum
    }
    
    opt_start <- as.numeric(start)
    opt_start
    
    # optimize
    #     opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max,known.x=opt_nons) # ,
    #   opt_results <- nmkb(opt_start, opt_gm, lower=opt_min, upper=opt_max,known.x=opt_nons) # result ~ 
    #     opt_results <- hjk(opt_start, opt_gm,control = list(maximize=TRUE,target=target)) # result ~ 
    ncol <- length(opt_start)+1
    optims <- matrix(0,nrow = 3,ncol = ncol,byrow = T)
   
    # this chooses the best optimization in 3 iterations!
    for(optimIter in 1:3) {
      opt_results <- hjkb(opt_start, opt_gm, control = list(maximize=TRUE), lower = as.numeric(opt_min), upper = as.numeric(opt_max)) # ,target=target  
      optims[optimIter,2:ncol] <- opt_results$par
      optims[optimIter,1] <- opt_results$value
    }
    max <- which(optims==max(optims[,1]), arr.ind=TRUE)
    row <- max[1]
    
    opt_results$par <- abs(optims[2,2:ncol])
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
    temp <- calculateNPV(temp,y_180,duringOptimization = F)
    temp
    
    #     stub <- merge(stub,financial[, c(ID, setdiff(colnames(financial),colnames(stub)))],by = ID)
    action <- stub
    
    #     t <- temp$CumProd
    #     temp$CumProd <- y_180
    #     temp$Actual <- t
    
    #     temp <- merge(temp,fin[, c(ID, setdiff(colnames(fin),colnames(temp)))],by = ID)
    action <- unlist(((as.numeric(temp) - as.numeric(stub))/as.numeric(stub))*100)
    action[1] <- stub$Wellname
    # setdiff(colnames(finances),colnames(newwhole$data))
    
    optimums[well, ] <- temp
    actions[well, ] <- action
  }
  print(paste("Additional BBLs of oil produced", sum_bbls, sep = " = "))
  write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
  write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
  
  return(optimums)
}