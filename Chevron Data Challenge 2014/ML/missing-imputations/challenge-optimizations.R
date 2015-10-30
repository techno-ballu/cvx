print("=============== loading script to build the optimizations on given data using given model ===============")

# challengeOptimizations1 <- function(toOptimize,model,startWells) {
#   M <- ncol(toOptimize)
#   N <- nrow(toOptimize)
#   names <- names(toOptimize)
#   actionables <- names(toOptimize)[!(names(toOptimize) %in% nonactionables)]
#   actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
#   optimums <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
#   actionables <- actionables[!(actionables %in% c(ID))]
#   toOptimize[[ID]] <- as.character(toOptimize[[ID]])
#   
#   for (well in 1:N) {
#     #   well <- 7
#     stub <- toOptimize[well, ]
#     print(stub$WellID)
#     
#     # this is to get min-max of Steen Scruggs & Kerner Carson wells
#     # name=unlist(strsplit(gsub("-", " ", stub$Wellname),split=" ",fixed=TRUE))[1]
#     # wellsData <- toOptimize[grepl(name,toOptimize$Wellname),]
#     
#     # create optimization wrapper for CUM
#     opt_gm <- function(x, known.x) { #
#       val <- stub[[METRIC]]
#       z <- stub[,-which(names(stub) %in% c(ID,METRIC))]
#       
#       # copy the data over top of the stub record
#       for (i in names(x)) {
#         if (i %in% actionables) {
#           z[[i]] <- x[[i]]
#         } else {
#           z[[i]] <- known.x[[i]]
#         }
#       }
#       # score the data and return the negative
#       y <- predict(model,newdata = z)
#       
#       #If these conditions are violated, the function returns a large positive number
#       # which the search procedure will avoid
#       #     if(y > val*1.1) return(10^38)
#       -y
#     }
#     
#     # start with given values and replace by mean for actionables
#     start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
#     
#     # lower
#     opt_min <- start
#     for (i in names(opt_min)) {
#       opt_min[[i]] <- min(toOptimize[[i]],na.rm = T)
#     }
#     opt_min
#     
#     # upper
#     opt_max <- start
#     for (i in names(opt_max)) {
#       opt_max[[i]] <- max(toOptimize[[i]],na.rm = T)
#     }
#     opt_max
#     
# #     opt_start <- startWell[,-which(names(stub) %in% c(ID,METRIC))]
#     opt_start <- start
#     for (i in names(opt_start)) {
#       if (i %in% actionables) {
#         opt_start[[i]] <- mean(toOptimize[[i]],na.rm = T)
#       }
#     }
#     opt_start
#     
#     opt_nons <- start
#     for (i in names(opt_nons)) {
#       if (i %in% actionables)
#         opt_nons[[i]] <- NA
#     }
#     opt_nons
#     
#     opt_gm(opt_start,opt_nons)
#     
#     # optimize
#     opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max,known.x=opt_nons) # ,
#     #   opt_results <- nmkb(opt_start, opt_gm, lower=opt_min, upper=opt_max,known.x=opt_nons) # result ~ 
#     
#     # view the optimized inputs & predicted output (CUM)
#     opt_results
#     
#     # test the optima
#     temp <- stub
#     action <- stub
#     for(i in names(opt_results$par)) {
#       opti <- opt_results$par[[i]]
#       ori <- temp[[i]]
#       action[[i]] <- "NA"
#       if (ori != 0)
#         action[[i]] <- ((opti - ori)/ori)*100
#       temp[[i]] <-  opti
#     }
#     
#     temp$EUR_o..Mstb. <- abs(opt_results$value)
#     action$EUR_o..Mstb. <- ((temp$EUR_o..Mstb. - stub$EUR_o..Mstb.)/stub$EUR_o..Mstb.) * 100
#     
#     optimums[well, ] <- temp
#     actions[well, ] <- action
#   }
#   write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
#   write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
#   
#   return(optimums)
# }

challengeOptimizations <- function(toOptimize,model) {
  toOptimize <- wells.to.optimize
  model <- data.model
  
  M <- ncol(toOptimize)
  N <- nrow(toOptimize)
  names <- names(toOptimize)
  actionables <- names(toOptimize)[!(names(toOptimize) %in% nonactionables)]
  actions <- data.frame(matrix(vector(), 0, M, dimnames=list(c(), names)), stringsAsFactors=T)
  names[length(names)+1] <- "Actual"
  optimums <- data.frame(matrix(vector(), 0, M+1, dimnames=list(c(), names)), stringsAsFactors=T)
  actionables <- actionables[!(actionables %in% c(ID))]
  toOptimize[[ID]] <- as.character(toOptimize[[ID]])
  
  for (well in 1:N) {
    well <- 15
    stub <- toOptimize[well, ]
    print(stub$WellID)
    
    # start with given values and replace by mean for actionables
    start <- stub[,-which(names(stub) %in% c(ID,METRIC))]
    target <- stub[[METRIC]]*1.1
    
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
      z <- stub[,-which(names(stub) %in% c(ID,METRIC))]
      
#       if (length(opt_start[which(opt_start==0,arr.ind = T)]) > 0) return(-10^38)
      z[is.na(as.numeric(opt_nons))] <- abs(x[is.na(as.numeric(opt_nons))])
#       z[!is.na(as.numeric(opt_nons))] <- opt_nons[!is.na(as.numeric(opt_nons))]
      # copy the data over top of the stub record
      #       for (i in names(x)) {
      #         # apply constraints
      # #         if (x[[i]] < opt_min[[i]] | x[[i]] > opt_max[[i]]) return(10^38)
      #         
      #         if (i %in% actionables) {
      #           z[[i]] <- x[[i]]
      #         } else {
      #           z[[i]] <- opt_nons[[i]]
      #         }
      #       }
      # score the data and return the negative
#       print(as.numeric(z))
      y <- predict(model,newdata = z)
      
      #If these conditions are violated, the function returns a large positive number
      # which the search procedure will avoid
      #     if(y > val*1.1) return(10^38)
      y
    }
    
    opt_start <- as.numeric(start)
    #     for (i in names(opt_start)) {
    #       if (i %in% actionables) {
    #         opt_start[[i]] <- startWells[k,][[i]]
    #       }
    #     }
    opt_start
    
    # optimize
    #         opt_results <- optim(opt_start, opt_gm, method="L-BFGS-B", control = list(trace=0,maxit=1000,REPORT=1),lower=opt_min, upper=opt_max) # ,
    #     opt_results <- optim(opt_start, opt_gm, method = "Nelder-Mead", control = list(maxit=5000)) # ,
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
    t <- temp$EUR_o..Mstb.
    temp$EUR_o..Mstb. <- abs(opt_results$value)
    action <- ((as.numeric(temp) - as.numeric(stub))/as.numeric(stub))*100
#     action$EUR_o..Mstb. <- ((temp$EUR_o..Mstb. - stub$EUR_o..Mstb.)/stub$EUR_o..Mstb.) * 100
    temp$Actual <- t
    
    #     optimums[well, ] <- cbResults
    optimums[well, ] <- temp
    actions[well, ] <- action
  }
  write.table(optimums, "prescriptions.csv",sep=",",row.names=FALSE, quote = FALSE)
  write.table(actions, "actions.csv",sep=",",row.names=FALSE, quote = FALSE)
  
  return(optimums)
}