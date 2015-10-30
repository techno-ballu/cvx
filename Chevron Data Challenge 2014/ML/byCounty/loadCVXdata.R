loadData <- function() {
  
  base.train <- read.csv("../base_training.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  base.test <- read.csv("../base_test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # add the EUR column to test data as 0
  base.test[[METRIC]] <- 0
  base.test[[METRIC]] <- unknownToNA(x=base.test[[METRIC]], unknown=c(0))
  
  base <- rbind(base.train, base.test)
  
  # remove the Deepest Zone as it will be added below
  base <- base[, -which(names(base) %in% c("Deepest_Zone"))]
#   # split Between Zone into 2
  zones=data.frame(do.call(rbind, strsplit(as.vector(base$Between_Zone), split = " --> ", fixed=TRUE)))
  cat("[base] Extracting the Top_Zone from Between_Zone column...")
  names(zones) <- c("Top_Zone", "Deepest_Zone")
  base <- cbind(base, zones)
  
  write.table(base, "base.csv", sep=",", row.names=FALSE, quote = FALSE)
  
  cat("\n[completions] Using the completions data without the Remarks column...\n")
  completions.train <- read.csv("../completions_training_removedRemarks.csv", header = T)
  completions.test <- read.csv("../completions_test_removedRemarks.csv", header = T)
  
  completions <- rbind(completions.train, completions.test)
  
  # unique completions
  uniqueCompletions <- unique( completions )
  
  uniqueCompletions$Treated.Stage.Height <- uniqueCompletions$Depth.Base - uniqueCompletions$Depth.Top
  uniqueCompletions$Fluid.Amount.Per.Foot <- uniqueCompletions$Fluid.Amount / uniqueCompletions$Treated.Stage.Height
  uniqueCompletions$Propping.Agent.Amount.Per.Foot <- uniqueCompletions$Propping.Agent.Amount / uniqueCompletions$Treated.Stage.Height
  
  write.table(uniqueCompletions, "completions.csv", sep=",", row.names=FALSE, quote = FALSE)
  
  geo.train <- read.csv("../geology_training.csv", header = TRUE, sep = ",")
  geo.test <- read.csv("../geology_test.csv", header = TRUE, sep = ",")
  
  geo <- rbind(geo.train, geo.test)
  
  # mark the handpicked columns
  handpicked <- c("CLFK..KHW.","U_SPBR..KHW.","L_SPBR..KHW.","WFMP..KHW.","WFMP_L..KHW.","WFMP_ATB..KH","CLINE..KHW.","STRN..KHW.","ATOK..KHW.","BEND..KHW.","ATOKA_L..KHW","MPLM..KHW.","WDFD..KHW.","DVNN..KHW.")
  # mark the interpolated columns
  interpolated <- c("CLEARFORK..MAP.","SPRABERRY_U..MAP.","SPRABERRY_L..MAP.","WOLFCAMP..MA","WOLFCAMP_L..","WFMP_ATB..MA","CLINE..MAP.","STRAWN..MAP.","ATOKA..MAP.","BEND..MAP.","ATOKA_L..MAP","MISSISSIPPIA","WOODFORD..MA","DEVINOAN_UNC")
  
  cat("\n[geology] Replace the NAs in handpicked columns by values from the interpolated columns...\n")
  # replace the Handpicked NAs by values from the Interpolated columns
  for(i in 1:length(handpicked)) {
    print(paste(handpicked[i], interpolated[i], sep = " = "))
    geo[[handpicked[i]]][is.na(geo[[handpicked[i]]])] <- geo[[interpolated[i]]][is.na(geo[[handpicked[i]]])]
  }
  
  cat("\n[geology] Dropped the interpolated columns...")
  # drop interpolated columns
  geo = geo[,-which(names(geo) %in% interpolated)]
  # colnames(geo) <- c("WellID","Clearfork","Upper_Spraberry","Lower_Spraberry","Wolfcamp","Lower_Wolfcamp",
  #                    "Amacker_Tippet_Base","Cline","Strawn","Atoka","Bend","Lower_Atoka","Mississippian",
  #                    "Woodford","Devonian")
  colnames(geo) <- c("WellID",1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  write.table(geo, "geology.csv", sep=",", row.names=FALSE, quote = FALSE)
  
  # load scores derived from Remarks!
  # scores <- read.csv("../score_sum.csv", header = T)
  
  # merging the files
  temp <- merge(base, uniqueCompletions, by="WellID")
  DATA <- merge(temp, geo, by="WellID")
  # DATA <- merge(DATA, scores, by="WellID")
  
  result <- list(DATA = DATA) #training = training, testing = testing, 
  return(result)
}