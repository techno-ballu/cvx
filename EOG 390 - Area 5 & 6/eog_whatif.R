# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
library(MASS)
library(caret)

# set the work directory
setwd("D:\\ubunto\\Completions\\Deliverables")

# load well wise Completions data
DATA <- read.csv("../eogcompletions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
print(paste("=============== Data has", nrow(DATA), "rows and", ncol(DATA), "columns ===============", sep = " "))

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

# load the AREA 5 model back from disk (prior variable name is restored)
load("eog_final_model5.rda",verbose = T)
area5 <- optim.model

# load the AREA 6 model back from disk (prior variable name is restored)
load("eog_final_model6.rda",verbose = T)
area6 <- optim.model

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

calculateWellCompletionCost <- function(amount.100.mesh,amount.others,stages,clusters) {
  # Completion well costs
  cost.100.mesh <- amount.100.mesh * DOLLARS.100.MESH.PER.POUND # <- financial$cost.100.mesh1
  cost.30by70.mesh <- amount.others * (0/3) * DOLLARS.30by70.MESH.PER.POUND # <- financial$cost.30by70.mesh1
  cost.20by40.mesh <- amount.others * (1/3) * DOLLARS.20by40.MESH.PER.POUND # <- financial$cost.20by40.mesh1
  cost.40by70.mesh <- amount.others * (2/3) * DOLLARS.40by70.MESH.PER.POUND # <- financial$cost.40by70.mesh1
  total.propant <- cost.100.mesh + cost.20by40.mesh + cost.30by70.mesh + cost.40by70.mesh # <- financial$CostTotalPropant1
  equipment <- STAGES.COMPLETED.PER.DAY * stages * DOLLARS.EQUIPMENT.PER.DAY # <- financial$CostEquipment1
  chemicals <- stages * DOLLARS.CHEMICALS.PER.STAGE # <- financial$CostChemical1
  fluids <- stages * GALLONS.FLUIDS.PER.STAGE * DOLLARS.FLUID.PER.GALLON # <- financial$CostFluids1
  fuel <- DOLLARS.FUEL # <- financial$CostFuel1
  
  simulation <- total.propant + equipment + chemicals + fluids + fuel # <- financial$TotalSimulation1
  completions <- simulation * 0.75 # <- financial$Completions1
  perforations <- clusters * stages * DOLLARS.PERFORATIONS.PER.CLUSTER # <- financial$Perforations1
  facilities <- DOLLARS.FACILITIES # <- financial$Facilities1
  return(simulation + completions + perforations + facilities)
}

# a function to calculate the financials!
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
  
  if (duringOptimization == TRUE)
    return(npv)
  return(financial)
}

METRIC <- "CumProd"
# inputs formats for the 2 areas
input5 <- read.csv("sample_whatif5.csv",header = T,sep = ",",stringsAsFactors = FALSE)
input6 <- read.csv("sample_whatif6.csv",header = T,sep = ",",stringsAsFactors = FALSE)
# what-if
whatif5 <- predict(area5, newdata = input5)
whatif5

whatif6 <- predict(area6, newdata = input6)
whatif6

input5[[METRIC]] <- whatif5
input6[[METRIC]] <- whatif6

temp5 <- calculateNPV(input5,whatif5,duringOptimization = F)
temp5 # this is the final result containing all financials calculated along with the CUM_180!
write.table(temp5, "what-if-results5.csv",sep=",",row.names=FALSE, quote = FALSE)

temp6 <- calculateNPV(input6,whatif6,duringOptimization = F)
temp6 # this is the final result containing all financials calculated along with the CUM_180!
write.table(temp6, "what-if-results6.csv",sep=",",row.names=FALSE, quote = FALSE)