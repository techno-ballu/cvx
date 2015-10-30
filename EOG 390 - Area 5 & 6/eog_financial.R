print("=============== loading script to calculate financial variables on oil wells ===============")

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

# fin_vars <- c("CUM_30PD_OIL_PROD","CUM_60PD_OIL_PROD","CUM_90PD_OIL_PROD",METRIC,"Stages","Clusters","Total.Propant.Used","Amount.100.mesh","Amount.Others")

calculateFinancial <- function(DATA,file) {
  
#   DATA <- original
  
  # copy a backup for financial
  financial <- DATA #[,which(names(DATA) %in% fin_vars)]
#   print(financial[1,])
  
  financial$CumProd <- financial$CumProd
  
  # 1. Stages
  financial$Stages <- financial$TREATED_LENGTH/financial$TREATED_LATERAL_PER_STAGE
  # 2. Total Propant Used
  financial[["Total.Propant.Used"]] <- financial$TREATED_LENGTH*financial$PROPANT_PER_FOOT
  # 3. Amount of 100 mesh sand used
  financial[["Amount.100.mesh"]] <- financial$Total.Propant.Used*financial$Fraction.100.MESH
  # 4. Amount of other sands used
  financial[["Amount.Others"]] <- financial$Total.Propant.Used - financial$Amount.100.mesh
  
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
  # irr <- financial[[""]]
  # payback
  
  write.table(financial, paste("financial_",file,".csv",sep = ""), sep=",", row.names=FALSE, quote = FALSE)
  
  # copy a backup for financial
#   financial <- DATA[,which(names(DATA) %in% fin_vars)]
#   financial <- cbind(DATA[[ID]],financial)
#   colnames(financial)[1] <- ID

  return(financial)
}


