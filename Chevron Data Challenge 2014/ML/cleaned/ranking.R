# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set the work directory
setwd("~/Chevron/DSChallenge/final")

# load the training data
data = read.csv("data_imputed.csv", header = TRUE)

require(relaimpo)
rankings <- calc.relimp(EUR_o..Mstb. ~.,data=data)
ranks=sort(rankings$lmg,decreasing=T)
ranks