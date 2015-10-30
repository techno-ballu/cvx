# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set the work directory
setwd("~/Chevron/DSChallenge/final")

# load the training data
data = read.csv("data_imputed.csv", header = TRUE)
# data = data[,c(-1,-4)]
linear = lm(EUR_o..Mstb. ~., data = data)

require(car)

v = vif(linear)
vif = data.frame(v)
vif