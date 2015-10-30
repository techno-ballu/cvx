# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set the work directory
setwd("~/Chevron/DSChallenge/final")

# load the training data
data = read.csv("data_forMC.csv", header = TRUE)
str(data)

# data <- na.omit(data)
# data = data[,c(-1,-4)]
linear = lm(EUR_o..Mstb. ~., data = data)
print(linear)

require(car)

v = vif(linear)
vif = data.frame(v)
vif