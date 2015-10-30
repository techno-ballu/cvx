# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set the work directory
setwd("~/Chevron/DSChallenge/final")

# load the training data
data = read.csv("data_labeled.csv", header = TRUE)
data = data[,c(-1,-5)]

data.imputed <- rfImpute(EUR_o..Mstb. ~., data)

write.table(data.imputed, "data_imputed.csv", sep=",", row.names=FALSE, quote = FALSE)