# clear the memory!
rm(list=ls(all=TRUE))
gc()

setwd("~/Chevron/DSChallenge")

training <- read.csv("training_final.csv", header = T)

for(i in names(training)){
  training[[i]][is.na(training[[i]])] <- mean(training[[i]], na.rm = T)
}

write.table(training, "training_final_NAx.csv", sep=",", row.names=FALSE, quote = FALSE)