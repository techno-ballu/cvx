# clear the memory!
rm(list=ls(all=TRUE))
gc()

setwd("~/Chevron/DSChallenge")
data <- read.csv("training_final.csv", header = T)
nums <- sapply(data, is.numeric)
nums

# Box Whisker plots for all?
boxplot(data[,1:2], horizontal=TRUE)

# Multiple histograms
par(mfrow=c(3, 3))
colnames <- dimnames(data)[[2]]
for (i in 2:8) {
  hist(data[,i])
}