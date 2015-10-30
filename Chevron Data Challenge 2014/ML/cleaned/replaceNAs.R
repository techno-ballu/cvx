# clear the memory!
rm(list=ls(all=TRUE))
gc()

setwd("~/Chevron/DSChallenge/cleaned")

data <- read.csv("well_data3.csv", header = T)

for(i in names(data)){
  data[[i]][is.na(data[[i]])] <- mean(data[[i]], na.rm = T)
}

write.table(data, "well_data4.csv", sep=",", row.names=FALSE, quote = FALSE)