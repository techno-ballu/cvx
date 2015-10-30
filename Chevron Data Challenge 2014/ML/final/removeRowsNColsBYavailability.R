# clear the memory!
rm(list=ls(all=TRUE))
gc()

setwd("/home/bolaka/Chevron/DSChallenge/final")

data <- read.csv("data.csv", header = TRUE, sep = ",")

ncol(data)
nrow(data)
NA_RowWise <- apply(data, 1, function(z) sum(is.na(z)))
hist(NA_RowWise)
quantile(NA_RowWise)
#eliminate rows whose missing > 20%
DelRows <- which(NA_RowWise > 10)
DelRows
length(DelRows)
dat1 <- data[-DelRows,]
nrow(dat1)
ncol(dat1)

NA_ColWise <- apply(dat1, 2, function(z) sum(is.na(z)))
hist(NA_RowWise)
quantile(NA_ColWise)
#eliminate cols whose missing > 20%
DelCols <- which(NA_ColWise > 330)
DelCols
dat2 <- dat1[,-DelCols]
nrow(dat2)
ncol(dat2)

write.table(dat2, "data_RowsColsGood.csv", sep=",", row.names=FALSE, quote = FALSE)