# clear the memory!
rm(list=ls(all=TRUE))
gc()

setwd("/home/bolaka/Chevron/DSChallenge/final")

data <- read.csv("data_cleaned.csv", header = TRUE, sep = ",")

# 20% missing threshold!
missingThreshold <- 0.1
cols <- ncol(data)
rows <- nrow(data)
NA_RowWise <- apply(data, 1, function(z) sum(is.na(z)))
hist(NA_RowWise)
quantile(NA_RowWise)
#eliminate rows whose missing > 20%
colsMissing <- floor(cols*missingThreshold)
colsMissing
DelRows <- which(NA_RowWise > colsMissing)
DelRows
length(DelRows)
dat1 <- data[-DelRows,]
nrow(dat1)
ncol(dat1)

NA_ColWise <- apply(dat1, 2, function(z) sum(is.na(z)))
hist(NA_RowWise)
quantile(NA_ColWise)
#eliminate cols whose missing > 20%
rowsMissing <- floor(rows*missingThreshold)
rowsMissing
DelCols <- which(NA_ColWise > rowsMissing)
DelCols
dat2 <- dat1[,-DelCols]
nrow(dat2)
ncol(dat2)

write.table(dat2, "data_RowsColsGood.csv", sep=",", row.names=FALSE, quote = FALSE)