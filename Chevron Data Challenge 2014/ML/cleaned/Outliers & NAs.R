# clear the memory!
rm(list=ls(all=TRUE))
gc()

setwd("/home/bolaka/Chevron/DSChallenge/final")

require(gdata)

# while reading this file, using na.strings = "NA" so that 
# blank fields are also considered to be missing values in logical, 
# integer, numeric and complex fields.
data <- read.csv("data_seasons.csv", header = TRUE, sep = ",", na.strings = "NA")

# deal "undefined" and empty strings as NAs in all columns
for(i in names(data)) {
  if (length(which(isUnknown(x=data[[i]], unknown = c("undefined","","#VALUE!")))) > 0) {
    print(paste(i,"qualifies for having UNDEFINEDs", sep = " "))
    data[[i]] <- unknownToNA(x=data[[i]], unknown=c("undefined","","#VALUE!"))
  }
}
str(data)

numericals <- sapply(data, is.numeric)

# deal outliers in numerical columns
data.numericals <- data[ , numericals]
str(data.numericals)
replaceOutliers <- function(x) {
  quantiles <- quantile(x, c(.05, .95), na.rm = TRUE)
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
data.withoutOutliers <- replaceOutliers(data.numericals)
data.others <- data[ , !numericals]
data.cleaned <- cbind(data.withoutOutliers, data.others)

write.table(data.cleaned, "data_cleaned.csv", sep=",", row.names=FALSE, quote = FALSE)
str(data.cleaned)
