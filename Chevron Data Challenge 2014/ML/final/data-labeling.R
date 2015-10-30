# clear the memory!
rm(list=ls(all=TRUE))
gc()

library(gdata)
# set work dir
setwd("~/Chevron/DSChallenge/final")
data <- read.csv("data_RowsColsGood.csv", header = T)
types <- lapply(data, class)
distincts <- lapply(data, function(c) unique(c))

for(i in names(data)){
  # find out type of column
  if (length(levels(distincts[[i]])) <= length(data[[i]])/2 && types[[i]] == "factor") {
    #     print(levels(distincts[[i]]))
    print(paste(i, " has ", length(levels(distincts[[i]])), " categories.", sep = ""))
    map <- mapLevels(data[[i]], )
    int <- as.integer(data[[i]])
    data[[i]] <- int
    #mapLevels(intCounty) <- mapCounty
    #all.equal(intCounty, productions$County) #TRUE
  }
}

write.table(data, "data_labeled.csv", sep=",", row.names=FALSE, quote = FALSE)