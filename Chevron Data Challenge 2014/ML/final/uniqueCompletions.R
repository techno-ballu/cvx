# clear the memory!
rm(list=ls(all=TRUE))
gc()

# set work dir
setwd("~/Chevron/DSChallenge/final")

# uniques
completions <- read.csv("completions_training_removedRemarks.csv", header = T)
uniqueCompletions <- unique( completions )
write.table(uniqueCompletions, "completions_training_uniques.csv", sep=",", row.names=FALSE, quote = FALSE)

# point completions to aggregated!
#completions <- read.csv("completions_aggr_test.csv", header = T)
base <- read.csv("base_training_withTopZone.csv", header = T)
base$Completion.Date = as.Date(base$Completion.Date, origin = "1900-01-01")

geo <- read.csv("geology_training_handPicked.csv", header = T)

temp <- merge(base, uniqueCompletions, by="WellID")
data <- merge(temp, geo, by="WellID")

write.table(data, "data.csv", sep=",", row.names=FALSE, quote = FALSE)