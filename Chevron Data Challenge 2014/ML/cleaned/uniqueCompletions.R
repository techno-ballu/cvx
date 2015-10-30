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

# add Completion.Month
base$Completion.Month = as.numeric(format(base$Completion.Date, "%m"))

# add Completion.Season
base$Completion.Season <- time2season(base$Completion.Date, out.fmt = "seasons", type = "default")

# not using this!
# geo <- read.csv("geology_training_handPicked.csv", header = T)

data <- merge(base, uniqueCompletions, by="WellID")
# data <- merge(temp, geo, by="WellID")

write.table(data, "data_seasons.csv", sep=",", row.names=FALSE, quote = FALSE)