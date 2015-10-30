# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
# require(gdata)
require(randomForest)
require(RWeka)
require(caret)
# require(fmsb)

# set the work directory
setwd("/home/bolaka/EOG-data/Completions")

# load the model back from disk (prior variable name is restored)
load("completions-best.rda",verbose = T)
# inputs
input <- read.csv("whatif-best.csv",header = T,sep = ",")
# what-if
whatif <- predict(best.fit, newdata = input)
whatif


