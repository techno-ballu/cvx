# clear the memory!
rm(list=ls(all=TRUE))
gc()

setwd("/home/bolaka/Chevron/DSChallenge/final")

base <- read.csv("base_training.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
table(base$Between_Zone)

# v = strsplit(base$Between_Zone, split=" --> ", fixed=TRUE)
# len <- length(v)
# 
# for(i in len) {
#   
# }
v=data.frame(do.call(rbind, strsplit(as.vector(base$Between_Zone), split = " --> ", fixed=TRUE)))
names(v) <- c("Top_Zone", "Deepest_Zone")

base <- cbind(base, v)

write.table(base, "base_training_withTopZone.csv", sep=",", row.names=FALSE, quote = FALSE)