# clear the memory!
rm(list=ls(all=TRUE))
gc()

# load the dependencies!
require(ggplot2)
require(ggmap)

# set the work directory
setwd("~/Chevron/DSChallenge/maps")

# load file
base <- read.csv("base_training.csv", header = TRUE)

# extract the latitudes & longitudes
area <- base$Subarea
lon <- base$Surface.Longitude
lat <- base$Surface.Latitude
eur <- base$EUR_o..Mstb.

# separate data frame to hold well coordinates
temp <- cbind(lon,lat)
temp <- cbind(temp,area)
data <- as.data.frame(cbind(temp,eur))

# start with points
pred.points <- ggplot(data = data, aes(x = lon, y = lat, colour = eur)) + geom_point()
print(pred.points)
ggsave(filename = "wells_points.png", plot = pred.points, scale = 1, width = 5, height = 3, dpi = 300)

# mean value of eurs
pred.stat <- ggplot(data = data, aes(x = lon, y = lat, z = eur)) + stat_summary2d(fun = mean)
print(pred.stat)
ggsave(filename = "wells_means.png", plot = pred.stat, scale = 1, width = 5, height = 3, dpi = 300)

# refine breaks and palette ----
require('RColorBrewer')
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
pred.stat.bin.width <- ggplot(data = data, aes(x = lon, y = lat, z = eur)) + 
  stat_summary2d(fun = median, binwidth = c(.01, .01)) + 
  scale_fill_gradientn(name = "Median", colours = YlOrBr, space = "Lab") +
  coord_map()
print(pred.stat.bin.width)
ggsave(filename = "wells_means_custom.png", plot = pred.stat.bin.width, scale = 1, width = 5, height = 3,
       dpi = 300)

# get the satellite map
map.in <- get_map(location = c(min(data$lon),min(data$lat),max(data$lon),max(data$lat)))
theme_set(theme_bw(base_size = 8))
pred.stat.map <- ggmap(map.in) %+% data + 
  aes(x = lon,y = lat,z = eur) +
  stat_summary2d(fun = median, binwidth = c(.01, .01), alpha = 0.8) + 
  scale_fill_gradientn(name = "EUR",colours = YlOrBr,space = "Lab") + 
  labs(x = "Longitude", y = "Latitude") +
  coord_map()
print(pred.stat.map)
ggsave(filename = "wells_means_map.png",
       plot = pred.stat.map,
       scale = 3,
       width = 5, height = 3,
       dpi = 300)