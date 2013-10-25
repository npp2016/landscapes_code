#This code imports site coordinates for the Hummingbird Monitoring Network, converts degrees, minutes,
#seconds to decimal degrees, and plots the data on a map of North America
#Sarah R. Supp and Anusha Shankar

library(maps)
library(mapdata)
library(maptools)
library(scales)
library(RgoogleMaps)
library(ggplot2)
library(ggmap)
library(rtiff)

wd = "C://Users/Anusha/Documents/Dropbox/NASA_Anusha/"
# wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/NASA_Anusha/"
setwd(wd)

## I've saved the dms file with the corrected values as a new csv, so we don't have to run the convert()
## code again. Leaving the function in just in case, minimized
hmn <- read.csv("dmsdd_corrected.csv", sep = ",", header = T)

#################### MAKE SWEET MAPS ########
# tutorial by K. Gilbert: http://www.molecularecologist.com/2012/09/making-maps-with-r/

#HMN study sites on a political map with country outlines
map("worldHires","usa", xlim=c(-128,-94), ylim=c(17,53), col="gray90", fill=T)
map("worldHires", "Canada", xlim=c(-128,-94), ylim=c(53,64), col="gray95", fill = TRUE, add=T)
map("worldHires", "Mexico", xlim=c(-128,-94), ylim=c(10,17), col="gray95", fill=T, add=T)
points(hmn$longitude, hmn$latitude, pch=19, col="red", cex=0.5)  #plot my sample sites
points(hmn$londd, hmn$latdd, pch=19, col="blue", cex=0.5)  #plot my sample sites

########## Make Even Sweeter R Google Maps

#plots just a terrain map of a specified area, no points
lat <- c(10,64) #define our map's ylim
lon <- c(-128,-94) #define our map's xlim
center = c(mean(lat), mean(lon))  #tell what point to center on
zoom <- 5  #zoom: 1 = furthest out (entire globe), larger numbers = closer in
terrmap <- GetMap(center=center, zoom=zoom, maptype= "terrain", destfile = "terrain.png") #lots of visual options, just like google maps: maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")

## QGIS tif map. Doesn't help
# qgismap <- readTiff("hmn.tif")
# plot(qgismap)

#can plot a subset of the points, but get an error with >34 :(
hmn$size <- "small"  #create a column indicating size of marker
hmn$col <- "red"   #create a column indicating color of marker
hmn$char <- ""   #normal Google Maps pinpoints will be drawn
mymarkers <- cbind.data.frame(hmn$latitude, hmn$longitude, hmn$size, hmn$col, hmn$char)   #create the data frame by binding my data columns of GPS coordinates with the newly created columns
names(mymarkers) <- c("lat", "lon", "size", "col", "char")  #assign column headings
lat <- c(10,64)  #now we are plotting the map
lon <- c(-128,-94)
terrain_close <- GetMap.bbox(lonR=range(lon), latR=range(lat), center=c(37,-111), destfile= "terr_hmn.png", markers= mymarkers, zoom=5, maptype="terrain")

#################### MAKE MAXIMUM SWEETNESS MAPS USING GGPLOT2 AND GGMAPS #######
# tutorial by D. Kahle & H. Wickham http://www.slideshare.net/ajayohri/sexy-maps-comes-to-r-ggplot-google-maps-ggmap-rstats

# next try tutorial for mapping on google
df = data.frame(x=hmn$londd, y = hmn$latdd)
map <- get_googlemap(center=c(-111,37), markers=df, scale = 1, maptype="terrain", zoom = 4)
ggmap(map, extent='device')

#make a map with a legend - BEST ONE SO FAR!!
# To get lighter map with state labels and boundaries, use maptype="terrain"
hmnmap <- qmap("Flagstaff, AZ", zoom = 4, legend = "bottomleft", maptype = "satellite", color="bw")
no_years <- hmnmap + 
  geom_point(aes(x = londd, y = latdd, colour = status, size=No_of_years),
            data = hmn)
no_years

## Change colors
myColors <- brewer.pal(3,"Set2")
names(myColors) <- levels(hmn$satus)
colScale <- scale_colour_manual(name = "grp",values = myColors)
no_years2 <- no_years + colScale
no_years2

## Just status, does not plot No_of_years
active_sites <- hmnmap +
  geom_point(aes(x = londd, y = latdd, colour = status), data = hmn) +
  scale_color_manual(values = c("red", "black"))
active_sites

ggsave(filename="Active_sites.jpeg", plot=active_sites, type="jpeg")