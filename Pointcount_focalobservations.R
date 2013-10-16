# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
#wd = "C://Users//Anusha//Documents//Dropbox//Hummingbirds//Pasantias_Patagonia_2013//Final_Databases_2013//Excel_CSV_versions"
wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions"
setwd(wd)

## Reading in point count and focal observation data
pointdata <- read.csv("updated_FocalObservationPointCountData.csv", na.strings="NA", sep=",", header=T)

## subset data that does not include surveys where no birds were seen
obs = subset(pointdata, Species.Code!="None")

## Plot species vs. site
sitespecies <- ggplot(pointdata, aes(x=Site, y=Species.Code)) + ylab("Species") + 
  geom_point(col="red") + theme_bw()
sitespecies

## Aggregate data; count the number of species seen at each session
richness <- aggregate(obs$Species.Code, by=list(obs$Month, obs$Day, obs$Year, obs$Session, 
                                                obs$Site, obs$Vegetation.Type), FUN=function(u) length(unique(u)))
names(richness) <- c("month", "day", "year", "session", "site", "vegtype", "S")

## Make a table for species richness at each site     
richnesssite <- aggregate(obs$Species.Code, by=list(obs$Site), 
                      FUN=function(u) length(unique(u)))
names(richnesssite) <- c("site", "S")

## Plot richness as a function of site   
siterichness <- ggplot(richnesssite, aes(x=site, y=S)) + xlab("Site") + 
  ylab ("Species richness") + geom_point() + geom_point(size=3) + theme_bw()
siterichness

## Richness by vegetation type?    
richnessveg <- aggregate(obs$Species.Code, by=list(obs$Vegetation.Type), 
                      FUN=function(u) length(unique(u)))
names(richnessveg) <- c("veg_type", "S")

## Plot richness as a function of vegetation type    #FIXME: x axes are not readable
vegrichness <- ggplot(richnessveg, aes(x=veg_type, y=S)) + xlab("Site") + 
  ylab ("Species richness") + geom_point() + geom_point(size=3) + theme_bw()
vegrichness

#----- TODO:
#compare richness at different times of day (session)
#plot richness as a function of time
#compare abundances across sites and habitat types
#should we include data from both point counts and focal obs together or separately?
