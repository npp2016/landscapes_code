# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# developed by: Anusha Shankar, Sarah Supp, and Catherine Graham


setwd("C://Users//Anusha//Documents//Dropbox//Hummingbirds//Pasantias_Patagonia_2013//Final_Databases_2013//Excel_CSV_versions")

## Reading in point count and focal observation data
pointdata <- read.csv("updated_FocalObservationPointCountData.csv", na.strings="NA", sep=",", header=T)

## Load packages
require(ggplot2)
require(reshape)

## Plot species vs. site
sitespecies <- ggplot(pointdata, aes(x=Site, y=Species.Code)) + ylab("Species") + 
  geom_point(col="red") + theme_bw()
sitespecies

## Make a table for species richness at each site
richnesssite <- aggregate(pointdata$Species.Code, by=list(pointdata$Site), 
                      FUN=function(u) length(unique(u)))

## Plot richness as a function of site
siterichness <- ggplot(richness, aes(x=Group.1, y=u)) + xlab("Site") + 
  ylab ("Species richness") + geom_point() + geom_point(size=3) + theme_bw()
siterichness

## Richness by vegetation type?
richnessveg <- aggregate(pointdata$Species.Code, by=list(pointdata$Vegetation.Type), 
                      FUN=function(u) length(unique(u)))

## Plot richness as a function of vegetation type
vegrichness <- ggplot(richness, aes(x=Group.1, y=x)) + xlab("Site") + 
  ylab ("Species richness") + geom_point() + geom_point(size=3) + theme_bw()
vegrichness