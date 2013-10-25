# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the vegetation structure data. Student to contact for data questions: Marisa
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are four separate .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versionsVegetation_Structure_data/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/Vegetation_Structure_data/"
setwd(wd)

## Read in csv files
canopy <- read.csv("CanopyAndGroundCover.csv")
siteveg_descrip <- read.csv("SiteVegetationDescriptors.csv")
genus_key <- read.csv("GenusKey.csv")

## This tree_shrub dataset is very confusing, has way too many blanks. Need to speak about how to correct this.
## The melt() function gets very confused with the blanks.
tree_shrub <- read.csv("TreeAndShrubSize.csv")

## Cleaning and aggregating data
m_canopy <- melt(data=canopy, id.vars="Site", measure.vars=c("percent_canopy_cover", 
                   "percent_subcanopy_cover", "percent_branches_without_leaves"), na.rm=T)
## Not working! Need to fix blanks
shrubdescrip <- melt(data=tree_shrub, id.vars="Point", measure.vars=c("Genus", "Height", "DBH_class"), na.rm=T)

## Plots
# Canopy cover by site
cc_site <- ggplot(m_canopy, aes(x=Site, fill=variable)) + geom_bar()
cc_site

# Densitometry by site
densitometry_site <- ggplot(canopy, aes(x=Site, y=Percent_densitometry)) + geom_boxplot()
densitometry_site

