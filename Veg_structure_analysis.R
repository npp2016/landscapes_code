# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the vegetation structure data. Student to contact for data questions: Marisa
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are five separate .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/Vegetation_Structure_data/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/Vegetation_Structure_data/"
setwd(wd)

## Read in csv files
canopy <- read.csv("CanopyAndGroundCover.csv")
siteveg_descrip <- read.csv("SiteVegetationDescriptors.csv")
genus_key <- read.csv("GenusKey.csv")
trees <- read.csv("trees.csv")
shrubs <- read.csv("shrubs.csv")

##-------- Cleaning and aggregating data

# melt canopy data using Site as id.vars
m_canopy <- melt(data=canopy, id.vars="Site", measure.vars=c("percent_canopy_cover", 
        "Percent_densitometry", "percent_subcanopy_cover", 
        "percent_branches_without_leaves"), na.rm=T)

# FIXED. @ Sarah- the height column was not named right. fixed now.
m_trees <- melt(data=trees, id.vars="Point", 
                    measure.vars=c("Genus", "Height_m", "DBH_class", "Canopy_radius_m"), na.rm=T)

# melt shrubs data
m_shrubs <- melt(shrubs, id.vars="Site", measure.vars=c("Genus", "shrubs_0to0.5m", 
                     "shrubs_0.5to1m", "shrubs_1to2m", "shrubs_2to3m", "shrubs_3plus_m"), na.rm=T)

##--------- Plots

# Canopy cover by site - useful
cc_site <- ggplot(m_canopy, aes(x=Site, fill=variable)) + geom_bar() + theme_bw
cc_site

# Densitometry by site
densitometry_site <- ggplot(canopy, aes(x=Site, y=Percent_densitometry)) + geom_boxplot() + theme_bw()
densitometry_site

# Plot tree height by genus
tree_genus_height <- ggplot(trees, aes(x=Genus, y=Height_m)) + geom_boxplot() + theme_bw() +
                              theme(axis.text.x=element_text(angle=90, vjust=0))
tree_genus_height

# Plot canopy radius by genus
tree_genus_canopy <-  ggplot(trees, aes(x=Genus, y=Canopy_radius_m)) + geom_boxplot() +
                    theme_bw() + theme(axis.text.x=element_text(angle=90, vjust=0))
tree_genus_canopy

# Plot dbh class by genus
tree_genus_dbh <- ggplot(trees, aes(x=Genus, y=DBH_class)) + geom_point() + theme_bw() +
  coord_flip() + facet_grid(~Site)
tree_genus_dbh


