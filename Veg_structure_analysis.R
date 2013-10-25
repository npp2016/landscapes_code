# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the vegetation structure data. Student to contact for data questions: Marisa
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are four separate .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/Vegetation_Structure_data/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/Vegetation_Structure_data/"
setwd(wd)

## Read in csv files
canopy <- read.csv("CanopyAndGroundCover.csv")
siteveg_descrip <- read.csv("SiteVegetationDescriptors.csv")
genus_key <- read.csv("GenusKey.csv")
trees <- read.csv("trees.csv")
shrubs <- read.csv("shrubs.csv")

## Cleaning and aggregating data
m_canopy <- melt(data=canopy, id.vars="Site", measure.vars=c("percent_canopy_cover", 
                   "percent_subcanopy_cover", "percent_branches_without_leaves"), na.rm=T)

## Not working! Need to fix blanks FIXME
treedescrip <- melt(data=trees, id.vars="Point", measure.vars=c("Genus", "Height", "DBH_class"), na.rm=T)

## Plots
# Canopy cover by site
cc_site <- ggplot(m_canopy, aes(x=Site, fill=variable)) + geom_bar()
cc_site

# Densitometry by site
densitometry_site <- ggplot(canopy, aes(x=Site, y=Percent_densitometry)) + geom_boxplot()
densitometry_site

# Plot tree height by genus
tree_genus_height <- ggplot(tree, aes(x=Genus, y=Height_m)) + geom_boxplot() + 
                              theme(axis.text.x=element_text(angle=90, vjust=0))
tree_genus_height

# Plot canopy radius by genus
tree_genus_canopy <-  ggplot(tree, aes(x=Genus, y=Canopy_radius_m)) + geom_boxplot() +
                              theme(axis.text.x=element_text(angle=90, vjust=0))
tree_genus_canopy

# Plot dbh class by genus
tree_genus_dbh <- ggplot(tree, aes(x=Genus, y=DBH_class)) + geom_point() + coord_flip() + facet_grid(~Site)
tree_genus_dbh

