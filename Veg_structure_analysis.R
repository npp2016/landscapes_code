# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the vegetation structure data. Contact for data questions: Marisa
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
cover <- read.csv("CanopyAndGroundCover.csv")
siteveg_descrip <- read.csv("SiteVegetationDescriptors.csv")
genus_key <- read.csv("GenusKey.csv")
trees <- read.csv("trees.csv")
shrubs <- read.csv("shrubs.csv")

##-------- Cleaning and aggregating data

#dataframe with canopy cover data only
canopy = cover[,1:13]
canopy[,c(10:13)] = canopy[,c(10:13)]/100
## @ Sarah - This should stay as names(canopy) right? It was names(ground)
names(canopy) = c("Day", "Month", "Year", "Site", "Transect", "Point", "Observer", "Direction", "Distance", 
                    "densitometry","canopy", "subcanopy", "barebranches")

# Renaming shrubs variables
names(shrubs) <- c("Date", "Site", "Transect", "Point", "Observer", "Genus", "num_indivs", "Type", "Direction",
                   "0to0.5m", "0.5to1m", "1to2m", "2to3m", "3plusm")

# dataframe with ground cover only
ground = cover[,c(1,2,3,4,5,6,7,8,9,14,15,16,17,18,19,20,21)]
ground[,c(10:17)] = ground[,c(10:17)]/100
names(ground) = c("Day", "Month", "Year", "Site", "Transect", "Point", "Observer", "Direction", "Distance", 
                  "shrub", "forb", "grass", "soil", "rock", "organicmaterial", "water", "disturbance")

# melt canopy data using Site as id.vars
# TODO need to figure out how to scale this. Maybe divide each percent_... value by densitometry/100 or something?
m_canopy <- melt(data=cover, id.vars=c("Site", "Point"), measure.vars=c("percent_canopy_cover", 
                                                                         "Percent_densitometry", "percent_subcanopy_cover", 
                                                                         "percent_branches_without_leaves"), na.rm=T)

# melt ground cover data
#FIXME -- Now I'm not sure melt is the right way to deal with this data 
# at each point, variables are taken at 9-17 locations away from the center.
# total set of variables should sum to 1
## AS: So, there is one set of measurements at 15m, in different directions
# And another set at 30m in different directions. I think we find an average of each 
# canopy_...value across directions at the same distance. Then we facet by distance, 
# and plot either stacked or separate plots of cover. Not sure how to execute melt() yet.
m_ground <- melt(data=cover, id.vars=c("Site", "Point"), 
                 measure.vars=c("percent_shrub_groundcover", "percent_forb_groundcover", "percent_grass_groundcover",
                                "percent_soil_groundcover", "percent_rock_groundcover", "percent_organic_material_groundcover",
                                "percent_water_groundcover", "percent_disturbance_groundcover"), na.rm=T)

#reshape trees data for values per point and genus for each of the three tree variables
m_trees <- melt(data=trees, id.vars=c("Site", "Point", "Genus"), 
                    measure.vars=c("Height_m", "DBH_class", "Canopy_radius_m"), na.rm=T)

# melt shrubs data 
m_shrubs <- melt(shrubs, id.vars=c("Site", "Genus", "Point"), 
                 measure.vars=c("0to0.5m", "0.5to1m", "1to2m", "2to3m", "3plusm"), na.rm=T)
names(m_shrubs) <- c("Site", "Genus", "Point", "size_class", "num_indivs")

#aggregate shrub data by site and size class
shb <- aggregate(num_indivs ~ Site + size_class, data = m_shrubs, FUN = sum)

##--------- Plots

# Canopy cover by site - useful. Removed densitometry. Not sure if this is enough or it still needs scaling
cc_site <- ggplot(m_canopy[!m_canopy$variable%in% "Percent_densitometry",],
                  aes(x=Site, fill=variable)) + geom_bar()
cc_site

# Densitometry by site
densitometry_site <- ggplot(cover, aes(x=Site, y=Percent_densitometry)) + geom_boxplot() + theme_bw()
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

##### LOOKING FOR DIFFERENCES AMONG THE TWO SITES
# Plot tree height by site
tree_site_height <- ggplot(trees, aes(x=Site, y=Height_m, fill=Site)) + geom_boxplot() + theme_bw()
tree_site_height

#Plot tree canopy radius by site
tree_site_canopy <- ggplot(trees, aes(x=Site, y=Canopy_radius_m, fill=Site)) + geom_boxplot() + theme_bw()
tree_site_canopy

#Plot DBH class by site
tree_site_dbh <- ggplot(subset(m_trees, variable=="DBH_class"), aes(Site, fill=factor(value))) + geom_bar(width=0.5)
tree_site_dbh

#Plot shrub size by site
shrub_site_size <- ggplot(shb, aes(x=size_class, weight=num_indivs, fill = Site)) + geom_bar() + facet_wrap(~ Site) +
   theme_bw() + theme(axis.text.x=element_text(angle=60, vjust=0.5)) 
shrub_site_size
