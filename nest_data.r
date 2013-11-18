# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the hummingbird nest data. Contact for data questions: Monica
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are multiple .csv files in this folder
#wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
setwd(wd)

## Read in csv files
nest <- read.csv("NestData.csv")
## Removed entry where Species is blank (Id 74) ASK SUSAN? ###################
nest <- nest[-74,]

##-------- Cleaning and aggregating data
m.nest <- melt(nest, id.vars=c("Id", "NestID", "Site"), 
               measure.vars=c("Species","Stage_Found", "Final_Result"), na.rm=T)

species_site <- aggregate(nest$Species, by=list(nest$Site, nest$Species), 
                          FUN=function(x) x=length(x))
names(species_site) <- c("Site", "Species", "Richness")

##--------- Plots

# TODO:
# plot number of individuals of each species found nesting at each site
nests_site <- ggplot(species_site, aes(x=Site, y=Richness, fill=factor(Site))) + geom_bar() + ylab("Nests") +
  facet_grid(~Species) + theme_bw()
nests_site

# plot number of nests found at each site - in first graph
# plot number of individuals, for each species that was observed nesting at each site - see first graph
# boxplots for nest height at the two sites
# plot tree genera that had nests in them at the two sites
# barplots for nest result at the two sites (num successful vs. depredated, etc.)
# map locations for nest sites (NOTE need to dbl check UTM zone first)
# boxplots for supporting branch diameter (could be interesting to know range)

