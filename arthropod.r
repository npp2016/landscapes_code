# Code for analyzing the insect data. Contact: Omar and Anusha
# From the Summer 2013 field study in Patagonia, AZ
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)
library(RColorBrewer)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are multiple .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
setwd(wd)

## Read in csv files
arthropods = read.csv("InsectCounts_Anusha.csv", header = T)

## Melt and aggregate data
##--- QUESTION: Should eggs and unknown remain in? #####
m.arthro <- melt(arthropods, id.vars=c("Site", "VegetationType", "Month", "NumPlantsBeaten"), 
     measure.vars= c("Acari", "Aranea", "Coleoptera", "Crustacea", "Diptera", "Eggs",
                     "Grylloblattaria", "Hemiptera", "Hymenoptera", "Lepidoptera",
                     "Mantodea", "Neuroptera", "Orthoptera", "Phasmida", "Pscocoptera",
                     "Thysanoptera", "Unknown", "Zoraptera"), na.rm=T)


agg.arthro <- aggregate(x=m.arthro$value, by=list(m.arthro$Site), FUN=sum)
names(agg.arthro) <- c("Site", "Count")
agg.arthro

## Scale counts by number of samples collected (measure of sampling effort)
## I want to scale it by number of plants beaten (NumPlantsBeaten), but half of them are NAs.
## FIXME: FIND OUT WHY

hc <- length(arthropods$Site[arthropods$Site=="HC"])
pl <- length(arthropods$Site[arthropods$Site=="PL/SC"])
samples <- c(hc, pl)
agg.arthro$scaled_count <- agg.arthro$Count/samples

# plot number of arthropods per site
ggplot(agg.arthro, aes(x=Site, y=Count, fill=Site)) + geom_bar(stat="identity") + theme_bw()
ggplot(agg.arthro, aes(x=Site, y=scaled_count, fill=Site)) + geom_bar(stat="identity") + theme_bw()

# plot insects across Order
insect_order <- ggplot(m.arthro, aes(x=variable, y=value)) + geom_point(size=1) +
  theme_bw() +  xlab("Order") + ylab("Count") +
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1)) 
insect_order