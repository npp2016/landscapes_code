# code for analyzing the floral phenology data. Contact:
# From the Summer 2013 field study in Patagonia, AZ
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(chron)
library(R.utils)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are multiple .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
setwd(wd)

## Read in csv files
pheno = read.csv("PhenologyData.csv", header = T)
floral = read.csv("FloralCensusData.csv", header = T)
nectar = read.csv("StandingCropData.csv", header = T)

## Add a column for julian day, to help plot time series data
for (row in 1:nrow(pheno)){
  line <- pheno[row,]
  pheno$julian[row] <- julian(line$Month, line$Day, line$Year,
                              origin. <- c(month=1, day=1, year=line$Year))
}

## subset only data from the two main landscapes
pheno <- subset(pheno, Site == "HC" | Site == "PL/SC")
# Removing rows with only Genus and no species name
pheno <- pheno[-c(which(pheno$PlantSpecies=="Cersium")),]

#replace plant species with species code? (easier to link between tables in the database w/o using regex)
# @Sarah: Did you mean in the future we should advocate using species codes?
# AS: Tried my hand at a strsplit anyway, made species codes!! Happy.
Gencode <- substr(pheno$PlantSpecies, 1,2)
Spcode <- 0
for (i in 1:length(pheno$PlantSpecies)) {
  Spcode[i] <- capitalize(substr(sapply(strsplit(as.character(pheno$PlantSpecies[i]), " "), "[[", 2), 1,2))
  pheno$Species[i] <- paste(Gencode[i],Spcode[i], collapse="", sep="")
}

#Melt pheno dataframe by species
m.pheno <- melt(pheno, id.vars="Species", measure.vars=c("TotalBuds", "TotalFlowers", "TotalFruits"))

#By date
m.pheno_date <- melt(pheno, id.vars="julian", 
                     measure.vars=c("TotalBuds", "TotalFlowers", "TotalFruits"))

## Melt counts by day and site
## There must be a more efficient way of doing multiple aggregates!
agg.buds <- aggregate(pheno$TotalBuds, by=list(pheno$julian, pheno$Species, pheno$Site), FUN=sum)
names(agg.buds) <- c("Date", "Species", "Site", "TotalBuds")
agg.flowers <- aggregate(pheno$TotalFlowers, by=list(pheno$Site, pheno$julian, pheno$Species), FUN=sum)
names(agg.flowers) <- c("Date", "Species", "Site", "TotalFlowers")
agg.fruits <- aggregate(pheno$TotalFruits, by=list(pheno$Site, pheno$julian, pheno$Species), FUN=sum)
names(agg.fruits) <- c("Date", "Species", "Site", "TotalFruits")
agg.pheno <- agg.buds
agg.pheno <- merge(agg.buds, agg.flowers, agg.fruits, by=intersect())


richnesstime <- aggregate(sppdata$Species.Code, by=list(sppdata$julian, sppdata$Session, sppdata$Site),
                          FUN=function(u) length(unique(u)))

#slice data may not be great (slice method was deemed not to work well)
# AS: So should we subset that out? - FIXME

#plot proportion of plant that is buds/flowers/fruits
#by species
phenol.sp <- ggplot(m.pheno, aes(x=Species, y=value, fill=Species)) + geom_boxplot() + 
  ylab("Count") + facet_grid(~variable) + theme_bw() +
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1)) 
phenol.sp

#by date?
phenol.date <- ggplot(m.pheno_date, aes(x=julian, y=variable)) + coord_flip() + geom_point() + 
  ylab("Count") + theme_bw()
+ theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1)) 
phenol.date

#by site?

#plot nectar by plant species, site, and date, time of day?
#need to go into data excel and make sure time shows up as time only #FIXME
#convert date to julian day
#volume (calculated from LengthNectar)
#calories
#NOTE: Volume, molarity, calories and LengthNectar are all related measurements. 
#      Probably most interesting to plot Volume and Calories

