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
## Anusha doing a test commit. I will delete this
## There are multiple .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
setwd(wd)

## Read in csv files
pheno = read.csv("PhenologyData.csv", header = T)
#Not using floral yet
floral = read.csv("FloralCensusData.csv", header = T)
nectar = read.csv("StandingCropData.csv", header = T)

##-------------------- FUNCTIONS

JulianConversion <- function(dat) {
  #converts date to julian day, so it can be easily plotted
  for (row in 1:nrow(dat)){
    line <- dat[row,]
    dat$julian[row] <- julian(line$Month, line$Day, line$Year,
                              origin. <- c(month=1, day=1, year=line$Year))
  }
  return(dat$julian)
}

SpeciesCode <- function(dat) {
  Gencode <- toupper(substr(dat$PlantSpecies, 1,2))
  Spcode <- 0
  for (i in 1:length(dat$PlantSpecies)) {
    Spcode[i] <- toupper(substr(sapply(strsplit(as.character(dat$PlantSpecies[i]), " "), "[[", 2), 1,2))
    dat$Species[i] <- paste(Gencode[i],Spcode[i], collapse="", sep="")
  }
  return(dat$Species)
}


##-------------------- CLEAN THE DATA
##### AS: NOTE to self- check metadata for how they calculated Totals ------##

## subset only data from the two main landscapes
pheno <- subset(pheno, Site == "HC" | Site == "PL/SC")
nectar <- subset(nectar, Site == "HC" | Site == "PL/SC")
# Removing rows with only Genus and no species name
pheno <- pheno[-c(which(pheno$PlantSpecies=="Cersium")),]

## Add a column for julian day, to help plot time series data

pheno$julian <- JulianConversion(pheno)
nectar$julian <- JulianConversion(nectar)

#replace plant species with species code? (easier to link between tables in the database w/o using regex)
# @Sarah: Did you mean in the future we should advocate using species codes? Or use them now?
# AS: Tried my hand at a strsplit anyway, made species codes!! Happy.
# @nushiamme: I meant that we should change the data in the database to use species code (we should discuss why later). But kudos for getting this to work!

pheno$Species <- SpeciesCode(pheno)  #FIXME: Ideally we want this to work without throwing out Cersium (unless you have good reason for not including it)
nectar$Species <- SpeciesCode(nectar)

#Melt dataframes by species, site and date
m.pheno <- melt(pheno, id.vars=c("Site","Species","julian"), 
                measure.vars=c("TotalBuds", "TotalFlowers", "TotalFruits"))
m.nectar <- melt(nectar, id.vars=c("Site", "Species", "julian"), 
                 measure.vars=c("Calories"))

##-------------------- PLOT THE DATA
#slice data may not be great (slice method was deemed not to work well)
# AS: So should we subset that out? - FIXME
# @nushiamme: This is a question for Susan. I don't think we can/should get rid of the data, but it means all results for this dataset are questionable.

# Phenology data
# plot proportion of plant that is buds/flowers/fruits. 
# TODO -------- Would this be more useful as proportions? I have plotted absolute numbers.
# @nushiamme: I think total is good here - we want to know how many flowers are on the landscape. Proportion would be more interesting when comparing phenology or timing for species?
#by species
phenol.sp <- ggplot(m.pheno, aes(x=Species, y=value, fill=Species)) + geom_boxplot() + 
  ylab("Count") + facet_grid(~variable) + theme_bw() +
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1)) 
phenol.sp

#by date
# @nushiamme: I think this one could be interesting to also plot as proportion - can watch phenology change over time.
phenol.date <- ggplot(m.pheno, aes(x=julian, y=value)) + stat_smooth(method='lm') +
  geom_point(size=2, col="dark green") + ylab("Count") + theme_bw() + facet_grid(~variable)
phenol.date

#by site
phenol.site <- ggplot(m.pheno, aes(x=Site, y=value)) + geom_boxplot() + 
  ylab("Count") + facet_grid(~variable) + theme_bw() +
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
phenol.site

## Nectar data
#volume (calculated from LengthNectar)
vol.site <- ggplot(nectar, aes(Site, Volume)) + geom_boxplot()
vol.site

#TODO: This plot is cool. But add a second one that facets by site.
vol.date.site <- ggplot(nectar, aes(julian, Volume)) + geom_point(aes(col=Species), alpha = 0.7) + 
  facet_grid(~Site)
vol.date.site

vol.species <- ggplot(nectar, aes(Species, Volume)) + geom_boxplot()
vol.species

## AS: Interesting small changes between calories and nectar. Trade-off for hummingbirds between
# nectar calories and water content? I wonder if resource use switches in dry seasons based
# on nectar volume more than calories. Energy is important, but how important is water?

#plot Calories by plant species, site, and date, time of day
cal.site <- ggplot(nectar, aes(Site, Calories)) + geom_boxplot()
cal.site
cal.date <- ggplot(nectar, aes(julian, Calories)) + geom_point(aes(col = Species), alpha = 0.75)
cal.date
cal.species <- ggplot(nectar, aes(Species, Calories)) + geom_boxplot()
cal.species

#relationship between volume and calorie content. 
# Note that Chilopsis generally has a different slope than the other plants
cal.volume <- ggplot(nectar, aes(Volume, Calories)) + 
  geom_point(aes(color=Species), alpha = 0.5, size=3) + theme_bw() +
  scale_color_brewer(palette="Set1")
cal.volume