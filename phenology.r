# code for analyzing the floral phenology data

## Load packages
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(chron)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are multiple .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Excel_CSV_versions/"
setwd(wd)

## Read in csv files
pheno = read.csv("PhenologyData.csv", header = T)
floral = read.csv("FloralCensusData.csv", header = T)
nectar = read.csv("StandingCropData.csv", header = T)

#TODO
#convert date to julian day
## Add a column for julian day, to help plot time series data
for (row in 1:nrow(pheno)){
  line = pheno[row,]
  pheno$julian[row] = julian(line$Month, line$Day, line$Year,
                              origin. = c(month=1, day=1, year=line$Year))
}

#replace plant species with species code? (easier to link between tables in the database w/o using regex)


#slice data may not be great (slice method was deemed not to work well)
#plot proportion of plant that is buds/flowers/fruits
#by species?
#by date?
#by site?

#plot nectar by plant species, site, and date, time of day?
#need to go into data excel and make sure time shows up as time only #FIXME
#convert date to julian day
#volume (calculated from LengthNectar)
#calories
#NOTE: Volume, molarity, calories and LengthNectar are all related measurements. 
#      Probably most interesting to plot Volume and Calories

