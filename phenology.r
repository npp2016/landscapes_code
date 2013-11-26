# code for analyzing the floral phenology data

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
pheno = read.csv("PhenologyData.csv", header = T)

#plot proportion of plant that is buds/flowers/fruits
#by date?
