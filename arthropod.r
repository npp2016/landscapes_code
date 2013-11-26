# code for analyzing the insect data

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

# plot number of arthropods per site
# plot insects across family/group