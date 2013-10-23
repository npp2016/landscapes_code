# This code is to run exploratory analyses on observational data 
# from the Summer 2013 field study in Patagonia, AZ
# This code deals with the vegetation structure data. Student to contact for data questions: Marisa
# Code developed by: Anusha Shankar, Sarah Supp, and Catherine Graham

## Load packages
library(ggplot2)
library(reshape)

##### CHOOSE WORKING DIRECTORY (uncomment the one you like)
## There are four separate .csv files in this folder
wd = "C://Users/Anusha/Documents/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Vegetation_Structure_data/"
#wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/Pasantias_Patagonia_2013/Final_Databases_2013/Vegetation_Structure_data/"
setwd(wd)

## Read in Canopy and Ground cover .csv
canopy <- read.csv("CanopyAndGroundCover.csv")

## Cleaning and aggregating data
