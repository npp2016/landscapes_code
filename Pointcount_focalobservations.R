setwd("C://Users//Anusha//Documents//Dropbox//Hummingbirds//Pasantias_Patagonia_2013//Final_Databases_2013//Excel_CSV_versions")

## Reading in point count and focal observation data
pointdata <- read.table("FocalObservationPointCountData.txt", na.strings="NA", sep=",", header=T)

