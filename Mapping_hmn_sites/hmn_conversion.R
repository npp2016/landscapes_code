## Code to convert [degree °, minutes ', seconds "N or W] to decimal degrees
## You don't need to convert the N and W to positive and negative.
# I would suggest doing some quality checks with some website conversion after runnin the code

## Anusha Shankar and Sarah Supp


setwd("D:\\Dropbox\\NASA_Anusha\\hmn map data")

## Reading in csv file with locations
dms <- read.csv("dms2dd.csv")
head(dms)

## Didn't use this. Save latitude and longitude as character vectors
# latdms <- as.character(hmndms$Latitude_dont)
# londms <- as.character(hmndms$Longitude_dont)

## Function to convert degree minute seconds to decimal degrees
convert <-function(coord){
  tmp1 <- strsplit(coord,"°")
  tmp2 <- strsplit(tmp1[[1]][2],"'")
  tmp3 <- strsplit(tmp2[[1]][2],"\"")
  dec <- c(as.numeric(tmp1[[1]][1]),as.numeric(tmp2[[1]][1]),as.numeric(tmp3[[1]]))
  c <-abs(dec[1])+dec[2]/60+dec[3]/3600
  c <-ifelse(dec[1]<0,-c,c)
  return(c)
}

n <- length(dms)

## Run loop to use convert() function
for(i in 1:n){
  dms$latdd[i] <- convert(as.character(dms$Latitude_dont[i]))
  dms$londd[i] <- convert(as.character(dms$Longitude_dont[i]))
}
## You get errors 
##"In convert(as.character(dms$Longitude_dont[i])) : NAs introduced by coercion"
# I think these can be ignored.

# Check if this worked
head(dms$Latitude_dont)
head(dms$latdd)
