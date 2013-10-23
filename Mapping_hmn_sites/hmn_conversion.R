setwd("C://Users//Anusha/Documents/Dropbox/NASA_Anusha/")

## Reading in csv file with locations
hmndms <- read.csv("dms2dd.csv")
head(hmndms)

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

## Run loop to use convert() function
for(i in 1:n){
  hmndms$latdd[i] <- convert(as.character(hmndms$Latitude_dont[i]))
  hmndms$londd[i] <- convert(as.character(hmndms$Longitude_dont[i]))
}