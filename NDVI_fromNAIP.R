# Purpose: calculate NDVI for a zillion images (or fewer). 
# Author: Tina Cormier
# Date: 3/26/14

library(raster)
###################### User INPUTS ########################

#indir containing only images to be NDVI'd
imgdir <- "C:/Share/tcormier/hummingbirds/imagery/NAIP/regional/tiles/tif/"
#image extensions
img.ext <- ".tif$"
#Specify output directory
outdir <- "C:/Share/tcormier/hummingbirds/imagery/NAIP/regional/tiles/ndvi/"

#which bands are the IR and Red bands?
IRband <- 4
REDband <- 1
######################## FUNCTIONS ########################

calcNDVI <- function(img, IRband, REDband) {
  NDVI <- (img[[IRband]] - img[[REDband]])/(img[[IRband]] + img[[REDband]])
  return(NDVI)
}#end calcNDVI function

######################### SCRIPT ##########################
#get list of images
pattern <- paste("*", img.ext, sep="")
imgs <- list.files(imgdir, pattern=pattern, full.names=T)

for (i in imgs) {
  print(date())
  print(i)
  outimg <- paste(outdir, unlist(strsplit(basename(i), "\\."))[1], "_ndvi.tif", sep="")
  print("reading image. . .")
  img <- brick(i)
  print("calculating NDVI")
  ndvi <- calcNDVI(img, IRband, REDband)
  
  #multiply by 100 and round off decimal - avoid writing out a huge float ras.
  ndvi100 <- round(ndvi*100)
  
  writeRaster(ndvi100,filename=outimg,dataType="INT4s", overwrite=T)
}#end img for loop
