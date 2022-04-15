# loading R libraries
library (raster)       # raster data
library(rasterVis)   # raster visualization     
library(sp)          # spatial data processing          
library(rgdal)       # spatial data processing      
library(RStoolbox)   # Image analysis
library(ggplot2)     # plotting
library(gridExtra)   # plot arrangement


#loading raster and meta data
#Landsat 8-9 OLI/TIRS C2 L1
# set up working directory
setwd("D:\\home_tower\\Home\\hahmad\\R")
# select all .tar file
files<- list.files(pattern = '.tar')
# untar all files using for loop
for (i in 1:9){
  untar(files[i],exdir=substr(files[i],1,40))
  print(paste0('finished untaring /unzipping',files[i]))
  i=i+1}
# check the directory
folders<-list.dirs()
# empty vector
metafiles<-c()
# find '..MLT..' containing files names in folders
for (i in 1:length(folders)){
  files<-list.files(folders[i],pattern = '(MTL)+(.txt)',full.names = TRUE)
  # append files to metafiles
  metafiles<-append(metafiles,files)
  
 
  }
## study area shapefile
study<-raster::shapefile('W:/Home/hahmad/public/Course/Sprin 2022/WFA8990/milestone project/data/Oktibbeha.shp')
# read meta data
# read meta data from metafiles
for (i in 1:length(metafiles)){
  # go to first folder and find all .TIF files
  setwd(folders[i])
  files<-list.files(pattern = '.TIF')
  metfile<-list.files(pattern = '(MTL)+(.txt)',full.names = TRUE)
  # read meta data
  meta<-readMeta(metfile)
  print(files)
  # read each files
  for (j in 1:length(files)){
    # read each files
    rasterfile<-raster(files[j])
    # TOP conversion each raster
    newTOP<- calc(rasterfile,fun = function(x){meta$CALRAD$gain[j]*x+meta$CALRAD$offset[j]})
    # cropped
    cropped<- crop(newTOP,extent(study))
    # masked
    masked<- mask(cropped,study)
    # write newTOP 
    writeRaster(masked,files[j],bylayer=F, overwrite=TRUE, format="GTiff")
    if (j==10){ #k2/k1
      newTOPk<- calc(newTOP,fun = function(x){meta$CALBT[2][[1]][1]/log(meta$CALBT[1][[1]][1]/x+1)})
      # kelvin to celcius
      newCelcius<- calc(newTOPk,fun = function(x){x-273.15})
      # cropped
      croppednewCelcius<- crop(newCelcius,extent(study))
      # masked
      maskedCelcius<- mask(croppednewCelcius,study)
      writeRaster(maskedCelcius,file = paste0(files[j],"C"),bylayer=F, overwrite=TRUE, format="GTiff")
    }
    if (j==11){ #k2/k1
      newTOPk<- calc(newTOP,fun = function(x){meta$CALBT[2][[1]][2]/log(meta$CALBT[1][[1]][2]/x+1)})
      # kelvin to celcius
      newCelcius<- calc(newTOPk,fun = function(x){x-273.15})
      # cropped
      croppednewCelcius<- crop(newCelcius,extent(study))
      # masked
      maskedCelcius<- mask(croppednewCelcius,study)
      writeRaster(maskedCelcius,file = paste0(files[j],"C"),bylayer=F, overwrite=TRUE, format="GTiff")
    }
  }
}



