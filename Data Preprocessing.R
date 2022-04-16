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

folders<-gsub('./',"",folders)

folders<-paste0("D:/home_tower/Home/hahmad/R/",folders)

# read meta data from metafiles
for (i in 1:length(metafiles)){
  # go to first folder and find all .TIF files
  setwd(folders[i])
  files<-list.files(pattern = '.TIF')
  metfile<-list.files(pattern = '(MTL)+(.txt)',full.names = TRUE)
  # read meta data
  meta<-readMeta(metfile)
  print('finished reading meta data')
  # read each files
  for (j in 1:length(files)){
    # read each files
    rasterfile<-raster(files[j])
    # if names(files) contains "B10.TIF" or "B11.TIF" then do the following
    if(str_detect(names(rasterfile),"B10") | str_detect(files[j],"B11")){
      # B10 conversio  DN to Top of atmopsheric reflectance
      newB10<- calc(rasterfile,fun = function(x){meta$CALRAD$gain[j]*x+meta$CALRAD$offset[j]})
      # thermal conversion
      newTOPk<- calc( newB10,fun = function(x){meta$CALBT[2][[1]][2]/log(meta$CALBT[1][[1]][2]/x+1)})
      # kelvin to celcius
      newCelcius<- calc(newTOPk,fun = function(x){x-273.15})
      print('finished B10 conversion')
      # projection change as study area
      study<- spTransform(study, proj4string(newCelcius)) 
      # cropped
      cropped<- crop(newCelcius,extent(study))
      # masked
      masked<- mask(cropped,study)
      # write masked raster
      writeRaster(masked,paste0(files[j],'.tif'),bylayer=F, overwrite=TRUE)
      print(paste0('Writing B10 ',files[j]))
      # conversion DN to top of atmopsheric reflectance
      newTOP11<- calc(rasterfile,fun = function(x){meta$CALRAD$gain[j]*x+meta$CALRAD$offset[j]})
      # Top of atmopsheric reflectance to  kelvin conversion
      newTOPk<- calc(newTOP11,fun = function(x){meta$CALBT[2][[1]][2]/log(meta$CALBT[1][[1]][2]/x+1)})
      # kelvin to celcius
      newCelcius<- calc(newTOPk,fun = function(x){x-273.15})
      print('finished B11 conversion')
      study<- spTransform(study, proj4string( newCelcius)) 
      # cropped
      cropped<- crop( newCelcius,extent(study))
      # masked
      masked<- mask(cropped,study)
      # write masked 
      writeRaster(masked,paste0(files[j],".tif"),bylayer=F, overwrite=TRUE)
      print(paste0('Writing B11 ',files[j]))
    } else{
      # TOP conversion each raster
      newTOP<- calc(rasterfile,fun = function(x){meta$CALRAD$gain[j]*x+meta$CALRAD$offset[j]})
      print('finished TOP conversion')
      study<- spTransform(study, proj4string(newTOP)) 
      # cropped
      cropped<- crop(newTOP,extent(study))
      # masked
      masked<- mask(cropped,study)
      # write newTOP 
      writeRaster(masked,paste0(files[j],".tif"),bylayer=F, overwrite=TRUE)
      print(paste0('Writing TOP ',files[j]))
    }
  }
}



