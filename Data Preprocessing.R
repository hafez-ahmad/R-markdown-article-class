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
#un zipping
for (i in 1:5){
  untar(list.files()[i],exdir=substr(list.files()[i],1,40))
  print(paste0('finished untaring /unzipping',list.files()[i]))}



meta<- list.files('data/raster_vector',pattern = '.txt')
#empty vector
metafiles<- c()
# append all names in the metafile vector
for (i in 1:4){
  mfilesnames<-paste0('meta_',substr(meta[i],18,25))
  metafiles<- c(metafiles, mfilesnames)
}

print(metafiles)

# read meta data 
meta_20191203<- readMeta('data/raster_vector/LC08_L1TP_022037_20220314_20220322_02_T1_MTL.txt')