# loading R libraries
library (raster)       # raster data
library(rasterVis)   # raster visualization     
library(sp)          # spatial data processing          
library(rgdal)       # spatial data processing      
library(RStoolbox)   # Image analysis
library(ggplot2)     # plotting
library(gridExtra)   # plot arrangement
library(stringr)
# loading raster and meta data 

data<- read.csv("W:/Home/hahmad/public/Download/aquatic_barrier_ranks_2022-03-25 _dam_all/aquatic_barrier_ranks_2022-02-26.csv")
coord<- SpatialPoints(cbind(data$lon,data$lat),proj4string = CRS("+proj=longlat"))
writeOGR(coord,'.','all_damsarpdata',driver = "ESRI Shapefile")
plot(coord)
df<- SpatialPointsDataFrame(coords = coord,data=data)
writeOGR(df,'.','all_damsarpdata',driver = "ESRI Shapefile")

str(data)
# landsat 8-9 OLI/TIRS C2 L1
meta<- list.files('data',pattern = '.txt')
meta

metafiles<- c()
for (i in 1:4){
  mfilesnames<-paste0('meta_',substr(meta[i],18,25))
  metafiles<- c(metafiles, mfilesnames)
}


# set up working directory
setwd("D:\\home_tower\\Home\\hahmad\\R")
# select all .tar file
files<- list.files(pattern = '.tar')
# untar all files using for loop
for (i in 1:9){
  untar(files[i],exdir=substr(files[i],1,40))
  print(paste0('finished untaring /unzipping',files[i]))
  i=i+1}





study<-raster::shapefile('W:/Home/hahmad/public/Course/Sprin 2022/WFA8990/milestone project/data/Oktibbeha.shp')
# check the directory
folders<-list.dirs()
folders
folders<- folders[ !folders %in% folders[1]]
folders<-gsub('./',"",folders)
folders<-paste0("D:/home_tower/Home/hahmad/R/",folders)
folders

metafiles<-c()
# find 'MLT' containing files in each  folders  and read the files each files as readMeta
for (i in 1:length(folders)){
  files<-list.files(folders[i],pattern = '(MTL)+(.txt)',full.names = TRUE)
  metafiles<-append(metafiles,files)
}
# set up working directory
# empty vector of ndvi
ndvilist<-c()
setwd("D:\\home_tower\\Home\\hahmad\\R")
years<-list.dirs()
years<- years[ !years %in% years[1]]
years<-substr(years,20,27)

for (i in 1:length(folders)){
  # go to first folder and find all .TIF files
  setwd(folders[i])
  files4<-raster(list.files(pattern = 'B4.TIF',full.names = TRUE))
  files5<-raster(list.files(pattern = 'B5.TIF',full.names = TRUE))
  study<- spTransform(study, proj4string(files4)) 
  # cropped
  cropped4<- crop(files4,extent(study))
  # masked
  masked4<- mask(cropped4,study)
  # cropped
  cropped5<- crop(files5,extent(study))
  # masked
  masked5<- mask(cropped5,study)
  # divide b4 with b5
  ndvi <- overlay(x = masked4, y = masked5, fun = function(x,y) (y-x)/(y+x))
  writeRaster(ndvi,paste0('D:\\home_tower\\Home\\hahmad\\Data\\','ndvi',substr(names(files4),18,25)), overwrite=TRUE,format="GTiff")
  # calculate NDVI mean
  ndvi_mean<- cellStats(ndvi,stat="mean")
  # append to ndvilist and with respective names of rasterfiles
  ndvilist<- c(ndvilist,ndvi_mean)
}
# ndvi as dataframe
ndvidf<- as.data.frame(ndvilist)
ndvidf$date<-years

write.csv(ndvidf,'D:\\home_tower\\Home\\hahmad\\Data\\ndvi.csv',row.names = FALSE)

setwd("D:\\home_tower\\Home\\hahmad\\R")
lstlist<- c()
# temperature
for (i in 1:length(folders)){
  # go to first folder and find all .TIF files
  setwd(folders[i])
  metafiles<-list.files(pattern = '(MTL)+(.txt)',full.names = TRUE)
  # read meta data
  meta<-readMeta(metafiles)
  b10_11_k1<- meta$CALBT[1]
  b10_11_k2<- meta$CALBT[2]
  K1_CONSTANT_BAND_10 = b10_11_k1[[1]][1] # b10_k1
  K2_CONSTANT_BAND_10 = b10_11_k2[[1]][1]
  files10<-raster(list.files(pattern = 'B10.TIF',full.names = TRUE))
  study<- spTransform(study, proj4string(files10)) 
  # cropped
  cropped10<- crop(files10,extent(study))
  # masked
  masked10<- mask(cropped10,study)
  # B10 conversio  DN to Top of atmopsheric reflectance
  newB10<- calc(masked10,fun = function(x){meta$CALRAD$gain[j]*x+meta$CALRAD$offset[j]})
  # thermal conversion
  #newTOPk<- calc( newB10,fun = function(x){meta$CALBT[2][[1]][2]/log(meta$CALBT[1][[1]][2]/x+1)})
  newTOPk<-calc(newB10,fun=function(x){K2_CONSTANT_BAND_10/log(K1_CONSTANT_BAND_10/x+1)})
  # kelvin to celcius
  newCelcius<- calc(newTOPk,fun = function(x){x-273.15})
  writeRaster(newCelcius,paste0('D:\\home_tower\\Home\\hahmad\\Data\\','Celcius',substr(names(files10),18,25)), overwrite=TRUE,format="GTiff")
  # calculate NDVI mean
  lst_mean<- cellStats(newCelcius,stat="mean")
  # append to ndvilist and with respective names of rasterfiles
  lstlist<- c(lstlist,lst_mean)
}

# ndvi as dataframe
lstdf<- as.data.frame(lstlist)
lstdf$date<-years
# write to csv
write.csv(lstdf,'D:\\home_tower\\Home\\hahmad\\Data\\lstdf.csv',row.names = FALSE)


# TOP conversion
setwd("D:\\home_tower\\Home\\hahmad\\R")
# temperature
for (i in 1:length(folders)){
  # go to first folder and find all .TIF files
  setwd(folders[i])
  metafiles<-list.files(pattern = '(MTL)+(.txt)',full.names = TRUE)
  # read meta data
  meta<-readMeta(metafiles)
  files4<-raster(list.files(pattern = 'B4.TIF',full.names = TRUE))
  files3<-raster(list.files(pattern = 'B3.TIF',full.names = TRUE))
  files2<-raster(list.files(pattern = 'B2.TIF',full.names = TRUE))
  # B10 conversion  DN to Top of atmopsheric reflectance
  fileop4<-calc(files4,fun = function(x){meta$CALRAD$gain[4]*x+meta$CALRAD$offset[4]})
  fileop3<-calc(files3,fun = function(x){meta$CALRAD$gain[3]*x+meta$CALRAD$offset[3]})
  fileo2<-calc(files2,fun = function(x){meta$CALRAD$gain[2]*x+meta$CALRAD$offset[2]})
  rasterstacked<- raster::stack(fileop4,fileop3,fileo2)
  study<- spTransform(study, proj4string(files4)) 
  # cropped
  cropped<- crop(rasterstacked,extent(study))
  # masked
  masked<- mask(cropped,study)
  
  writeRaster(masked,paste0('D:\\home_tower\\Home\\hahmad\\GISdata\\','TOP',substr(names(files4),18,25)), overwrite=TRUE,format="GTiff")
  
}

