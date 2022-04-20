library(tidyverse)  # add  data analysis including ggplot
library (raster)       # raster data
library(rasterVis)   # raster visualization     
library(sp)          # spatial data processing          
library(rgdal)       # spatial data processing      
library(RStoolbox)   # Image analysis
library(ggplot2)     # plotting
library(gridExtra)   # plot arrangement
library(lubridate)  # date time
library(ggpubr) # arranging plots
library(ggspatial) # annotation 
library(reshape2)
library(ggstatsplot)

shp <- sf::st_read('data/raster_vector/Oktibbeha.shp')
msp <- sf::st_read('data/raster_vector/Mississippi.shp')
#shp<- fortify(shp)
#msp<- fortify(msp)
Oktibbeha<- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = 'grey')

ggm1<- ggplot() + 
  geom_sf(data = msp, fill = "white") + 
  geom_sf(data = shp, fill = NA, color = "red", size = 1.2) +
  theme_void()
ggm2<- ggplot()+
  geom_sf(data = shp, fill = 'blue', color = "red", size = 1.2)+
  theme(plot.background = element_rect(fill = "#BFD5E3"))

Oktibbeha<- cowplot::ggdraw() +
  cowplot::draw_plot(ggm2) +
  cowplot::draw_plot(ggm1, x = 0.02, y = 0.65, width = 0.35, height = 0.35)

ggsave("Figures_or_Maps/Oktibbeha.png",plot=Oktibbeha,device="png",dpi=500)

# land surface and ndvi plot

library(ggpubr)
ndvi_lst<- cbind(lstdf,ndvidf)
ndvi_lst$date<- NULL
ndvilst<- ggscatter(
  ndvi_lst,
  x='ndvilist',y='lstlist',add='reg.line',
  xlab='Normalized Difference Vegetation Index',
  ylab='Land Surface  Temperature (C)',
  add.params = list(color = "red", fill = "lightgray"), # Customize regression line
  conf.int = TRUE # Add confidence interval
)+stat_cor(
  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
  label.y = 25
)
ggsave("ndvilst.png",plot=ndvilst,device="png",dpi=500)

# land use / land cover classification function 
# it requires training sample, and then train model and predict classes
classify_raster <- function(trainshp, rasterfile){
  # training sample
  sampl<- terra::vect(trainshp)
  # random sample
  ptsampl<- terra::spatSample(sampl,1000,method='random')
  # make matrix
  xy<-as.matrix(geom(ptsampl)[,c('x','y')])
  # extract data
  df<- raster::extract(rasterfile,xy)[,-1]
  # data frame of sample
  sampdata<- data.frame(class=ptsampl$Classname,df)
  # set up model and train model
  #cartmodel<-rpart::rpart(as.factor(class)~., data=sampdata)
  cartmodel<-randomForest::randomForest(as.factor(class)~., data=sampdata)
  # predict classes
  classified<- predict(rasterfile,cartmodel,type='class',na.rm=TRUE)
  lulc<- which.max(classified)
  cls<- names(classified)
  df<- data.frame(id=1:5,class=cls)
  levels(lulc)<- df
  calculated_Area<-as.data.frame(zonal(area(classified),classified,'sum'))
  colnames(calculated_Area)<- c('class','area_km')
  calculated_Area$class<- c('Baresoil','Built up','Roads','Vegetation','Water')
  calculated_Area$area_km<- calculated_Area$area_km/1e+6
  return(c(classified,df,calculated_Area))
  
}
# apply function 
# multiband raster files
classified2020_6<-classify_raster('data/raster_vector/2020_06.shp',raster::stack('data/raster_vector/LC08_L1TP_022037_20200612_20200823_02_T1.tif'))

classified2021_2<-classify_raster('data/raster_vector/2020_06.shp',raster::stack('data/raster_vector/LC08_L1TP_022037_20210223_20210303_02_T1.tif'))
classified2021_6<-classify_raster('data/raster_vector/2020_06.shp',raster::stack('data/raster_vector/LC08_L1TP_022037_20210615_20210622_02_T1.tif'))

classified2022_3<-classify_raster('data/raster_vector/2020_06.shp',raster::stack('data/raster_vector/LC08_L1TP_022037_20220314_20220322_02_T1.tif'))


landusmap<-function(data){
  ggplot(data =data ) +
    geom_raster(aes(x = x, y = y, fill = as.character(layer_value))) + 
    scale_fill_manual(name = "Land cover",
                      values = c("#FFD700", "#FF4040", "#DCDCDC", "#458B00", "#104E8B"),
                      labels = c('Baresoil','Built up','Roads/Highway','Vegetation','Water'),
                      na.translate = FALSE) +
    coord_sf(expand = F,crs=3160) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_rect(fill = "white", color = "black"))+
    # spatial-aware automagic scale bar
    annotation_scale(location = "bl") +
    # spatial-aware automagic north arrow
    annotation_north_arrow(location = "br", which_north = "true",height = unit(1.5, "cm"),width = unit(1, "cm"))
}

# plotting 
p2020_2<- landusmap(as.data.frame(classified2020_6[[1]],xy=TRUE))



# land cover updated
#id class name
#1 vegetation
#2 builtup
#3 baresoil
#4 roads/highway
#5 water

# land cover
classify_raster <- function(trainshp, rasterfile){
  # training sample
  sampl<- terra::vect(trainshp)
  # random sample
  ptsampl<- terra::spatSample(sampl,1000,method='random')
  # make matrix
  xy<-as.matrix(geom(ptsampl)[,c('x','y')])
  # extract data
  df<- raster::extract(rasterfile,xy)[,-1]
  # data frame of sample
  sampdata<- data.frame(class=ptsampl$Classname,df)
  # set up model and train model
  #cartmodel<-rpart::rpart(as.factor(class)~., data=sampdata)
  cartmodel<-e1071::svm(as.factor(class)~., data=sampdata)
  # predict classes
  classified<- predict(rasterfile,cartmodel,type='class',na.rm=TRUE)
  return(classified)
  
}
# apply function 
# multiband raster files
classified2018_3<-classify_raster('W:/Home/hahmad/public/temp/2018_03.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20180303.tif'))
classified2018_6<-classify_raster('W:/Home/hahmad/public/temp/2018_06.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20180607.tif'))

classified2019_3<-classify_raster('W:/Home/hahmad/public/temp/2019_03.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20180607.tif'))
classified2019_6<-classify_raster('W:/Home/hahmad/public/temp/2019_07.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20190306.tif'))

classified2020_3<-classify_raster('W:/Home/hahmad/public/temp/2019_03.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20200221.tif'))
classified2020_6<-classify_raster('W:/Home/hahmad/public/temp/2019_03.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20200612.tif'))

classified2021_3<-classify_raster('W:/Home/hahmad/public/temp/2021_03.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20210223.tif'))
classified2021_6<-classify_raster('W:/Home/hahmad/public/temp/2021_06.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20210615.tif'))

classified2022_3<-classify_raster('W:/Home/hahmad/public/temp/2022_03.shp',raster::stack('D:/home_tower/Home/hahmad/GISdata/TOP20220314.tif'))


landusmap<-function(data){
  ggplot(data =data ) +
    geom_raster(aes(x = x, y = y, fill = as.character(layer_value))) + 
    scale_fill_manual(name = "Land cover",
                      values = c("#FFD700", "#FF4040", "#DCDCDC", "#458B00", "#104E8B"),
                      labels = c('Baresoil','Built up','Roads/Highway','Vegetation','Water'),
                      na.translate = FALSE) +
    coord_sf(expand = F,crs=3160) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_rect(fill = "white", color = "black"))+
    # spatial-aware automagic scale bar
    annotation_scale(location = "bl") +
    # spatial-aware automagic north arrow
    annotation_north_arrow(location = "br", which_north = "true",height = unit(1.5, "cm"),width = unit(1, "cm"))
}

library(ggspatial)
# color: 'Vegetation','Builtup','Baresoil','Roads/Highway','Water'
# plotting 
p2018_3<- landusmap(as.data.frame(classified2018_3,xy=TRUE))
#classified2021_2
p2018_6<- landusmap(as.data.frame(classified2018_6,xy=TRUE))

p2019_3<- landusmap(as.data.frame(classified2019_3,xy=TRUE))
p2019_6<- landusmap(as.data.frame(classified2019_6,xy=TRUE))

p2020_3<- landusmap(as.data.frame(classified2020_3,xy=TRUE))
#classified2021_2
p2020_6<- landusmap(as.data.frame(classified2020_6,xy=TRUE))

p2021_3<- landusmap(as.data.frame(classified2021_3,xy=TRUE))
p2021_6<- landusmap(as.data.frame(classified2021_6,xy=TRUE))

p2022_3<- landusmap(as.data.frame(classified2022_3,xy=TRUE))

setwd("D:\\home_tower\\Home\\hahmad\\Data")
# subplots
landcover<-ggarrange(p2018_3,p2018_6,p2019_3,p2019_6,p2020_3,p2020_6,p2021_3,p2021_6,p2022_3,nrow = 3,ncol=3,labels = c("a","b","c","d","e","f","g","h","i"),common.legend = TRUE)
# save it 
ggsave("landcover.png",plot=landcover,device="png",dpi=300,width = 14, height = 10,units='in')

# area calculation and bar plot

landdf<- as.data.frame(classified2020_6[4])
landdf$AreaKm2020_2<-classified2020_6$area_km
landdf$AreaKm2021_2<-classified2021_2$area_km
landdf$AreaKm2021_6<-classified2021_6$area_km
landdf$AreaKm2022_3<-classified2022_3$area_km
landdf<- melt(landdf)
names(landdf)<- c('landcover','Year','AreaKm')


levels(landdf$Year)[levels(landdf$Year)=='AreaKm2020_2']<-"Feb,2020"
levels(landdf$Year)[levels(landdf$Year)=='AreaKm2021_2']<-"Feb,2021"
levels(landdf$Year)[levels(landdf$Year)=='AreaKm2021_6']<-"Jun 2021"
levels(landdf$Year)[levels(landdf$Year)=='AreaKm2022_3']<-"Mar,2022"


landcoverbar<-ggplot(landdf) +
  aes(x = landcover, fill = landcover, weight = AreaKm) +
  geom_bar() +
  scale_fill_manual(
    values = c(Baresoil = "#FFD700",
               `Built up` = "#FF4040",
               Roads = "#DCDCDC",
               Vegetation = "#458B00",
               Water = "#104E8B")
  ) +
  labs(
    x = "Land cover",
    y = "Area Sqaured Kilometer",
    title = ""
  ) +
  ggthemes::theme_stata() +
  theme(legend.position = "none") +
  facet_wrap(vars(Year))

# save it 
ggsave("Figures_or_Maps/landcoverbar.png",plot=landcoverbar,device="png",dpi=500)




# temperature conversion
band_10<-raster('LC08_L1TP_022037_20200221_20200822_02_T1_B10.TIF')
band_11<-raster('LC08_L1TP_022037_20200221_20200822_02_T1_B11.TIF')
RADIANCE_ADD_BAND_10 <- metadata1$CALRAD$offset[10] # RADIANCE_ADD_BAND 
RADIANCE_ADD_BAND_11<-metadata1$CALRAD$offset[11] # RADIANCE_ADD_BAND 
RADIANCE_MULT_BAND_10<-metadata1$CALRAD$gain[10] # RADIANCE_MULT_BAND
RADIANCE_MULT_BAND_11<-metadata1$CALRAD$gain[11] # RADIANCE_MULT_BAND
#Calculate TOA from DN:
toa_band10 <- calc(band_10, fun=function(x){RADIANCE_MULT_BAND_10 * x + RADIANCE_ADD_BAND_10})
toa_band11 <- calc(band_11, fun=function(x){RADIANCE_MULT_BAND_11 * x + RADIANCE_ADD_BAND_11})

b10_11_k1<-metadata1$CALBT[1]

b10_11_k2<-metadata1$CALBT[2]


#Values from Metafile
K1_CONSTANT_BAND_10 = b10_11_k1[[1]][1] # b10_k1
K1_CONSTANT_BAND_11 = b10_11_k1[[1]][2] #b11_k1

K2_CONSTANT_BAND_10 = b10_11_k2[[1]][1]
K2_CONSTANT_BAND_11 = b10_11_k2[[1]][2]

# calculate LST in kelvin for band 10 and 11

tempK10<-calc(toa_band10,fun=function(x){K2_CONSTANT_BAND_10/log(K1_CONSTANT_BAND_10/x+1)})
tempK11<-calc(toa_band11,fun=function(x){K2_CONSTANT_BAND_11/log(K1_CONSTANT_BAND_11/x+1)})
#raster::writeRaster(masked,filename="W:/Home/hahmad/public/Course/Sprin #2022/WFA8990/milestone #project/data/LC08_L1TP_022037_20200612_20200823_02_T1.tif",bylayer=F, #overwrite=TRUE, format="GTiff") 

lstmap<-function(data){
  ggplot(data =data ) +
    geom_tile(aes(x = x, y = y, fill = layer))+
    scale_fill_gradientn(colors=rev(brewer.pal(11,'Spectral')),
                         name='LST',
                         na.value = 'transparent',
                         labels=(c("-6", "-4" ,"-2" ,"0" , "2" , "4",  "6" , "8" , "10" ,"12", "14")),
                         breaks=seq(-6,14,2),
                         limits=c(-6,14),
                         guide = 'colorbar')+
    coord_equal()+
    xlab("Longitude")+                            # X-axis label
    ylab("Latitude")+                             # Y-axis label
    theme_bw()}
lstmap(as.data.frame(masked,xy=TRUE))
landcover<-ggarrange(p2020_2, p2021_2, p2021_6,p2022_3,nrow = 2,ncol=2,labels = c("a","b","c","d"),label.x=0.9,common.legend = TRUE)
ggsave("Figures_or_Maps/ndvi_2020_06.png",plot=ndviplot,device="png",dpi=500)



# ndvi function
ndvi<-function(raster_data){
  datast<- raster::stack(raster_data)
  names(datast)<-paste0(rep("B", times = 10, length.out = NA, each = 1),1:10)
  ndvi_datast<- (datast$B5-datast$B4/datast$B5+datast$B4)
  ndvi_datast_rprj<- raster::projectRaster(ndvi_datast,crs="+proj=longlat +datum=WGS84 +no_defs")
  reclass<-matrix(c(-Inf,-1,0,
                    -1,0,1,
                    -1,0,1),ncol=3)
  #reclassify the raster using the reclass object - reclass_m
  ndviclassified <- reclassify(ndvi_datast_rprj,
                               reclass)
  return (ndviclassified)
}

ndvi1<-ndvi('data/raster_vector/LC08_L1TP_022037_20200221_20200822_02_T1.tif')
ndvi2<-ndvi('data/raster_vector/LC08_L1TP_022037_20210223_20210303_02_T1.tif')
ndvi3<-ndvi('data/raster_vector/LC08_L1TP_022037_20210615_20210622_02_T1.tif')
ndvi4<-ndvi('data/raster_vector/LC08_L1TP_022037_20220314_20220322_02_T1.tif')


## Set up color gradient with 100 values between 0.0 and 1.0
breaks <- seq(-1, 1, by = 0.1)

NDVImap<-function(data){
  ggplot(data =data ) +
    geom_tile(aes(x = x, y = y, fill = layer))+
    scale_fill_gradientn(colors=rev(brewer.pal(11,'BuGn')),
                         name='NDVI',
                         na.value = 'transparent',
                         labels=(c("-1","-0.8","-0.6 ","-0.4 ","-0.2 ","0 ","0.2 ","0.4 "  ,"0.6 ","0.8 ","1 ")),
                         breaks=seq(-1, 1, by = 0.2),
                         limits=c(-1,1),
                         guide = 'colorbar')+
    coord_equal()+
    xlab("Longitude")+                            # X-axis label
    ylab("Latitude")+                             # Y-axis label
    theme_bw()}
NDVImap(as.data.frame(masked,xy=TRUE))

#png(file='Figures_or_Maps/ndvi_2020_06.png', width=1800, height=1800, res=300)
#print(ndviplot)
#dev.off()
landcover<-ggarrange(p2020_2, p2021_2, p2021_6,p2022_3,nrow = 2,ncol=2,labels = c("a","b","c","d"),label.x=0.9,common.legend = TRUE)
ggsave("Figures_or_Maps/ndvi_2020_06.png",plot=ndviplot,device="png",dpi=500)

# LST Map 
setwd("D:\\home_tower\\Home\\hahmad\\Data")
lstrasters<- list.files(pattern = "(Celcius)")

lstplotlist<-c()
for (i in 1:length(lstrasters)){
  lstdata<-raster::projectRaster(raster(lstrasters[i]),crs="+proj=longlat +datum=WGS84 +no_defs")
  data<- as.data.frame(lstdata,xy=TRUE)
  names(data)<- c('x','y','layer')
  plotting<-ggplot(data=data)+
    geom_tile(aes(x=x,y=y,fill=layer))+
    scale_fill_gradientn(colors=rev(brewer.pal(11,'Spectral')),
                         name='LST',
                         na.value = 'transparent',
                         labels=(c("4",  "8",  "12", "16 ", "20", "24", "28", "32")),
                         breaks=seq(4,32,4),
                         limits=c(-1,32),
                         guide = 'colorbar')+
    coord_equal()+
    labs(xlab=NULL,                           
         ylab=NULL)+
    theme_void()
  lstplotlist[[i]]<-plotting
}

#title = paste0('LST:',substr(lstrasters[i],8,16))
lstmapped<- ggpubr::ggarrange(plotlist = lstplotlist,nrow = 3,ncol = 3,labels = c("a","b","c","d","e","f","g","h","i"),common.legend = TRUE)
#gpubr::ggexport("lstmap.png",plot=lstmapped)
ggsave("lstmap.png",plot=lstmapped,device="png",dpi=500,width = 3,height = 2, units = "in")


# NDVI
setwd("D:\\home_tower\\Home\\hahmad\\Data")
ndvirasters<- list.files(pattern = "(ndvi2)")
ndvirasters

ndviplotlist<-c()
for (i in 1:length(ndvirasters)){
  ndvidata<-raster::projectRaster(raster(ndvirasters[i]),crs="+proj=longlat +datum=WGS84 +no_defs")
  data<- as.data.frame(ndvidata,xy=TRUE)
  names(data)<- c('x','y','layer')
  plotting<-ggplot(data=data)+
    geom_tile(aes(x=x,y=y,fill=layer))+
    scale_fill_gradientn(colors=brewer.pal(6,'RdYlGn'),
                         name='NDVI',
                         na.value = 'transparent',
                         labels=(c("-1 ","-0.6 ","-0.2","0.2 ","0.6 ","1 " )),
                         breaks=seq(-1,1,0.4),
                         limits=c(-1,1),
                         guide = 'colorbar')+
    coord_equal()+
    labs(xlab=NULL,                           
         ylab=NULL)+
    theme_void()
  ndviplotlist[[i]]<-plotting
}


ndvimapped<- ggpubr::ggarrange(plotlist = ndviplotlist,nrow = 3,ncol = 3,labels = c("a","b","c","d","e","f","g","h","i"),common.legend = TRUE)
ggsave("ndvimap.png",plot=ndvimapped,device="png",dpi=300,width = 14, height = 10)


# classification classes
# method [manual/or landcover]
# colors scheme?
# content
# correlation analysis
# cloud cover
