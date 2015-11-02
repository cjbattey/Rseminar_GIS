#######################################################################
##################### GIS in R - R Seminar Fall 2015 ##################
#######################################################################
setwd("~/Desktop/Rseminar_GIS/")
#install.packages('ggplot2')
#install.packages('sp')
#install.pacakges('rgdal')
#install.packages('raster')
#install.packages('rgeos')
#install.packages('RColorBrewer')
library(raster);library(ggplot2);library(RColorBrewer)

################################ Shapefiles, rasters, and simple GIS data summaries in R ##################################
#R is great for *some* GIS tasks. If you're exploring data and want to quickly demo a visalization or click around a map, try
#qGIS or arcMap. If you're working with a bunch of files, want to automate repetitive tasks, or just love ggplot2, R is
#a good choice.

#Reading in shapefiles (range maps of the two most common hummingbirds in Pacific NW - Anna's and Rufous)
anhu <- shapefile("./ranges/Calypte_anna_22688199.shp")
ruhu <- shapefile("./ranges/Selasphorus_rufus_22688296.shp")

#check out the data
summary(anhu)
head(anhu@data)

#plot with base graphics
plot(anhu)

#Looks weird. The map has separate polygons for breeding, postbreeding, and wintering ranges in the same shapefile. 
#Filter polygons by the associated data with the "[ ]" function.  
anhu.b <- anhu[anhu@data$SEASONAL == 1,]
ruhu.b <- ruhu[ruhu@data$SEASONAL == 2,]
plot(anhu.b)

#change projections with spTransform. 3395 is a mercator projection, 4087 is equidistant cylindrical. 
anhu.proj <- spTransform(anhu.b, CRS("+init=epsg:3395"))

#standard vector functions are all available 
plot(union(anhu.b,ruhu.b))
plot(intersect(anhu.b,ruhu.b))

#Buffering. If the spatial object is projected in a lat/long system, the unit of width is meters. Otherwise, map units. 
plot(buffer(anhu.proj,width=1e4))

####Plotting shapefiles with ggplot2
#Convert sp objects to dataframes with fortify()
anhu.df <- fortify(anhu.b)
ruhu.df <- fortify(ruhu.b)
#load in a country outlines file and crop to the plot extent (ggplot does not deal well with polygons including points 
#outside the plotting window extents)
map <- shapefile("cntry06/cntry06.shp")
admin <- crop(map,c(-150,-105,25,65))
admin.df <- fortify(admin)
#plot with geom_polygon()
ggplot()+coord_map(xlim=c(-150,-105),ylim=c(25,65))+theme_bw()+
  geom_polygon(data=anhu.df,aes(x=long,y=lat,group=group),fill="violet")+
  geom_polygon(data=ruhu.df,aes(x=long,y=lat,group=group),fill="orangered",alpha=0.7)+
  geom_polygon(data=admin.df,aes(x=long,y=lat,group=group),fill=NA,col="black")
#pretty! 

####### Regional species diversity ######
#let's build a raster where the cell value equals the number of species within a clade that occur in a given area. 
##build an empty raster on a lat/long grid at 10min resolution
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=1/6, vals=0)

#test out rasterizing with just the anhu range.
anhu.r <- rasterize(anhu.b,west)
plot(anhu.r)

#summarizing multiple shapefiles: loop over all the shapefiles in the rangeMaps folder, rasterize, add the rasters.  
files <- list.files("./ranges/")
files <- files[grep(".shp",files)]
n.species <- crop(r,c(-150,-30,-42,55))

for (i in files){
  range <- shapefile(paste("./ranges/",i,sep=""))
  range.r <- rasterize(range,west,1,background=0)
  n.species <- range.r+n.species
}

plot(n.species)
#writeRaster(n.species,"~/Desktop/beeHum_div_10min.tif")


###########################################################################
############## loop #2: visualizing hummingbird migration #################
###########################################################################

#read in rufous hummingbird eBird reports and a shorelines map
ruhu <- read.delim("ebd_rufhum_relNov-2014/ebd_rufhum_relNov-2014.txt")
ruhu$date <- as.Date(ruhu$OBSERVATION.DATE,"%m/%d/%Y")
ruhu$month <- as.numeric(substr(ruhu$date,6,7))
ruhu$monthName <- months(ruhu$date)
ruhuW <- subset(ruhu,LONGITUDE < -96)
map <- crop(shapefile("cntry06/cntry06.shp"),c(-145,-60,10,62))

  
#read in an effort raster (total # reports per grid cell at 10 min resolution)
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=1, vals=0)
effort <- crop(resample(raster("effort.tif"),r),c(-145,-60,10,62))
r.ruhu <- crop(r,c(-145,-60,10,62))

##loop over months: subset occurrence records, rasterize, divide by effort, write to file
#as a static figure
old.par <- par()
pdf(paste("./ruhu_freq/freq_",i,".pdf",sep="",width=6.5,height=9))
par(mfrow=c(3,4),mar=c(2.5,3.5,1,.5),mgp=c(1.5, .5, 0),oma=c(0,0,3,0))
for(i in c(1:12)){
  a <- subset(ruhuW,month == i)
  locs <- SpatialPoints(data.frame(a$LONGITUDE,a$LATITUDE))
  log.frequency <- log(rasterize(locs,r.ruhu,fun="count")/effort)
  plot(log.frequency,col=brewer.pal(n=6,name="YlOrRd"),legend=F,axes=F,breaks=c(-5,-1,0,1,2,3),
       main=paste(a$monthName[1]),xaxs="i", yaxs="i")+plot(map,col=NA,add=TRUE)+mtext("Rufous Hummingbird Report Frequency", 
                                                                                      outer = TRUE,side = 3,cex = 1.2,line = 1)
}
dev.off()
par(old.par)

#or output new pdf's for each month, then gif it up in photoshop
for(i in c(1:12)){
  a <- subset(ruhuW,month == i)
  locs <- SpatialPoints(data.frame(a$LONGITUDE,a$LATITUDE))
  log.frequency <- log(rasterize(locs,r.ruhu,fun="count")/effort)
  pdf(paste("./ruhu_freq/freq_",i,".pdf",sep=""))
  plot(log.frequency,col=brewer.pal(n=6,name="YlOrRd"),legend=F,axes=T,breaks=c(-5,-1,0,1,2,3))+plot(map,col=NA,add=TRUE)
  dev.off()
}





