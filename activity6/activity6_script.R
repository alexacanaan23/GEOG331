#Alexa Canaan
#Activity 6

#LOAD R PACKAGES
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#READ IN VECTOR DATA - GLACIER OUTLINES

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
plot(g1966,axes="TRUE")
g1998 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
plot(g1998,axes="TRUE")
g2005 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
plot(g2005,axes="TRUE")
g2015 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
plot(g2015,axes="TRUE")
str(g2015)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

#vector object
g1966@proj4string

#get polygon coordinates
g1966@polygons[[1]]@Polygons[[1]]

#QUESTION 1 - NO CODE

#WORKING WITH RASTER DATA
#read in rgb imagery from landsat
redL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)
