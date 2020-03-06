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
g1998 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

str(g2015)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

#vector object
g1966@proj4string

#QUESTION 1 - NO CODE
