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
g1966 <- readOGR("~/Desktop/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_1966.shp")
plot(g1966,axes="TRUE")

g1998 <- readOGR("~/Desktop/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_1998.shp")
plot(g1998,axes="TRUE")

g2005 <- readOGR("~/Desktop/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_2005.shp")
plot(g2005,axes="TRUE")

g2015 <- readOGR("~/Desktop/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_2015.shp")
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

#spplot maps vector data with different colors
spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#WORKING WITH RASTER DATA
#read in rgb imagery from landsat
redL <- raster("~/Desktop/GitHub/GEOG331/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("~/Desktop/GitHub/GEOG331/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("~/Desktop/GitHub/GEOG331/a06/glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#holistically make a plot with all 3 stacks - true color with eyes RGB

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
#stretch changes the way colors are displayed
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#use ext to zoom on a specific area
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("~/Desktop/GitHub/GEOG331/a06/NDVI/NDVI_",ndviYear[i],".tif"))
}

#look at a single raster fn from 2003
str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

#QUESTION 2 - NO CODE

plot(NDVIraster[[1]])

#QUESTION 3

#plot NDVI w/1996 polygons
#par(mai=c(1,1,1,1))
par(mfrow = c(1,2))
plot(NDVIraster[[1]], axes=TRUE, xaxis="i", yaxis="i")
plot(g1966, axes=TRUE)

#VECTOR ANALYSIS: GLACIER RETREAT

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#QUESTION 4

#plot map w/ max NDVI and glaciers in 2015
par(mfrow = c(1,2))
plot(max(NDVIraster[[13]]))
plot(g2015p, col="transparent", border="black")


#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#join all the data together into a table for analysis
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

#make a plot of the area for each glacier
par(mfrow = c(1,1))
par(mai=c(1,1,1,1))
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
} 

#QUESTION 5

#calculate the percentage change in area b/w 1966 and 2015
#new columns
gAll$pct.change15<-NA

#actual equation
for (i in 1:39){
  gAll$pct.change15[i]<-((gAll$a2015m.sq[i]-gAll$a1966m.sq[i])/(gAll$a1966m.sq[i]))*100
  g2015p$pct.change15[i]<-gAll$pct.change15[i]
}

#spplot maps glaciers in 2015 showing % change

par(mfrow = c(1,1))
par(mai=c(1,1,1,1))
spplot(g2015p, "pct.change15")

#VISUALIZE GLACIER LOSS - GDIFFERENCE REMOVES OVERLAPPING AREAS
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot with NDVI
par(mfrow = c(1,1))
par(mai=c(0.5,0.5,0.5,0.5))
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#QUESTION 6

#find glacier with largest % loss
min(gAll$pct.change15) #-84.72067
which.min(gAll$pct.change15) #5
gAll$GLACNAME[5] #"Boulder Glacier"

#display the extent of glacial loss for all years
g1966p_boulder<-subset(g1966p, GLACNAME=="Boulder Glacier")
g2015p_boulder<-subset(g2015p, GLACNAME=="Boulder Glacier")
diffboulder<-gDifference(g1966p_boulder,g2015p_boulder)

plot(g1966p_boulder,
     main="84.72% Glacial Loss of Boulder Glacier")
plot(g2015p_boulder,add=TRUE)
plot(diffboulder, col="black",border=NA, add=TRUE)

#RASTER DATA ANALYSIS: DOES MORE VEGETATION GROW WITH GLACIAL RETREAT?
#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}
plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)
#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

#QUESTION 7 - NO CODE

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units
#use zonal stats to get a stat applied across an index
#zonal only works with 2 rasters
#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#QUESTION 8 - NO CODE

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

#QUESTION 9 

#add mean change in NDVI to the 2015 glacier polygons
g2015p$meanchange <- meanChange[1:39]
#plot where mean change in vegetation is color coded
spplot(g2015p, "meanchange")

#QUESTION 10 - NO CODE

#QUESTION 11

#QUESTION 12 - NO CODE

#QUESTION 13 - NO CODE
