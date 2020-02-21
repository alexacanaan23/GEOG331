#Alexa Canaan
#activity 3

#TESTING YOUR CODE - example code

#create a function. The names of the arguements for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#READ IN THE DATA
#UNDERSTAND THE NATURE OF YOUR QA/QC

#QUESTION 3
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("C:\\Users\\acanaan\\Documents\\GitHub\\GEOG331\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)

#upload data from mac
#datW <- read.csv("~/Desktop/GitHub/GEOG331/a03/bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)

#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

#upload data from mac
#sensorInfo <-   read.csv("~/Desktop/GitHub/GEOG331/a03/bewkes_weather.csv",na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#DATA QA/QC

#helpful for dates and times
#install.packages("lubridate")
#load package to working environment using library
library(lubridate)
#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#CHECK MISSING DATA
#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp))) #lots of missing data

#make a plot with filled in points (using pch) to ask researchers
#soil moisture missing data values
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#SETTING UP TESTS FOR QA/QC

#VISUAL TESTS

#make a plot with filled in points (using pch)
#plot air temperature observations
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
#reasonable plot

#create a new column that will have NA if 1st arg is true, and 1 if it's false
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#REALISTIC VALUES CHECK

#QUESTION 4
#check the values at the extreme range of the data and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,] 

#measurements outside of sensor capabilities

#QUESTION 5

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#test using assert function from part 1
assert(length(lightscale)==length(datW$precipitation), "error: unequal length")

#QUESTION 6

#filter out storms in wind and air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#remove suspect measurements from wind speed measurements in new column
datW$wind.speed2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                           ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#test using assert to verify data is filtered as expected
for (i in length(datW$wind.speed2))
{
  assert(is.na(datW$wind.speed2[i])==TRUE, "filtered")
}

#plot with both lines and points of windspeed with new data
plot(datW$DD, datW$wind.speed2, xlab = "Day of Year", ylab= "Wind Speed",
     type = "n")
#points(datW$DD[datW$wind.speed > 0], datW$wind.speed[datW$wind.speed >0], col= "tomato3", pch = 5)
#lines(datW$DD[datW$wind.speed > 0], datW$wind.speed[datW$wind.speed >0], col= "tomato3", pch = 5)
points(datW$wind.speed2[datW$wind.speed2 >0], col= "blue")
lines(datW$wind.speed2[datW$wind.speed2 >0], col= "blue")

#FINISHING YOUR QA/QC

#QUESTION 7

#check the day that the outage occured, datW$doy and datW$soil.moisture
View(datW)

#check soil moisture with precipitation, blank plot and add the points
plot(datW$doy[datW$doy>=185 & datW$doy<=193], datW$precipitation[datW$doy>=185 & datW$doy<=193], xlab = "Day of Year", ylab = "Precipitation", type="n")
points(datW$soil.moisture[datW$doy>=185 & datW$doy<=193], col="blue")
points(datW$precipitation[datW$doy>=185 & datW$doy<=193], col="red")

#check soil temp with air temp, blank plot and add the points
plot(datW$doy[datW$doy>=185 & datW$doy<=193], datW$air.tempQ2[datW$doy>=185 & datW$doy<=193], xlab = "Day of Year", ylab = "Temperature", type="n")
points(datW$soil.temp[datW$doy>=185 & datW$doy<=193], col="blue")
points(datW$air.tempQ2[datW$doy>=185 & datW$doy<=193], col="red")

#QUESTION 8

#table with average air temperature, wind speed, soil moisture, soil temperature, total precipitation

#calculate desired averages and sums, with given number of observations
#average air temperature with no na's
avg_air_temp<-subset(datW$air.tempQ2, is.na(datW$air.tempQ2)==FALSE)

#average wind speed with no na's
avg_wind_speed<-subset(datW$wind.speed2, is.na(datW$wind.speed2)==FALSE)

#average soil moisture with no na's
avg_soil_moist<-subset(datW$soil.moisture, is.na(datW$soil.moisture)==FALSE)

#average soil temperature with no na's
avg_soil_temp<-subset(datW$soil.temp, is.na(datW$soil.temp)==FALSE)

#total precipitation with no na's
sum_prcp<-subset(datW$precipitation, is.na(datW$precipitation)==FALSE)


#create table
info_table<-data.frame(round(mean(avg_air_temp),1), round(mean(avg_wind_speed),1), round(mean(avg_soil_moist),2), round(mean(avg_soil_temp),0), round(sum(sum_prcp),2))
#rename column names
names(info_table) <- c("Average Air Temperature", "Average Wind Speed", "Average Soil Moisture", "Average Soil Temperature", "Total Precipitation")
print(info_table)

#download packages to allow me to export the table
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
#print table in neat format
formattable(info_table)

#date range of data

#date range of air temperautre
range(datW$doy[is.na(datW$air.tempQ2)==FALSE])

#date range of wind speed
range(datW$doy[is.na(datW$wind.speed2)==FALSE])

#date range of soil moisture
range(datW$doy[is.na(datW$soil.moisture)==FALSE])

#date range of soil temperature
range(datW$doy[is.na(datW$soil.temp)==FALSE])

#date range of precipitation
range(datW$doy[is.na(datW$precipitation)==FALSE])

#QUESTION 9

#display graphs in a 2x2 format for the 4 total plots
par(mfrow = c(2,2))
#soil moisture plot
plot(datW$DD, datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture", col = "blue")
#air temperature plot
plot(datW$DD, datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temperature", col = "darkgreen")
#soil temperature plot
plot(datW$DD, datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature", col = "purple")
#precipitation plot
plot(datW$DD, datW$precipitation, xlab = "Day of Year", ylab = "Precipitation", col = "brown")

