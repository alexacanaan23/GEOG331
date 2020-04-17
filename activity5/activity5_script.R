#Alexa Canaan
#activity 5

#QUESTION 1 - NO CODE NECESSARY

#load in lubridate
library(lubridate)

#read in streamflow data
#datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
#                na.strings = c("Eqp"))
#read data on the mac
datH <- read.csv("~/Desktop/GitHub/GEOG331/a05/stream_flow_data.csv", na.strings = c("Eqp"))
#preview data
head(datH)

#read in precipitation data
#hourly precipitation is in mm
#datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")
#read data on the mac
datP <- read.csv("~/Desktop/GitHub/GEOG331/a05/2049867.csv")
#preview data
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#QUESTION 2

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay-1/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay-1/365))      
#format precipitaiton date column
datP$day<-ymd_hm(datP$DATE)
#strip time
datP$dateonly<-format(as.Date(datP$day), "%m/%d/%Y")


#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#QUESTION 3 - No code
#QUESTION 4 
#look up documentation
help(expression())

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")
#start new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#QUESTION 5
#add a line that shows 2017 observations
#create a data frame that just has 2017 observations
datD2017<-subset(datD, datD$year==2017)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,180),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
#show sd
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)  
lines(datD2017$doy, datD2017$discharge, col="red")
axis(1, at= c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), #tick intervals
     labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) #tick labels
axis(2, seq(0,180, by=30),
     seq(0,180, by=30),
     las = 2)#show ticks at 90 degree angle

#QUESTION 6 - NO CODE

#QUESTION 7
#create a dataframe that indicates what days have a full 24 hours
#of precipitation measurements

#give each obs a count
datP["Count"]<-1

#dataframe with all days and their number of observations
datC<-aggregate(datP[c("Count")], by=list(datP$dateonly), FUN="sum")

#days with a full 24 hours of precipitation measurements
datC1<-subset(datC, datC$Count==24)
#check length = 60
length(datC1$Group.1)

#indicate only days with full measures, start with all = FALSE
datP["Full"]<-FALSE

#indicate if obs is part of day of full measure
datP <- transform(datP, Full= ifelse(datP$dateonly %in% datC1$Group.1, TRUE, FALSE))

#dataframe that indicates what days have full 24 hr measure with discharge data
datCombine<- datD
datCombine<-transform(datCombine, Full= ifelse(datCombine$date %in% datC1$Group.1, TRUE, FALSE))
datCombine$Full <- factor(datCombine$Full)

#range of days of full measure is from 2007 to 2013
datCombine<-subset(datCombine, datCombine$year>=2007 & datCombine$year<=2013)
#plot of discharge measures
library(ggplot2)
ggplot(dat=datCombine, aes(x=date, y=discharge, fill=Full))+
  geom_point(aes(color = Full))+
  ggtitle("Discharge")+
  xlab("Date")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
  theme_bw()+
  theme(axis.text.x = element_blank())

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#QUESTION 8
#subsest discharge and precipitation within range of interest
hydro1D <- datD[datD$doy >= 1 & datD$doy < 3 & datD$year == 2011,]
hydro1P <- datP[datP$doy >= 1 & datP$doy < 3 & datP$year == 2011,]

min(hydro1D$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydro1D$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydro1D$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydro1P$HPCP))+.5
#scale precipitation to fit on the 
hydro1P$pscale <- (((yh-yl)/(pm-pl)) * hydro1P$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydro1D$decDay,
     hydro1D$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydro1P)){
  polygon(c(hydro1P$decDay[i]-0.017,hydro1P$decDay[i]-0.017,
            hydro1P$decDay[i]+0.017,hydro1P$decDay[i]+0.017),
          c(yl,hydro1P$pscale[i],hydro1P$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()


#QUESTION 9
#make a new column to indicate season
datD$season<-NA

#indicate season for 2016
#indicate winter for days before march 19
datD$season[datD$year==2016 & datD$doy<79]<-"Winter"
#indicate spring for days between march 3 and june 20
datD$season[datD$year==2016 & datD$doy>=79 & datD$doy<172]<-"Spring"
#indicate summer for days between june 20 and september 22
datD$season[datD$year==2016 & datD$doy>=172 & datD$doy<266]<-"Summer"
#indicate fall for days between september 22 and december 21
datD$season[datD$year==2016 & datD$doy>=266 & datD$doy<356]<-"Fall"
#indicate winter for days including december 21 and on
datD$season[datD$year==2016 & datD$doy>=356]<-"Winter"

#indicate season for 2017
#indicate winter for days before march 3
datD$season[datD$year==2017 & datD$doy<79]<-"Winter"
#indicate spring for days between march 3 and june 20
datD$season[datD$year==2017 & datD$doy>=79 & datD$doy<171]<-"Spring"
#indicate summer for days between june 20 and september 22
datD$season[datD$year==2017 & datD$doy>=171 & datD$doy<265]<-"Summer"
#indicate fall for days between september 22 and december 21
datD$season[datD$year==2017 & datD$doy>=265 & datD$doy<355]<-"Fall"
#indicate winter for days including december 21 and on
datD$season[datD$year==2017 & datD$doy>=355]<-"Winter"

#ggplot by season for 2016 for discharge
ggdat<- subset(datD, year==2016)
ggplot(data= ggdat, aes(season,discharge)) + 
  geom_violin(fill="pink")+
  ggtitle("Discharge for 2016 by Season")+
  xlab("Season")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
  theme_bw()

#ggplot by season for 2017 for discharge
ggdat1<- subset(datD, year==2017)
ggplot(data= ggdat1, aes(season,discharge)) + 
  geom_violin(fill="purple")+
  ggtitle("Discharge for 2017 by Season")+
  xlab("Season")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
  theme_bw()