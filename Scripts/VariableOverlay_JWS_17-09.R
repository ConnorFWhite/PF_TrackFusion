library(TeleFunc)
library(fields)
library(rgdal)

wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/SpatialData/harbor_wgs"
setwd("C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/SpatialData/harbor_wgs")
coast<-readOGR(dsn=path.expand("C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/SpatialData/harbor_wgs/harbor_wgs.shp"),layer="harbor_wgs")


wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/"

shark<-"JWS_17-09_20170517"




dat<-read.csv(paste(wd,"/",shark,"/",shark,"_VR100Fused.csv",sep=""))
dat$DateTime<-as.POSIXct(dat$DateTime,format="%Y-%m-%d %H:%M:%OS")


setwd("C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/Figures")

#quartz(height=10,width=15)

png(paste(shark,"_Track.png",sep=""))
par(mar=c(0,0,0,0))
plot(coast,ylim=c(33.73,33.76), xlim=c(-118.14,-118.105),col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(1:nrow(dat),col=c(rev(tim.colors(128/2)),tim.colors(128/2))))
#points(dat$Latitude ~ dat$Longitude, cex=.1,pch=16,col=color.scale(1:nrow(dat)))
scaleBar(max = 2,unit = "km",x = -118.126,y = 33.731)
colorRampLegend(x=-118.1392,y=33.73,
                lims=as.numeric(c(min(dat$DateTime),max(dat$DateTime))),
                laboff=0.0001,tick=0.0005,at=as.numeric(seq(as.POSIXct("2017-05-17 12:00:00"),length.out=7,by=3600*4)),
                labels=format(seq(as.POSIXct("2017-05-17 12:00:00"),length.out=7,by=3600*4),format="%H:%M %m-%d"),
                height=0.012,width=0.001,nlevels=180,legend.lab="Time",legoff=8)
dev.off()

png(paste(shark,"_TrackDepth.png",sep=""))
#Depth
par(mar=c(0,0,0,0))
plot(coast,ylim=c(33.73,33.76), xlim=c(-118.14,-118.105),col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(dat$Depth*-1, zlim=c(-6,0),transparent.color = "darkblue"))
image.plot(legend.only=TRUE,zlim=c(-6,0),smallplot = c(.1,.13,.1,.4))
scaleBar(max = 2,unit = "km",x = -118.126,y = 33.731)
text("Depth (m)",x=-118.133,y=33.739,cex=1.5,font=2,srt=-90,pos=4)
dev.off()


png(paste(shark,"_TrackTemp.png",sep=""))
#Temp
par(mar=c(0,0,0,0))
plot(coast,ylim=c(33.73,33.76), xlim=c(-118.14,-118.105),col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(dat$Temp, zlim=c(17.05,20),transparent.color = "darkred"))
image.plot(legend.only=TRUE,zlim=c(17.05,20),smallplot = c(.1,.13,.1,.4))
scaleBar(max = 2,unit = "km",x = -118.126,y = 33.731)
text("Temperature (c)",x=-118.133,y=33.7415,cex=1.5,font=2,srt=-90,pos=4)
dev.off()


png(paste(shark,"_TrackODBA.png",sep=""))
#ODBA
par(mar=c(0,0,0,0))
plot(coast,ylim=c(33.73,33.76), xlim=c(-118.14,-118.105),col="tan",bg="lightblue")
ODBAScale<-filter(collapse(datG[,7],freq = 25),filter=rep(1,60)/60,sides=2,circular=TRUE)
points(dat$Lat_Est ~ dat$Long_Est,, cex=.5,pch=16,col=color.scale(filter(dat$ODBA,filter=rep(1,30)/30,sides=2,circular=TRUE), zlim=c(0,.12),transparent.color="darkred"))
image.plot(legend.only=TRUE,zlim=c(0,.12),smallplot = c(.1,.13,.1,.4))
text("ODBA (g)",x=-118.133,y=33.739,cex=1.5,font=2,srt=-90,pos=4)
scaleBar(max = 2,unit = "km",x = -118.126,y = 33.731)
dev.off()


png(paste(shark,"_TrackTBF.png",sep=""))
#TBF
par(mar=c(0,0,0,0))
plot(coast,ylim=c(33.73,33.76), xlim=c(-118.14,-118.105),col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(filter((1/dat$TBPeriod),filter=rep(1,30)/30,sides=2,circular=TRUE), zlim=c(1/c(2,1)),transparent.color="darkred"))
image.plot(legend.only=TRUE,zlim=1/c(2,1),smallplot = c(.1,.13,.1,.4))
text("Tailbeat Freq(Hz)",x=-118.1335,y=33.7415,cex=1.5,font=2,srt=-90,pos=4)
scaleBar(max = 2,unit = "km",x = -118.126,y = 33.731)
dev.off()


png(paste(shark,"_TrackTBA.png",sep=""))
#TBA
par(mar=c(0,0,0,0))
plot(coast,ylim=c(33.73,33.76), xlim=c(-118.14,-118.105),col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(filter(dat$TBAmp,filter=rep(1,30)/30,sides=2,circular=TRUE),zlim=c(2,4),transparent.color="darkred"))
image.plot(legend.only=TRUE,zlim=c(2,4),smallplot = c(.1,.13,.1,.4))
text("Tailbeat Amp",x=-118.1335,y=33.7405,,cex=1.5,font=2,srt=-90,pos=4)
scaleBar(max = 2,unit = "km",x = -118.126,y = 33.731)
dev.off()



png(paste(shark,"_TrackHeading.png",sep=""))
#Heading #Looks like fucking playdoh
cols<-c(colorRampPalette(c("red","blue"))(40),
        colorRampPalette(c("blue","green"))(40),
        colorRampPalette(c("green","yellow"))(40),
        colorRampPalette(c("yellow","red"))(40))
par(mar=c(0,0,0,0))
plot(coast,ylim=c(33.73,33.76), xlim=c(-118.14,-118.105),col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(dat$Heading,zlim=c(0,2*pi),col=cols,transparent.color="darkred"))
scaleBar(max = 2,unit = "km",x = -118.126,y = 33.731)

as<-seq(0,2*pi,length.out=length(cols))
x<- -118.1349
y<-  33.734
points(I(cos(as)*0.0032 + y ) ~ I(sin(as)*0.004 + x),col=cols,pch=16)
as<-seq(0,1.7*pi,by=pi/2)
text( y=I(cos(as)*0.0032 + y ), x=I(sin(as)*0.004 + x), labels=c("N","E","S","W"),font=2,pos=c(3,4,1,2),cex=1.5)
dev.off()