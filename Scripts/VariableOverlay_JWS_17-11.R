library(TeleFunc)
library(fields)
library(rgdal)

wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/SpatialData/WestCoast"
setwd("C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/SpatialData/WestCoast")
coast<-readOGR(dsn=path.expand("C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/SpatialData/WestCoast/WestAndBaja.shp"),layer="WestAndBaja")


wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/"

shark<-"JWS_17-11_20170525"



dat<-read.csv(paste(wd,"/",shark,"/",shark,"_VR100Fused.csv",sep=""))
dat$DateTime<-as.POSIXct(dat$DateTime,format="%Y-%m-%d %H:%M:%OS")


xlims<-c(-118.14,-117.7)
ylims<-c(33.3,33.8)
setwd("C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/Figures")

#quartz(height=10,width=15)

#png(paste(shark,"_Track.png",sep=""),width = 400*2,height=400*2,res=72*2)
#par(mar=c(0,0,0,0))
#plot(coast,ylim=ylims, xlim=xlims,col="tan",bg="lightblue")
#points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(1:nrow(dat),col=c(rev(tim.colors(128/2)),tim.colors(128/2))))
#points(dat$Latitude ~ dat$Longitude, cex=.1,pch=16,col="black")
#scaleBar(max = 25,unit = "km",x = -118.126,y = 33.731)
#colorRampLegend(x=-118.1392,y=33.73,
#                lims=as.numeric(c(min(dat$DateTime),max(dat$DateTime))),
#                laboff=0.0001,tick=0.0005,at=as.numeric(seq(as.POSIXct("2017-05-17 12:00:00"),length.out=7,by=3600*4)),
#                labels=format(seq(as.POSIXct("2017-05-17 12:00:00"),length.out=7,by=3600*4),format="%H:%M %m-%d"),
#                height=0.012,width=0.001,nlevels=180,legend.lab="Time",legoff=8)
#dev.off()

png(paste(shark,"_TrackDepth.png",sep=""),width = 400*2,height=400*2,res=72*2)
#Depth
par(mar=c(0,0,0,0))
plot(coast,ylim=ylims, xlim=xlims,col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(dat$Depth*-1, zlim=c(-20,0),transparent.color = "darkblue"))
image.plot(legend.only=TRUE,zlim=c(-20,0),smallplot = c(.05,.08,.15,.45))
scaleBar(max = 20,unit = "km")
text("Depth (m)",x=-118.13,y=33.5,cex=1.5,font=2,srt=-90,pos=4)
dev.off()


png(paste(shark,"_TrackTemp.png",sep=""),width = 400*2,height=400*2,res=72*2)
#Temp
par(mar=c(0,0,0,0))
plot(coast,ylim=ylims, xlim=xlims,col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(dat$Temp, zlim=c(14,18),transparent.color = "darkblue"))
image.plot(legend.only=TRUE,zlim=c(14,17.5),smallplot = c(.05,.08,.1,.4))
scaleBar(max = 20,unit = "km")
text("Temperature (c)",x=-118.13,y=33.5,cex=1.5,font=2,srt=-90,pos=4)
dev.off()


png(paste(shark,"_TrackODBA.png",sep=""),width = 400*2,height=400*2,res=72*2)
#ODBA
par(mar=c(0,0,0,0))
plot(coast,ylim=ylims, xlim=xlims,col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est,, cex=.5,pch=16,col=color.scale(filter(dat$ODBA,filter=rep(1,30)/30,sides=2,circular=TRUE), zlim=c(0,.12),transparent.color="darkred"))
image.plot(legend.only=TRUE,zlim=c(0,.12),smallplot = c(.05,.08,.1,.4))
text("ODBA (g)",x=-118.13,y=33.46,cex=1.5,font=2,srt=-90,pos=4)
scaleBar(max = 20,unit = "km")
dev.off()


png(paste(shark,"_TrackTBF.png",sep=""),width = 400*2,height=400*2,res=72*2)
#TBF
par(mar=c(0,0,0,0))
plot(coast,ylim=ylims, xlim=xlims,col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(filter((1/dat$TBPeriod),filter=rep(1,30)/30,sides=2,circular=TRUE), zlim=c(1/c(2,1)),transparent.color="darkred"))
image.plot(legend.only=TRUE,zlim=1/c(2,1),smallplot = c(.05,.08,.1,.4))
text("Tailbeat Freq(Hz)",x=-118.13,y=33.5,cex=1.5,font=2,srt=-90,pos=4)
scaleBar(max = 20,unit = "km")
dev.off()


png(paste(shark,"_TrackTBA.png",sep=""),width = 400*2,height=400*2,res=72*2)
#TBA
par(mar=c(0,0,0,0))
plot(coast,ylim=ylims, xlim=xlims,col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(filter(dat$TBAmp,filter=rep(1,30)/30,sides=2,circular=TRUE),zlim=c(2,4),transparent.color="darkred"))
image.plot(legend.only=TRUE,zlim=c(2,4),smallplot = c(.05,.08,.1,.4))
text("Tailbeat Amp",x=-118.13,y=33.49,,cex=1.5,font=2,srt=-90,pos=4)
scaleBar(max = 20,unit = "km")
dev.off()



png(paste(shark,"_TrackHeading.png",sep=""),width = 400*2,height=400*2,res=72*2)
#Heading #Looks like fucking playdoh
cols<-c(colorRampPalette(c("red","blue"))(40),
        colorRampPalette(c("blue","green"))(40),
        colorRampPalette(c("green","yellow"))(40),
        colorRampPalette(c("yellow","red"))(40))
par(mar=c(0,0,0,0))
plot(coast,ylim=ylims, xlim=xlims,col="tan",bg="lightblue")
points(dat$Lat_Est ~ dat$Long_Est, cex=.5,pch=16,col=color.scale(dat$Heading,zlim=c(0,2*pi),col=cols,transparent.color="darkred"))
scaleBar(max = 20,unit = "km")

as<-seq(0,2*pi,length.out=length(cols))
x<- -118.1368
y<-  33.43
points(I(cos(as)*0.018 + y ) ~ I(sin(as)*0.024 + x),col=cols,pch=16,cex=1.5)
as<-seq(0,1.7*pi,by=pi/2)
text( y=I(cos(as)*0.018 + y ), x=I(sin(as)*0.024 + x), labels=c("N","E","S","W"),font=2,pos=c(3,4,1,2),cex=1.5)
dev.off()