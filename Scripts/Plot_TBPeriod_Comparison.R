library(devtools)
install_github("ConnorFWhite/TeleFunc/TeleFunc")
library(AssFunc)
library(TeleFunc)
library(fields)



wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/"

shark<-"JWS_17-09_20170517"

dat<-read.csv(paste(wd,"/",shark,"/",shark,"_VR100Fused.csv",sep=""))
dat$DateTime<-as.POSIXct(dat$DateTime,format="%Y-%m-%d %H:%M:%OS")
sunCalc(dat$DateTime[1],long = dat$Long_Est[1],lat = dat$Lat_Est[1])

png(paste(wd,"/Figures/TBPeriod_Comparion.png",sep=""),height=400*2,width=800*2,res=72*2)
par(mfrow=c(1,2),mar=c(4,5,3,2))

Ascent<-event(dat$VV < -0.01,ends=2,duration = c(10,NA))
Descent<-event(dat$VV > 0.01,ends=2,duration = c(10,NA))
level<-event(dat$VV < 0.01 & dat$VV > -0.01,ends=2,duration = c(10,NA))

plot(dat$TBPeriod[eventVector(Descent)]~dat$DateTime[eventVector(Descent)],
     type="n",ylim=c(1,2),ylab="Tailbeat Period",las=1,xlab=" ",main=shark)
abline(v=as.POSIXct("2017-05-18 5:49"),lwd=4,col="gold3")
abline(v=as.POSIXct("2017-05-17 19:48"),lwd=4,col="gold3")
points(dat$TBPeriod[eventVector(Descent)]~dat$DateTime[eventVector(Descent)],col="blue",pch=16,cex=.5)
points(dat$TBPeriod[eventVector(Ascent)]~dat$DateTime[eventVector(Ascent)],col="red",pch=16,cex=.5)
points(dat$TBPeriod[eventVector(level)]~dat$DateTime[eventVector(level)],pch=16,cex=.5)




shark<-"JWS_17-08_20170510"

dat<-read.csv(paste(wd,"/",shark,"/",shark,"_VR100Fused.csv",sep=""))
dat$DateTime<-as.POSIXct(dat$DateTime,format="%Y-%m-%d %H:%M:%OS")
sunCalc(dat$DateTime[1],long = dat$Long_Est[1],lat = dat$Lat_Est[1])


Ascent<-event(dat$VV < -0.01,ends=2,duration = c(10,NA))
Descent<-event(dat$VV > 0.01,ends=2,duration = c(10,NA))
level<-event(dat$VV < 0.01 & dat$VV > -0.01,ends=2,duration = c(10,NA))

plot(dat$TBPeriod[eventVector(Descent)]~dat$DateTime[eventVector(Descent)],
     type="n",ylim=c(1,2),ylab="Tailbeat Period",las=1,xlab=" ",main=shark)
abline(v=as.POSIXct("2017-05-11 5:55"),lwd=4,col="gold3")
abline(v=as.POSIXct("2017-05-10 19:42"),lwd=4,col="gold3")
points(dat$TBPeriod[eventVector(Descent)]~dat$DateTime[eventVector(Descent)],col="blue",pch=16,cex=.5)
points(dat$TBPeriod[eventVector(Ascent)]~dat$DateTime[eventVector(Ascent)],col="red",pch=16,cex=.5)
points(dat$TBPeriod[eventVector(level)]~dat$DateTime[eventVector(level)],pch=16,cex=.5)

dev.off()

