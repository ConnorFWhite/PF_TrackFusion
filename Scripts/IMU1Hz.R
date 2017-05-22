#################################################
#################################################
#################################################
#################################################  Calculate Statistics and Sample at 1 Hz
#################################################
#################################################
#################################################



#Load In 
dat<-read.csv(paste(wd,shark,"/IMU/",shark,"_Standardized.csv",sep=""))
dat$DateTime<-as.POSIXct(dat$DateTime,format="%Y-%m-%d %H:%M:%OS")

####Calculate the TailbeatFrequency and amplitude, downsampled to 5hz for computation ease and speed
library(WaveletComp)
my.w <- analyze.wavelet(dat[seq(1,(nrow(dat)),by=5),], "GyroZ",
                        loess.span = 0,
                        lowerPeriod = .75,
                        upperPeriod = 3,
                        dt = .16, dj = 1/20,
                        n.sim=1,make.pval=FALSE)

mat<-my.w$Power
out<-apply(my.w$Power,MARGIN = 2,which.max)
period<-my.w$Period[out]
amp<-out
for(i in 1:length(out)){
  amp[i]<-my.w$Ampl[out[i],i]
}


datG<-Gsep(dat[,c("AccelIntX","AccelIntY", "AccelIntZ")],filt = rep(1,100)/100)
PR<-pitchRoll(datG,degrees = FALSE)
datMG<-Gsep(dat[,c("MagX","MagY", "MagZ")],filt = rep(1,100)/100)

PR1hz<-apply(PR,MARGIN = 2, FUN = function(x){collapse(x,freq=25)})
ODBA1hz<-collapse(datG[,7],freq=25)
magXYZ1hz<-apply(datMG,MARGIN = 2, FUN = function(x){collapse(x,freq=25)})
heading<-magHead(PR = PR1hz,magxyz = magXYZ1hz,magOff = magOff)
heading<-heading + pi + declination
amp1Hz<-filter(amp,filter=rep(1,5)/5,sides=2,circular=TRUE)[seq(1,length(amp),by=5)]
per1Hz<-filter(period,filter=rep(1,5)/5,sides=2,circular=TRUE)[seq(1,length(amp),by=5)]
Depth1Hz<-collapse(dat$Depth,freq=25)
VV1hz<-collapse(dat$VV,freq=25)
Temp1Hz<-collapse(dat$Temperature,freq=25)
Time1Hz<-dat$DateTime[seq(1,nrow(dat),by=25)]


dat1hz<-data.frame(Time1Hz,Depth1Hz,VV1hz,Temp1Hz,PR1hz,heading, per1Hz,amp1Hz,ODBA1hz)

colnames(dat1hz)<-c("DateTime","Depth","VV", "Temp", "Pitch", "Roll", "Heading", "TBPeriod","TBAmp","ODBA")
dat1hz$DateTime<-format(dat1hz$DateTime,format="%Y-%m-%d %H:%M:%S")

write.csv(dat1hz,paste(wd,shark,"/IMU/",shark,"_1HZ.csv",sep=""),row.names=FALSE)






