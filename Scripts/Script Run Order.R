###########
###########Running for first shark
###########

wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/"

shark<-"JWS_17-08_20170510"

IMU_Start<-as.POSIXct("2017-05-10 08:18:00")
release<-as.POSIXct("2017-05-10 10:26:30")
pop<-as.POSIXct("2017-05-11 13:59:15")


releaseLat<- 33.745119
releaseLong<- -118.11874

popLat<- 33.740915
popLong<- -118.119847

dat_Freq<-25
latOrg<- 33.7373
longOrg<- -118.12212

magOff = c(0.0881,0.0415,0)
declination<- 11.5 * (pi/ 180)

source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/PFFunctions.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/IMU_MergeStandardization.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/IMU1Hz.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/mergeIMU_VR100.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/ParticleFilterRun.R")


###############
###############Running for second shark
###############
library(TeleFunc)
library(gRumble)
wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/"

shark<-"JWS_17-09_20170517"

IMU_Start<-as.POSIXct("2017-05-17 06:33:00")
release<-as.POSIXct("2017-05-17 10:00:47")
pop<-as.POSIXct("2017-05-18 11:54:20")


releaseLat<- 33.746748
releaseLong<- -118.128611

popLat<- 33.74226
popLong<-  -118.12477

dat_Freq<-25
latOrg<- 33.7373
longOrg<- -118.12212

magOff = c(0.0881,0.0415,0)
declination<- 11.5 * (pi/ 180)

source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/PFFunctions.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/IMU_MergeStandardization.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/IMU1Hz.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/mergeIMU_VR100.R")
source("C:/Users/Connor/Documents/PF_TrackFusion/Scripts/ParticleFilterRun.R")

