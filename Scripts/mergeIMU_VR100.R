#################################################
#################################################
#################################################
#################################################  Merge IMU 1hz and VR100 Data together
#################################################
#################################################
#################################################



####Bringing in the VR100Data

datVR<-read.csv(paste(wd,shark,"/Track/",shark,"_VR100.csv",sep=""))

datVR$DateTime <- as.POSIXct(paste(datVR$Date,datVR$Time,sep=" "),format="%m/%d/%Y %H:%M:%S")

datVR<-datVR[which(datVR$DateTime>release & datVR$DateTime<pop),]
datVR<-datVR[-which(is.na(datVR$Latitude)),]
datVR<-datVR[which(datVR$Freq..kHz. %in% c(63,69)),]
datVR<-datVR[which(datVR$Gain..dB.<20),]
datVR<-datVR[order(datVR$DateTime),]

carts<-lat2cart(lat = datVR$Latitude, long=datVR$Longitude, latOrg = latOrg, longOrg = longOrg)
datVR$Y<-carts[,2]
datVR$X<-carts[,1]


datVR<-datVR[,c("DateTime", "Signal..dB.","Gain..dB.", "Latitude","Longitude","X","Y")]



####### Load in 1hz IMU data
dat<-read.csv(paste(wd,shark,"/IMU/",shark,"_1HZ.csv",sep=""))
dat$DateTime<-as.POSIXct(dat$DateTime,format="%Y-%m-%d %H:%M:%S")

#Combining VR100 and IMU data
PFdat<-merge(datVR,dat,by="DateTime",all=TRUE)
PFdat<-PFdat[-which(duplicated(PFdat$DateTime)),]

#CLeaning it up a bit if we got less than 3 detections in 2 minutes, they are considered spurious and are removed
pres<-filter(!is.na(PFdat$Signal..dB.),filter=rep(1,120))
PFdat$Signal..dB.[pres<3]<-NA
PFdat$Latitude[pres<3]<-NA
PFdat$Longitude[pres<3]<-NA
PFdat$X[pres<3]<-NA
PFdat$Y[pres<3]<-NA

PFdat$DateTime<-format(PFdat$DateTime,format="%Y-%m-%d %H:%M:%S")


#Loading in release and pop location
PFdat$Latitude[1]<-releaseLat
PFdat$Longitude[1]<-releaseLong
carts<-lat2cart(lat = releaseLat, long=releaseLong, latOrg = latOrg, longOrg = longOrg)
PFdat$Y[1]<-carts[,2]
PFdat$X[1]<-carts[,1]
PFdat$Signal..dB.[1]<-100

carts<-lat2cart(lat = popLat, long=popLong, latOrg = latOrg, longOrg = longOrg)
PFdat$Latitude[nrow(PFdat)]<-popLat
PFdat$Longitude[nrow(PFdat)]<-popLong
PFdat$Y[nrow(PFdat)]<-carts[,2]
PFdat$X[nrow(PFdat)]<-carts[,1]
PFdat$Signal..dB.[nrow(PFdat)]<-100



write.csv(PFdat,paste(wd,"/",shark,"/",shark,"_VR100Merged.csv",sep=""),row.names=FALSE)


