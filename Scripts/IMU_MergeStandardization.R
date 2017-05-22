#################################################
#################################################
#################################################
#################################################  Read In, Standardize, and merge IMU _Data and Depth Data
#################################################
#################################################
#################################################

#To Install Nessesary Packages
#library(devtools)
#install_github("connorfwhite/TeleFunc/Telefunc")


library(TeleFunc)

setwd(paste(wd,shark,"/IMU",sep=""))

#read in IMU and Pressure Data
dat<-read.csv(paste(shark,"_INER.csv",sep=""))
datTD<-read.csv(paste(shark,"_PTMP.csv",sep=""))

#Subset so we only have the data columns we want
dat<-dat[,c("Time.from.Start..s.","AccelIntX","AccelIntY", "AccelIntZ",
            "MagX", "MagY", "MagZ",
            "GyroX", "GyroY", "GyroZ")]
datTD<-datTD[,c("Time.from.Start..s.", "Pressure", "Temperature")]

#Convert Pressure to Depth, smooth depth a little and calculate VV
datTD$Depth<-(datTD$Pressure - 1000)/100
datTD$Depth<-filter(datTD$Depth,filter=rep(1,5)/5)
datTD$VV<-c(0,datTD$Depth[2:(length(datTD$Depth))] - datTD$Depth[1:(length(datTD$Depth)-1)])

#Remove Pressure
datTD<-datTD[,-2]

#Merge IMU and Pressure Data Together
dat<-merge(dat,datTD,by = "Time.from.Start..s.",all = TRUE)

#Interpolate the Depth Temperature and VV data to same frequency as IMU data to remove NAs
dat$Depth<-eventInterp(dat$Depth,x = is.na(dat$Depth),ends = 2)
dat$VV<-eventInterp(dat$VV,x = is.na(dat$VV),ends = 2)
dat$Temperature<-eventInterp(dat$Temperature,x = is.na(dat$Temperature),ends=2)

#calculate the time of each data point and then clip the deployment to only when it was on the animal
datTime <- IMU_Start + dat$Time.from.Start..s.
dat<-dat[which(datTime > release & datTime < pop),]
datTime<-datTime[which(datTime > release & datTime < pop)]

#Rename axis to global frame
colnames(dat)<-c("Time.from.Start..s.",
                 "AccelIntZ", "AccelIntX", "AccelIntY",
                 "MagZ", "MagX", "MagY",
                 "GyroZ", "GyroX", "GyroY",
                 "Temperature", "Depth", "VV")  
#Reorder so that in xyz
dat<-dat[,c("Time.from.Start..s.",
            "AccelIntX", "AccelIntY", "AccelIntZ",
            "MagX", "MagY", "MagZ",
            "GyroX", "GyroY", "GyroZ",
            "Temperature", "Depth", "VV")] 
dat$DateTime<-format(datTime,format="%Y-%m-%d %H:%M:%OS3")


write.csv(dat,paste(wd,shark,"/IMU/",shark,"_Standardized.csv",sep=""),row.names=FALSE)

