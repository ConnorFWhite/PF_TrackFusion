
#################################################
#################################################
#################################################
#################################################  Run Particle Filter
#################################################
#################################################
#################################################


PFdat<-read.csv(paste(wd,"/",shark,"/",shark,"_VR100Merged.csv",sep=""))



PF_forward<-pfRun(SignalDB=PFdat$Signal..dB.,X=PFdat$X,Y=PFdat$Y,headings=PFdat$Heading,
                  nPart=1000,startSD=1000,
                  headSD = 75*(pi/180),
                  speed = 1.2, speedSD = 3,
                  near=15,maxD=300,midDB=80,rate=7,
                  min = 15,den = .25,
                  er=4500,timebuff=10)

PF_rev<-pfRun(SignalDB=rev(PFdat$Signal..dB.),X=rev(PFdat$X),Y=rev(PFdat$Y),headings=rev(PFdat$Heading)+pi,
              nPart=1000,startSD=1000,
              headSD = 45*(pi/180),
              speed = 1.2, speedSD = 2,
              near=10,maxD=200,midDB=70,rate=7,
              min = 15,den = .25,
              er=4500,timebuff=10)
PF_rev<-apply(PF_rev,MARGIN = 2,rev)


#Fusing the forward and backward tracks together based on their probability
p_forward<-eval(estX=PF_forward[,1],estY=PF_forward[,2],Xob=PFdat$X,Yob=PFdat$Y,signOb=PFdat$Signal..dB.)
p_rev<-eval(estX=PF_rev[,1],estY=PF_rev[,2],Xob=PFdat$X,Yob=PFdat$Y,signOb=PFdat$Signal..dB.)
p_forward<-eventInterp(p_forward,is.na(p_forward),ends = 2)
p_rev<-eventInterp(p_rev,is.na(p_rev),ends=2)
p_forward<-filter(p_forward,filter=rep(1,600)/600,sides=2,circular=TRUE)
p_rev<-filter(p_rev,filter=rep(1,600)/600,sides=2,circular=TRUE)
psum<-p_forward + p_rev
p_forward<-p_forward/psum
p_rev<-p_rev/psum
PF_avg<-PF_forward
PF_avg[,1]<-(PF_forward[,1]*p_forward) +(PF_rev[,1] * p_rev)
PF_avg[,2]<-(PF_forward[,2]*p_forward) +(PF_rev[,2] * p_rev)



###########Project Functions now working, some times give an error, chat lat_1 to lat_0
library(raster)
library(proj4)
#xy_F<-project(PF_forward[,c(1,2)],
#              proj= '+proj=lcc +lon_0= -118.12212 +lat_1=33.7373 +datum=WGS84 +units=m ',
#              inverse=TRUE)
#colnames(xy_F)<-c("Long_Est","Lat_Est")


#xy_R<-project(PF_rev[,c(1,2)],
#              proj= '+proj=lcc +lon_0=-118.12212 +lat_1=33.7373 +datum=WGS84 +units=m ',
#              inverse=TRUE)
#colnames(xy_R)<-c("Long_Est","Lat_Est")

xy<-proj4::project(PF_avg[,c(1,2)],
            proj= '+proj=lcc +lon_0=-118.12212 +lat_1=33.7373 +datum=WGS84 +units=m ', inverse =TRUE)
colnames(xy)<-c("Long_Est","Lat_Est")

dat<-data.frame(PFdat,xy)
dat<-dat[,-c(6,7)]


write.csv(dat,paste(wd,"/",shark,"/",shark,"_VR100Fused.csv",sep=""),row.names=FALSE)






