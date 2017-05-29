#Pasting the single dsg csv outputs together
wd<-"C:/Users/Connor/Dropbox (NEAQ BEPP Group)/JWS_AccelData/"

shark<-"JWS_17-11_20170525"


setwd(paste(wd,shark,"/IMU/TagOffLoad",sep=""))
files<-list.files(pattern="INER")

INER<-NULL
for(i in 1:length(files)){
  INER<-rbind(INER,read.csv(files[i]))
}
INER<-INER[order(INER$Sample),]
INER<-INER[,-which(colnames(INER)=="X")]

write.csv(INER,file=paste(wd,shark,"/IMU/",shark,"_","INER.csv",sep=""),row.names=FALSE)



files<-list.files(pattern="PTMP")

PTMP<-NULL
for(i in 1:length(files)){
  PTMP<-rbind(PTMP,read.csv(files[i]))
}
PTMP<-PTMP[,-which(colnames(PTMP)=="X")]
PTMP<-PTMP[order(PTMP$Sample),]
write.csv(PTMP,file=paste(wd,shark,"/IMU/",shark,"_","PTMP.csv",sep=""),row.names=FALSE)
