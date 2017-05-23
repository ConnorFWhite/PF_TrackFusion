head<-function(nPart,head=0, headSD=pi){
  heads<-rnorm(n=nPart, mean=head,sd=headSD)
  return(heads)
}

speed<-function(nPart,speed=1, speedSD = 0.5){
  speed<-rnorm(nPart,mean=speed,sd=speedSD)
  return(speed)
}

locDif<-function(heads,speeds){
  x<-(sin(heads) * speeds)
  y<-(cos(heads) * speeds)
  
  return(cbind(x,y))
}

move<-function(loc,locDif){
  x<-loc[,1] + locDif[,1]
  y<-loc[,2] + locDif[,2]
  
  return(cbind(x,y))
}

#calculates the distance from particles to the AUV
pdist<-function(px,py,X,Y){
  sqrt((X-px)^2 + (Y-py)^2)
}

distmean<-function(signalDB,maxD=200, near=1, midDB=70, rate=5){
  return((maxD+((near-maxD)/(1+exp((midDB-signalDB)/rate)))))
}

distSD<-function(distmean,min=5, den=1){
  return((distmean/den + min))
}

partWeight<-function(ops,mean,sd){
  p_ops<-dnorm(ops,mean,sd)
  return(p_ops)
}


initialize<-function(x,y,nPart,sd){
  x<-cbind(rnorm(n = nPart,mean=x,sd = sd))
  y<-cbind(rnorm(n = nPart,mean=y,sd = sd))
  return(cbind(x,y))
}


timeoff<-function(X,Y){
  lack<-event(is.na(X),ends=2)
  midX<-eventInterp(X,events = lack)
  midY<-eventInterp(Y,events = lack)
  TimeUnknown<-rep(0,length(X))
  
  for(i in 1:nrow(lack)){
    dur<-(lack[i,2]-lack[i,1]) + 1
    val<-seq(dur,1,by=-1)
    val2<-1:(round(dur/2 +.1))
    val[val2]<-val2
    
    TimeUnknown[(lack[i,1]:lack[i,2])]<-val
  }
  return(cbind(midX,midY,TimeUnknown))
}



pfRun<-function(SignalDB,X=PFdat$X,Y=PFdat$Y,headings,
                nPart=500,startSD=100,
                headSD = 90*(pi/180),
                speed = 2, speedSD = 2,
                near=10,maxD=200,midDB=70,rate=7,
                min = 10,den = .2,
                er=400,timebuff=300,
                chopexp=0,chopD=300){
  parts<-initialize(x=X[which(!is.na(X))[1]],y=Y[which(!is.na(Y))[1]],nPart = nPart,sd=startSD)
  
  X_p<-rep(0,length(headings))
  Y_p<-rep(0,length(headings))
  Y_sd<-rep(0,length(headings))
  X_sd<-rep(0,length(headings))
  
  unknownPos<-timeoff(X,Y)
  
  for(i in 1:length(headings)){
    pdists<-pdist(px = parts[,1],py = parts[,2],X = unknownPos[i,1],Y=unknownPos[i,2])
    
    if(!is.na(SignalDB[i])){
      pdists<-pdist(px = parts[,1],py = parts[,2],X = X[i],Y=Y[i])
      
      mean<-distmean(signalDB = SignalDB[i],near=near,maxD=maxD,midDB=midDB,rate=rate)
      sd<-distSD(mean,min = min,den = den)
      
      p_weight<-partWeight(ops = pdists,mean=mean,sd = sd)
      
      p_weight<-p_weight/sum(p_weight)
      
      
    }else{
      timex<-(unknownPos[i,3] +timebuff)
      start<-(1/sqrt(4*pi*er*timex))
      exponent<- exp(-1*(pdists^2)/(4*er*timex))
      p_weight<-start*exponent
      
      #cut of the top of the distribution 
      plim<-(1/sqrt(4*pi*er*timex))*exp(-1*((chopD + (chopexp*timex))^2)/(4*er*timex))
      p_weight[p_weight>plim]<-plim
    }
    #Standardize
    p_weight<-p_weight/sum(p_weight)
    
    #sample based on weight
    row<-sample(x = (1:nPart),size = nPart,prob = p_weight,replace = TRUE)
    parts<-parts[row,]
    
    heads<-head(nPart = nPart, head=headings[i], headSD = headSD)
    speeds<-speed(nPart = nPart, speed = speed, speedSD = speedSD)
    pdif<-locDif(heads = heads,speeds = speeds)
    
    parts<-move(loc = parts,locDif = pdif)
    
    X_p[i]<-mean(parts[,1])
    Y_p[i]<-mean(parts[,2])
    X_sd[i]<-sd(parts[,1])
    Y_sd[i]<-sd(parts[,2])
  }
  return(cbind( X_p, Y_p, X_sd, Y_sd))
}


eval<-function(estX=PF_forward[,1],estY=PF_forward[,2],Xob=PFdat$X,Yob=PFdat$Y,signOb=PFdat$Signal..dB.){
  dists<-pdist(px = estX,py = estY,X = Xob,Y=Yob)
  mean<-distmean(signalDB = signOb,near=10,maxD=200,midDB=70,rate=7)
  sd<-distSD(mean,min = 10,den = .2)
  p_weight<-partWeight(ops = dists,mean=mean,sd = sd)
  return(p_weight)
}


colorRampLegend<-function(x,y,at,labels,lims,height=0.012,width=0.001,laboff=0.0005,tick=0.0005,nlevels=180,legend.lab="",legoff=9){
  x0<-rep(x, nlevels)
  x1<-rep(x+width,nlevels)
  y0=seq(y,y+height,length.out=nlevels)
  y1=seq(y,y+height,length.out=nlevels)
  cols<-c(rev(tim.colors(nlevels/2)),tim.colors(nlevels/2))
  
  segments(x0=x0,x1=x1,y0=y0,y1=y1,col=cols,lwd=2)
  rect(xleft = x,ybottom = y,ytop = y+height,xright = x+width,lwd=2.3)
  
  loc<-round((at-lims[1])/(lims[2]-lims[1])*nlevels)
  text(x = x1[loc]+laboff,
       y = y1[loc],
       labels=labels,pos = 4)
  segments(x0=x1[loc],x1=(x1[loc]+tick),y0=y1[loc],y1=y1[loc],lwd=3)
  
  text(legend.lab,x= x+width*legoff,y=y0[nlevels/2],cex=1.5,font=2,srt=-90,pos=4)
}



