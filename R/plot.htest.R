plot.htest <-
function(x,col.region,conf.int,col.conf,border,lwd,pch,...)
 {
  if(missing(col.region))
   col.region="lightblue"
  if(missing(conf.int))
   conf.int=TRUE
  if(missing(col.conf))
   col.conf="gray"
  if(missing(border))
   border=1
  if(missing(lwd))
   lwd=2
  if(missing(pch))
   pch=13

   z=as.numeric(x$estimate[1])                                                  #get estimated values
   y=as.numeric(x$estimate[2])
   method=as.character(x$method)                                                #get method
   if(identical(method,"Welch Two Sample t-test"))                              #value of two sample t-test
    z=z-y
   mu0=as.numeric(x$null.value)                                                 #get mu0
   t0=as.numeric(x$statistic)                                                   #get t0
   df=as.numeric(x$parameter)                                                   #get dgrees of freedom out of t.test
   conf.level=attr(x$conf.int,"conf.level")                                     #get conf.level
   conf=x$conf.int[1]                                                           #get conf.int
   conf2=x$conf.int[2]
   alternative=as.character(x$alternative)                                      #get alternative

   plotx=seq(-4*sqrt((df+1)/(df-1)),4*sqrt((df+1)/(df-1)),length=1000)+mu0      #generate plotting coordinates
   ploty=dt(seq(-4*sqrt((df+1)/(df-1)),4*sqrt((df+1)/(df-1)),length=1000),df)

   plot(x=plotx,y=ploty,axes=FALSE,ylab="",xlab="",col="white",                 #empty plot
        xlim=c(min(plotx),max(plotx)+(max(plotx)-min(plotx))/3))

   axis(3,at=c(round(min(plotx),0),round(t0+mu0,2),mu0,round(max(plotx),0)),
           labels=c(round(min(plotx),0)-mu0,round(t0,2),0,round(max(plotx),0)-mu0))
   box()
   if(identical(alternative,"two.sided")==TRUE)                                 #plot conf.int of t-dist.
   {
    polygon(x=c(seq(min(plotx),qt((1-conf.level)/2,df)+mu0,length=1000),qt((1-conf.level)/2,df)+mu0),
            y=c(dt(seq(min(plotx)-mu0,qt((1-conf.level)/2,df),length=1000),df),0),col=col.region,border=NA)
    polygon(x=c(seq(max(plotx),qt(conf.level+(1-conf.level)/2,df)+mu0,length=1000),qt(conf.level+(1-conf.level)/2,df)+mu0),
            y=c(dt(seq(max(plotx)-mu0,qt(conf.level+(1-conf.level)/2,df),length=1000),df),0),col=col.region,border=NA)
    if(conf.int==TRUE)
     polygon(x=c(conf,conf,conf2,conf2),y=c(-1,max(ploty)+1,max(ploty)+1,-1),density=5,col=col.conf)
    lines(x=plotx,y=ploty,lwd=lwd,col=border)
    axis(1,at=c(round(min(plotx),0),qt((1-conf.level)/2,df)+mu0,mu0,qt(conf.level+(1-conf.level)/2,df)+mu0,round(max(plotx),0)),
         labels=c(round(min(plotx),0),round(qt((1-conf.level)/2,df),2)+mu0,mu0,round(qt(conf.level+(1-conf.level)/2,df),2)+mu0,round(max(plotx),0)))
   }
   if(identical(alternative,"less")==TRUE)
   {
    polygon(x=c(seq(min(plotx),qt(1-conf.level,df)+mu0,length=1000),qt(1-conf.level,df)+mu0),
            y=c(dt(seq(min(plotx)-mu0,qt(1-conf.level,df),length=1000),df),0),col=col.region,border=NA)
    if(conf.int==TRUE)
     polygon(x=c(min(plotx)-mu0,min(plotx)-mu0,conf2,conf2),y=c(-1,max(ploty)+1,max(ploty)+1,-1),density=5,col=col.conf)
    lines(x=plotx,y=ploty,lwd=lwd,col=border)
    axis(1,at=c(round(min(plotx),0),round((max(plotx)-mu0)/2+mu0),mu0,qt(1-conf.level,df)+mu0,round(max(plotx),0)),
         labels=c(round(min(plotx),0),round((max(plotx)-mu0)/2+mu0),mu0,round(qt(1-conf.level,df),2)+mu0,round(max(plotx),0)))
   }
   if(identical(alternative,"greater")==TRUE)
   {
    polygon(x=c(seq(max(plotx),qt(conf.level,df)+mu0,length=1000),qt(conf.level,df)+mu0),
            y=c(dt(seq(max(plotx)-mu0,qt(conf.level,df),length=1000),df),0),col=col.region,border=NA)
    if(conf.int==TRUE)
     polygon(x=c(conf,conf,max(plotx)+mu0,max(plotx)+mu0),y=c(-1,max(ploty)+1,max(ploty)+1,-1),density=5,col=col.conf)
    lines(x=plotx,y=ploty,lwd=lwd,col=border)
    axis(1,at=c(round(min(plotx),0),round((min(plotx)-mu0)/2+mu0),mu0,qt(conf.level,df)+mu0,round(max(plotx),0)),
         labels=c(round(min(plotx),0),round((min(plotx)-mu0)/2+mu0),mu0,round(qt(conf.level,df),2)+mu0,round(max(plotx),0)))
   }
   par(new=TRUE)
   plot(x=plotx,y=ploty,axes=FALSE,ylab="",xlab=x$data.name,type="l",           #plot t-distribution
        xlim=c(min(plotx),max(plotx)+(max(plotx)-min(plotx))/3),col=border,...)
   abline(h=0)
   abline(v=mu0,lty=2)
   abline(v=z,lwd=2,lty=6)
   lines(x=c(t0,t0)+mu0,y=c(0,dt(0,df)/2),col="red")
   points(t0+mu0,0,pch=pch,col="red",cex=2,lwd=1.5)
   text(t0+mu0,dt(0,df)/2,labels=round(t0+mu0,2),pos=3,col="red",lwd=2,font=2)

   if(conf.int==TRUE)
    legend("topright", xjust=0.5,yjust=0.5,legend=c(expression(H[0]),expression(mu),expression(t[fitted]),expression(alpha),"conf.int"),
           col=c(1,1,2,-1,-1),lty=c(2,6,1,-1,-1),lwd=c(1,2,1,-1,-1),fill=c("transparent","transparent","transparent",col.region,col.conf),
           border=c("transparent","transparent","transparent",1,1),density=c(-1,-1,-1,-1,25),merge=TRUE,y.intersp=2,inset=0.04,)
   else
    legend("topright", xjust=0.5,yjust=0.5,legend=c(expression(H[0]),expression(mu),expression(t[fitted]),expression(alpha)),
           col=c(1,1,2,-1),lty=c(2,6,1,-1),lwd=c(1,2,1,-1),fill=c("transparent","transparent","transparent",col.region),
           border=c("transparent","transparent","transparent",1),density=c(-1,-1,-1,-1),merge=TRUE,y.intersp=2,inset=0.04,)
   legend("topleft",legend=c(paste("t-value:",round(t0,2)),paste("df:",round(df,2))),y.intersp=2,bty="n")

invisible(conf.int)
 }

