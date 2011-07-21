plot.power.htest <-
function(x,col,col.line,lwd,main,xlab,ylab,...)
{
  DB = FALSE
   old.par <- par(no.readonly = TRUE)
   on.exit(par(old.par))
   if(missing(x))
    stop('x must be given as an object of class "power.htest"!')
   if(missing(col))
   {
    col=numeric(3)
    col[1]=rgb(1,0,0,0.5)
    col[2]=rgb(0,1,0,0.5)
    col[3]=rgb(0,0,1,0.5)
   }
   if(length(col)==1)
    col[2]=rgb(0,1,0,0.5)
   if(length(col)<3)
    col[3]="transparent"
   if(missing(col.line))
   {
    col.line=numeric(2)
    col.line[1]=1
    col.line[2]=1
   }
   if(length(col.line)<2)
    col.line[2]=col.line[1]
   if(missing(lwd))
   {
    lwd=numeric(2)
    lwd[1]=1
    lwd[2]=1
   }
   if(length(lwd)<2)
    lwd[2]=lwd[1]
   if(missing(main))
    main="Visualization: Power of t.test"
   if(missing(xlab))
    xlab="x"
   if(missing(ylab))
    ylab="f(x)"


 mu=abs(x$delta)                                                                #get real mu
 sd = NA

 if(identical(x$method,"One-sample t test power calculation"))
 {
   sd=x$sd/sqrt((x$n)-1)                                                        #get sd
 }
 if(identical(x$method,"Two-sample t test power calculation"))
 {
   sd=x$sd/sqrt((x$n-1)/2)                                                      #get sd
 }

 plotx1=seq(-4*sd,5*sd,length=1000)                                             #generate plotting coordinates
 plotx2=seq(-4*sd,5*sd,length=1000)+mu
 ploty=dnorm(seq(-4*sd,5*sd,length=1000),mean=0,sd)

 plot(c(plotx1,plotx2),c(rep(ploty,2)),main=main,xlab=xlab,ylab=ylab,
 bty="n",col="white",...)

 if(identical(x$alternative,"two.sided"))
 {
  polygon(x=c(min(plotx1),seq(min(plotx1),qnorm((x$sig.level)/2,mean=0,sd=sd),length=1000),qnorm((x$sig.level)/2,mean=0,sd=sd)),
          y=c(0,dnorm(seq(min(plotx1),qnorm((x$sig.level)/2,mean=0,sd=sd),length=1000),mean=0,sd=sd),0),col=col[1],border=NA)
  polygon(x=c(max(plotx1),seq(max(plotx1),qnorm(1-(x$sig.level)/2,mean=0,sd=sd),length=1000),qnorm(1-(x$sig.level)/2,mean=0,sd=sd)),
          y=c(0,dnorm(seq(max(plotx1),qnorm(1-(x$sig.level)/2,mean=0,sd=sd),length=1000),mean=0,sd=sd),0),col=col[1],border=NA)
  polygon(x=c(min(plotx1),seq(min(plotx2),qnorm(1-(x$sig.level)/2,mean=0,sd=sd),length=1000),qnorm(1-(x$sig.level)/2,mean=0,sd=sd)),
          y=c(0,dnorm(seq(min(plotx2)-mu,qnorm(1-(x$sig.level)/2,mean=0,sd=sd)-mu,length=1000),mean=0,sd=sd),0),col=col[2],border=NA)
  polygon(x=c(max(plotx2),seq(max(plotx2),qnorm(1-(x$sig.level)/2,mean=0,sd=sd),length=1000),qnorm(1-(x$sig.level)/2,mean=0,sd=sd)),
          y=c(0,dnorm(seq(max(plotx2)-mu,qnorm(1-(x$sig.level)/2,mean=0,sd=sd)-mu,length=1000),mean=0,sd=sd),0),col=col[3],border=NA)
  lines(plotx1,ploty,col=col.line[1],lwd=lwd[1])
  lines(plotx2,ploty,col=col.line[2],lwd=lwd[2])
 }
 if(identical(x$alternative,"one.sided"))
 {
  polygon(x=c(seq(max(plotx1),qnorm((1-x$sig.level),mean=0,sd=sd),length=1000),qnorm((1-x$sig.level),mean=0,sd=sd)),
          y=c(dnorm(seq(max(plotx1),qnorm((1-x$sig.level),mean=0,sd=sd),length=1000),mean=0,sd=sd),0),col=col[1],border=NA)
  polygon(x=c(seq(min(plotx2),qnorm(1-(x$sig.level),mean=0,sd=sd),length=1000),qnorm(1-(x$sig.level),mean=0,sd=sd)),
          y=c(dnorm(seq(min(plotx2)-mu,qnorm(1-(x$sig.level),mean=0,sd=sd)-mu,length=1000),mean=0,sd=sd),0),col=col[2],border=NA)
  polygon(x=c(max(plotx2),seq(max(plotx2),qnorm(1-(x$sig.level),mean=0,sd=sd),length=1000),qnorm(1-(x$sig.level),mean=0,sd=sd)),
          y=c(0,dnorm(seq(max(plotx2)-mu,qnorm(1-(x$sig.level),mean=0,sd=sd)-mu,length=1000),mean=0,sd=sd),0),col=col[3],border=NA)
  lines(plotx1,ploty,col=col.line[1],lwd=lwd[1])
  lines(plotx2,ploty,col=col.line[2],lwd=lwd[2])
 }
 abline(v=0)
 abline(v=mu,lty=2)
 if(DB==TRUE)
 {
  db=qnorm(1-x$power,sd=sd,mean=mu)
  if(identical(x$alternative,"one.sided"))
   db2=qnorm(1-(x$sig.level),mean=0,sd=sd)
  if(identical(x$alternative,"two.sided"))
   db2=qnorm(1-(x$sig.level)/2,mean=0,sd=sd)
  fail=abs(db2-db)
  abline(v=db,lty=5)
  abline(v=db2,col="orange")
  legend("topright",legend=c("Type I error","Type II error",x$sig.level,x$delta,x$sd,x$n,fail),
         fill=c(col[1],col[2],0,0,0,0,0),y.intersp=1,inset=0.04)
  }
  if(DB==FALSE)
  {
   legend("topright",legend=c(c(expression(bold("Type I error:")),x$sig.level,
          expression(bold("Type II error:")),paste(round(1-x$power,6),"(", round(1-x$power,4)*100,"%)"),
          expression(bold("Power:")),paste(round(x$power,6),"(", round(x$power,4)*100,"%)"))),
          fill=c(col[1],0,col[2],0,col[3],0),y.intersp=1,inset=0.04,border=c(1,0,1,0,1,0))
  }
 invisible()
}

