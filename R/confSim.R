confSim <-
function(fun=mean,conf.level=0.95,mu=0,stdev=1,sleep=0.2,trials=100,n=8,N=20,xlim,sim=TRUE)
{
 if(identical(fun,mean)==FALSE && identical(fun,sd)==FALSE)
  stop("function is not yet supported! ")
 if(missing(xlim))
 {if(identical(fun,mean))
   xlim=c(mu-7*stdev/sqrt(n),mu+10*stdev/sqrt(n))
  if(identical(fun,sd))
   xlim=c(0,1.75*stdev*sqrt(n-1))
 }
 old.par <- par(no.readonly = TRUE)
 on.exit(par(old.par))
 
 val=numeric(trials);q1=numeric(trials);q2=numeric(trials);count=0;temp=0;
 color=numeric(trials);lwd.vec=numeric(trials);lty.vec=numeric(trials)
                                                           
 for (i in 1:trials)
 {
  if(identical(fun,mean))
  {
   xlab=expression(bar(x))
   val=t.test(rnorm(n,mu,stdev),conf.level=conf.level)                           #t.test
   q1[i]=val$conf.int[[1]]
   q2[i]=val$conf.int[[2]]
   if (mu<q1[i]||mu>q2[i])
   {
    color[i]=2
    lwd.vec[i]=2
    lty.vec[i]=2
   }
   else
   {
    color[i]=1 
    lwd.vec[i]=1
    lty.vec[i]=1
   }
  }
  if(identical(fun,sd))
  {
   xlab=expression(sd)
   val=sd(rnorm(n,mu,stdev))                                  
   q1[i]=val*sqrt((n-1)/qchisq((1-conf.level)/2,df=n-1))
   q2[i]=val*sqrt((n-1)/qchisq(conf.level+(1-conf.level)/2,df=n-1))  
  if (stdev>q1[i]||stdev<q2[i])
   {
    color[i]=2
    lwd.vec[i]=2
    lty.vec[i]=2
   }
   else
   {
    color[i]=1 
    lwd.vec[i]=1
    lty.vec[i]=1
   }
  }  
 }
if(sim==TRUE)
{
for(i in 1:trials)
 {
  if(i<=N)                                                                      #Add first intervals
  {
   plot(mu,0,col="transparent",ylim=c(0,N),xlim=xlim,xlab=xlab,                #Basic plot
        ylab="Trial No.",main="Confidence Intervals")
   axis(2,at=0:N,labels=NA,tcl=-0.25)
   if(identical(fun,mean))
    abline(v=mu)
   if(identical(fun,sd))
    abline(v=stdev) 
   for(j in 1:i)
   {
    if(conf.level==1)
     abline(h=j)
    lines(x=c(q1[j],q2[j]),y=c(j,j),col=color[j],lwd=lwd.vec[j],lty=lty.vec[j])
    lines(x=c(q1[j],q1[j]),y=c(j+0.01*N,j-0.01*N),col=color[j],lwd=lwd.vec[j],lty=lty.vec[j])
    lines(x=c(q2[j],q2[j]),y=c(j+0.01*N,j-0.01*N),col=color[j],lwd=lwd.vec[j],lty=lty.vec[j])
   }
   Sys.sleep(sleep)
   if(identical(fun,mean))
    {
     if (mu<q1[i]||mu>q2[i])
     count=count+1
    }
    if(identical(fun,sd))
    {
     if (stdev>q1[i]||stdev<q2[i])
     count=count+1
    }
  }
  if(i>N)
  {
   plot(mu,0,col="transparent",ylim=c(i-N,i),xlim=xlim,xlab=xlab,             #Basic plot
        ylab="Trial No.",main="Confidence Intervals")
   axis(2,at=seq(i-N+1,i,length=N),labels=NA,tcl=-0.25)     
   if(identical(fun,mean))
    abline(v=mu)
   if(identical(fun,sd))
    abline(v=stdev)
   for(j in 0:N)
   {
    if(conf.level==1)
     abline(h=i-j)
    lines(x=c(q1[i-j],q2[i-j]),y=c(i-j,i-j),col=color[i-j],lwd=lwd.vec[i-j],lty=lty.vec[i-j])                     #Add further intervals
    lines(x=c(q1[i-j],q1[i-j]),y=c((i-j)+0.01*N,(i-j)-0.01*N),col=color[i-j],lwd=lwd.vec[i-j],lty=lty.vec[i-j])
    lines(x=c(q2[i-j],q2[i-j]),y=c((i-j)+0.01*N,(i-j)-0.01*N),col=color[i-j],lwd=lwd.vec[i-j],lty=lty.vec[i-j])
   }
   Sys.sleep(sleep)
   if(identical(fun,mean))
    {
     if (mu<q1[i]||mu>q2[i])
     count=count+1
    }
    if(identical(fun,sd))
    {
     if (stdev>q1[i]||stdev<q2[i])
     count=count+1
    }
  }
  legend("topright",legend=c(paste("Trials:",trials),paste("Size:",n),paste("mean:",mu),
        paste("sd:",stdev),paste("conf.level:",conf.level),paste("Out:",count)),inset=0.04,bg="white")
 }
}
if(sim==FALSE)
{
 plot(mu,0,col="transparent",ylim=c(0,trials),xlim=xlim,xlab=xlab,             #Basic plot
      ylab="Trial No.",main="Confidence Intervals of normal random numbers")
 axis(2,at=0:trials,labels=NA,tcl=-0.25)
 if(identical(fun,mean))
    abline(v=mu)
 if(identical(fun,sd))
    abline(v=stdev)
 for(i in 1:trials)
{
    if(conf.level==1)
     abline(h=i)
    lines(x=c(q1[i],q2[i]),y=c(i,i),col=color[i],lwd=lwd.vec[i],lty=lty.vec[i])
    lines(x=c(q1[i],q1[i]),y=c(i+0.005*trials,i-0.005*trials),col=color[i],lwd=lwd.vec[i],lty=lty.vec[i])
    lines(x=c(q2[i],q2[i]),y=c(i+0.005*trials,i-0.005*trials),col=color[i],lwd=lwd.vec[i],lty=lty.vec[i])  
    if(identical(fun,mean))
    {
     if (mu<q1[i]||mu>q2[i])
     count=count+1
    }
    if(identical(fun,sd))
    {
     if (stdev>q1[i]||stdev<q2[i])
     count=count+1
    }
 }
 legend("topright",legend=c(paste("Trials:",trials),paste("Size:",n),paste("mean:",mu),
        paste("sd:",stdev),paste("conf.level:",conf.level),paste("Out:",count)),inset=0.04,bg="white")
}
invisible(list(q1,q2))
}

