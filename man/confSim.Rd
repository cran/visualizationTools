\name{confSim}
\alias{confSim}
\title{
Visualization/Simulation of the Position of Confidence Intervals
}
\description{
Function visualize the positions of several confidence intervals of normal
distributed values in dependence of a few parameters as sample size, 
confidence level, etc.
}
\usage{
confSim(fun = mean, conf.level = 0.95, mu = 0, stdev = 1, sleep = 0.2, trials = 100, n = 8, N = 20, xlim, sim = TRUE)
}
\arguments{
  \item{fun}{
 gives the type of which confidence interval is regarded. Possibilities for fun 
 are in this version \code{mean} and \code{sd}. \code{mean} is the default and 
 will lead to a visualization of confidence intervals for the mean of the values
 of each trial. \code{sd} will lead to the visualization of the confidence intervals
 for the standard deviation.   
}
  \item{conf.level}{
 gives the confidence level for the confidence intervals. It must be set between 
 0 and 1. The default value for \code{conf.level} is 0.95. 
}
  \item{mu}{
 gives the mean of the random normal distributed values dragged for each trial.
 By default 0.
}
  \item{stdev}{
 gives the standard deviation of the random normal distributed values dragged 
 for each trial. By default 1.
}
  \item{sleep}{
 gives the time in seconds between each step of the visualization in which a 
 single line is drawn.
}
  \item{trials}{
 gives the number of trials, i.e. the number of samples that are drawn. The 
 default value is 100. 
}
  \item{n}{
 gives the sample size for each trial. By default there will be drawn 8 values 
 for each trial.
}
  \item{N}{
 gives the number of trials that will be displayed at the y axis at the same time.
 This parameter loses his function if \code{sim} ist set to FALSE. By default
 \code{N} will be 20
}
  \item{xlim}{
 gives the range of the x-axis and can be used to compare the visualization of
 different settings. If this value is missing the function will adjust
 the range automatically.
}
  \item{sim}{
 a logigal value specifying whether the visualization should be animated or not.
 By default \code{sim} is set to TRUE. 
}
}
\details{
\if{html}{
	The following graph shows the output of the function \code{confSim()}. The
  horizontal lines are marking the random confidence intervals. The vertical line
  gives the target value which is in this example the mean provided by the paramter 
  \code{fun}. Confidence intervals that do not cover the target value are plotted
  red, bold and dashed. On the right top of the graph is a legend that gives the 
  most important information about the plot and summarizes how many intervals did not
  cover the target value at all.              \cr
	
\Sexpr[stage=render,results=rd,echo=FALSE]{
  library( utils )
  if(identical(as.character(installed.packages()[,1]["base64"]),"base64")==FALSE)
  {
   install.packages("base64",repos="http://cran.r-project.org")
  }
	library( base64 )
	library( grDevices )
	library( graphics )
	library( stats )

	pngfile <- tempfile()
	png( pngfile, width = 600, height = 400 )
	
confSim <-
function(fun=mean,conf.level=0.95,mu=0,stdev=1,sleep=0.2,trials=100,n=8,N=20,xlim,sim=TRUE)
{
 set.seed(12345)
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
  confSim(fun=mean,trials=25,N=25,conf.level=0.8,sim=FALSE)
	dev.off()
	img( pngfile, Rd = TRUE )
}
}
}

\value{
 The function \code{confSim()} returns an invisible list with 2 entries,
 which contain the x-coordinates for the end points of every single 
 confidence interval.   
}
\author{
     Thomas Roth thomas.roth@tu-berlin.de\cr
     Etienne Stockhausen stocdarf@mailbox.tu-berlin.de
}
\note{
This function is not yet finished, but can be used as dexcribed above. \cr
It is planned to integrate further functions.
}
\seealso{
  \code{\link{Sys.sleep}}
}
\examples{
 confSim()                                                                      #function used with default settings
}