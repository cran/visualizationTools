\name{qrk}
\alias{qrk}
\title{
  Simulation of wrong permanent regulation in quality-control-charts.
}
\description{
  Simulate different types of control charts and shows the influence of a wrong  
  permanent regulation of the considered values.
}
\usage{
qrk(FUN, mu, stdev, n, N, cl, wl, sl, numPlot, seed, rest, ...)
}

\arguments{
  \item{FUN}{
     gives the type of the control chart. The implemented 
     possibilities are the functions mean, median and sd.
     For an individual observation chart mean and median can be used but
     n must be set to \sQuote{1}. Default value is \sQuote{mean}. 
}
  \item{mu}{
     is the expected value of the randomised single values 
     (normal distribution). Default value is \sQuote{1}. 
}
  \item{stdev}{
     is the standard deviation of the randomised single values
     (normal distribution). Default value is \sQuote{0.1}.  
}
  \item{n}{
     gives the sample size of the regarded groups. 
     Default value is \sQuote{1}.  
}
  \item{N}{
     is the number of points that will be plotted after the 
     limits are calculated and drawn. Default value is \sQuote{1000}. 
}
  \item{cl}{
     A vector giving the quantiles of the control lines. 
     By default \sQuote{c(0.00135,0.99865)}.
}
  \item{wl}{
     A vector giving the quantiles of the warning lines.
     By default \sQuote{c(0.0225,0.9775)}.
}
  \item{sl}{
     A vector optional giving the position of the tolerances. By default 
     \sQuote{FALSE} (not drawn)
}
  \item{numPlot}{
     gives the number of points visualised on the screen before the points is moving.
     Default value is \sQuote{20}. 
}
  \item{seed}{
     Variable for \code{\link{set.seed}} to generate repeatable results.
     Default value is \sQuote{125879}. 
}
  \item{rest}{
     Value for \code{\link{Sys.sleep}} i.e. break time in seconds between the plotting of the points.
     Default value is \sQuote{0.1}.
}
  \item{\dots}{
     Further graphical parameters see \code{\link{par}}
}
}
\details{
 The adjustment of the next value is be shown by the blue arrow on the right
 side of the plot. \cr 

\if{html}{
	The following graph shows the output of the function \code{qrk()}. The plots are
  truncated and the legend boxes are shifted in the HTML-version of this graph. The
  original version is much better and can be obtained with for instance 
  \code{example(qrk)}.\cr
  The plot region is divided in four parts. In the topleft and in the bottomleft
  are each time a quality-control-chart with indicated sigma limits (black, dashed-pointed
  line), warn-limits (red, dashed line) and control limits (red, bold, dashed line).
  The quality-control chart in the topleftz shows the behavior of measured points,
  if there is a wrong permanent process regulation, whereby the one in the bottomleft
  shows a quality-control-chart without any regulation. The regulation is visualized
  with the blue arrow at the end of both plots wich simulate an adjusting of the 
  process (e.g. changing parameters of a machine).\cr
  Right to the quality-control-charts are (topright and bottomright) are in each 
  case a legend that provides the user with the most important information about
  the running process. In the given example the mean value, the standard deviation
  and the number of measured points beyond the control-limits for the running process
  are displayed. Furthermore there is a legend that explains the different lines 
  used in the plots and a counter for the plotted values.  
	
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
	
.charToDistfunc <-
function(distribution, type = "r")
{
   fun = NULL

   if(identical("beta",distribution))
    fun = eval(parse(text = paste(type, "beta", sep = "")))
   if(identical("cauchy",distribution))
     fun = eval(parse(text = paste(type, "cauchy", sep ="")))
   if(identical("chi-squared",distribution))
     fun = eval(parse(text = paste(type, "chisq", sep ="")))
   if(identical("exponential",distribution))
     fun = eval(parse(text = paste(type, "exp", sep ="")))
   if(identical("f",distribution))
     fun = eval(parse(text = paste(type, "f", sep ="")))
   if(identical("log-normal",distribution))
     fun = eval(parse(text = paste(type, "lnorm", sep ="")))
   if(identical("logistic",distribution))
     fun = eval(parse(text = paste(type, "logis", sep ="")))
   if(identical("negative binomial",distribution))
     fun = eval(parse(text = paste(type, "nbinom", sep ="")))
   if(identical("normal",distribution))
     fun = eval(parse(text = paste(type, "norm", sep ="")))
   if(identical("poisson",distribution))
     fun = eval(parse(text = paste(type, "pois", sep ="")))
   if(identical("t",distribution))
     fun = eval(parse(text = paste(type, "t", sep ="")))
   if(identical("weibull",distribution))
     fun = eval(parse(text = paste(type, "weibull", sep ="")))
   if(identical("gamma",distribution))
    fun = eval(parse(text = paste(type, "gamma", sep = "")))
   if(identical("unif",distribution))
    fun = eval(parse(text = paste(type, "unif", sep = "")))
   return(fun)
}

.FUNk <-
function(mu,stdev,n)                                                      #Function to calculate Factor k
{
help1=c()
mean1=c()
median1=c()

for(i in 1:10000)
{
help1=rnorm(n,mu,stdev)
mean1[i]=mean(help1)
median1[i]=median(help1)
}
k=sd(median1)/sd(mean1)
return(k)
}

qrk <-
function(FUN,mu,stdev,n,N,cl,wl,sl,numPlot,seed,rest,...)
{
 old.par <- par(no.readonly = TRUE)
 on.exit(par(old.par))
 if(missing(FUN))
   FUN=mean
 if(missing(mu))
   mu=1
 if(missing(stdev))
   stdev=0.1
 if(missing(n))
   n=1
 if(missing(N))
   N=1000
 if(missing(cl))
   cl=c(0.00135,0.99865)
 if(missing(wl))
   wl=c(0.0225,0.9775)
 if(missing(sl))
   sl=FALSE
 if(missing(numPlot))
   numPlot=20
 if(missing(seed))
   seed=125879
 if(missing(rest))
   rest=0.1

 set.seed(seed)

 konst=mu                                                                       #constant expected value
 stdevvar=stdev
 i=1
 count1=0; count2=0                                                             #count values out of control
 temp1=c(n*N); temp2=c(n*N)                                                     #helping values
 group=c(n*N)                                                                   #group for values
 Value1=c(N); Value2=c(N)                                                       #values
 temp1[1:n]=rnorm(n,mu,stdevvar); temp2[1:n]=rnorm(n,mu,stdev)
 group[1:n]=1
 Value1[1]=match.fun(FUN)(temp1); Value2[1]=match.fun(FUN)(temp2)               #initial values
 singsd=c()                                                                     #stdev of the groups
 singsd[1]=sd(temp2[1:n])
 FUNname=deparse(substitute(FUN))
 if(n>1)
  cn=sqrt(2/(n-1))*(factorial(n/2 - 1) / factorial((n-1)/2 - 1))                #Correction factor (mean)
 if(FUNname=="median" && n!=1)
  k=.FUNk(konst,stdev,n)                                                         #Correction factor (median)


  if(FUNname!="sd" && is.numeric(sl)==FALSE)                                    #ylim for plots
   ylimes=c(konst-4*stdev/sqrt(n),konst+4*stdev/sqrt(n))
  if(FUNname=="sd" && is.numeric(sl)==FALSE)
   ylimes=c(stdev-stdev,stdev+1.5*stdev)
  if(FUNname!="sd" && is.numeric(sl)==TRUE)
   ylimes=c(min(sl,c(konst-4*stdev/sqrt(n),konst+4*stdev/sqrt(n))),max(sl,c(konst-4*stdev/sqrt(n),konst+4*stdev/sqrt(n))))
  if(FUNname=="sd" && is.numeric(sl)==TRUE)
   ylimes=c(min(sl,c(stdev-1.5*stdev,stdev+1.5*stdev)),max(sl,c(stdev-1.5*stdev,stdev+1.5*stdev)))

 if(n==1)
  FRun=125
 else
  FRun=round(125/n)

 if(n==1)
 {
  headline1=expression("Individual observation chart - Permanent regulation")
  headline2=expression("Individual observation chart - No permanent regulation")
  ylabel=expression(x)
 }
 if(n>1 && FUNname=="mean")
 {
  headline1=expression(paste(bar(x),"-chart - Permanent regulation"))
  headline2=expression(paste(bar(x),"-chart - No permanent regulation"))
  ylabel=expression(bar(x))
 }
 if(n>1 && FUNname=="median")
 {
  headline1=expression(paste(tilde(x),"-chart - Permanent regulation"))
  headline2=expression(paste(tilde(x),"-chart - No permanent regulation"))
  ylabel=expression(tilde(x))
 }
 if(FUNname=="sd")
 {
  headline1=expression("s-chart - Permanent regulation")
  headline2=expression("s-chart - No permanent regulation")
  ylabel=expression(s)
 }

 while (i<N+FRun)
 {
  Sys.sleep(rest)                                                               #System take a break for better visualisation
  temp1[(i*n+1):(n*(i+1))]=rnorm(n,mu,stdev)
  group[(i*n+1):(n*(i+1))]=i+1
  Value1[i+1]=match.fun(FUN)(temp1[(i*n+1):(n*(i+1))])                          #continous corrected values
  temp2[(i*n+1):(n*(i+1))]=rnorm(n,konst,stdev)
  Value2[i+1]=match.fun(FUN)(temp2[(i*n+1):(n*(i+1))])                          #values for normal process

  singsd[i+1]=sd(temp2[(i*n+1):(n*(i+1))])                                      #stdev of the groups

  if(i>FRun)
  {
   if(n==1)
    S=sd(Value2[1:FRun])/sqrt(n)                                                #sigma for calculating control and warn limits
   if(n>1 && FUNname=="mean")
    S=mean(singsd[1:FRun])/(cn*sqrt(n))
   if(n>1 && FUNname=="median")
    S=k*mean(singsd[1:FRun])/(sqrt(n))
   if(FUNname!="sd")
   {
    lcl=qnorm(cl[1],konst,S);ucl=qnorm(cl[2],konst,S)                           #lower and upper control lines
    lwl=qnorm(wl[1],konst,S);uwl=qnorm(wl[2],konst,S)                           #lower and upper warning lines
   }
   if(FUNname=="sd")
   {
    lcl=sqrt(qchisq(cl[1],n)/n)*mean(singsd[1:FRun]); ucl=sqrt(qchisq(cl[2],n)/n)*mean(singsd[1:FRun])
    lwl=sqrt(qchisq(wl[1],n)/n)*mean(singsd[1:FRun]); uwl=sqrt(qchisq(wl[2],n)/n)*mean(singsd[1:FRun])
   }
  }
  if(i>FRun)
  {
   if(Value1[i+1]  < lcl || Value1[i+1] > ucl)
    count1=count1+1                                                             #counting values out of control space
  }
  if(i>FRun)
  {
   if(Value2[i+1]  < lcl || Value2[i+1] > ucl)
    count2=count2+1                                                             #counting values out of control space
  }
  mu=mu+(konst-Value1[i+1])                                                     #Correct expectation for next
  i=i+1                                                                         #counting variable for loop

  if(i<FRun)
   subt="! First running - CALCULATING CONTROL LIMITS !"
  else
   subt=""

  split.screen(matrix(c(0,0.75,0.5,1, 0,0.75,0,0.5, 0.5,1,0.5,1 , 0.5,1,0,0.5), #split output window and adjust size
  byrow=TRUE,ncol=4))
  screen(1)                                                                     #selecting screen
  if(i<numPlot)
  {
   plot(Value1,xlim=c(0,numPlot+2),axes=FALSE,ylim=ylimes,
   main=headline1,xlab="",ylab=ylabel,sub=subt,...)
   if(is.numeric(sl)==TRUE)
    {abline(sl[1],0); abline(sl[2],0)}
  }
  else
  {
   par(xaxs="r")
   plot(Value1[(i-numPlot):i],xlim=c(0,numPlot+2),axes=FALSE,ylim=ylimes,
   main=headline1,xlab="",ylab=ylabel,sub=subt,...)
   if(is.numeric(sl)==TRUE)
    {abline(sl[1],0); abline(sl[2],0)}
  }
  par(mar=c(4,4,3,2))                                                           #adjust size of plotting space
  axis(2)
  box(col="grey")
  if(i > FRun+1)
   {
    if(FUNname!="sd")
    {
     abline(konst+S,0,lty=4,col="black",lwd=0.1);abline(konst+2*S,0,lty=4,col="black",lwd=0.1)
     abline(konst+3*S,0,lty=4,col="black",lwd=0.1);abline(konst-S,0,lty=4,col="black",lwd=0.1)
     abline(konst-2*S,0,lty=4,col="black",lwd=0.1);abline(konst-3*S,0,lty=4,col="black",lwd=0.1)
      abline(lcl,0,lty=2,col="red",lwd=3); abline(ucl,0,lty=2,col="red",lwd=3)     #set control lines
      abline(lwl,0,lty=2,col="red",lwd=2); abline(uwl,0,lty=2,col="red",lwd=2)     #set warning lines
     text(0,konst-1.3*S,labels="15.60",cex=0.5);text(0,konst-2.3*S,labels="2.25",cex=0.5)
     text(0,konst-3.3*S,labels="0.15",cex=0.5);text(0,konst+1.4*S,labels="84.40",cex=0.5)
     text(0,konst+2.4*S,labels="97.75",cex=0.5);text(0,konst+3.4*S,labels="99.85",cex=0.5)
    }
    else
    {
     abline(sqrt(qchisq(0.156,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1);abline(sqrt(qchisq(0.0225,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1)
     abline(sqrt(qchisq(0.00135,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1);abline(sqrt(qchisq(0.844,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1)
     abline(sqrt(qchisq(0.9775,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1);abline(sqrt(qchisq(0.99865,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1)
      abline(lcl,0,lty=2,col="red",lwd=3); abline(ucl,0,lty=2,col="red",lwd=3)     #set control lines
      abline(lwl,0,lty=2,col="red",lwd=2); abline(uwl,0,lty=2,col="red",lwd=2)     #set warning lines
     text(0,sqrt(qchisq(0.0001,n)/n)*mean(singsd[1:FRun]),labels="0.15",cex=0.5);text(0,sqrt(qchisq(0.91,n)/n)*mean(singsd[1:FRun]),labels="84.40",cex=0.5)
     text(0,sqrt(qchisq(0.1,n)/n)*mean(singsd[1:FRun]),labels="15.60",cex=0.5);text(0,sqrt(qchisq(0.01,n)/n)*mean(singsd[1:FRun]),labels="2.25",cex=0.5)
     text(0,sqrt(qchisq(0.991,n)/n)*mean(singsd[1:FRun]),labels="97.75",cex=0.5);text(0,sqrt(qchisq(0.9996,n)/n)*mean(singsd[1:FRun]),labels="99.85",cex=0.5)
    }
   }
  if(FUNname!="sd")
  {
   abline(konst,0,lwd=2)                                                        #set line showing mean value
   arrows(numPlot+1,mu,numPlot+3,mu,length=0.1,code=1,col="blue",lwd=2)         #arrow showing the expected mean value
  }
  if(FUNname=="sd")
  {
   abline(stdev,0,lwd=2)
   arrows(numPlot+1,Value1[i],numPlot+3,Value1[i],length=0.1,code=1,col="blue",lwd=2)
  }
  screen(3)
  plot(seq(1,10),col="white",axes=FALSE,xlab="",ylab="")                        #empty plot necessary for using text
  par(mar=c(0,0,0,0))
  text(6.5,2,"Out",adj=1); text(7,2,"="); text(9,2,count1,adj=0.4)              #insert counter for values out of control lines
  text(6.5,4,"S",adj=1); text(7,4,"="); text(9,4,round(sd(Value1),2),font=2,adj=0.4)#insert sigma
  if(n==1)
   text(6.5,6,expression(bar(x)),adj=1)
  if(FUNname=="mean" && n>1)
   text(6.5,6,expression(bar(X)),adj=1)
  if(FUNname=="median" && n>1)
   text(6.5,6,expression(bar(tilde(X))),adj=1)
  if(FUNname=="sd")
   text(6.5,6,expression(bar(s)),adj=1)
  text(7,6,"="); text(9,6,round(mean(Value1),2),adj=0.4)                        #insert expected mean value
  text(6,11,"Values:")
  if(i>FRun)
   text(9,11,i-FRun)
  else text(9,11,"0")
  lines(c(5,11),c(0,0),lwd=2); lines(c(5,11),c(8,8))                            #draw box around the text
  lines(c(5,5),c(0,8)); lines(c(11,11),c(0,8),lwd=2)

 screen(2)                                                                      #selecting screen
  if(i<numPlot)
  {
   plot(Value2,xlim=c(0,numPlot+2),axes=FALSE,ylim=ylimes,
   main=headline2,xlab="",ylab=ylabel,...)
   if(is.numeric(sl)==TRUE)
    {abline(sl[1],0); abline(sl[2],0)}
  }
  else
  {
   plot(Value2[(i-numPlot):i],xlim=c(0,numPlot+2),axes=FALSE,ylim=ylimes,
   main=headline2,xlab="",ylab=ylabel,...)
   if(is.numeric(sl)==TRUE)
    {abline(sl[1],0); abline(sl[2],0)}
  }
  par(mar=c(4,4,3,2))                                                           #adjust size of plotting space
  axis(2)
  box(col="grey")
   if(i > FRun+1)
   {
    if(FUNname!="sd")
    {
     abline(konst+S,0,lty=4,col="black",lwd=0.1);abline(konst+2*S,0,lty=4,col="black",lwd=0.1)
     abline(konst+3*S,0,lty=4,col="black",lwd=0.1);abline(konst-S,0,lty=4,col="black",lwd=0.1)
     abline(konst-2*S,0,lty=4,col="black",lwd=0.1);abline(konst-3*S,0,lty=4,col="black",lwd=0.1)
      abline(lcl,0,lty=2,col="red",lwd=3); abline(ucl,0,lty=2,col="red",lwd=3)     #set control lines
      abline(lwl,0,lty=2,col="red",lwd=2); abline(uwl,0,lty=2,col="red",lwd=2)     #set warning lines
     text(0,konst-1.3*S,labels="15.60",cex=0.5);text(0,konst-2.3*S,labels="2.25",cex=0.5)
     text(0,konst-3.3*S,labels="0.15",cex=0.5);text(0,konst+1.4*S,labels="84.40",cex=0.5)
     text(0,konst+2.4*S,labels="97.75",cex=0.5);text(0,konst+3.4*S,labels="99.85",cex=0.5)
    }
    else
    {
     abline(sqrt(qchisq(0.156,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1);abline(sqrt(qchisq(0.0225,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1)
     abline(sqrt(qchisq(0.00135,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1);abline(sqrt(qchisq(0.844,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1)
     abline(sqrt(qchisq(0.9775,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1);abline(sqrt(qchisq(0.99865,n)/n)*mean(singsd[1:FRun]),0,lty=4,col="black",lwd=0.1)
      abline(lcl,0,lty=2,col="red",lwd=3); abline(ucl,0,lty=2,col="red",lwd=3)     #set control lines
      abline(lwl,0,lty=2,col="red",lwd=2); abline(uwl,0,lty=2,col="red",lwd=2)     #set warning lines
     text(0,sqrt(qchisq(0.0001,n)/n)*mean(singsd[1:FRun]),labels="0.15",cex=0.5);text(0,sqrt(qchisq(0.91,n)/n)*mean(singsd[1:FRun]),labels="84.40",cex=0.5)
     text(0,sqrt(qchisq(0.1,n)/n)*mean(singsd[1:FRun]),labels="15.60",cex=0.5);text(0,sqrt(qchisq(0.01,n)/n)*mean(singsd[1:FRun]),labels="2.25",cex=0.5)
     text(0,sqrt(qchisq(0.991,n)/n)*mean(singsd[1:FRun]),labels="97.75",cex=0.5);text(0,sqrt(qchisq(0.9996,n)/n)*mean(singsd[1:FRun]),labels="99.85",cex=0.5)
    }
   }
  if(FUNname!="sd")
  {
   abline(konst,0,lwd=2)                                                        #set line showing mean value
   arrows(numPlot+1,konst,numPlot+3,konst,length=0.1,code=1,col="blue",lwd=2)   #arrow showing the expected mean value
  }
  if(FUNname=="sd")
  {
   abline(stdev,0,lwd=2)
   arrows(numPlot+1,stdev,numPlot+3,stdev,length=0.1,code=1,col="blue",lwd=2)
  }
  screen(4)
  plot(seq(1,10),col="white",axes=FALSE,xlab="",ylab="")                        #empty plot necessary for using text
  par(mar=c(0,0,0,0))
  text(6.5,5,"Out",adj=1); text(7,5,"="); text(9,5,count2,adj=0.4)              #insert counter for values out of control lines
  text(6.5,7,"S",adj=1); text(7,7,"="); text(9,7,round(sd(Value2),2),font=2,adj=0.4)#insert sigma
  if(n==1)
   text(6.5,9,expression(bar(x)),adj=1)
  if(FUNname=="mean" && n>1)
   text(6.5,9,expression(bar(X)),adj=1)
  if(FUNname=="median" && n>1)
   text(6.5,9,expression(bar(tilde(X))),adj=1)
  if(FUNname=="sd")
   text(6.5,9,expression(bar(s)),adj=1)
  text(7,9,"="); text(9,9,round(mean(Value2),2),adj=0.4)                        #insert expected mean value
  lines(c(5,11),c(3,3),lwd=2); lines(c(5,11),c(11,11))                          #draw box around the text
  lines(c(5,5),c(3,11)); lines(c(11,11),c(3,11),lwd=2)
  legend(x=5,y=2,legend="control-limits",col="red",text.col="black",lty=2,lwd=3,bty="n")    #legend
  legend(x=5,y=0.5,legend="warn-limits",col="red",text.col="black",lty=2,lwd=2,bty="n")
  legend(x=5,y=-1,legend="sigma-limits",col="black",text.col="black",lty=4,lwd=0.1,bty="n")
  lines(c(5,5),c(-2.5,2)); lines(c(11,11),c(-2.5,2),lwd=2)                          #draw box around the legend
  lines(c(5,11),c(-2.5,-2.5),lwd=2); lines(c(5,11),c(2,2))


  Sys.sleep(rest)                                                               #System take a break for better visualisation
  close.screen(all=TRUE)
 }

if(length(Value1)!=length(temp1))
 {
  Value1[(N+FRun+1):(n*(N+FRun))]=NA
  Value2[(N+FRun+1):(n*(N+FRun))]=NA
 }
output=data.frame(Value1,Value2,"|",group,temp1,temp2)
names(output)=c("Regulated Values","Not-regulated Values","|","Group","Single regulated Values","Single not-regulated Values")
invisible(output)
}

  qrk(type="o",pch=20,n=5,FUN=mean,N=50,rest=0,seed=12345)
	dev.off()
	img( pngfile, Rd = TRUE )
}
}
}


\value{
 \sQuote{qrk} returns a data.frame in which plotted regulated and not-regulated values
 are listed, as well as the single values and for both , which are given in groups 
 to \sQuote{FUN} to obtain the plotted values.  
}
\references{
 \itemize{
   \item Vardeman, Stephen B. & Jobe, J. Marcus (1999) Statistical Quality Assurance Methods 
   for Engineers, 1st ed. New York: John Wiley & Sons.\cr
    
  \item Montgomery, D.C. (2005) Introduction to
   Statistical Quality Control, 5th ed. New York: John Wiley & Sons.
 }   
}
\author{
  Thomas Roth: thomas.roth@tu-berlin.de\cr
  Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}

\seealso{
 \code{\link{par}}\cr
 \code{\link{set.seed}}\cr
 \code{\link{Sys.sleep}} 
}
\examples{
  par(ask=FALSE)
  qrk(type="o",pch=20,n=5,FUN=mean,N=500,rest=0)
  par(ask=TRUE)
}

