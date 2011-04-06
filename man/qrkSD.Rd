\name{qrkSD}
\alias{qrkSD}
\title{
 Simulation of a spread or a drift in control charts.
}            
\description{
 \sQuote{qrkSD} shows the influence of a spread or a drift in control charts.
 Therefore the breaks of the 4 main Western Electric alarm rules are be counted
 and visualized. The spread or/ and the drift of the normal distribution 
 represented in the control chart is visualized too.  
}
\usage{
qrkSD(FUN, mu, stdev, n, N, cl, wl, sl, numPlot, seed, rest, 
      drift, spread, start, ENDdr, ENDsp, speed, ...)
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
     \sQuote{FALSE} (not drawn).
}
  \item{numPlot}{
     gives the number of points visualised on the screen before the points are moving.
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
  \item{drift}{
     A logical value indicating whether a drift should be initialized or not.
     Default value is \sQuote{FALSE}.
}
  \item{spread}{
     A logical value indicating whether a spread should be initialized or not.
     Default value is \sQuote{FALSE}.
}
  \item{start}{
     A numeric value between 0 and 1. \sQuote{start} gives the percentage of passed 
     plotted points after which a spread and/ or  drift should be initialized. 
     \sQuote{start} has no effect if both parameter \sQuote{spread} and \sQuote{drift}
     are set as \sQuote{FALSE}. Default value is \sQuote{0}.
}
  \item{ENDdr}{
     A positive numeric value. \sQuote{ENDdr} factorised with \sQuote{stdev}
     gives the value on which the drift will be end if the mean reaches there. 
     Default value is \sQuote{0}. 
}
  \item{ENDsp}{
     A positive numeric value. \sQuote{ENDdr} factorised with \sQuote{stdev}
     gives the value on which the spread will be end if the standard deviation 
     reaches there. Default value is \sQuote{0}. 
}
  \item{speed}{
     A postive numeric value. Controls as a factor the speed of the spread and/
     or the drift.
}
  \item{\dots}{
     Further graphical parameters see \code{\link{par}}
}
}
\details{
 On the left side of the output window the control chart is shown in which each
 violation of one of the 4 main Western Electric alarm rules ist spotted out with 
 red pointmarks.\cr
 On the right upper side the violations for each rule are counted.\cr
 
 The 4 main Western Electric alarm rules are:
 \itemize{ 
  \item Rule 1: One point outside of the control limits.
  \item Rule 2: Two of three consecutive points outside the warning limits but inside the control limits.
  \item Rule 3: Four of five consecutive points beyond the one-sigma limits.
  \item Rule 4: Eight consecutive points on one side of the center.
  }

\if{html}{
	The following graph shows the output of the function \code{qrkSD}. On the left 
  is a quality-control-chart with indicated sigma limits (black, dashed-pointed
  line), warn-limits (red, dashed line) and control limits (red, bold, dashed line).
  The measured points are plotted in black and will turn red, if one of the Western
  Electric alarm rules is infringed. These infringements are counted for each rule
  on the topright. Furthermore the most important values about the plotted measurement
  points, like the mean value and the standard deviation are provided there too.
  On the bottomleft is a plot that shows the distribution of values during the process
  and visalize the spread and/ or the drift of the measured values. The grey curve
  gives the distribution in the beginning and the blue line gives the current 
  diostribution of measured and plotted values in the control-chart.	
	
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

qrkSD <-
function(FUN,mu,stdev,n,N,cl,wl,sl,numPlot,seed,rest,drift,spread,start,ENDdr,ENDsp,speed,...)
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
   seed=01245
 if(missing(rest))                                                              #Time between the warnings
   rest=0.1
 if(missing(drift))                                                             #initiate a drift
   drift=FALSE
 if(missing(spread))                                                            #initiate spreading of stdev
   spread=FALSE
 if(missing(start))                                                             #Gives the startpoint for the drift/spread in percent of N
   start=0
 if(start < 0 || start > 1)
   stop("start has to be a number between 0 and 1")
 if(missing(ENDdr))                                                             #Gives the endpoint for the drift in sigma limits
   ENDdr=0
 if(missing(ENDsp))                                                             #Gives the endpoint for the spread in relation to stdev
   ENDsp=0
 if(missing(speed))                                                             #velocity of the drift
   speed=1
 if(speed<=0)
   stop("speed must be a positive value except 0")

 set.seed(seed)

 i=1
 count1=0;count2=0;count3=0;count4=0                                            #count values for Western Electric Alarm Rules
 temp=c(n*N)                                                                    #helping values
 group=c(n*N)                                                                   #group for values
 Value=c(N)                                                                     #values
 temp[1:n]=rnorm(n,mu,stdev)
 group[1:n]=1
 Value[1]=match.fun(FUN)(temp)                                                  #initial values
 singsd=c()                                                                     #stdev of the groups
 singsd[1]=sd(temp[1:n])
 FUNname=deparse(substitute(FUN))
 if(n>1)
  cn=sqrt(2/(n-1))*(factorial(n/2 - 1) / factorial((n-1)/2 - 1))                #Correction factor (mean)
 if(FUNname=="median" && n!=1)
  k=.FUNk(mu,stdev,n)                                                           #Correction factor (median)


  if(FUNname!="sd" && is.numeric(sl)==FALSE)                                    #ylim for plots
   ylimes=c(mu-4*stdev/sqrt(n),mu+4*stdev/sqrt(n))
  if(FUNname=="sd" && is.numeric(sl)==FALSE)
   ylimes=c(stdev-stdev,stdev+1.5*stdev)
  if(FUNname!="sd" && is.numeric(sl)==TRUE)
   ylimes=c(min(sl,c(mu-4*stdev/sqrt(n),mu+4*stdev/sqrt(n))),max(sl,c(mu-4*stdev/sqrt(n),mu+4*stdev/sqrt(n))))
  if(FUNname=="sd" && is.numeric(sl)==TRUE)
   ylimes=c(min(sl,c(stdev-1.5*stdev,stdev+1.5*stdev)),max(sl,c(stdev-1.5*stdev,stdev+1.5*stdev)))

 if(n==1)
  FRun=125
 else
  FRun=round(125/n)

 colVec=c(N+FRun)
 colVec[1:(N+FRun)]="black"

 konst=mu
 stdev2=stdev
 ENDdr=ENDdr*stdev
 ENDsp=ENDsp*stdev
 step=ENDdr/(N-FRun)
 step2=ENDsp/(N-FRun)

 if(n==1)
 {
  headline=expression("Individual observation chart")
  ylabel=expression(x)
 }
 if(n>1 && FUNname=="mean")
 {
  headline=expression(paste(bar(x),"-chart"))
  ylabel=expression(bar(x))
 }
 if(n>1 && FUNname=="median")
 {
  headline=expression(paste(tilde(x),"-chart"))
  ylabel=expression(tilde(x))
 }
 if(FUNname=="sd")
 {
  headline=expression("s-chart")
  ylabel=expression(s)
 }

 while (i<N+FRun)
 {
  Sys.sleep(rest)                                                               #System take a break for better visualisation
  temp[(i*n+1):(n*(i+1))]=rnorm(n,mu,stdev)
  group[(i*n+1):(n*(i+1))]=i+1
  Value[i+1]=match.fun(FUN)(temp[(i*n+1):(n*(i+1))])                            #values

  if(drift !=FALSE && (i-FRun)>start*N)
  {
   if(ENDdr>0 && mu<konst+ENDdr)
    mu=mu+speed*step
   if(ENDdr<0 && mu>konst+ENDdr)
    mu=mu+speed*step
  }

  if(spread !=FALSE && (i-FRun)>start*N)
  {
   if(stdev<ENDsp)
   stdev=stdev+speed*step2
  }

  Rule=FALSE
  singsd[i+1]=sd(temp[(i*n+1):(n*(i+1))])                                       #stdev of the groups

  if(i>FRun)
  {
   if(n==1)
    S=sd(Value[1:FRun])/sqrt(n)                                                 #sigma for calculating control and warn limits
   if(n>1 && FUNname=="mean")
    S=mean(singsd[1:FRun])/(cn*sqrt(n))
   if(n>1 && FUNname=="median")
    S=k*mean(singsd[1:FRun])/(sqrt(n))
   if(FUNname!="sd")
   {
    lcl=qnorm(cl[1],konst,S);ucl=qnorm(cl[2],konst,S)                                 #lower and up per control lines
    lwl=qnorm(wl[1],konst,S);uwl=qnorm(wl[2],konst,S)                                 #lower and upper warning lines
   }
   if(FUNname=="sd")
   {
    lcl=sqrt(qchisq(cl[1],n)/n)*mean(singsd[1:FRun]); ucl=sqrt(qchisq(cl[2],n)/n)*mean(singsd[1:FRun])
    lwl=sqrt(qchisq(wl[1],n)/n)*mean(singsd[1:FRun]); uwl=sqrt(qchisq(wl[2],n)/n)*mean(singsd[1:FRun])
   }
  }
  if(i>FRun)
  {
   #One point outside of the control limits (Western Electric Alarm Rule 1)
   if(Value[i+1]  < lcl || Value[i+1] > ucl)
   {
    count1=count1+1                                                             #counting values
    Rule=TRUE
    colVec[i+1]="red"
   }
   #Two of three consecutive points outside the warning limits but inside the control limits (Western Electric Alarm Rule 2)
   if(Value[i+1] < lwl && Value[i+1] >lcl)
   {
    counthelp1=1
    if(Value[i] < lwl && Value[i] >lcl)
    {
     counthelp1=counthelp1+1
     if(Value[i-1] < lwl && Value[i-1] >lcl)
      counthelp1=counthelp1+1
    }
    else counthelp1=0
   }
   else counthelp1=0
   if(counthelp1==2)
   {
    count2=count2+1                                                             #counting values
    Rule=TRUE
    colVec[i+1]="red"; colVec[i]="red"; colVec[i-1]="red"
   }
   if(Value[i+1] > uwl && Value[i+1] < ucl)
   {
    counthelp2=1
    if(Value[i] > uwl && Value[i] < ucl)
    {
     counthelp2=counthelp2+1
     if(Value[i-1] > uwl && Value[i-1] < ucl)
      counthelp2=counthelp2+1
    }
    else counthelp2=0
   }
   else counthelp2=0
   if(counthelp2==2)
   {
    count2=count2+1                                                             #counting values
    Rule=TRUE
    colVec[i+1]="red"; colVec[i]="red"; colVec[i-1]="red"
   }
   #Four of five consecutive points beyond the one-sigma limits (Western Electric Alarm Rule 3)
   if(FUNname != "sd")
    {lsl=qnorm(0.156,konst,S);usl=qnorm(0.844,konst,S)}
   else
    {lsl=sqrt(qchisq(0.156,n)/n)*mean(singsd[1:FRun]) ;usl=sqrt(qchisq(0.844,n)/n)*mean(singsd[1:FRun])}
    counthelp3=0; counthelp4=0
   if(Value[i+1] < lsl && Value[i+1] > lcl)
   {
    counthelp3=1
    if(Value[i] < lsl && Value[i] > lcl)
     counthelp3=counthelp3+1
    else
     counthelp3=counthelp3-1
    if(Value[i-1] < lsl && Value[i-1] > lcl)
     counthelp3=counthelp3+1
    else
     counthelp3=counthelp3-1
    if(Value[i-2] < lsl && Value[i-2] > lcl)
     counthelp3=counthelp3+1
    else
     counthelp3=counthelp3-1
    if(Value[i-3] < lsl && Value[i-3] > lcl)
     counthelp3=counthelp3+1
    else counthelp3=counthelp3-1
   }
   if(counthelp3>2)
   {
    count3=count3+1
    Rule=TRUE
    colVec[i+1]="red"; colVec[i]="red"; colVec[i-1]="red"; colVec[i-2]="red"; colVec[i-3]="red"
   }
   if(Value[i+1] > usl && Value[i+1] < ucl)
   {
    counthelp4=1
    if(Value[i] > usl && Value[i] < ucl)
     counthelp4=counthelp4+1
    else
     counthelp4=counthelp4-1
    if(Value[i-1] > usl && Value[i-1] < ucl)
     counthelp4=counthelp4+1
    else
     counthelp4=counthelp4-1
    if(Value[i-2] > usl && Value[i-2] < ucl)
     counthelp4=counthelp4+1
    else
     counthelp4=counthelp4-1
    if(Value[i-3] > usl && Value[i-3] < ucl)
     counthelp4=counthelp4+1
    else counthelp4=counthelp4-1
   }
   if(counthelp4>2)
   {
    count3=count3+1
    Rule=TRUE
    colVec[i+1]="red"; colVec[i]="red"; colVec[i-1]="red"; colVec[i-2]="red"; colVec[i-3]="red"
   }
   #Eight consecutive points on one side of the center (Western Electric Alarm Rule 3)
   if(FUNname!="sd")
   {
   if(Value[i+1]>konst)
    if(Value[i]>konst)
     if(Value[i-1]>konst)
      if(Value[i-2]>konst)
       if(Value[i-3]>konst)
        if(Value[i-4]>konst)
         if(Value[i-5]>konst)
          if(Value[i-6]>konst)
           {
            count4=count4+1
            Rule=TRUE
            colVec[i+1]="red"
           }
   if(Value[i+1]<konst)
    if(Value[i]<konst)
     if(Value[i-1]<konst)
      if(Value[i-2]<konst)
       if(Value[i-3]<konst)
        if(Value[i-4]<konst)
         if(Value[i-5]<konst)
          if(Value[i-6]<konst)
           {
            count4=count4+1
            Rule=TRUE
            colVec[i+1]="red"; colVec[i]="red"; colVec[i-1]="red"; colVec[i-2]="red"; colVec[i-3]="red"
            colVec[i-4]="red"; colVec[i-5]="red"; colVec[i-6]="red"
           }
   }
   if(FUNname=="sd")
   {
   if(Value[i+1]>stdev)
    if(Value[i]>stdev)
     if(Value[i-1]>stdev)
      if(Value[i-2]>stdev)
       if(Value[i-3]>stdev)
        if(Value[i-4]>stdev)
         if(Value[i-5]>stdev)
          if(Value[i-6]>stdev)
           {
            count4=count4+1
            Rule=TRUE
            colVec[i+1]="red"; colVec[i]="red"; colVec[i-1]="red"; colVec[i-2]="red"; colVec[i-3]="red"
            colVec[i-4]="red"; colVec[i-5]="red"; colVec[i-6]="red"
           }
   if(Value[i+1]<stdev)
    if(Value[i]<stdev)
     if(Value[i-1]<stdev)
      if(Value[i-2]<stdev)
       if(Value[i-3]<stdev)
        if(Value[i-4]<stdev)
         if(Value[i-5]<stdev)
          if(Value[i-6]<stdev)
           {
            count4=count4+1
            Rule=TRUE
            colVec[i+1]="red"; colVec[i]="red"; colVec[i-1]="red"; colVec[i-2]="red"; colVec[i-3]="red"
            colVec[i-4]="red"; colVec[i-5]="red"; colVec[i-6]="red"
           }
   }
  }
  i=i+1                                                                         #counting variable for loop

  if(i<FRun)
   subt="! Set-up data - CALCULATING CONTROL LIMITS !"
  else
   subt=""
  if((i-FRun)==start*N-1 || (i-FRun)==start*N)
  {
   subt="! Drift/Spread is being created !"
   Sys.sleep(3)
  }

  split.screen(matrix(c(0,0.75,0,1, 0.75,1,0,1),byrow=TRUE,ncol=4))             #split output window and adjust size

  screen(1)                                                                     #selecting screen
  if(i<numPlot)
  {
   plot(Value,xlim=c(0,numPlot+1),axes=FALSE,ylim=ylimes,
   main=headline,xlab="",ylab=ylabel,sub=subt,col=colVec[1:numPlot],type="o",...)
   if(is.numeric(sl)==TRUE)
    {abline(sl[1],0); abline(sl[2],0)}
  }
  else
  {
   plot(Value[(i-numPlot):i],xlim=c(0,numPlot+1),axes=FALSE,ylim=ylimes,
   main=headline,xlab="",ylab=ylabel,sub=subt,col=colVec[(i-numPlot):i],type="o",...)
   if(i>FRun)
    lines(c(1:(numPlot+1)),c(Value[(i-numPlot):i]))
   if(is.numeric(sl)==TRUE)
    {abline(sl[1],0); abline(sl[2],0)}
  }
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
   abline(konst,0,lwd=2)                                                           #set line showing mean value
  if(FUNname=="sd")
   abline(stdev,0,lwd=2)

  screen(2)
  split.screen(matrix(c(0,1,0.5,1, 0,1,0,0.5),byrow=TRUE,ncol=4))               #split output window and adjust size
  screen(3)
  plot(seq(1,10),col="white",axes=FALSE,xlab="",ylab="")                        #empty plot necessary for using text
  par(mar=c(0,0,0,0))
  text(-1,7,"Rule 1"); text(4.5,7,"="); text(9.5,7,count1,adj=0.4)              #insert counter for values out of control
  text(-1,6,"Rule 2"); text(4.5,6,"="); text(9.5,6,count2,adj=0.4)
  text(-1,5,"Rule 3"); text(4.5,5,"="); text(9.5,5,count3,adj=0.4)
  text(-1,4,"Rule 4"); text(4.5,4,"="); text(9.5,4,count4,adj=0.4)
  text(-1,8,"S",adj=1); text(4.5,8,"="); text(9,8,round(sd(Value),2),font=2,adj=0.4)#insert sigma
  if(n==1)
   text(-1,9,expression(bar(x)),adj=1)
  if(FUNname=="mean" && n>1)
   text(-1,9,expression(bar(X)),adj=1)
  if(FUNname=="median" && n>1)
   text(-1,9,expression(bar(tilde(X))),adj=1)
  if(FUNname=="sd")
   text(-1,9,expression(bar(s)),adj=1)
  text(4.5,9,"="); text(9,9,round(mean(Value),2),adj=0.4)                       #insert expected mean value
  text(-1,11,"Values:")
  if(i>FRun)
   text(9,11,i-FRun)
  else text(9,11,"0")

  legend(x=-2,y=2,legend="control-limit",col="red",text.col="black",lty=2,lwd=3,bty="n",cex=0.7)    #legend
  legend(x=-2,y=1,legend="warn-limit",col="red",text.col="black",lty=2,lwd=2,bty="n",cex=0.7)
  legend(x=-2,y=0,legend="sigma-limit",col="black",text.col="black",lty=4,lwd=0.1,bty="n",cex=0.7)

  screen(4)
  par(mar=c(5,0,0,1))
   plot(y=seq(mu-3*stdev,mu+3*stdev,length=1000),x=dnorm(seq(mu-3*stdev,mu+3*stdev,length=1000),mu,stdev),
        xlab="",ylab="",type="l",col="blue",xlim=c(0,dnorm(konst,konst,stdev2)),ylim=range(seq(konst-3*stdev2,konst+3*stdev2,length=1000)),lwd=2)
   lines(y=seq(konst-3*stdev2,konst+3*stdev2,length=1000),x=dnorm(seq(konst-3*stdev2,konst+3*stdev2,length=1000),konst,stdev2),col="grey")
   abline(konst,0,lty=1,col="grey")
   abline(mu,0,lwd=2)
   if(i>FRun+1)
   {
    abline(konst-S,0,lty=4,lwd=0.1,bty="n",cex=0.7);abline(konst-2*S,0,lty=4,lwd=0.1,bty="n",cex=0.7);abline(konst-3*S,0,lty=4,lwd=0.1,bty="n",cex=0.7)
    abline(konst+S,0,lty=4,lwd=0.1,bty="n",cex=0.7);abline(konst+2*S,0,lty=4,lwd=0.1,bty="n",cex=0.7);abline(konst+3*S,0,lty=4,lwd=0.1,bty="n",cex=0.7)
   }

  Sys.sleep(rest)                                                               #System take a break for better visualisation
  close.screen(all=TRUE)
 }

if(length(Value)!=length(temp))
 {
  Value[(N+FRun+1):(n*(N+FRun))]=NA
 }
output=data.frame(Value,"|",group,temp)
names(output)=c("Values","|","Group","Single Values")
invisible(output)
}
  qrkSD(pch=20,n=1,FUN=mean,N=150,rest=0,numPlot=25,drift=TRUE,spread=TRUE,
        start=0.05,ENDdr=1.5,ENDsp=1.5,speed=2,seed=12345)
	dev.off()
	img( pngfile, Rd = TRUE )
}
}
}

\value{
 \sQuote{qrkSD} returns a data.frame in which the plotted regulated values
 are listed, as well as the single values, which are given in groups 
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
  Thomas Roth: thomas.roth@tu-berlin.de \cr
  Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de  
   
}
\seealso{
 \code{\link{par}}\cr
 \code{\link{set.seed}}\cr
 \code{\link{Sys.sleep}} 
}
\examples{
par(ask=FALSE)
qrkSD(pch=20,n=1,FUN=mean,N=1000,rest=0,numPlot=25,drift=TRUE,spread=TRUE,
      start=0.05,ENDdr=1.5,ENDsp=1.5,speed=2)
par(ask=TRUE)
}