\name{plot.htest}
\alias{plot.htest}
\title{
 Function to visualize results of a t-test.
}
\description{
 This function visualize graphically the results of a t-test by using the values
 provided by \code{\link{t.test}}.
}
\usage{
\method{plot}{htest}(x, col.region, conf.int, col.conf, border, lwd, pch, ...)
}
\arguments{
  \item{x}{
  is an object of class \dQuote{htest}
}
  \item{col.region}{
  is a color string, a numeric value or a function giving
  the color for the visualization of the significance niveau of the test 
  (type I error).Default value is \sQuote{lightblue} 
}
  \item{conf.int}{
  is a logical value appropriating whether the confidence intervall
  calculated by \code{\link{t.test}} is to be drawn. Default value is \sQuote{TRUE}
}
  \item{col.conf}{
  is a color string, a numeric value or a function giving the 
  color for the shading lines, which are showing the confidence intervall 
  calculated by \code{\link{t.test}}.This variable has no effect if 
  \sQuote{conf.int} is set as \sQuote{FALSE}. Default value is \sQuote{gray} 
}
  \item{border}{
  is a color string, a numeric value or a function giving
  the color for the border of the plotted curve. Default value is \sQuote{1}
}
  \item{lwd}{
  is giving the line width of the plotted curve,
  a _positive_ number, defaulting to \sQuote{1}. Default value is \sQuote{2}
}
  \item{pch}{
  is either an integer specifying a symbol or a single character
  to be used as marking the fitted t-value given by \code{\link{t.test}}
}
  \item{\dots}{
 Other aguments that can be passed to \code{\link{plot}}
}
}
\details{
\if{html}{
	The following graph visualizes the result of a t-test for a normal-distributed
  sample with a sample size of 16. The bold black graph illustrates the matching 
  t-distribution (depending on the degress of freedom \sQuote{df}). The thin, black,
  dashed line gives the value which is assumed in the H0-hypothesis. The bold, black,
  point-dashed line gives the true mean value out of the sample on which the t-test
  is based. The red mark gives the fitted t-value (also in red) which is rounded to
  two significant digits. The blue painted areas give the rejection region whichs size
  depends on the chosen confidence level. The grey shaded area marks the confidence interval
  given by the function \code{\link{t.test}} and will only be shown if the parameter \sQuote{conf.int}
  is set to TRUE (what will be the case with default settings).\cr \cr
  There are now two ways to decide whether the H0-hypothesis can be rejected or not: \cr
  \itemize{
   \item First way: 
                    When the fitted t-value (red) is in the rejection area (blue),
                    the H0-hypothesis has to be rejected and the alternative hypothesis 
                    is true. Is fitted t-value between or beyond the rejection region
                    the H0-hypothesis has to be maintained.\cr
   \item Second way (only if \sQuote{conf.int} is set to TRUE): 
                    To maintain the H0-hypothesis the assumed value for the mean
                    of the sample (thin, black, dashed line) has to be inside the
                    gray shaded area marking the confidence intervall provided by
                    \code{\link{t.test}}. Is it beyond the this area the H0-hypothesis 
                    has to be rejected and the alternative hypothesis is true. \cr
}                 
According to the describtion above the H0-hypothesis in the example below must 
obviously be maintained.     	
	
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

plot.htest <-
function(x,col.region,conf.int,col.conf,border,lwd,pch,...)
 {
  set.seed(12345)
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
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
 
 x=c(29.60, 28.26, 30.50, 31.79, 29.24, 29.75, 27.24, 30.31, 29.27,
       27.63, 31.79, 27.43, 30.18, 28.65, 28.30, 29.28)
 to=t.test(x,mu=30,alternative="two.sided",conf.level=0.95)
 plot(to)
	dev.off()
	img( pngfile, Rd = TRUE )
}
}
}

\value{
 plot.htest does not return any value. Values used are returned by 
 \code{\link{t.test}}.
}
\author{
     Thomas Roth: thomas.roth@tu-berlin.de\cr
     Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}
\note{
  The development of this function is fully completed for the usage of 
  \code{\link{t.test}}. \cr
  Enhancements for further test methods are planned to be implemented in future.  
}

\seealso{
  \code{\link{t.test}}\cr
  \code{\link{plot}}\cr 
  \code{\link{par}}
}
\examples{
 # to --> TestObject
 var=c(29.60, 28.26, 30.50, 31.79, 29.24, 29.75, 27.24, 30.31, 29.27,
       27.63, 31.79, 27.43, 30.18, 28.65, 28.30, 29.28)
 to=t.test(var,mu=30,alternative="two.sided",conf.level=0.95)
 plot(to)
 ###
 var=c(37.86, 39.52, 40.39, 38.63, 40.39, 39.16, 37.31, 40.38, 40.22, 
       39.72, 40.29, 39.27, 38.68, 40.00, 39.66, 40.04, 39.34, 39.94)
 to=t.test(var,mu=40,alternative="less",conf.level=0.95)
 plot(to,col.region="yellow")
 ###
 var=c(503.08, 499.48, 505.47, 508.91, 502.10, 503.47, 496.76, 504.95,
       502.18, 497.81, 508.91, 497.29, 504.61, 500.53, 499.60, 502.22,
       508.83, 497.62, 500.84, 510.83, 504.07, 496.38, 510.06, 497.25)
 to=t.test(var,mu=500,alternative="greater",conf.level=0.90)
 plot(to,col.region="orange",conf.int=FALSE)
 ###
 MachineA=c(20.46, 19.92, 20.82, 21.34, 20.31, 20.52, 19.51, 20.74, 
            20.33, 19.67, 21.34, 19.59, 20.69, 20.08, 19.94, 20.33)
 MachineB=c(19.33, 19.90, 21.65, 20.46, 19.12, 21.51, 19.27, 20.23,
            19.28, 20.07, 21.75, 21.08, 19.71, 19.63, 20.44, 19.27)
 to=t.test(MachineA,MachineB,alternative="two.sided",conf.level=0.95)
 plot(to,col.region="purple",col.conf=rgb(0.2,0.8,0,0.6))
}
