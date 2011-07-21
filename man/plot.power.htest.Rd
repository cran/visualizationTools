\name{plot.power.htest}
\alias{plot.power.htest}
\title{
 Function to visualize the power of a t-test.        
}
\description{
 This function visualize graphically the power, the type I error and 
 the type II error of a t-test by using the values provided by \code{\link{power.t.test}}. 
}
\usage{
\method{plot}{power.htest}(x, col, col.line, lwd, main, xlab, ylab, ...)
}
\arguments{
  \item{x}{
  is an object of class \dQuote{power.htest}
}
  \item{col}{
 is a vector giving the colors for visualize the type I error, the
 type II error and the power
} 
  \item{col.line}{
 is a vector giving the color of the lines of the distribution curves
}
  \item{lwd}{
 is a vector giving the width of the lines of the distribution curves
}
  \item{main}{
 giving an overall title for the plot: see \code{\link{title}}
}
  \item{xlab}{
 giving a title for the x axis : see \code{\link{title}}   
}
  \item{ylab}{
 giving a title for the y axis : see \code{\link{title}} 
}
  \item{\dots}{
 Other aguments that can be passed to \code{\link{plot}}
}
}
\details{
\if{html}{
	The following graph shows the output of the function \code{plot.power.htest}.
  As it is described above the class of the parameter \sQuote{x} has to be \dQuote{power.htest}.
  The function uses the information out of \code{\link{power.t.test}} and plots two
  t-distributions. The space between the two means of the distributions (black line
  and dashed, black line) illsutrates the delta value. The blue colored area represents
  the power of the t-test, the red area represents the type I error (alpha error) and
  the green one represents the type II error (beta error). 	
	
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

plot.power.htest <-
function(x,col,col.line,lwd,main,xlab,ylab,...)
{
  DB = FALSE
#   old.par <- par(no.readonly = TRUE)
#   on.exit(par(old.par))
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
          expression(bold("Type II error:")),paste(round(1-x$power,6),"(", round(1-x$power,4)*100,")"),
          expression(bold("Power:")),paste(round(x$power,6),"(", round(x$power,4)*100,")"))),
          fill=c(col[1],0,col[2],0,col[3],0),y.intersp=1,inset=0.04,border=c(1,0,1,0,1,0))
  }
 invisible()
}
  po=power.t.test(n=30,delta=7,sd=12,sig.level=0.05,
                 type = "one.sample", alternative="one.sided")
  plot(po,main="one.sample one.sided")
	dev.off()
	img( pngfile, Rd = TRUE )
}
}
}

\value{
 plot.power.htest does not return any value.
}

\author{
     Thomas Roth: thomas.roth@tu-berlin.de\cr
     Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}
\note{
 The development of this function is not yet fully completed.\cr 
 There are minor deviations between the calculated value and the plotted value of the power
 which are insignificant.\cr
 The function can be used with very small deviations to visualize the power of t-tests
 with sample numbers n greater 10. 
 For smaller sample numbers this function does not deliver right visualizations, especially for
 type one.sample! Please consider this, if you want to visualize the power of a t-test.
 
}
\seealso{
 \code{\link{title}}\cr 
 \code{\link{power.t.test}}
}
\examples{
 #po --> PowerObject
 par(mfrow=c(2,2))
 po=power.t.test(n=30,delta=7,sd=12,sig.level=0.05,
                 type = "one.sample", alternative="one.sided")
 plot(po,main="one.sample one.sided")
 po=power.t.test(n=30,delta=7,sd=12,sig.level=0.05,
                 type = "one.sample", alternative="two.sided")
 plot(po,main="one.sample two.sided")
 po=power.t.test(n=30,delta=7,sd=12,sig.level=0.05,
                 type = "two.sample", alternative="one.sided") 
 plot(po,main="two.sample one.sided")  
 po=power.t.test(n=30,delta=7,sd=12,sig.level=0.05,
                 type = "two.sample", alternative="two.sided")
 plot(po,main="two.sample two.sided")
}