\name{ocCurve}
\alias{ocCurve}
\title{
Function to plot oc-Curves.
}
\description{
This function plots oc-Curves by using the parameter out of \code{\link{power.t.test}}
}
\usage{
ocCurve(n, sig.level, alternative, type, distribution, col, xlim, 
        ylim, ...)
}
\arguments{
  \item{n}{
  can be a single value or a vector of vealues giving the number of observations
  per group: see \code{\link{power.t.test}}
}
  \item{sig.level}{
  a single value giving the significance level (Type I error probability) 
  for the test: see \code{\link{power.t.test}}
}
  \item{alternative}{
  is a char value giving the alternative (one- or two sided) 
  for the test: see \code{\link{power.t.test}}
}
  \item{type}{
  is a char value giving the type (one- or two sample,paired) 
  for the test: see \code{\link{power.t.test}}
}
  \item{distribution}{
  is a char value indicating the test-procedure used to get the plotting-values.
  Further information see Note. 
}
  \item{col}{
 \sQuote{col} a vector of colors for the plotted oc-curves.
}
  \item{xlim}{
  giving the x limits (x1, x2) of the plot.
  Note that x1 > x2 is allowed and leads to a \sQuote{reversed axis}.
}
  \item{ylim}{
  giving the y limits of the plot.
}
  \item{\dots}{
 other graphical parameters (see \code{\link{par}}).
}
}
\details{
\if{html}{
	The following graph shows the oc-curves (red) for several numbers of 
  observations per group \sQuote{n} (In that case: 100,50,25,15,10,8,7,6,5,4,3,2).
  On thhe y-axis are the values for beta (Type II Error), which give the 
  probability of accepting H0. In this example alpha (Type I Error) is 5 percent
  and the oc-curves are plotted for a two-sided t.test. 	
	
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

ocCurve <-
function(n,sig.level,alternative,type,distribution,col,xlim,ylim,...)
{
 set.seed(12345)
 old.par <- par(no.readonly = TRUE)
 on.exit(par(old.par))
 if(missing(n))
  n=2
 if(missing(sig.level))
  sig.level=0.05
 if(missing(alternative))
  alternative="two.sided"
 if(missing(type))
  type="two.sample"
 if(missing(distribution))
  distribution="t"
 if(missing(col))
  col=rep(1,length(n))
 if(length(col)!=length(n))
  col=c(col,rep(1,(length(n)-length(col))))
 if(missing(xlim))
  xlim=c(0,3)
 if(missing(ylim))
  ylim=c(0,1)

 beta=matrix(ncol=100,nrow=length(n))
 d=seq(min(xlim),max(xlim),length=100);temp=numeric(length(n))
 plot(0,0,xlim=xlim,ylim=ylim,col="white",xlab=expression(d==frac(abs(~mu[1]-mu[0]),sigma)),
      ylab=expression(beta),bty="n",axes=FALSE,...)
 axis(1)
 if(alternative=="one.sided")
 {
  axis(2,at=c(0,0.2,0.4,0.6,0.8,1,1-sig.level))
  abline(h=1-sig.level,col="gray")
 }
 if(alternative=="two.sided")
 {
  axis(2,at=c(0,0.2,0.4,0.6,0.8,1,1-sig.level/2))
  abline(h=1-sig.level/2,col="gray")
 }
 for(i in 1:length(n))
 {
 for(j in 1:100)
 {
  if(distribution=="t")
   beta[i,j]=1-power.t.test(n=n[i],delta=d[j],sd=1,sig.level=sig.level,alternative=alternative,type=type)$power
 }
 if(as.integer(i)-i>=0)                                                         # VERY BAD CODE!!! DO NOT COPY 
  temp[i]=power.t.test(n=n[i],power=0.825,sd=1,sig.level=sig.level,alternative=alternative,type=type)$delta
 else
  temp[i]=power.t.test(n=n[i],power=0.775,sd=1,sig.level=sig.level,alternative=alternative,type=type)$delta
 lines(d,beta[i,],col=col[i])
 if(as.integer(i)-i>=0)                                                         # VERY BAD CODE!!! DO NOT COPY 
  text(temp[i],0.175,n[i],cex=0.75)
 else
  text(temp[i],0.225,n[i],cex=0.75)
 }
 return(beta)
}

ocCurve(n=c(2,3,4,5,6,7,8,10,15,25,50,100),sig.level=0.05,col=rep(2,12),
         main="OC-Curves") 
	dev.off()
	img( pngfile, Rd = TRUE )
}
}
}

\value{
 ocCurves() returns the values for beta listed in a matrix.
}
\references{
 Montgomery, D.C. (2005) Introduction to
 Statistical Quality Control, 5th ed. New York: John Wiley & Sons.
}
\author{
     Thomas Roth: thomas.roth@tu-berlin.de \cr
     Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}
\note{
 This function is not yet fully finished. This Version will only work for t-test.
 The parameter distribution can therfore only be \dQuote{t}. In further Versions the 
 visualization of oc-curves of other tests, like the chi-squared-test, will be implemented. 
}

\seealso{
 \code{\link{power.t.test}}\cr 
 \code{\link{par}}
}
\examples{
 ocCurve(n=c(2,3,4,5,6,7,8,10,15,25,50,100),sig.level=0.05,col=rep(2,12),
         main="OC-Curves")
}