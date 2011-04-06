\name{LLN}
\alias{LLN}
\title{
 Function to visualize the \dQuote{Law of Large Numbers}.
}
\description{
 LLN visualize the \dQuote{Law of Large Nimbers} by apply a chosen function
 to a population of random values out of a chosen distribution.
 The results of the function are plotted as a graph.  
}
\usage{
LLN(n, fun = mean, distr, param, sleep, main, type, xlab, ylab, ...)
}
\arguments{
  \item{n}{
  is a single value that gives the number of random values. 
}
  \item{fun}{
  gives the function that is applied to the population given by distribution.
}
  \item{distr}{
  is a string that gives the type of distribution for the random population. For further information 
  see \sQuote{Details}.
}
  \item{param}{
  is a list that gives the parameters needed to specify the distribution. Notice that the number
  of observations \sQuote{n} is the same as the above and must not be specified in that list anymore!
}
  \item{sleep}{
  is a single value and gives the time in seconds the system will pause between plotting two values. Variable for
 the internal usage of \code{\link{Sys.sleep}}.
}
  \item{main}{
  is an optional string and gives an overall title for the plot: see \sQuote{title}
}
  \item{type}{
  is an optional string and gives the type for the plot: see \code{\link{plot}}
}
  \item{xlab}{
  is an optional string and gives a title for the x axis: see \sQuote{title}
}
  \item{ylab}{
  is an optional string and gives a title for the y axis: see \sQuote{title}
}
  \item{\dots}{
 Arguments to be passed to methods, such as graphical parameters (see \sQuote{par}).
}
}
\details{
 LLN will accept the following distributions:  \cr
 \itemize{\item \dQuote{beta}                              \cr
          \item \dQuote{cauchy}                            \cr
          \item \dQuote{chi-squared}                       \cr
          \item \dQuote{exponential}                       \cr
          \item \dQuote{f}                                 \cr
          \item \dQuote{log-normal}                        \cr
          \item \dQuote{logistic}                          \cr
          \item \dQuote{negative binomial}                 \cr
          \item \dQuote{normal}                            \cr
          \item \dQuote{poisson}                           \cr
          \item \dQuote{t}                                 \cr
          \item \dQuote{weibull}                           \cr
          \item \dQuote{gamma}                             \cr
          \item \dQuote{unif}                              \cr
}                 
\if{html}{
	The following graph visualize the Law of Large Numbers for the function \sQuote{mean} 
  used to a sample of 100 normal-distributed values with the mean of two and a 
  standard deviation of three. The mean - which is the target in that case - 
  is marked by a black, dashed line. The red line shows the mean value of all the 
  sample values until the value of n on the x-axis. In that case it becomes obvious,
  that the red line converges with growing n values to the mean given by the 
  parameter \sQuote{param}. 	                                                            #LLN(n=100,distr="normal",fun=mean,param=list(mean=2,sd=0.5),col=2,sleep=0)  
	
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

LLN <-
function(n,fun=mean,distr,param,sleep,main,type,xlab,ylab,...)
{
 set.seed(123456)
 old.par <- par(no.readonly = TRUE)
 on.exit(par(old.par))
 if(missing(n))
  n=1000
 if(missing(distr))
  stop("distr is missing")
 if(missing(param))
  stop("param is missing")
 if(missing(sleep))
  sleep=0
 if(missing(main))
  main="Visualization: Law of Large Numbers"
 if(missing(type))
  type="l"
 if(missing(xlab))
  xlab="n"
 if(missing(ylab))
  {
   if(identical(deparse(substitute(fun)),"mean"))
     ylab=expression(bar(x))
    else
    {
     if(identical(deparse(substitute(fun)),"median"))
      ylab=expression(tilde(x))
     else
     {
      if(identical(deparse(substitute(fun)),"max"))
       ylab=expression(x[max])
      else
      {
       if(identical(deparse(substitute(fun)),"min"))
        ylab=expression(x[min])
       else
       {
        if(identical(deparse(substitute(fun)),"sd"))
         ylab=expression(s)
        else
        {
         if(identical(deparse(substitute(fun)),"range"))
          ylab=expression(R)
         else
          ylab=deparse(substitute(fun))
        }
       }
      }
     }
    }
   }

 value=numeric(n);temp=numeric(n)
 param$n=1
 for(i in 1:n)
 {
  temp[i]=do.call(.charToDistfunc(distr,type="r"),param)
  value[i]=fun(temp[2:i])
 }
 for(i in 1:n)
 {
  plot(value[1:i],xlim=c(0,n),main=main,type=type,xlab=xlab,ylab=ylab,...)
  if(identical(deparse(substitute(fun)),"mean"))
  {
   if(identical(distr,"beta"))
    abline(h=param[[1]]/(param[[1]]+param[[2]]),lty=2)                           #cauchy hat keinen Erwartungswert!
   if(identical(distr,"chi-squared"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"exponential"))
    abline(h=1/param[[1]],lty=2)
   if(identical(distr,"f"))
    abline(h=param[[2]]/(param[[2]]-2),lty=2)
   if(identical(distr,"log-normal"))
    abline(h=exp(param[[1]]+((param[[2]])^2)/2),lty=2)
   if(identical(distr,"logistic"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"negative binomial"))
   {
    if(identical(names(param[2]),"prob"))
     abline(h=(param[[1]]*(1-param[[2]]))/param[[2]],lty=2)
    if(identical(names(param[2]),"mu"))
     abline(h=param[[2]],lty=2)
   }
   if(identical(distr,"normal"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"poisson"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"t"))
    abline(h=0,lty=2)
   if(identical(distr,"weibull"))
    abline(h=param[[2]]*gamma(1+1/param[[1]]),lty=2)
   if(identical(distr,"gamma"))
   {
    if(identical(names(param[2]),"rate"))
     abline(h=param[[1]]/param[[2]],lty=2)
    if(identical(names(param[2]),"scale"))
     abline(h=param[[1]]*param[[2]],lty=2)
   }
   if(identical(distr,"unif"))
    abline(h=(param[[1]]+param[[2]])/2,lty=2)
  }

  if(identical(deparse(substitute(fun)),"sd"))
  {
   if(identical(distr,"beta"))
    abline(h=sqrt((param[[1]]*param[[2]])/((param[[1]]+param[[2]]+1)*(param[[1]]+param[[2]])^2)),lty=2)                           #cauchy hat keinen Erwartungswert!
   if(identical(distr,"chi-squared"))
    abline(h=sqrt(2*param[[1]]),lty=2)
   if(identical(distr,"exponential"))
    abline(h=1/param[[1]],lty=2)
   if(identical(distr,"f"))
    abline(h=sqrt((2*(param[[2]]^2)*(param[[1]]+param[[2]]-2))/(param[[1]]*((param[[2]]-2)^2)*(param[[2]]-4))),lty=2)
   if(identical(distr,"log-normal"))
    abline(h=sqrt(exp(2*param[[1]]+param[[2]]^2)*(exp(param[[2]]^2)-1)),lty=2)
   if(identical(distr,"logistic"))
    abline(h=sqrt(((param[[2]])^2*(pi)^2)/3),lty=2)
   if(identical(distr,"negative binomial"))
    abline(h=sqrt((param[[1]]*(1-param[[2]]))/(param[[2]]^2)),lty=2)
   if(identical(distr,"normal"))
    abline(h=param[[2]],lty=2)
   if(identical(distr,"poisson"))
    abline(h=sqrt(param[[1]]),lty=2)
   if(identical(distr,"t"))
    abline(h=param[[1]]/(param[[1]]-2),lty=2)
   if(identical(distr,"weibull"))
    abline(h=sqrt((param[[2]])^2*(gamma(1+2/param[[1]])-gamma(1+1/param[[1]])^2)),lty=2)
   if(identical(distr,"gamma"))
   {
    if(identical(names(param[2]),"rate"))
     abline(h=sqrt(param[[1]]/param[[2]]^2),lty=2)
    if(identical(names(param[2]),"scale"))
     abline(h=sqrt(param[[1]]*param[[2]]^2),lty=2)
   }
   if(identical(distr,"unif"))
    abline(h=(param[[2]]-param[[1]])/(2*sqrt(3)),lty=2)
  }
  Sys.sleep(sleep)
 }

return(list(temp,value))
}

LLN(n=100,distr="normal",fun=mean,param=list(mean=2,sd=0.5),col=2,sleep=0)  
	dev.off()
	img( pngfile, Rd = TRUE )
}
}
}
\value{
 LLN returns a list. The first element gives the n single random values of
 the chosen distribution. The second object gives the values on which the
 function \sQuote{fun} is applied and which are plotted.
 }
\references{
\itemize{ \item Montgomery, D.C. (2005) Introduction to 
 Statistical Quality Control, 5th ed. New York: John Wiley & Sons. \cr
 
 \item Blume, J.D.; Royall, R.M. (February 2003) Illustrating the Law of Large Numbers 
 (and Confidence Intervals), The American Statistical Association Vol. 57 No. 1, 
 p. 51-55
 }
} 
\author{
     Thomas Roth: thomas.roth@tu-berlin.de \cr
     Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}
\note{
 A line for the expected value is in this version only be drawn for the functions 
 \sQuote{mean} and \sQuote{sd}.\cr
 The idea of visualize the Law of Large Numbers is mentioned by Blume
 and Royall (2003).
}
\seealso{
  \code{\link{plot}} \cr
  \code{\link{Sys.sleep}}
}
\examples{
par(ask=FALSE)
 LLN(n=100,distr="normal",fun=mean,param=list(mean=2,sd=0.5),col=2)   
par(ask=TRUE)
}                                                                      