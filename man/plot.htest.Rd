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
