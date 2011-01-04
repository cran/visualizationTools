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
 with sample numbers n greater 30. 
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