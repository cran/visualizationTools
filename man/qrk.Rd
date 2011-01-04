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
 side of the plot.
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

