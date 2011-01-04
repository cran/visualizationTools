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
 Furthermore there is a smaller plot of a normal distribution on the left down side 
 in which the spread and/or the drift is/are visualized.
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