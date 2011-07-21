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
     possibilities are the functions \code{\link{mean}}, \code{\link{median}} and \code{\link{sd}}.\cr
     For an individual observation chart \code{\link{mean}} and \code{\link{median}} can be used but
     \code{n} must be set to \sQuote{1}. Default value is \code{\link{mean}}. 
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
     gives the number of points visualised on the screen before the chart is moving.
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
	
  \out{<img src="../doc/qrk.png" alt="image not found"/>}
  \ifelse{latex}{}{}
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

