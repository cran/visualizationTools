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
  per group: see \code{\link{power.t.test}}.\cr
  By default \code{n} is set to \sQuote{2}.
}
  \item{sig.level}{
  a single value giving the significance level (Type I error probability) 
  for the test: see \code{\link{power.t.test}}.\cr
  By default \code{sig.level} is set to \sQuote{0.05}.
}
  \item{alternative}{
  is a char value giving the alternative (one- or two sided) 
  for the test: see \code{\link{power.t.test}}.
  By default \code{alternative} is set to \sQuote{two.sided}.
}
  \item{type}{
  is a char value giving the type (one- or two sample,paired) 
  for the test: see \code{\link{power.t.test}}.\cr
  By default \code{type} is set to \sQuote{two.sample}.
}
  \item{distribution}{
  is a char value indicating the test-procedure used to get the plotting-values.\cr
  Further information see Note. By default \code{distribution} is set to \dQuote{t}.
}
  \item{col}{
  is a vector of colors for the plotted oc-curves. By default all curves will be black.
}
  \item{xlim}{
  giving the x limits (x1, x2) of the plot.
  Note that x1 > x2 is allowed and leads to a \sQuote{reversed axis}. \cr
  By default the x range is between \sQuote{0} and \sQuote{3}.
}
  \item{ylim}{
  giving the y limits of the plot. By default the y range is between \sQuote{0} and \sQuote{1}.
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

  \out{<img src="../doc/OC.png" alt="image not found"/>}
  \ifelse{latex}{}{}	

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