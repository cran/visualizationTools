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
  is a single value that gives the number of random values.\cr
  By default \code{n} is set to \sQuote{1000}. 
}
  \item{fun}{
  gives the function that is applied to the population given by distribution.\cr
  By default \code{fun} is set to \code{\link{mean}}.
}
  \item{distr}{
  is a string that gives the type of distribution for the random population. For further information 
  see Details.
}
  \item{param}{
  is a list that gives the parameters needed to specify the distribution. Notice that the number
  of observations \sQuote{n} is the same as the above and must not be specified in that list anymore!
}
  \item{sleep}{
  is a single value and gives the time in seconds the system will pause between plotting two values.\cr Variable for
  the internal usage of \code{\link{Sys.sleep}}. By default set to \sQuote{0}.
}
  \item{main}{
  is an optional string and gives an overall title for the plot: see \sQuote{title}.\cr
  By default \code{main} is set to \dQuote{Visualization: Law of Large Numbers}.
}
  \item{type}{
  is an optional string and gives the type for the plot: see \code{\link{plot}}.\cr
  By default \code{type} is set to \dQuote{l}.
}
  \item{xlab}{
  is an optional string and gives a title for the x axis: see \sQuote{title}.
  By default \code{xlab} is set to \dQuote{n}.
}
  \item{ylab}{
  is an optional string and gives a title for the y axis: see \sQuote{title}. \cr
  By default \code{ylab} is created internally depending on \code{fun}.
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
          \item \dQuote{binomial}
}                 
\if{html}{
	The following graph visualize the Law of Large Numbers for the function \sQuote{mean} 
  used to a sample of 100 normal-distributed values with the mean of two and a 
  standard deviation of three. The mean - which is the target in that case - 
  is marked by a black, dashed line. The red line shows the mean value of all the 
  sample values until the value of n on the x-axis. In that case it becomes obvious,
  that the red line converges with growing n values to the mean given by the 
  parameter \sQuote{param}. 	                                                            #LLN(n=100,distr="normal",fun=mean,param=list(mean=2,sd=0.5),col=2,sleep=0)  
	
  \out{<img src="../doc/LLN.png" alt="image not found"/>}
  \ifelse{latex}{}{}
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