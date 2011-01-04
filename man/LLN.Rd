\name{LLN}
\alias{LLN}
\title{
 Function to visualize the Law of Large Numbers.
}
\description{
 LLN visualize the Law of Large Nimbers by apply a chosen function
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
          \item \dQuote{unif}
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