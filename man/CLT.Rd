\name{CLT}
\alias{CLT}
\title{
 Function to visualize the Central Limit Theorem
}
\description{
 Function visualize the Central Limit Theorem by apply a chosen function to a population 
 of random values out of different distributions and plotting the result step by step in a histogram.
}
\usage{
CLT(fun, type, times, distribution, param, sleep, col, line.col, seed,
    main)
}
\arguments{
  \item{fun}{
    gives the function that is applied to the population of distributions given by distribution.
}
  \item{type}{
    gives the type of the histogram. It can be \dQuote{density} or \dQuote{counts}.
}
  \item{times}{
    give the number of runs for creating a population and plotting the histograms.                
}
  \item{distribution}{
    gives a vector of used distributions for the population
}
  \item{param}{
    a list of list, which contain the paramater for the single distributions given by distribution.
    Even if distribution has length = 1, param has to be a list of a single list!
}
  \item{sleep}{
    a single value giving the time in seconds between the single plots of the histogram. Variable for the command
    \code{\link{Sys.sleep}}.
}
  \item{col}{
    a vector giving the colors of all plotted histograms.
}
  \item{line.col}{
    color of the plotted density line (only if type="density").
}
  \item{seed}{
    value for the command \code{\link{set.seed}}.
}                                                             
  \item{main}{
    vector for the mains of every single histogram.
}
}
\value{
 CLT returns a invisble list containing a matrix including all randomized values of the different distributions
 and a vector containing the results of executing fun on every row of that matrix. This vector represents the 
 values plotted in the histogram.   
}
\author{
     Thomas Roth thomas.roth@tu-berlin.de\cr
     Etienne Stockhausen stocdarf@mailbox.tu-berlin.de
}
\examples{
CLT(fun=mean,times=100,distribution=c("normal","weibull",
    "gamma","normal","beta"),
    param=list(list(mean=0,sd=0.01,n=100),list(shape=1,scale=3,n=100),
    list(n=100,shape=0.1),list(mean=2,sd=0.1,n=100),
    list(n=100,shape1=1,shape2=2)),
    seed=123,col=c(rep("grey",5),"green")) 
}