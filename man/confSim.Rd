\name{confSim}
\alias{confSim}
\title{
Visualization/Simulation of the Position of Confidence Intervals
}
\description{
Function visualize the positions of several confidence intervals of normal
distributed values in dependence of a few parameters as sample size, 
confidence level, etc.
}
\usage{
confSim(fun = mean, conf.level = 0.95, mu = 0, stdev = 1, sleep = 0.2, trials = 100, n = 8, N = 20, xlim, sim = TRUE)
}
\arguments{
  \item{fun}{
 gives the type of which confidence interval is regarded. Possibilities for fun 
 are in this version \code{mean} and \code{sd}. \code{mean} is the default and 
 will lead to a visualization of confidence intervals for the mean of the values
 of each trial. \code{sd} will lead to the visualization of the confidence intervals
 for the standard deviation.   
}
  \item{conf.level}{
 gives the confidence level for the confidence intervals. It must be set between 
 0 and 1. The default value for \code{conf.level} is 0.95. 
}
  \item{mu}{
 gives the mean of the random normal distributed values dragged for each trial.
 By default 0.
}
  \item{stdev}{
 gives the standard deviation of the random normal distributed values dragged 
 for each trial. By default 1.
}
  \item{sleep}{
 gives the time in seconds between each step of the visualization in which a 
 single line is drawn.
}
  \item{trials}{
 gives the number of trials, i.e. the number of samples that are drawn. The 
 default value is 100. 
}
  \item{n}{
 gives the sample size for each trial. By default there will be drawn 8 values 
 for each trial.
}
  \item{N}{
 gives the number of trials that will be displayed at the y axis at the same time.
 This parameter loses his function if \code{sim} ist set to FALSE. By default
 \code{N} will be 20
}
  \item{xlim}{
 gives the range of the x-axis and can be used to compare the visualization of
 different settings. If this value is missing the function will adjust
 the range automatically.
}
  \item{sim}{
 a logigal value specifying whether the visualization should be animated or not.
 By default \code{sim} is set to TRUE. 
}
}
\details{
\if{html}{
	The following graph shows the output of the function \code{confSim()}. The
  horizontal lines are marking the random confidence intervals. The vertical line
  gives the target value which is in this example the mean provided by the paramter 
  \code{fun}. Confidence intervals that do not cover the target value are plotted
  red, bold and dashed. On the right top of the graph is a legend that gives the 
  most important information about the plot and summarizes how many intervals did not
  cover the target value at all.              \cr
	
  \out{<img src="../doc/confSim.png" alt="image not found"/>}
  \ifelse{latex}{}{}
}
}

\value{
 The function \code{confSim()} returns an invisible list with 2 entries,
 which contain the x-coordinates for the end points of every single 
 confidence interval.   
}
\author{
     Thomas Roth thomas.roth@tu-berlin.de\cr
     Etienne Stockhausen stocdarf@mailbox.tu-berlin.de
}
\note{
This function is not yet finished, but can be used as dexcribed above. \cr
It is planned to integrate further functions.
}
\seealso{
  \code{\link{Sys.sleep}}
}
\examples{
 confSim()                                                                      #function used with default settings
}