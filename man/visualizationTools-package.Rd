\name{visualizationTools-package}
\alias{visualizationTools-package}
\alias{visualizationTools}
\docType{package}
\title{
Package contains a few functions to visualize statistical circumstances.
}
\description{
Package contains function to visualize a t-test, the power of a t-test,
control charts and the influence of regulating them, Oc-curves, 
the Law of large Numbers and confidence intervals.
}
\details{
\tabular{ll}{
Package: \tab visualizationTools\cr
Type: \tab Package\cr
Version: \tab 0.2.02\cr
Date: \tab 2011-07-21\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
}
\author{
Thomas Roth\cr
Etienne Stockhausen

Maintainer: \cr
 T. Roth <thomas.roth@tu-berlin.de> \cr
 E. Stockhausen <stocdarf@mailbox.tu-berlin.de>
}
\references{
 \itemize{
   \item Vardeman, Stephen B. & Jobe, J. Marcus (1999) Statistical Quality Assurance Methods 
   for Engineers, 1st ed. New York: John Wiley & Sons.
    
   \item Montgomery, D.C. (2005) Introduction to
   Statistical Quality Control, 5th ed. New York: John Wiley & Sons.  
   
   \item Blume, J.D.; Royall, R.M. (February 2003) Illustrating the Law of Large Numbers 
   (and Confidence Intervals), The American Statistical Association Vol. 57 No. 1,
   p. 51-55
 }
}
\examples{
 par(ask=FALSE)
 LLN(n=100,distr="unif",fun=median,param=list(min=2,max=7),col="orange")
 par(ask=TRUE)
 ocCurve(n=c(2,3,4,5,6,7,8,10,15,25,50,100),
         sig.level=0.05,col=rep(2,12),main="OC-Curves")
 var=c(29.60, 28.26,30.50,31.79,29.24,29.75,27.24,30.31,29.27,27.63,31.79,
       27.43,30.18,28.65,28.30,29.28)              
 to=t.test(var,mu=30,alternative="two.sided",conf.level=0.95)
 plot(to)
 po=power.t.test(n=30,delta=7,sd=12,sig.level=0.05,type="one.sample",
                 alternative="one.sided")
 plot(po,main="one.sample one.sided")
 plot(0,0,col="white",axes=FALSE,ylab="",xlab="")
 par(ask=FALSE)
 qrk(type="o",pch=20,n=5,FUN=mean,N=250,rest=0)
 par(ask=TRUE)
 plot(0,0,col="white",axes=FALSE,ylab="",xlab="")
 par(ask=FALSE)
 qrkSD(pch=20,n=1,FUN=mean,N=250,rest=0,numPlot=25,drift=TRUE,
       spread=TRUE,start=0.05,ENDdr=1.5,ENDsp=1.5,speed=2)
 par(ask=TRUE)
 plot(0,0,col="white",axes=FALSE,ylab="",xlab="")
 par(ask=FALSE)
 CLT(fun=mean,times=100,distribution=c("normal","weibull",
 "gamma","normal","beta"),
 param=list(list(mean=0,sd=0.01,n=100),list(shape=1,scale=3,n=100),
 list(n=100,shape=0.1),list(mean=2,sd=0.1,n=100),
 list(n=100,shape1=1,shape2=2)),
 seed=123,col=c(rep("grey",5),"green")) 
 par(ask=TRUE)
 plot(0,0,col="white",axes=FALSE,ylab="",xlab="")
 par(ask=FALSE)
 confSim()
 par(ask=TRUE)
}
