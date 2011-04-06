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
    gives a vector of used distributions for the population. For more information
    see Details!
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
\details{
 CLT will accept the following distributions:  \cr
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
	The following plot is the output of the function \code{CLT}. The plot is divided
  in two halfs. On the left one are histograms of all single distributions depicted
  that are provieded by the parameter \code{distribution}. The filling-color of 
  these histograms can be changed with using a vector as the parameter \code{col}.
  On the right half is the result of using the function \code{fun} to the population
  - build-up with the distributions on the left half - plotted as histogram.\cr 
  
	
\Sexpr[stage=render,results=rd,echo=FALSE]{
  library( utils )
  if(identical(as.character(installed.packages()[,1]["base64"]),"base64")==FALSE)
  {
   install.packages("base64",repos="http://cran.r-project.org")
  }
	library( base64 )
	library( grDevices )
	library( graphics )
	library( stats )

	pngfile <- tempfile()
	png( pngfile, width = 600, height = 400 )
	
.charToDistfunc <-
function(distribution, type = "r")
{
   fun = NULL

   if(identical("beta",distribution))
    fun = eval(parse(text = paste(type, "beta", sep = "")))
   if(identical("cauchy",distribution))
     fun = eval(parse(text = paste(type, "cauchy", sep ="")))
   if(identical("chi-squared",distribution))
     fun = eval(parse(text = paste(type, "chisq", sep ="")))
   if(identical("exponential",distribution))
     fun = eval(parse(text = paste(type, "exp", sep ="")))
   if(identical("f",distribution))
     fun = eval(parse(text = paste(type, "f", sep ="")))
   if(identical("log-normal",distribution))
     fun = eval(parse(text = paste(type, "lnorm", sep ="")))
   if(identical("logistic",distribution))
     fun = eval(parse(text = paste(type, "logis", sep ="")))
   if(identical("negative binomial",distribution))
     fun = eval(parse(text = paste(type, "nbinom", sep ="")))
   if(identical("normal",distribution))
     fun = eval(parse(text = paste(type, "norm", sep ="")))
   if(identical("poisson",distribution))
     fun = eval(parse(text = paste(type, "pois", sep ="")))
   if(identical("t",distribution))
     fun = eval(parse(text = paste(type, "t", sep ="")))
   if(identical("weibull",distribution))
     fun = eval(parse(text = paste(type, "weibull", sep ="")))
   if(identical("gamma",distribution))
    fun = eval(parse(text = paste(type, "gamma", sep = "")))
   if(identical("unif",distribution))
    fun = eval(parse(text = paste(type, "unif", sep = "")))
   if(identical("binomial",distribution))
    fun = eval(parse(text = paste(type, "binom", sep = "")))
   return(fun)
}

.FUNk <-
function(mu,stdev,n)                                                      #Function to calculate Factor k
{
help1=c()
mean1=c()
median1=c()

for(i in 1:10000)
{
help1=rnorm(n,mu,stdev)
mean1[i]=mean(help1)
median1[i]=median(help1)
}
k=sd(median1)/sd(mean1)
return(k)
}

CLT <-
function(fun,type,times,distribution,param,sleep,col,line.col,seed,main)
{
 set.seed(12345)
 old.par <- par(no.readonly = TRUE)
 on.exit(par(old.par))
 if(missing(fun))
  fun=mean                                                                                     #setting defaults
 if(missing(type))
  type="density"
 if(missing(times))
  times=500
 if(missing(distribution))
  stop("distribution(s) is (are) missing")
 if(missing(param))
  stop("param must be set as a list for given distributions")
 if(missing(sleep))
  sleep=0
 if(missing(line.col))
  line.col="red"
 if(missing(seed))
  seed=FALSE
 if(seed!=FALSE)
  set.seed(seed)                                                                               #set seed
 i=0;num=0;N=0                                                                                 #set counter, num = number of columns
 funVec=numeric(times);temp=numeric()                                                          #funVec = Solution to be plotted
 l=length(distribution)                                                                        #number of distributions
 
 Name=deparse(substitute(fun))
 if(Name=="mean")                                                                              #Names for used function fun
  funName=expression(bar(x))
 else
 {
  if(Name=="median")
   funName=expression(tilde(x))
  else
  {
   if(Name=="range")
    funName="R"
   else
   {
    if(Name=="sd")
     funName="s"
    else
    {
     if(Name=="min")
      funName=expression(x[min])
     else
     {
      if(Name=="max")
       funName=expression(x[max])
      else
       funName=deparse(substitute(fun))
     }
    }
   }
  }
 } 
                                                                            
 if(missing(col)) 
  color=rep("lightblue",l+1)                                                                   #setting default for histogram colors       
 else
 {
  color=rep("lightblue",l+1)
  color[1:length(col)]=col
 }                                                                           
  maine=numeric(l+1)                                                                           #setting default mains
  for(i in 1:l)
   {
    maine[i]=distribution[i]
   } 
  if(Name=="mean")                                                                           
   maine[l+1]=expression(bold(paste("Histogram of ",bar(x))))
  else
  {
   if(Name=="median")
    maine[l+1]=expression(bold(paste("Histogram of ",tilde(x))))
   else
   {
    if(Name=="range")
     maine[l+1]="Histogram of R"
    else
    {
     if(Name=="sd")
      maine[l+1]="Histogram of s"
     else
     {
      if(Name=="min")
       maine[l+1]=expression(bold(paste("Histogram of ",x[min])))
      else
      {
       if(Name=="max")
        maine[l+1]=expression(bold(paste("Histogram of ",x[max])))
       else
        maine[l+1]=paste("Histogram of ",deparse(substitute(fun)))
      }
     }
    }
   }
  }
 if(missing(main))
 {}
 else 
  maine[1:length(main)]=main 
                     
  if(l<=16)
   N=layout(mat=matrix(data=c(1:16,rep(17,times=16)),ncol=8,nrow=4))
  if(l<=12)
   N=layout(mat=matrix(data=c(1:12,rep(13,times=12)),ncol=6,nrow=4))
  if(l<=9)
   N=layout(mat=matrix(data=c(1:9,rep(10,times=9)),ncol=6,nrow=3))
  if(l<=6)
   N=layout(mat=matrix(data=c(1:6,rep(7,times=6)),ncol=6,nrow=2))
  if(l<=4)
   N=layout(mat=matrix(data=c(1:4,rep(5,times=4)),ncol=4,nrow=2))  
  if(l<=2)
   N=layout(mat=matrix(data=c(1:2,rep(3,times=2)),ncol=2,nrow=2))
  if(l==1)
   N=layout(mat=matrix(data=c(1,2),ncol=2,nrow=1)) 
   
 i=0
 for(i in 1:l)                                                                                 #Create Matrix and fill with NAs
  num=num+param[[i]]$n                                                                         #Get number of columns
 Mat=matrix(NA,ncol=num,nrow=times)                                                            #Create Matrix and fill with NAs
 i=0;j=0                                                                                       #reset counter
 for(i in 1:times)                                                                             #loop for rows
 {
  rest=1                                                                                       #reset to catch right column for next entry
  for(j in 1:l)                                                                                #loop for columns
  {
   Mat[i,(rest):(rest+param[[j]]$n-1)]=do.call(.charToDistfunc(distribution[j],                #Fill matrix with random distribution values
                                               type="r"),param[[j]])                                    
   rest=rest+param[[j]]$n                                                                      #new startpoint for the next entry
  }
 }
 funVec=apply(Mat,1,fun)                                                                       #use fun function for every row
 forplot=hist(funVec,plot=FALSE)                                                               #get xlim and ylim
 i=0                                                                                           #reset counter
 
 for(i in 1:times)                                                                             #loop for the plot
 { 
  if(type=="density")
  {
   rest=1
   for(j in 1:l)
   {
    temp=Mat[,(rest):(rest+param[[j]]$n-1)]
    hist(temp,freq=FALSE,main=maine[j],col=color[j],xlab=paste("n=",param[[j]]$n))
    rest=rest+param[[j]]$n
   }
   k=0
   if(l!=(N-1))                                                                                #fill missing plot-window
    {
     for(k in 1:(N-l-1))
      plot(1,col="white",axes=FALSE,xlab="",ylab="")
    }         
    hist(funVec[1:i],xlim=c(min(forplot$breaks),max(forplot$breaks)),                          #plot main histogram
         ylim=c(0,max(forplot$density)+0.2*max(forplot$density)),
         breaks=forplot$breaks,freq=FALSE,col=color[j+1],main=maine[j+1],xlab=funName)
    lines(density(c(0,funVec[1:i])),col=line.col)
    legend("topright", legend=c(c(expression(bold("Value:")),i),                               #creates legend
          c(expression(bold("Times:")),times),
          c(expression(bold("Distributions:")),distribution)))
    Sys.sleep(sleep)
    j=0
    } 
  if(type=="counts")
  {
   rest=1
   for(j in 1:l)
   {
    temp=Mat[,(rest):(rest+param[[j]]$n-1)]
    hist(temp,main=maine[j],col=color[j],xlab=paste("n=",param[[j]]$n))
    rest=rest+param[[j]]$n
   }
   k=0
   if(l!=(N-1))                                                                                #fill missing plot-window
    {
     for(k in 1:(N-l-1))
      plot(1,col="white",axes=FALSE,xlab="",ylab="")
    }         
    hist(funVec[1:i],xlim=c(min(forplot$breaks),max(forplot$breaks)),                          #plot main histogram
         ylim=c(0,max(forplot$counts)+0.2*max(forplot$counts)),
         breaks=forplot$breaks,freq=TRUE,col=color[j+1],main=maine[j+1],xlab=funName)
    legend("topright", legend=c(c(expression(bold("Value:")),i),                               #creates legend
          c(expression(bold("Times:")),times),
          c(expression(bold("Distributions:")),distribution)))
    Sys.sleep(sleep)
    j=0
    } 
 }
 invisible(list(Mat,funVec))
}

CLT(fun=mean,times=100,distribution=c("normal","weibull",
    "gamma","normal","beta"),
    param=list(list(mean=0,sd=0.01,n=100),list(shape=1,scale=3,n=100),
    list(n=100,shape=0.1),list(mean=2,sd=0.1,n=100),
    list(n=100,shape1=1,shape2=2)),
    seed=123,col=c(rep("grey",5),"green"),sleep=0) 
	dev.off()
	img( pngfile, Rd = TRUE )
}
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