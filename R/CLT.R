CLT <-
function(fun,type,times,distribution,param,sleep,col,line.col,seed,main)
{
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

