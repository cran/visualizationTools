LLN <-
function(n,fun=mean,distr,param,sleep,main,type,xlab,ylab,...)
{
 if(missing(n))
  n=1000
 if(missing(distr))
  stop("distr is missing")
 if(missing(param))
  stop("param is missing")
 if(missing(sleep))
  sleep=0
 if(missing(main))
  main="Visualization: Law of Large Numbers"
 if(missing(type))
  type="l"
 if(missing(xlab))
  xlab="n"
 if(missing(ylab))
  {
   if(identical(deparse(substitute(fun)),"mean"))
     ylab=expression(bar(x))
    else
    {
     if(identical(deparse(substitute(fun)),"median"))
      ylab=expression(tilde(x))
     else
     {
      if(identical(deparse(substitute(fun)),"max"))
       ylab=expression(x[max])
      else
      {
       if(identical(deparse(substitute(fun)),"min"))
        ylab=expression(x[min])
       else
       {
        if(identical(deparse(substitute(fun)),"sd"))
         ylab=expression(s)
        else
        {
         if(identical(deparse(substitute(fun)),"range"))
          ylab=expression(R)
         else
          ylab=deparse(substitute(fun))
        }
       }
      }
     }
    }
   }

 value=numeric(n);temp=numeric(n)
 param$n=1
 for(i in 1:n)
 {
  temp[i]=do.call(.charToDistfunc(distr,type="r"),param)
  value[i]=fun(temp[2:i])
 }
 for(i in 1:n)
 {
  plot(value[1:i],xlim=c(0,n),main=main,type=type,xlab=xlab,ylab=ylab,...)
  if(identical(deparse(substitute(fun)),"mean"))
  {
   if(identical(distr,"beta"))
    abline(h=param[[1]]/(param[[1]]+param[[2]]),lty=2)                           #cauchy hat keinen Erwartungswert!
   if(identical(distr,"chi-squared"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"exponential"))
    abline(h=1/param[[1]],lty=2)
   if(identical(distr,"f"))
    abline(h=param[[2]]/(param[[2]]-2),lty=2)
   if(identical(distr,"log-normal"))
    abline(h=exp(param[[1]]+((param[[2]])^2)/2),lty=2)
   if(identical(distr,"logistic"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"negative binomial"))
   {
    if(identical(names(param[2]),"prob"))
     abline(h=(param[[1]]*(1-param[[2]]))/param[[2]],lty=2)
    if(identical(names(param[2]),"mu"))
     abline(h=param[[2]],lty=2)
   }
   if(identical(distr,"normal"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"poisson"))
    abline(h=param[[1]],lty=2)
   if(identical(distr,"t"))
    abline(h=0,lty=2)
   if(identical(distr,"weibull"))
    abline(h=param[[2]]*gamma(1+1/param[[1]]),lty=2)
   if(identical(distr,"gamma"))
   {
    if(identical(names(param[2]),"rate"))
     abline(h=param[[1]]/param[[2]],lty=2)
    if(identical(names(param[2]),"scale"))
     abline(h=param[[1]]*param[[2]],lty=2)
   }
   if(identical(distr,"unif"))
    abline(h=(param[[1]]+param[[2]])/2,lty=2)
  }

  if(identical(deparse(substitute(fun)),"sd"))
  {
   if(identical(distr,"beta"))
    abline(h=sqrt((param[[1]]*param[[2]])/((param[[1]]+param[[2]]+1)*(param[[1]]+param[[2]])^2)),lty=2)                           #cauchy hat keinen Erwartungswert!
   if(identical(distr,"chi-squared"))
    abline(h=sqrt(2*param[[1]]),lty=2)
   if(identical(distr,"exponential"))
    abline(h=1/param[[1]],lty=2)
   if(identical(distr,"f"))
    abline(h=sqrt((2*(param[[2]]^2)*(param[[1]]+param[[2]]-2))/(param[[1]]*((param[[2]]-2)^2)*(param[[2]]-4))),lty=2)
   if(identical(distr,"log-normal"))
    abline(h=sqrt(exp(2*param[[1]]+param[[2]]^2)*(exp(param[[2]]^2)-1)),lty=2)
   if(identical(distr,"logistic"))
    abline(h=sqrt(((param[[2]])^2*(pi)^2)/3),lty=2)
   if(identical(distr,"negative binomial"))
    abline(h=sqrt((param[[1]]*(1-param[[2]]))/(param[[2]]^2)),lty=2)
   if(identical(distr,"normal"))
    abline(h=param[[2]],lty=2)
   if(identical(distr,"poisson"))
    abline(h=sqrt(param[[1]]),lty=2)
   if(identical(distr,"t"))
    abline(h=param[[1]]/(param[[1]]-2),lty=2)
   if(identical(distr,"weibull"))
    abline(h=sqrt((param[[2]])^2*(gamma(1+2/param[[1]])-gamma(1+1/param[[1]])^2)),lty=2)
   if(identical(distr,"gamma"))
   {
    if(identical(names(param[2]),"rate"))
     abline(h=sqrt(param[[1]]/param[[2]]^2),lty=2)
    if(identical(names(param[2]),"scale"))
     abline(h=sqrt(param[[1]]*param[[2]]^2),lty=2)
   }
   if(identical(distr,"unif"))
    abline(h=(param[[2]]-param[[1]])/(2*sqrt(3)),lty=2)
  }
  Sys.sleep(sleep)
 }

return(list(temp,value))
}

