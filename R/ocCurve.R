ocCurve <-
function(n,sig.level,alternative,type,distribution,col,xlim,ylim,...)
{
 old.par <- par(no.readonly = TRUE)
 on.exit(par(old.par))
 if(missing(n))
  n=2
 if(missing(sig.level))
  sig.level=0.05
 if(missing(alternative))
  alternative="two.sided"
 if(missing(type))
  type="two.sample"
 if(missing(distribution))
  distribution="t"
 if(missing(col))
  col=rep(1,length(n))
 if(length(col)!=length(n))
  col=c(col,rep(1,(length(n)-length(col))))
 if(missing(xlim))
  xlim=c(0,3)
 if(missing(ylim))
  ylim=c(0,1)

 beta=matrix(ncol=100,nrow=length(n))
 d=seq(min(xlim),max(xlim),length=100);temp=numeric(length(n))
 plot(0,0,xlim=xlim,ylim=ylim,col="white",xlab=expression(d==frac(abs(~mu[1]-mu[0]),sigma)),
      ylab=expression(beta),bty="n",axes=FALSE,...)
 axis(1)
 if(alternative=="one.sided")
 {
  axis(2,at=c(0,0.2,0.4,0.6,0.8,1,1-sig.level))
  abline(h=1-sig.level,col="gray")
 }
 if(alternative=="two.sided")
 {
  axis(2,at=c(0,0.2,0.4,0.6,0.8,1,1-sig.level/2))
  abline(h=1-sig.level/2,col="gray")
 }
 for(i in 1:length(n))
 {
 for(j in 1:100)
 {
  if(distribution=="t")
   beta[i,j]=1-power.t.test(n=n[i],delta=d[j],sd=1,sig.level=sig.level,alternative=alternative,type=type)$power
 }
 if(i%%2==0)
  temp[i]=power.t.test(n=n[i],power=0.825,sd=1,sig.level=sig.level,alternative=alternative,type=type)$delta
 else
  temp[i]=power.t.test(n=n[i],power=0.775,sd=1,sig.level=sig.level,alternative=alternative,type=type)$delta
 lines(d,beta[i,],col=col[i])
 if(i%%2==0)
  text(temp[i],0.175,n[i],cex=0.75)
 else
  text(temp[i],0.225,n[i],cex=0.75)
 }
 return(beta)
}

