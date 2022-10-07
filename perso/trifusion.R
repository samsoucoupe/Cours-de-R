
trifusion<-function(x){
  if(length(x)==1) return(x)
  else{
    n<-length(x)
    m<-ceiling(n/2)
    y<-trifusion(x[1:m])
    z<-trifusion(x[(m+1):n])
    return(fusion(y,z))
  }
}

fusion<-function(x,y){
  if(length(x)==0) return(y)
  else if(length(y)==0) return(x)
  else{
    if(x[1]<y[1]) return(c(x[1],fusion(x[-1],y)))
    else return(c(y[1],fusion(x,y[-1])))
  }
}
perftrifusion<-function(n){
  x<-sample(1:n)
  system.time(trifusion(x))
}

perftrifusion(10)
perftrifusion(100)
perftrifusion(1000)
perftrifusion(10000)