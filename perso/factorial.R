

F<-function(n){
  dic<-c(rep(0,n))
  Fac<-function(n){
    if (dic[n]!=0) return(dic[n])
    if (n==0) return(1)
    if (n==1) return(1)
    dic[n]<-n*Fac(n-1)
    return(dic[n])
  }
return(Fac(n))
}


F(500)