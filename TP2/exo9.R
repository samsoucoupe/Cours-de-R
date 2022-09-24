precision<-1/2**18
f<-function(x){
  sin(x)/sqrt(x**4+1)
}
derivéedef<-function(x){
  return((f(x+precision)-f(x))/precision)
}
derivésecondedef<-function(x){
  return((derivéedef(x+precision)-derivéedef(x))/precision)
}

derivésecondedef(sqrt(2))