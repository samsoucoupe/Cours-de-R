PiEuler<-function(n){
  x<-1
  for(i in 1:n){
    x<-x+(1/i**2)
  }
  return(x)
}

cat(sprintf('Calcul approximatif de pi=%.16f :\n',pi))
k <- 1
while (k <= 7){
  n <- 10**k
  cat(sprintf("PiEuler(%d) --> %.10f\n",n, PiEuler(n)))
  k <- k+1
}