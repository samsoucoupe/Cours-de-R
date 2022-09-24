sample(6, size=1)
sample(1:6, size=1)
sample(c(1, 2, 3, 4, 5, 6), size=1)

sample(100:200,size=1)

RandPair<- function(n) {
  return(2*sample(n%/%2, size = 1))
}

RandPair_corrige<-function(n){
  return(2*sample(n/2,size=1))
}

RandPair(10)
RandPair_corrige(10)

MonteCarlo<-function(){
  return(sample(c(2,3,5),size=1))
}

MonteCarlo()

LasVegas<-function(){
  return(sample(c(2,3,3,5,5,5),size=1))
}


table(replicate(1000, LasVegas()))

RandChiffres2<- function(n) {
  return(sample(0:9, size = n))
}
RandChiffres2(5)

RandChiffres<- function(n) {
  return(sample((1*(10**n-1)):(1*10**(n)-1), size = 1))
}

RandChiffres(2)