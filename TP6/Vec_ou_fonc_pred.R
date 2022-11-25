#Programmez une fonction NoMult(k,a,b) retournant le vecteur des entiers non multiples de k dans l’intervalle [a,b]. Essayez de programmer une fonction sans boucle, en la vectorisant ou utilisant la fonction filter.

NoMult<-function (mult,start,end){
  x<-seq(start,end)
    return(x[x%%mult!=0])
}

NoMult(5,10,30)
NoMult(2,11,31)

PGCD <- function(a,b) {
  while ( b != 0 ) {
    tmp<- a %% b
    a <- b
    b <-tmp
  }
  return(a);
}
CoPremiers <- function(k,a,b) {
  x<-a:b
  x<-Filter(function(x) PGCD(k,x)==1,x)
  return(x)

}

CoPremiers(15,10,30)