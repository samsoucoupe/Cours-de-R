Tranche <- function(s, b, h, p) {
  if (s < b) {
    return(0)
  }
  else {
    if (s < h) {
      return((s - b) * (p / 100))
    }
    return((h - b) * (p / 100))
  }
}

arrondi <- function(x) {
  x<-x*100
  x<-x - x %% 1
  x<-x/100
  return(x)

}

Impot <- function(s) {
  liner_max<-25000
  hh <- Tranche(s, 8000, 25000, 10) #au dessus de huit mille
  if(s>liner_max){
    diffhvc <- s - liner_max #audessus de 25000
  }
  else{diffhvc<-0}
  hh <- hh + (diffhvc * (20 / 100)) #au dessus de vingt cinq mille
  return(arrondi(hh))
}

Tranche(1500, 2000, 3000, 10)
Tranche(2500, 2000, 3000, 10)
Tranche(4000, 2000, 3000, 10)
Impot(40005.69)