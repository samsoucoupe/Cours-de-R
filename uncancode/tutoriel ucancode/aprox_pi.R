serie <- function(x){
  res <- 0
  for (i in 0:x){
    res <- res + (4*((-1)**i))/((2*i)+1)
  }
  return (res)
}


#utilisé jusqu'a 1900
historique <- function(x){
  res <- 0
  for (i in 0:x){
  
   res <- res+((12**(1/2))*((-1)**i))/((2i+1)*(3**i))
  }
  return(res)
}
#NON FONCTIONNEL




somme_inf <- function(x){
  res <- 0
  for (i in 0:x){
    res <- res+ (((-1)**i)/((2*i)+1))
  }
  return(4*res)
}
# recursive
math <- function(n){
  A <- 1
  B <- 1/(2**(1/2))
  C <- 1/4
  for (i in 0:n){
    A1 <- (A+B)/2 
    B1 <- (A*B)**(1/2)
    C1 <- C-(2**i)(((A-B)/2)**2)
    A <- A1
    B <- B1
    C <- c1
  }  
  return((((A+B)/2)**2)/C)
}
  
  
  
  
  