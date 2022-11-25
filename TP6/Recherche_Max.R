#Programmez la fonction Max2(a, b) prenant en argument deux nombres a et b, et retournant le maximum de ces deux nombres.
#Programmez la fonction Max3(a, b, c) prenant en argument trois nombres a, b et c, et retournant le maximum de ces trois nombres.

Max2<-function(a, b) {
  if (a > b) {
    return(a)
  } else {
    return(b)
  }
}

Max3<-function(a,b,c){
if (a>b){
if (a>c){ return(a) }
else { return(c) }}
else {
if (b>c){ return(b) }
else { return(c) }
}
}


#Programmez la fonction MaxF(x) prenant un vecteur numérique x et retournant le maximum de x.
MaxF<-function(x){
  M<-x[1]
  for (i in 2:length(x)){
    if (x[i]>M){ M<-x[i] }
  }
    return(M)
}


#Programmez la fonction MaxFrom(x,i) prenant un vecteur numérique x et retournant le maximum de x à partir de l’indice i inclus.


MaxFrom<-function(x,i){
  M<-x[i]
  for (j in i:length(x)){

    if (x[j]>M){ M<-x[j] }
  }
    return(M)
}


Max_From_predef<-function(x,i){
  return(max(tail(x,i)))
}




MaxAndIdx<-function(x){
  index<-1

  for (i in 1:length(x)){
    if (x[index]<x[i]){index<-i}
  }
  return (c(x[index],index))
}

MaxAndIdx2 <- function(x) c(max(x), which.max(x))

Max2_vect<-function(x){
  #max3(x[1],x[2],x[3]) renvoi les 2 plus grandes valeurs
  max3<-function(a,b,c){
    if (a>b){
      if (a>c){return(c(a,Max2(b,c)))}
      else {return(c(c,a))}
    }
    else {
      if (b>c){return(c(b,c))}
      else {return(c(c,b))}
    }


    }
    #max2(x[1],x[2]) renvoi la plus grande valeur
  if(length(x)==0){return(c("-Inf","-Inf"))}
  if (length(x)==1){return(c(x[1],"-Inf"))}
  if (length(x)==2){
    if (x[1]>x[2]){return(c(x[1],1))}
    else {return(c(x[2],2))}
  }
    M<-c(max(x[1],x[2]),min(x[1],x[2]))
    index<-1

    while(index<=length(x)){
      M<-max3(M[1],M[2],x[index])
      index<-index+1

    }
    return(M)
}


Kmax <- function(x, k) {
  k <- max(0,k)
  return( head( sort(x, decreasing = TRUE), k) )
}


#Test des fonction MAx

if (TRUE)
{ #Test de Max2
  Max2(1, 2)
  Max2(2, 1)
  Max2(1, 1)

  #Test de Max3
  Max3(1, 2, 3)
  Max3(1, 3, 2)
  Max3(3, 1, 2)
  Max3(3, 2, 1)
  Max3(2, 3, 1)
  Max3(2, 1, 3)
  Max3(1, 1, 1)

  #Test de MaxF
  MaxF(c(1, 2, 3))
  MaxF(c(1, 3, 2))
  MaxF(c(3, 1, 2))
  MaxF(c(3, 2, 1))
  MaxF(c(2, 3, 1))
  MaxF(c(2, 1, 3))
  MaxF(c(1, 1, 1))


  #Test de MaxFrom
  MaxFrom(c(1, 2, 3, 4, 5), 2)
  MaxFrom(c(1, 2, 3, 4, 5), 3)
  MaxFrom(c(1, 2, 3, 4, 5), 4)
  MaxFrom(c(1, 2, 3, 4, 5), 5)

  MaxFrom(c(1, 2, 3, 4, 5), 1)
  MaxFrom(c(5, 4, 3, 2, 1), 2)


  #MAxFrom predef
  Max_From_predef(c(1,2,3,4,5,6,7,8,9,10),5)
  Max_From_predef(c(10,9,8,7,6,5,4,3,2,1),5)

    #MAxAndIdx
  x <- runif(6);
  print(x)
  print(MaxAndIdx(x))
  cat("Max2_vect\n")
  print(Max2_vect(c()))
  x <- runif(6);
  print(x)
  print(Max2_vect(x))
  cat("KMAX\n")
  x <- sample(1:100, 6, replace = TRUE)
  print(x)
  print(Kmax(x, 0))
  print(Kmax(x, 2))
  print(Kmax(x, 4))
  print(Kmax(x, 7))

}






