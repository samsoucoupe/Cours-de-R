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

Max2_vect<-function(x){
  M<-x[1]
  M2<-x[2]
  if (M<M2){M<-x[2];M2<-x[1]}
    for (i in 3:length(x)){
        if (x[i]>M){ M2<-M; M<-x[i] }
        else if (x[i]>M2){ M2<-x[i] }
    }
        return(c(M,M2))
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
}






