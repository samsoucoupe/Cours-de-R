#ecrire un code qui donne le n-ieme chiffre significatif d'un nombre en base donnée





#code

convertiseur_nat<-function(n,base){
  a<-c()
  while(n>0){
    a<-c(n%%base,a)
    n<-n%/%base
  }
  return(a)
}

#convertiseur decimal
convertiseur_dec<-function(n,base){

  a<-c()
  while(n>0){
    x<-n*base
    a<-c(a,floor(x))
    n<-x-floor(x)
  }
  #verifie vect de longeur null
  if(length(a)==0){
    return(numeric(0))
  }

  return(a)
}

#donne le n-ieme chiffre significatif d'un nombre en base donnée et si pas possible donne NA
NTHDIG <- function(x,base,n){

  if (x==0 && n!=1) return(NA)
  if(x==0 && n==1) return(0)
  x<-abs(x)
  if (x<1) {
    a<-convertiseur_dec(x,base)
    #vire les 0 du debut
    while(a[1]==0){
      a<-tail(a,-1)
    }

    if (length(a)<n) return(NA)
    return(a[n])
  }
  if (x>=1) {
    a<-convertiseur_nat(floor(x),base)
    b<-convertiseur_dec(x-floor(x),base)

    #vire les 0 du debut si a est vide
    if(length(a)==0){
      while(b[1]==0){
        b<-tail(b,-1)
      }}
    c<-c(a,b)
    if (length(c)<n) return(NA)
    return(c[n])

  }
}

#test


stream<-file("stdin","r")
x<-scan(stream,what="numeric",quiet = TRUE)
close(stream)

nbr<-x[1]
base<-x[2]
n<-x[3]

nbr<-as.numeric(nbr)
base<-as.numeric(base)
n<-as.numeric(n)


for (i in 4:(nbr+3)){
  d<-as.numeric(x[i])

  cat(NTHDIG(d,base,n),"\n")
}

#test
stopifnot(is.na(NTHDIG(0,2,1)))












