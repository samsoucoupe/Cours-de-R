####################################################
## ÉCRIRE VOTRE CODE ICI
#####################################################

entre_10<- c(18,10,0.00000000,0.50000000,1.00000000,2048.00000000,0.00781250,0.50781250,1.00781250,2048.00781250,2147483647.00000000,-0.00000000,-0.50000000,-1.00000000,-2048.00000000,-0.00781250,-0.50781250,-1.00781250,-2048.00781250,-2147483647.00000000)
sortie_10<-c(0,5,1,2,7,5,1,2,2,0,5,1,2,7,5,1,2,2)

entre_16<-c(3,16,10,163,179)
sortie_16<-c(10,10,11)
FirstDigit <- function(n,base){
  if  (n==0) return(0)
  n<-abs(n)
  while (!(n<=base && n>=1)){

    if (n<1) n<-n*base
    if (n>base) n<-n/base

  }
  n<-as.integer(n)
  return(n)

}


FirstDigit_vec<-function(vect_entree,base){
  vect_sortie<-c()
  for (i in 1:length(vect_entree)){
    vect_sortie<-c(vect_sortie,FirstDigit(vect_entree[i],base))
  }
  return(vect_sortie)
}

####################################################
## SCRIPT PRINCIPAL À NE PAS MODIFIER
####################################################

## Lire un vecteur (par défaut d'entiers) sur l'entrée standard.
## Capturer le flux d'entrée standard.
#stream <- file("stdin","r")
## Lire le flux jusqu'à la fin dans un vecteur.
#x <- scan(file=stream, what= numeric())
## Ferme proprement le flux (pour éviter un warning).
#close(stream)
x<-entre
## On boucle sur les cas de test
#for(n in tail(x, -2)) {
#  cat(FirstDigit(n, x[2]), "\n", sep = "")
#}
test_vect<-function(a){
  cat(a,"\n",sep=" ")
  cat(FirstDigit_vec(a[3:length(a)], a[2]), "\n", sep = " ")
}

test_vect(entre_10)
test_vect(entre_16)

